# ==============================================================================
# Module L: Logging, Error Handling, Testing Utilities
# ==============================================================================
# Structured logging, context-rich errors/warnings, column/type assertions,
# and dataset snapshotting for QC.
# ==============================================================================

# ---- log_info() / log_warn() / log_error() -----------------------------------
#' Structured logging functions
#'
#' @description
#' Emit structured log messages with timestamp, module, severity, and context.
#' Routes to the active [log_sink] or console.
#'
#' @param message Character. Log message (supports `{glue}` syntax).
#' @param ... Named values for glue interpolation.
#' @param module Character. Originating module. Default `NA`.
#' @param domain Character. Domain context. Default `NA`.
#' @param .sink `log_sink` or `NULL`. Target sink.
#'
#' @return Invisibly returns `NULL`.
#'
#' @name logging
#' @export
log_info <- function(message, ..., module = NA_character_,
                     domain = NA_character_, .sink = NULL) {
  .emit_log("INFO", message, ..., module = module, domain = domain,
            .sink = .sink)
}

#' @rdname logging
#' @export
log_warn <- function(message, ..., module = NA_character_,
                     domain = NA_character_, .sink = NULL) {
  .emit_log("WARN", message, ..., module = module, domain = domain,
            .sink = .sink)
}

#' @rdname logging
#' @export
log_error <- function(message, ..., module = NA_character_,
                      domain = NA_character_, .sink = NULL) {
  .emit_log("ERROR", message, ..., module = module, domain = domain,
            .sink = .sink)
}

#' Internal log emission
#' @keywords internal
.emit_log <- function(level, message, ..., module, domain, .sink) {
  ts <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  msg <- glue::glue(message, ..., .envir = parent.frame(2))
  entry <- list(
    timestamp = ts,
    level     = level,
    module    = module,
    domain    = domain,
    message   = as.character(msg)
  )

  if (!is.null(.sink) && inherits(.sink, "log_sink")) {
    if (.sink$levels[[level]] >= .sink$threshold) {
      .sink$entries <- c(.sink$entries, list(entry))
      if (!is.null(.sink$file)) {
        cat(paste(ts, level, msg, "\n"), file = .sink$file, append = TRUE)
      }
    }
  }

  switch(level,
         "INFO"  = cli::cli_alert_info("{ts} [{level}] {msg}"),
         "WARN"  = cli::cli_alert_warning("{ts} [{level}] {msg}"),
         "ERROR" = cli::cli_alert_danger("{ts} [{level}] {msg}"))

  invisible(NULL)
}

# ---- stop_with_context() / warn_with_context() -------------------------------
#' Error and warning with rich context
#'
#' @description
#' Throws an error or warning that includes contextual information:
#' domain, variable, rule_id, and a data snippet for debugging.
#'
#' @param message Character. Error message.
#' @param domain Character or `NA`.
#' @param variable Character or `NA`.
#' @param rule_id Character or `NA`.
#' @param data Tibble or `NULL`. A small data slice for context.
#' @param call Environment. The calling environment. Default `caller_env()`.
#'
#' @name context_errors
#' @export
stop_with_context <- function(message,
                              domain = NA_character_,
                              variable = NA_character_,
                              rule_id = NA_character_,
                              data = NULL,
                              call = rlang::caller_env()) {
  ctx <- list(domain = domain, variable = variable, rule_id = rule_id)
  if (!is.null(data)) {
    ctx$data_snippet <- utils::head(data, 5)
  }
  rlang::abort(
    message = paste0("[", domain, ".", variable, "] ", message),
    class = "sdtmbuilder_error",
    context = ctx,
    call = call
  )
}

#' @rdname context_errors
#' @export
warn_with_context <- function(message,
                              domain = NA_character_,
                              variable = NA_character_,
                              rule_id = NA_character_,
                              data = NULL,
                              call = rlang::caller_env()) {
  ctx <- list(domain = domain, variable = variable, rule_id = rule_id)
  if (!is.null(data)) {
    ctx$data_snippet <- utils::head(data, 5)
  }
  rlang::warn(
    message = paste0("[", domain, ".", variable, "] ", message),
    class = "sdtmbuilder_warning",
    context = ctx,
    call = call
  )
}

# ---- assert_cols() -----------------------------------------------------------
#' Assert that required columns exist in a data frame
#'
#' @description
#' Checks that all specified column names are present in the tibble.
#' Throws an informative error listing missing columns if any are absent.
#'
#' @param data Data frame or tibble.
#' @param cols Character vector. Required column names.
#' @param context Character. Context string for error message.
#'   Default `""`.
#'
#' @return Invisibly returns `TRUE`.
#'
#' @section Unit tests:
#' - All columns present → pass
#' - Missing column → error listing missing names
#' - Empty cols → pass
#' - NULL data → error
#'
#' @export
assert_cols <- function(data, cols, context = "") {
  checkmate::assert_data_frame(data)
  missing <- setdiff(cols, names(data))
  if (length(missing) > 0L) {
    msg <- glue::glue(
      "Missing required column(s): {paste(missing, collapse = ', ')}",
      if (nzchar(context)) " [{context}]" else ""
    )
    abort(msg, class = "sdtmbuilder_assertion")
  }
  invisible(TRUE)
}

# ---- assert_types() ----------------------------------------------------------
#' Assert that columns have expected types
#'
#' @description
#' Validates that specified columns are of the expected R types
#' (character, numeric, integer, logical, Date, POSIXct).
#'
#' @param data Data frame.
#' @param type_map Named character vector. Names = column names,
#'   values = expected types (`"character"`, `"numeric"`, `"integer"`,
#'   `"logical"`, `"Date"`, `"POSIXct"`).
#' @param context Character.
#'
#' @return Invisibly returns `TRUE`.
#'
#' @section Unit tests:
#' - Correct types → pass
#' - Wrong type → error listing mismatches
#' - Column missing → error (delegates to assert_cols)
#'
#' @export
assert_types <- function(data, type_map, context = "") {
  assert_cols(data, names(type_map), context)

  mismatches <- character()
  for (col in names(type_map)) {
    expected <- type_map[[col]]
    actual <- class(data[[col]])[1]
    # Flexible matching
    ok <- switch(expected,
                 "character" = is.character(data[[col]]),
                 "numeric"   = is.numeric(data[[col]]),
                 "integer"   = is.integer(data[[col]]),
                 "logical"   = is.logical(data[[col]]),
                 "Date"      = inherits(data[[col]], "Date"),
                 "POSIXct"   = inherits(data[[col]], "POSIXct"),
                 FALSE)
    if (!ok) {
      mismatches <- c(mismatches,
                      glue::glue("{col}: expected {expected}, got {actual}"))
    }
  }

  if (length(mismatches) > 0L) {
    msg <- paste("Type assertion failed:", paste(mismatches, collapse = "; "))
    if (nzchar(context)) msg <- paste0(msg, " [", context, "]")
    abort(msg, class = "sdtmbuilder_assertion")
  }

  invisible(TRUE)
}

# ---- snapshot_dataset() ------------------------------------------------------
#' Save a QC snapshot of a dataset
#'
#' @description
#' Saves the current state of a dataset as an RDS file for audit trail /
#' debugging purposes.  Includes metadata about the snapshot context.
#'
#' @param data Tibble.
#' @param label Character. Descriptive label (e.g., `"ae_after_ct"`,
#'   `"lb_pre_finalize"`).
#' @param outdir Character. Snapshot directory. Default `"snapshots/"`.
#' @param domain Character or `NA`.
#' @param step Character or `NA`. Build step identifier.
#'
#' @return Invisibly returns the snapshot file path.
#'
#' @section Unit tests:
#' - Snapshot saved and readable
#' - Metadata attributes preserved
#' - Directory created if missing
#'
#' @export
snapshot_dataset <- function(data,
                             label,
                             outdir = "snapshots/",
                             domain = NA_character_,
                             step = NA_character_) {
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

  ts <- format(Sys.time(), "%Y%m%dT%H%M%S")
  fname <- file.path(outdir,
                     paste0("snap_", label, "_", ts, ".rds"))

  snapshot <- list(
    data      = data,
    label     = label,
    domain    = domain,
    step      = step,
    nrow      = nrow(data),
    ncol      = ncol(data),
    timestamp = Sys.time()
  )

  saveRDS(snapshot, fname)
  log_info("Snapshot saved: {fname}", module = "util")
  invisible(fname)
}
