# ==============================================================================
# Module A: Package Primitives — S3 Classes and Constructors
# ==============================================================================
# These classes define the core data structures that flow through every other
# module.  Each constructor validates inputs and returns a typed list.
# ==============================================================================

# ---- new_sdtm_config() ------------------------------------------------------
#' Create a validated SDTM study configuration object
#'
#' @description
#' Constructs a configuration object that encapsulates all study-level settings
#' needed by the build pipeline: study identity, timezone, reference start date
#' rules, visit mapping, controlled terminology paths, sponsor conventions,
#' imputation policies, and logging preferences.
#'
#' **Does NOT** load data or metadata—only stores settings.
#'
#' @param studyid Character (required). The study identifier, e.g. `"STUDY001"`.
#' @param timezone Character. IANA timezone string, default `"UTC"`.
#' @param ref_start_rule List (required). How to determine RFSTDTC.
#'   Must contain `var` (character) and `source` (character), e.g.
#'   `list(var = "RFSTDTC", source = "DM")` or a function.
#' @param visit_map Data frame or `NULL`. Columns: `VISIT`, `VISITNUM`,
#'   `VISITDY` (optional), `WINDOW_LO`, `WINDOW_HI` (optional).
#' @param ct_paths Character vector. Paths to controlled terminology files
#'   (CDISC CT packages, sponsor extensions).
#' @param sponsor_overrides Named list. Domain or variable-level overrides:
#'   custom labels, lengths, sort keys, derivation exceptions.
#' @param imputation_policy List or function. Partial-date imputation rules.
#'   Example: `list(default_day = 1L, default_month = 1L, method = "first")`.
#' @param log_level Character. One of `"DEBUG"`, `"INFO"`, `"WARN"`, `"ERROR"`.
#'   Default `"INFO"`.
#' @param legacy_quirks Named list. Backward-compat flags, e.g.
#'   `list(seq_zero_based = FALSE, blank_as_na = TRUE)`.
#' @param create_supp Logical. Default `TRUE`. When `FALSE`, supplemental
#'   qualifier variables are kept in the main domain instead of being
#'   split into a SUPP-- dataset. `build_domain()` uses this value
#'   unless explicitly overridden.
#'
#' @return An S3 object of class `sdtm_config` (a named list).
#'
#' @section Edge cases:
#' - `timezone` must be valid (`OlsonNames()`); error if not.
#' - `ref_start_rule` must be a list with `var` + `source`, or a function.
#' - `visit_map` is validated for required columns if provided.
#' - Unknown `sponsor_overrides` keys emit a warning but are kept.
#'
#' @section Unit tests:
#' - Required `studyid` missing → error
#' - Invalid timezone → error
#' - Default values populated correctly
#' - Override precedence: sponsor > default
#' - `visit_map` with wrong columns → error
#' - `imputation_policy` as function and as list both work
#'
#' @export
new_sdtm_config <- function(studyid,
                            timezone = "UTC",
                            ref_start_rule,
                            visit_map = NULL,
                            ct_paths = character(),
                            sponsor_overrides = list(),
                            imputation_policy = list(),
                            log_level = "INFO",
                            legacy_quirks = list(),
                            create_supp = TRUE) {
  # --- input validation -------------------------------------------------------
  checkmate::assert_string(studyid, min.chars = 1L)

  checkmate::assert_choice(timezone, OlsonNames())
  checkmate::assert_choice(log_level, c("DEBUG", "INFO", "WARN", "ERROR"))

  if (is.list(ref_start_rule)) {
    # Accept new format (sdtm_domain/sdtm_variable/raw_dataset/raw_variable)
    # or legacy format (var/source)
    has_new   <- any(c("sdtm_domain", "sdtm_variable", "sdtm_path", "raw_dataset", "raw_variable") %in% names(ref_start_rule))
    has_legacy <- all(c("var", "source") %in% names(ref_start_rule))
    if (!has_new && !has_legacy) {
      abort("`ref_start_rule` list must contain 'sdtm_domain'/'sdtm_variable'/'sdtm_path'/'raw_dataset'/'raw_variable' or legacy 'var'/'source' elements.")
    }
  } else if (!is.function(ref_start_rule)) {
    abort("`ref_start_rule` must be a named list or a function.")
  }

  if (!is.null(visit_map)) {
    checkmate::assert_data_frame(visit_map)
    required_vm_cols <- c("VISIT", "VISITNUM")
    missing_vm <- setdiff(required_vm_cols, names(visit_map))
    if (length(missing_vm) > 0L) {
      abort(glue("visit_map missing required columns: {paste(missing_vm, collapse = ', ')}"))
    }
  }

  checkmate::assert_character(ct_paths)

  # --- construct --------------------------------------------------------------
  obj <- list(
    studyid            = studyid,
    timezone           = timezone,
    ref_start_rule     = ref_start_rule,
    visit_map          = visit_map,
    ct_paths           = ct_paths,
    sponsor_overrides  = sponsor_overrides,
    imputation_policy  = imputation_policy,
    log_level          = log_level,
    legacy_quirks      = legacy_quirks,
    create_supp        = create_supp,
    .created_at        = Sys.time()
  )

  structure(obj, class = "sdtm_config")
}

#' @export
print.sdtm_config <- function(x, ...) {
  cli::cli_h1("SDTM Configuration")
  cli::cli_li("Study: {x$studyid}")
  cli::cli_li("Timezone: {x$timezone}")
  cli::cli_li("Log level: {x$log_level}")
  cli::cli_li("CT paths: {length(x$ct_paths)} file(s)")
  cli::cli_li("Visit map: {if (is.null(x$visit_map)) 'none' else paste(nrow(x$visit_map), 'visits')}")
  cli::cli_li("Overrides: {length(x$sponsor_overrides)} key(s)")
  invisible(x)
}

# ---- new_meta_bundle() -------------------------------------------------------
#' Create a combined metadata bundle
#'
#' @description
#' Wraps validated target metadata, source metadata, and controlled terminology
#' library into a single object for convenient passing through the pipeline.
#'
#' **Does NOT** read files—takes already-loaded and validated tibbles.
#'
#' @param target_meta Tibble. Validated target variable metadata
#'   (output of [validate_target_meta()] or [read_study_metadata_excel()]).
#' @param source_meta Tibble or `NULL`. Validated source column metadata
#'   (output of [infer_source_meta()] or manually curated).
#' @param ct_lib Tibble or `NULL`. Controlled terminology library
#'   (output of [read_study_ct_excel()]).
#' @param value_level_meta Tibble or `NULL`. Value-level metadata.
#' @param domain_meta Tibble or `NULL`. Domain-level metadata with build
#'   order, keys, description, and structure (from [read_study_metadata_excel()]).
#'
#' @return S3 object of class `meta_bundle`.
#'
#' @section Edge cases:
#' - `target_meta` or `source_meta` not tibble → error.
#' - Domains in `target_meta` not found in `source_meta` → warning.
#'
#' @section Unit tests:
#' - Roundtrip: create + extract components
#' - Warning when domain mismatch
#' - NULL `ct_lib` is accepted
#'
#' @export
new_meta_bundle <- function(target_meta,
                            source_meta = NULL,
                            ct_lib = NULL,
                            value_level_meta = NULL,
                            domain_meta = NULL) {
  checkmate::assert_tibble(target_meta, min.rows = 1L)
  if (!is.null(source_meta)) checkmate::assert_tibble(source_meta, min.rows = 1L)

  # Validate domain_meta if provided
  if (!is.null(domain_meta)) {
    checkmate::assert_tibble(domain_meta, min.rows = 1L)
    dm_required <- c("domain", "class", "class_order", "domain_level_order")
    dm_miss <- setdiff(dm_required, names(domain_meta))
    if (length(dm_miss)) {
      abort(paste("domain_meta missing required columns:", paste(dm_miss, collapse = ", ")))
    }
  }

  target_domains <- unique(target_meta$domain)
  source_datasets <- if (!is.null(source_meta)) unique(source_meta$dataset) else character()

  obj <- list(
    target_meta      = target_meta,
    source_meta      = source_meta,
    ct_lib           = ct_lib,
    value_level_meta = value_level_meta,
    domain_meta      = domain_meta,
    target_domains   = target_domains,
    source_datasets  = source_datasets
  )

  structure(obj, class = "meta_bundle")
}

#' @export
print.meta_bundle <- function(x, ...) {
  cli::cli_h1("SDTM Metadata Bundle")
  cli::cli_li("Target domains: {paste(x$target_domains, collapse = ', ')}")
  cli::cli_li("Target variables: {nrow(x$target_meta)}")
  cli::cli_li("Source datasets: {paste(x$source_datasets, collapse = ', ')}")
  cli::cli_li("CT library: {if (is.null(x$ct_lib)) 'none' else paste(nrow(x$ct_lib), 'terms')}")
  if (!is.null(x$domain_meta)) {
    build_order <- paste(x$domain_meta$domain, collapse = " -> ")
    cli::cli_li("Domain build order ({nrow(x$domain_meta)} domains): {build_order}")
    classes <- unique(x$domain_meta$class)
    cli::cli_li("Domain classes: {paste(classes, collapse = ', ')}")
  }
  if (!is.null(x$value_level_meta)) {
    cli::cli_li("Value-level metadata: {nrow(x$value_level_meta)} conditions")
  }
  invisible(x)
}

# ---- new_rule_set() ----------------------------------------------------------
#' Create a rule set object
#'
#' @description
#' Holds the compiled derivation rules for one or more domains, including
#' dependency information, rule types, and parameter blocks.  Created by
#' [compile_rules()].
#'
#' **Does NOT** execute rules—only stores them.
#'
#' @param rules Named list of lists. Top level keyed by domain; each element
#'   is a list of per-variable rule objects.
#' @param dependency_info Named list. Per-domain dependency edges
#'   (from [infer_rule_dependencies()]).
#' @param rule_types Character vector of all distinct rule types found.
#' @param compile_log Character vector. Messages from compilation.
#'
#' @return S3 object of class `rule_set`.
#'
#' @section Edge cases:
#' - Empty `rules` → warning, returns valid but empty rule set.
#' - Unknown rule type → warning in compile_log.
#'
#' @section Unit tests:
#' - Round-trip: create + query domain
#' - Empty domain
#' - Subsetting by domain
#'
#' @export
new_rule_set <- function(rules,
                         dependency_info = list(),
                         rule_types = character(),
                         compile_log = character()) {
  if (length(rules) == 0L) {
    warn("Creating an empty rule_set.")
  }

  obj <- list(
    rules           = rules,
    dependency_info  = dependency_info,
    rule_types       = rule_types,
    compile_log      = compile_log,
    .n_domains       = length(rules),
    .n_rules         = sum(purrr::map_int(rules, length))
  )

  structure(obj, class = "rule_set")
}

#' @export
print.rule_set <- function(x, ...) {
  cli::cli_h1("SDTM Rule Set")
  cli::cli_li("Domains: {x$.n_domains}")
  cli::cli_li("Total rules: {x$.n_rules}")
  cli::cli_li("Rule types: {paste(x$rule_types, collapse = ', ')}")
  if (length(x$compile_log) > 0L) {
    cli::cli_li("Compile messages: {length(x$compile_log)}")
  }
  invisible(x)
}

# ---- new_build_context() -----------------------------------------------------
#' Create a build context for domain construction
#'
#' @description
#' Mutable environment-like object that carries state through a domain build:
#' current data frame, computed caches (e.g. DM subject table, CT lookups),
#' provenance tracking, and error accumulator.
#'
#' @param domain Character (required). The domain being built.
#' @param config `sdtm_config` object (required).
#' @param meta_bundle `meta_bundle` object (required).
#' @param rule_set `rule_set` object (required).
#' @param dm_cache Tibble or `NULL`. Pre-loaded DM data for subject-level joins.
#' @param ct_cache Named list. Pre-resolved controlled terminology lookups.
#' @param sv_cache Tibble or `NULL`. Subject visits for visit derivation.
#'
#' @return S3 object of class `build_context` (backed by an environment for
#'   mutability during the build pipeline).
#'
#' @section Edge cases:
#' - Domain not in `meta_bundle` → error.
#' - Caches populated lazily on first access if NULL.
#'
#' @section Unit tests:
#' - Create + inspect
#' - Domain mismatch detected
#' - Cache lazy-load works
#'
#' @export
new_build_context <- function(domain,
                              config,
                              meta_bundle,
                              rule_set,
                              dm_cache = NULL,
                              ct_cache = list(),
                              sv_cache = NULL) {
  checkmate::assert_string(domain, min.chars = 1L)
  checkmate::assert_class(config, "sdtm_config")

  ctx_env <- new.env(parent = emptyenv())
  ctx_env$domain      <- domain
  ctx_env$config      <- config
  ctx_env$meta_bundle <- meta_bundle
  ctx_env$rule_set    <- rule_set
  ctx_env$dm_cache    <- dm_cache
  ctx_env$ct_cache    <- ct_cache
  ctx_env$sv_cache    <- sv_cache
  ctx_env$provenance  <- tibble::tibble(
    var       = character(),
    rule_id   = character(),
    source    = character(),
    timestamp = character()
  )
  ctx_env$errors   <- character()
  ctx_env$warnings <- character()
  ctx_env$log      <- character()

  structure(ctx_env, class = "build_context")
}

#' @export
print.build_context <- function(x, ...) {
  cli::cli_h1("Build Context: {x$domain}")
  cli::cli_li("Config study: {x$config$studyid}")
  cli::cli_li("Provenance rows: {nrow(x$provenance)}")
  cli::cli_li("Errors: {length(x$errors)}")
  cli::cli_li("Warnings: {length(x$warnings)}")
  invisible(x)
}

# ---- new_validation_report() -------------------------------------------------
#' Create a validation report accumulator
#'
#' @description
#' Structured container for validation findings (errors, warnings, notes)
#' generated by the validation module.  Findings can be appended incrementally
#' during the build and summarized at the end.
#'
#' @param domain Character or `NULL`. If `NULL`, report is cross-domain.
#' @param title Character. Human-readable title for the report.
#'
#' @return S3 object of class `validation_report` (a list).
#'
#' @section Unit tests:
#' - Create empty report
#' - Add findings + summarize
#' - Severity filtering
#'
#' @export
new_validation_report <- function(domain = NULL, title = "SDTM Validation Report") {
  obj <- list(
    domain   = domain,
    title    = title,
    findings = tibble::tibble(
      rule_id   = character(),
      severity  = character(),
      domain    = character(),
      variable  = character(),
      key_value = character(),
      message   = character(),
      n_records = integer(),
      example   = character()
    ),
    .created_at = Sys.time(),
    .finalized  = FALSE
  )

  structure(obj, class = "validation_report")
}

#' Add a finding to a validation report
#'
#' @param report `validation_report` object.
#' @param rule_id Character. Identifier of the validation rule.
#' @param severity Character. One of `"ERROR"`, `"WARNING"`, `"NOTE"`.
#' @param domain Character.
#' @param variable Character or `NA`.
#' @param key_value Character or `NA`. Example key for locating the issue.
#' @param message Character. Human-readable description.
#' @param n_records Integer. Count of affected records.
#' @param example Character. Example value(s).
#'
#' @return Updated `validation_report`.
#'
#' @export
add_finding <- function(report,
                        rule_id,
                        severity = "WARNING",
                        domain = NA_character_,
                        variable = NA_character_,
                        key_value = NA_character_,
                        message = "",
                        n_records = 0L,
                        example = NA_character_) {
  checkmate::assert_class(report, "validation_report")
  checkmate::assert_choice(severity, c("ERROR", "WARNING", "NOTE"))

  new_row <- tibble::tibble(
    rule_id   = rule_id,
    severity  = severity,
    domain    = domain,
    variable  = variable,
    key_value = key_value,
    message   = message,
    n_records = as.integer(n_records),
    example   = example
  )

  report$findings <- dplyr::bind_rows(report$findings, new_row)
  report
}

#' @export
print.validation_report <- function(x, ...) {
  cli::cli_h1(x$title)
  if (!is.null(x$domain)) cli::cli_li("Domain: {x$domain}")
  n_err  <- sum(x$findings$severity == "ERROR")
  n_warn <- sum(x$findings$severity == "WARNING")
  n_note <- sum(x$findings$severity == "NOTE")
  cli::cli_li("Errors: {n_err} | Warnings: {n_warn} | Notes: {n_note}")
  invisible(x)
}

# ---- new_log_sink() ----------------------------------------------------------
#' Create a structured log sink
#'
#' @description
#' Captures log messages at different severity levels with timestamps,
#' module identifiers, and optional context.  Used internally by all modules
#' and can be redirected to console, file, or both.
#'
#' @param log_level Character. Minimum level to capture. Default `"INFO"`.
#' @param file Character or `NULL`. Path to log file.  If `NULL`, console only.
#' @param context_fields Character vector. Extra fields to include in each entry.
#'
#' @return S3 object of class `log_sink` (backed by an environment).
#'
#' @section Unit tests:
#' - Messages below threshold are not captured
#' - File output works
#' - Flush empties buffer
#'
#' @export
new_log_sink <- function(log_level = "INFO",
                         file = NULL,
                         context_fields = character()) {
  levels <- c("DEBUG" = 1L, "INFO" = 2L, "WARN" = 3L, "ERROR" = 4L)
  checkmate::assert_choice(log_level, names(levels))

  env <- new.env(parent = emptyenv())
  env$threshold <- levels[[log_level]]
  env$levels    <- levels
  env$file      <- file
  env$context_fields <- context_fields
  env$entries   <- list()

  structure(env, class = "log_sink")
}

#' @export
print.log_sink <- function(x, ...) {
  cli::cli_h1("Log Sink")
  cli::cli_li("Threshold: {names(x$levels)[x$levels == x$threshold]}")
  cli::cli_li("Entries: {length(x$entries)}")
  cli::cli_li("File: {x$file %||% '(console only)'}")
  invisible(x)
}
