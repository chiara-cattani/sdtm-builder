# ==============================================================================
# End-to-End Verification Harness
# ==============================================================================
# Single function that: loads dummy study → compiles rules → builds all 4
# domains (AE, CM, MH, PR) → validates → reports pass/fail.
# ==============================================================================

#' Run end-to-end pipeline check
#'
#' @description
#' Exercises the full sdtmbuilder pipeline on the built-in dummy study data.
#' Returns a summary list with status and optional domain datasets.
#'
#' @param verbose Logical. Default `TRUE`. Print progress and results.
#' @param return_data Logical. Default `FALSE`. If `TRUE`, include built domains
#'   in the return value.
#' @param starter_kit_dir Character or `NULL`. Path to starter kit.
#' @param output_dir Character or `NULL`. If provided, export XPT files here.
#'
#' @return Named list with:
#'   - `status`: `"PASS"` or `"FAIL"`
#'   - `domains`: list of domain summaries (nrows, ncols, errors, warnings)
#'   - `data`: (if `return_data=TRUE`) named list of domain tibbles
#'   - `reports`: list of validation reports
#'   - `log`: character vector of messages
#'
#' @export
check_end_to_end <- function(verbose = TRUE, return_data = FALSE,
                             starter_kit_dir = NULL,
                             output_dir = NULL) {

  log_msgs <- character()
  .log <- function(msg) {
    log_msgs <<- c(log_msgs, msg)
    if (verbose) cli::cli_alert_info(msg)
  }

  # ------ Step 1: Load dummy study data --------------------------------------
  .log("Step 1: Loading dummy study data...")
  study <- tryCatch(
    make_dummy_study(seed = 123, starter_kit_dir = starter_kit_dir),
    error = function(e) {
      .log(paste("FATAL: Could not load dummy study:", e$message))
      return(NULL)
    }
  )
  if (is.null(study)) {
    return(list(status = "FAIL", log = log_msgs))
  }

  config      <- study$config
  target_meta <- study$target_meta
  source_meta <- study$source_meta
  ct_lib      <- study$ct_lib
  raw_data    <- study$raw_data

  .log(glue::glue("  Loaded {length(raw_data)} source datasets"))
  for (nm in names(raw_data)) {
    .log(glue::glue("    {nm}: {nrow(raw_data[[nm]])} rows x {ncol(raw_data[[nm]])} cols"))
  }

  # ------ Step 2: Compile rules ----------------------------------------------
  .log("Step 2: Compiling rules from metadata...")
  rule_set <- tryCatch(
    compile_rules(target_meta, source_meta, ct_lib),
    error = function(e) {
      .log(paste("FATAL: Rule compilation failed:", e$message))
      return(NULL)
    }
  )
  if (is.null(rule_set)) {
    return(list(status = "FAIL", log = log_msgs))
  }

  domains_in_rules <- names(rule_set$rules)
  .log(glue::glue("  Compiled rules for {length(domains_in_rules)} domains: {paste(domains_in_rules, collapse=', ')}"))
  for (dom in domains_in_rules) {
    n_rules <- length(rule_set$rules[[dom]])
    .log(glue::glue("    {dom}: {n_rules} rules"))
  }

  # ------ Step 3: Build each domain ------------------------------------------
  .log("Step 3: Building domains...")
  mvp_domains <- intersect(c("DM", "AE", "CM", "MH", "PR", "EX", "VS", "LB"), domains_in_rules)
  built <- list()
  reports <- list()
  domain_summaries <- list()
  any_error <- FALSE

  for (dom in mvp_domains) {
    .log(glue::glue("  Building {dom}..."))
    result <- tryCatch(
      build_domain(
        domain      = dom,
        target_meta = target_meta,
        source_meta = source_meta,
        raw_data    = raw_data,
        config      = config,
        rule_set    = rule_set,
        verbose     = verbose
      ),
      error = function(e) {
        .log(glue::glue("  ERROR building {dom}: {e$message}"))
        return(NULL)
      }
    )

    if (is.null(result)) {
      any_error <- TRUE
      domain_summaries[[dom]] <- list(
        status = "FAIL", nrow = 0, ncol = 0, errors = 1, warnings = 0
      )
      next
    }

    built[[dom]] <- result$data
    reports[[dom]] <- result$report

    n_err  <- sum(result$report$findings$severity == "ERROR")
    n_warn <- sum(result$report$findings$severity == "WARNING")

    domain_summaries[[dom]] <- list(
      status   = if (n_err == 0) "PASS" else "WARN",
      nrow     = nrow(result$data),
      ncol     = ncol(result$data),
      errors   = n_err,
      warnings = n_warn
    )

    .log(glue::glue("  {dom}: {nrow(result$data)} rows x {ncol(result$data)} cols | Errors: {n_err}, Warnings: {n_warn}"))
  }

  # ------ Step 4: Export (optional) ------------------------------------------
  if (!is.null(output_dir) && length(built) > 0) {
    .log(glue::glue("Step 4: Exporting to {output_dir}..."))
    for (dom in names(built)) {
      tryCatch({
        export_xpt(built[[dom]], dom, output_dir, target_meta)
        .log(glue::glue("  Exported {dom}.xpt"))
      }, error = function(e) {
        .log(glue::glue("  Export error for {dom}: {e$message}"))
      })
    }
  }

  # ------ Step 5: Summary ----------------------------------------------------
  overall_errors <- sum(vapply(domain_summaries, function(s) s$errors, numeric(1)))
  overall <- if (any_error || length(built) == 0) "FAIL"
             else if (overall_errors > 0) "WARN"
             else "PASS"

  .log("========================================")
  .log(glue::glue("Overall status: {overall}"))
  .log(glue::glue("Domains built: {length(built)}/{length(mvp_domains)}"))
  .log(glue::glue("Total validation errors: {overall_errors}"))

  if (verbose) {
    cli::cli_h2("End-to-End Check Result")
    for (dom in names(domain_summaries)) {
      s <- domain_summaries[[dom]]
      icon <- if (s$status == "PASS") "\u2705" else if (s$status == "WARN") "\u26A0\uFE0F" else "\u274C"
      cli::cli_text("{icon} {dom}: {s$nrow} rows x {s$ncol} cols | E:{s$errors} W:{s$warnings}")
    }
    if (overall == "PASS") {
      cli::cli_alert_success("All domains built successfully!")
    } else if (overall == "WARN") {
      cli::cli_alert_warning("Domains built with validation findings")
    } else {
      cli::cli_alert_danger("Some domains failed to build")
    }
  }

  result_list <- list(
    status   = overall,
    domains  = domain_summaries,
    reports  = reports,
    log      = log_msgs
  )

  if (return_data) {
    result_list$data <- built
  }

  result_list
}
