# ==============================================================================
# run_study(): High-Level Study Orchestrator
# ==============================================================================
# Single-call entry point that reads metadata, loads raw data, builds all
# domains, exports datasets and optionally generates R programs.
# ==============================================================================

#' Run a complete SDTM study pipeline
#'
#' @description
#' One-call entry point for building all SDTM domains from your study files.
#'
#' Given a `config.yaml` (or individual paths), this function:
#' 1. Reads `Study_Metadata.xlsx` and `Study_CT.xlsx`
#' 2. Loads raw datasets from the raw-data directory (`.sas7bdat`, `.csv`, `.xpt`, …)
#' 3. Compiles derivation rules from the metadata
#' 4. Builds every domain defined in the metadata
#' 5. Exports each domain as XPT **and** RDA (or custom formats)
#' 6. Optionally generates an R program per domain in the programs directory
#'
#' @param config_path Character or `NULL`. Path to a `config.yaml` file.
#'   If provided, `metadata_path`, `ct_path`, `raw_dir`, `output_dir` and
#'   `programs_dir` are read from the YAML (but explicit arguments override).
#' @param metadata_path Character or `NULL`. Path to `Study_Metadata.xlsx`.
#' @param ct_path Character or `NULL`. Path to `Study_CT.xlsx`.
#' @param raw_dir Character or `NULL`. Directory containing raw datasets.
#' @param output_dir Character or `NULL`. Directory for exported datasets.
#' @param programs_dir Character or `NULL`. Directory for generated R programs.
#' @param export_formats Character vector. Default `c("xpt", "rda")`.
#' @param generate_programs Logical. Default `TRUE`. Write an R script per
#'   domain into `programs_dir`.
#' @param domains Character vector or `NULL`. If `NULL`, builds all domains
#'   found in the metadata.
#' @param create_supp Logical or `NULL`. Default `NULL` (inherits from config).
#' @param validate Logical. Default `TRUE`.
#' @param verbose Logical. Default `TRUE`.
#'
#' @return Named list with:
#'   \describe{
#'     \item{results}{Named list of `build_result` objects, one per domain.}
#'     \item{target_meta}{The target metadata tibble used.}
#'     \item{ct_lib}{The controlled-terminology tibble used.}
#'     \item{rule_set}{The compiled rule set.}
#'     \item{config}{The `sdtm_config` object used.}
#'     \item{exported}{Named list of export paths per domain.}
#'     \item{programs}{Named list of generated program paths (or `NULL`).}
#'   }
#'
#' @section Folder Structure:
#' ```
#' my_study/
#' ├── metadata/
#' │   ├── config.yaml
#' │   ├── Study_Metadata.xlsx
#' │   └── Study_CT.xlsx
#' ├── raw/
#' │   ├── dm.sas7bdat
#' │   ├── ae.sas7bdat
#' │   └── ...
#' └── sdtm/
#'     ├── datasets/
#'     │   ├── dm.xpt
#'     │   ├── dm.rda
#'     │   └── ...
#'     └── programs/
#'         ├── dm.R
#'         ├── ae.R
#'         └── ...
#' ```
#'
#' @examples
#' \dontrun{
#' # Using a config.yaml file (stored in metadata/)
#' out <- run_study("metadata/config.yaml")
#'
#' # Using explicit paths (no config.yaml needed)
#' out <- run_study(
#'   metadata_path = "metadata/Study_Metadata.xlsx",
#'   ct_path       = "metadata/Study_CT.xlsx",
#'   raw_dir       = "raw",
#'   output_dir    = "sdtm/datasets",
#'   programs_dir  = "sdtm/programs"
#' )
#'
#' # Datasets only (no R programs generated)
#' out <- run_study("metadata/config.yaml", generate_programs = FALSE)
#' }
#'
#' @export
run_study <- function(config_path = NULL,
                      metadata_path = NULL,
                      ct_path = NULL,
                      raw_dir = NULL,
                      output_dir = NULL,
                      programs_dir = NULL,
                      export_formats = c("xpt", "rda"),
                      generate_programs = TRUE,
                      domains = NULL,
                      create_supp = NULL,
                      validate = TRUE,
                      verbose = TRUE) {

  .log <- function(msg) {
    if (verbose) cli::cli_alert_info(msg)
  }

  # ---- 1. Resolve paths from config.yaml or arguments -----------------------
  cfg_yaml <- NULL
  if (!is.null(config_path)) {
    checkmate::assert_file_exists(config_path)
    cfg_yaml <- yaml::read_yaml(config_path)
    .log("Read config from {config_path}")
  }

  # Helper: explicit argument wins, else config.yaml, else default
  .resolve <- function(explicit, yaml_key, default = NULL) {
    if (!is.null(explicit)) return(explicit)
    if (!is.null(cfg_yaml) && !is.null(cfg_yaml$paths[[yaml_key]])) {
      return(cfg_yaml$paths[[yaml_key]])
    }
    default
  }

  metadata_path  <- .resolve(metadata_path, "metadata", "metadata/Study_Metadata.xlsx")
  ct_path        <- .resolve(ct_path, "ct", "metadata/Study_CT.xlsx")
  raw_dir        <- .resolve(raw_dir, "raw_data", "raw")
  output_dir     <- .resolve(output_dir, "output", "sdtm/datasets")
  programs_dir   <- .resolve(programs_dir, "programs", "sdtm/programs")

  # Resolve export_formats and generate_programs from config if not overridden
  if (!is.null(cfg_yaml$export$formats)) {
    export_formats <- cfg_yaml$export$formats
  }
  if (!is.null(cfg_yaml$export$generate_programs)) {
    generate_programs <- cfg_yaml$export$generate_programs
  }

  # Resolve studyid
  studyid <- cfg_yaml$studyid %||% "UNKNOWN"

  .log("Study ID: {studyid}")
  .log("Metadata: {metadata_path}")
  .log("CT:       {ct_path}")
  .log("Raw data: {raw_dir}")

  .log("Output:   {output_dir}")
  .log("Programs: {programs_dir}")

  # ---- 2. Read metadata ------------------------------------------------------
  .log("Step 1: Reading metadata...")
  checkmate::assert_file_exists(metadata_path)
  study_meta <- read_study_metadata_excel(metadata_path)
  target_meta     <- study_meta$target_meta
  domain_meta     <- study_meta$domain_meta
  value_level_meta <- study_meta$value_level_meta

  ct_lib <- NULL
  if (file.exists(ct_path)) {
    ct_lib <- read_study_ct_excel(ct_path)
    .log("  Loaded CT: {nrow(ct_lib)} terms")
  } else {
    .log("  No CT file found at {ct_path}, proceeding without CT")
  }

  # Expand value-level metadata if present
  if (!is.null(value_level_meta) && nrow(value_level_meta) > 0L) {
    .log("  Expanding {nrow(value_level_meta)} value-level rows...")
    target_meta <- expand_value_level_meta(target_meta, value_level_meta)
  }

  .log("  target_meta: {nrow(target_meta)} variables across {length(unique(target_meta$domain))} domains")

  # ---- 3. Load raw data ------------------------------------------------------
  .log("Step 2: Loading raw data from {raw_dir}...")
  checkmate::assert_directory_exists(raw_dir)
  raw_data <- load_raw_datasets(raw_dir)
  .log("  Loaded {length(raw_data)} datasets: {paste(names(raw_data), collapse = ', ')}")

  # ---- 4. Build config object ------------------------------------------------
  .log("Step 3: Building configuration...")

  # Parse ref_start_rule from YAML
  rsr_yaml <- cfg_yaml$ref_start_rule
  if (!is.null(rsr_yaml)) {
    # Support both formats: {var, source} and {method, dataset, column}
    ref_start_rule <- if (!is.null(rsr_yaml$var)) {
      list(var = rsr_yaml$var, source = rsr_yaml$source)
    } else if (!is.null(rsr_yaml$column)) {
      list(var = rsr_yaml$column, source = rsr_yaml$dataset)
    } else {
      list(var = "rfstdtc", source = "dm_raw")
    }
  } else {
    ref_start_rule <- list(var = "rfstdtc", source = "dm_raw")
  }

  # Parse visit_map
  visit_map <- NULL
  if (!is.null(cfg_yaml$visit_map)) {
    visit_map <- dplyr::bind_rows(lapply(cfg_yaml$visit_map, function(v) {
      tibble::tibble(
        VISITNUM  = as.integer(v$visitnum %||% v$VISITNUM),
        VISIT     = as.character(v$visit %||% v$VISIT),
        START_DAY = as.integer(v$start_day %||% v$START_DAY %||% NA),
        END_DAY   = as.integer(v$end_day %||% v$END_DAY %||% NA)
      )
    }))
  }

  config <- new_sdtm_config(
    studyid        = studyid,
    timezone       = cfg_yaml$timezone %||% "UTC",
    ref_start_rule = ref_start_rule,
    visit_map      = visit_map,
    create_supp    = create_supp %||% (cfg_yaml$create_supp %||% TRUE)
  )

  # ---- 5. Compile rules ------------------------------------------------------
  .log("Step 4: Compiling rules...")
  rule_set <- compile_rules(target_meta, ct_lib = ct_lib)
  doms_in_rules <- names(rule_set$rules)
  .log("  Rules for {length(doms_in_rules)} domains: {paste(doms_in_rules, collapse = ', ')}")

  # ---- 6. Build all domains --------------------------------------------------
  .log("Step 5: Building domains...")
  results <- build_all_domains(
    target_meta      = target_meta,
    raw_data         = raw_data,
    config           = config,
    rule_set         = rule_set,
    domains          = domains,
    domain_meta      = domain_meta,
    value_level_meta = value_level_meta,
    create_supp      = create_supp,
    validate         = validate,
    verbose          = verbose
  )

  built_domains <- names(results)
  .log("  Built {length(built_domains)} domains: {paste(built_domains, collapse = ', ')}")

  # ---- 7. Export datasets ----------------------------------------------------
  .log("Step 6: Exporting datasets to {output_dir}...")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  exported <- list()

  for (dom in built_domains) {
    dom_data <- results[[dom]]$data
    if (is.null(dom_data)) next

    # Export the main domain
    export_domain(
      data        = dom_data,
      domain      = dom,
      output_dir  = output_dir,
      formats     = export_formats,
      xpt_version = 8L,
      target_meta = target_meta,
      domain_meta = domain_meta
    )
    exported[[dom]] <- TRUE

    # Export SUPP if present
    if (!is.null(results[[dom]]$supp) && nrow(results[[dom]]$supp) > 0L) {
      supp_dom <- paste0("SUPP", dom)
      export_domain(
        data       = results[[dom]]$supp,
        domain     = supp_dom,
        output_dir = output_dir,
        formats    = export_formats,
        xpt_version = 8L
      )
    }
  }

  # ---- 8. Generate R programs ------------------------------------------------
  generated_programs <- NULL
  if (generate_programs) {
    .log("Step 7: Generating R programs to {programs_dir}...")
    dir.create(programs_dir, recursive = TRUE, showWarnings = FALSE)
    generated_programs <- list()

    for (dom in built_domains) {
      prog_path <- file.path(programs_dir, paste0(tolower(dom), ".R"))
      tryCatch({
        gen_domain_script(
          domain        = dom,
          rule_set      = rule_set,
          target_meta   = target_meta,
          config        = config,
          domain_meta   = domain_meta,
          output_path   = prog_path,
          metadata_path = metadata_path,
          ct_path       = ct_path,
          raw_dir       = raw_dir,
          output_dir    = output_dir,
          tpt_source_var = config$tpt_source_var
        )
        generated_programs[[dom]] <- prog_path
      }, error = function(e) {
        if (verbose) cli::cli_alert_warning("Could not generate program for {dom}: {e$message}")
      })
    }
    .log("  Generated {length(generated_programs)} programs")
  }

  # ---- 9. Summary ------------------------------------------------------------
  cli::cli_rule("Study Pipeline Complete")
  cli::cli_alert_success("Domains built: {paste(built_domains, collapse = ', ')}")
  cli::cli_alert_success("Datasets in:   {output_dir}")
  if (!is.null(generated_programs)) {
    cli::cli_alert_success("Programs in:   {programs_dir}")
  }

  # Report validation summary
  total_errors <- 0L
  total_warns  <- 0L
  for (dom in built_domains) {
    rpt <- results[[dom]]$report
    if (!is.null(rpt)) {
      n_err <- sum(rpt$findings$severity == "ERROR", na.rm = TRUE)
      n_wrn <- sum(rpt$findings$severity == "WARN", na.rm = TRUE)
      total_errors <- total_errors + n_err
      total_warns  <- total_warns + n_wrn
      status_icon <- if (n_err > 0L) "\u26a0\ufe0f" else "\u2705"
      .log("  {status_icon} {dom}: {nrow(results[[dom]]$data)} rows | E:{n_err} W:{n_wrn}")
    }
  }

  invisible(list(
    results    = results,
    target_meta = target_meta,
    ct_lib     = ct_lib,
    rule_set   = rule_set,
    config     = config,
    exported   = exported,
    programs   = generated_programs
  ))
}


#' Get the template config.yaml path
#'
#' Returns the path to the built-in template `config.yaml` that can be copied
#' to your study folder as a starting point.
#'
#' @param copy_to Character or `NULL`. If provided, copies the template to
#'   this path. The directory will be created if it doesn't exist.
#' @return Invisible path to the template file (or the copy destination).
#'
#' @examples
#' # Just get the path
#' get_template_config()
#'
#' # Copy to your study folder
#' get_template_config(copy_to = "my_study/metadata/config.yaml")
#'
#' @export
get_template_config <- function(copy_to = NULL) {
  template <- system.file("extdata", "template_config.yaml",
                          package = "sdtmbuilder")
  if (!nzchar(template)) {
    abort("Template config.yaml not found. Is sdtmbuilder installed correctly?")
  }

  if (!is.null(copy_to)) {
    dir.create(dirname(copy_to), recursive = TRUE, showWarnings = FALSE)
    file.copy(template, copy_to, overwrite = FALSE)
    cli::cli_alert_success("Template config.yaml copied to {copy_to}")
    return(invisible(copy_to))
  }

  cli::cli_alert_info("Template config: {template}")
  invisible(template)
}
