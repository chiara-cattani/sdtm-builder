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
#'   If `NULL` (the default), auto-discovers `config.yaml` or
#'   `metadata/config.yaml` in the current working directory.
#'   If provided, `metadata_path`, `ct_path`, `raw_dir`, `output_dir` and
#'   `programs_dir` are read from the YAML (but explicit arguments override).
#' @param metadata_path Character or `NULL`. Path to `Study_Metadata.xlsx`.
#' @param ct_path Character or `NULL`. Path to `Study_CT.xlsx`.
#' @param raw_dir Character or `NULL`. Directory containing raw datasets.
#' @param output_dir Character or `NULL`. Directory for exported datasets.
#' @param programs_dir Character or `NULL`. Directory for generated R programs.
#' @param export_formats Character vector. Default `c("xpt", "rda")`.
#' @param xpt_version Integer. SAS transport version: `5` (v5 legacy) or
#'   `8` (v8, default). Inherited from `config.yaml` `export.xpt_version`
#'   when not set explicitly.
#' @param generate_programs Logical. Default `TRUE`. Write an R script per
#'   domain into `programs_dir`.
#' @param domains Character vector or `NULL`. If `NULL`, builds all domains
#'   found in the metadata.
#' @param create_supp Logical or `NULL`. Default `NULL` (inherits from config).
#' @param drop_empty_perm Logical or named list. Controls whether permissible
#'   (PERM) variables that are entirely empty are dropped from exported datasets.
#'   - `TRUE`: drop empty PERM variables for all domains.
#'   - `FALSE` (default): keep all PERM variables even if empty.
#'   - Named list: set per-domain overrides. Use `".default"` for the study-level
#'     default and domain names for exceptions, e.g.
#'     `list(.default = TRUE, DM = FALSE, AE = FALSE)`.
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
#' # Auto-discover config.yaml in the working directory
#' setwd("my_study")
#' out <- run_study()
#'
#' # Or pass the config path explicitly
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
#' out <- run_study(generate_programs = FALSE)
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
                      xpt_version = 8L,
                      generate_programs = TRUE,
                      domains = NULL,
                      create_supp = NULL,
                      drop_empty_perm = FALSE,
                      validate = TRUE,
                      verbose = TRUE) {

  .log <- function(msg) {
    if (verbose) cli::cli_alert_info(msg)
  }

  # ---- 1. Resolve paths from config.yaml or arguments -----------------------
  cfg_yaml <- NULL

  # Auto-discover config.yaml if not explicitly provided
  if (is.null(config_path)) {
    candidates <- c("config.yaml", "metadata/config.yaml")
    found <- candidates[file.exists(candidates)]
    if (length(found) > 0L) {
      config_path <- found[1L]
      .log("Auto-discovered config: {config_path}")
    }
  }

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

  # Resolve export_formats and generate_programs: explicit arg wins, else config.yaml
  if (missing(export_formats) && !is.null(cfg_yaml$export$formats)) {
    export_formats <- cfg_yaml$export$formats
  }
  if (missing(xpt_version) && !is.null(cfg_yaml$export$xpt_version)) {
    xpt_version <- as.integer(cfg_yaml$export$xpt_version)
  }
  if (missing(generate_programs) && !is.null(cfg_yaml$export$generate_programs)) {
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
  target_meta      <- study_meta$target_meta
  domain_meta      <- study_meta$domain_meta
  value_level_meta <- study_meta$value_level_meta
  sources_meta     <- study_meta$sources_meta
  source_cols_meta <- study_meta$source_cols_meta

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

  # ---- Pre-Validation: Check metadata & CT structure before proceeding ------
  .log("Step 2-PreVal: Validating metadata & CT structure...")
  pre_val_report <- validate_metadata_ct_structure(
    target_meta = target_meta,
    ct_lib = ct_lib,
    value_level_meta = value_level_meta,
    domain_meta = domain_meta
  )

  # Check for critical errors
  pre_val_errors <- pre_val_report$findings %>%
    dplyr::filter(.data$severity == "ERROR")

  if (nrow(pre_val_errors) > 0L) {
    cli::cli_h2("⚠️  PRE-VALIDATION ERRORS FOUND")
    cli::cli_alert_danger("Cannot proceed - critical metadata/CT issues must be fixed:")
    cli::cli_text("")
    for (i in seq_len(nrow(pre_val_errors))) {
      cli::cli_text("  ✗ {pre_val_errors$message[i]}")
    }
    cli::cli_text("")
    cli::cli_text("Please review and update your metadata/CT files, then run run_study() again.")
    cli::cli_text("For detailed report, run:")
    cli::cli_code("validate_and_report_metadata_ct(study_meta$target_meta, ct_lib, ...)")
    stop("Pre-validation failed - cannot proceed", call. = FALSE)
  }

  # Show warnings/notes for informational purposes
  pre_val_warnings <- pre_val_report$findings %>%
    dplyr::filter(.data$severity == "WARNING")

  if (nrow(pre_val_warnings) > 0L) {
    cli::cli_alert_warning("Pre-validation warnings ({nrow(pre_val_warnings)}):")
    for (i in seq_len(min(3, nrow(pre_val_warnings)))) {
      cli::cli_text("  ⚠ {pre_val_warnings$message[i]}")
    }
    if (nrow(pre_val_warnings) > 3L) {
      cli::cli_text("  ... and {nrow(pre_val_warnings) - 3} more warning(s)")
    }
    cli::cli_text("")
  }

  .log("Pre-validation complete: All critical issues resolved ✓")

  # ---- 3a. Early rule compilation (before data loading) ---------------------
  # This allows us to understand which source datasets are truly needed
  .log("Step 2a: Compiling rules (for smart data loading)...")
  
  # Extract variables with derivation rules for selected domains
  # (filter to domains early so we know what to compile)
  vars_with_derivation <- NULL
  if (!is.null(domains)) {
    domains_upper <- toupper(domains)
    vars_to_compile <- target_meta %>%
      dplyr::filter(.data$domain %in% domains_upper, 
                    !is.na(.data$derivation), 
                    .data$derivation != "") %>%
      dplyr::distinct(.data$domain, .data$var)
    
    if (nrow(vars_to_compile) > 0L) {
      vars_with_derivation <- vars_to_compile
    }
  }
  
  # Compile rules with granular domain/variable filtering
  rule_set <- compile_rules(target_meta, source_meta = sources_meta, 
                            ct_lib = ct_lib,
                            domains = domains,
                            vars = vars_with_derivation)
  
  # Extract source datasets from compiled rules for smart loading
  source_datasets_from_rules <- get_source_datasets_from_rules(
    rule_set, target_meta = target_meta, sources_meta = sources_meta
  )
  
  # Combine with domain-based sources for comprehensive coverage
  required_by_domain <- get_required_datasets(domains = domains, 
                                              sources_meta = sources_meta)
  all_required_sources <- union(required_by_domain, source_datasets_from_rules)
  
  doms_in_rules <- names(rule_set$rules)
  .log("  Rules for {length(doms_in_rules)} domains: {paste(doms_in_rules, collapse = ', ')}")
  if (length(all_required_sources) > 0L) {
    .log("  Required source datasets: {paste(sort(all_required_sources), collapse = ', ')}")
  }

  # ---- 3b. Load raw data (smart selection based on compiled rules) ---------
  .log("Step 2b: Loading raw data from {raw_dir}...")
  checkmate::assert_directory_exists(raw_dir)
  
  # Load data with smart selection based on what's actually needed
  raw_data <- if (length(all_required_sources) > 0L) {
    # Load specific required datasets
    load_raw_datasets_with_sources(raw_dir, sources = all_required_sources, 
                                   verbose = verbose)
  } else {
    # Fallback to domain-based loading
    load_raw_datasets_selective(raw_dir, domains = domains, 
                                sources_meta = sources_meta,
                                verbose = verbose)
  }
  .log("  Loaded {length(raw_data)} datasets: {paste(names(raw_data), collapse = ', ')}")

  # ---- 4. Build config object ------------------------------------------------
  .log("Step 3: Building configuration...")

  # Parse ref_start_rule from YAML
  rsr_yaml <- cfg_yaml$ref_start_rule
  # Parse ref_start_date — new format with sdtm + raw fallback
  ref_start_date <- cfg_yaml$ref_start_date
  if (!is.null(ref_start_date)) {
    # Resolve sdtm.path relative to the study directory (where config lives)
    sdtm_path_raw <- ref_start_date$sdtm$path
    if (!is.null(sdtm_path_raw) && !is.na(sdtm_path_raw) && nchar(sdtm_path_raw) > 0) {
      if (!file.exists(sdtm_path_raw)) {
        # Try relative to config directory
        sdtm_path_resolved <- file.path(dirname(config_path), sdtm_path_raw)
        if (file.exists(sdtm_path_resolved)) {
          sdtm_path_raw <- normalizePath(sdtm_path_resolved, winslash = "/")
        }
      } else {
        sdtm_path_raw <- normalizePath(sdtm_path_raw, winslash = "/")
      }
    } else {
      sdtm_path_raw <- NULL
    }

    ref_start_rule <- list(
      sdtm_domain   = ref_start_date$sdtm$domain   %||% "DM",
      sdtm_variable = ref_start_date$sdtm$variable  %||% "RFSTDTC",
      sdtm_path     = sdtm_path_raw,
      raw_dataset   = ref_start_date$raw$dataset     %||% "dm",
      raw_variable  = ref_start_date$raw$variable    %||% "rfstdtc"
    )
  } else if (!is.null(rsr_yaml)) {
    # Legacy format: ref_start_rule with {var, source} or {method, dataset, column}
    ref_start_rule <- if (!is.null(rsr_yaml$var)) {
      list(sdtm_domain = "DM", sdtm_variable = "RFSTDTC",
           raw_dataset = rsr_yaml$source %||% "dm",
           raw_variable = rsr_yaml$var %||% "rfstdtc")
    } else if (!is.null(rsr_yaml$column)) {
      list(sdtm_domain = "DM", sdtm_variable = "RFSTDTC",
           raw_dataset = rsr_yaml$dataset %||% "dm",
           raw_variable = rsr_yaml$column %||% "rfstdtc")
    } else {
      list(sdtm_domain = "DM", sdtm_variable = "RFSTDTC",
           raw_dataset = "dm", raw_variable = "rfstdtc")
    }
  } else {
    ref_start_rule <- list(sdtm_domain = "DM", sdtm_variable = "RFSTDTC",
                           raw_dataset = "dm", raw_variable = "rfstdtc")
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
    create_supp    = create_supp %||% (cfg_yaml$create_supp %||% FALSE)
  )
  # Store output_dir so build_domain can load DM from disk if needed
  config$output_dir <- output_dir
  # Store full YAML config for trial design plugins (TV, TI, IE)
  config$cfg_yaml <- cfg_yaml
  # Store hooks directory for study-level pre-processing scripts
  hooks_dir <- cfg_yaml$paths$hooks %||% file.path(dirname(metadata_path), "hooks")
  config$hooks_dir <- if (dir.exists(hooks_dir)) hooks_dir else NULL

  # ---- 4. Build all domains --------------------------------------------------
  .log("Step 3: Building domains...")
  results <- build_all_domains(
    target_meta      = target_meta,
    raw_data         = raw_data,
    config           = config,
    rule_set         = rule_set,
    domains          = domains,
    domain_meta      = domain_meta,
    value_level_meta = value_level_meta,
    sources_meta     = sources_meta,
    source_cols_meta = source_cols_meta,
    create_supp      = create_supp,
    validate         = validate,
    verbose          = verbose
  )

  built_domains <- names(results)
  .log("  Built {length(built_domains)} domains: {paste(built_domains, collapse = ', ')}")

  # ---- 4a. Build RELREC (Related Records) ------------------------------------
  relrec_data <- NULL
  # Only build RELREC if the user requested it (or requested all domains)
  build_relrec_flag <- is.null(domains) || "RELREC" %in% toupper(domains)
  if (build_relrec_flag && !is.null(cfg_yaml$relrec) && length(cfg_yaml$relrec) > 0L) {
    .log("Step 3a: Building RELREC (Related Records)...")

    # Collect built domain tibbles for identity/seq_lookup specs
    built_domain_data <- stats::setNames(
      lapply(built_domains, function(d) results[[d]]$data),
      built_domains
    )

    relrec_data <- tryCatch(
      build_relrec(
        relationship_specs = cfg_yaml$relrec,
        raw_data           = raw_data,
        config             = config,
        built_domains      = built_domain_data
      ),
      error = function(e) {
        if (verbose) cli::cli_alert_warning("RELREC build failed: {e$message}")
        NULL
      }
    )

    if (!is.null(relrec_data) && nrow(relrec_data) > 0L) {
      .log("  RELREC: {nrow(relrec_data)} rows")
    } else {
      .log("  RELREC: 0 rows (no relationships found in data)")
    }
  }

  # ---- 7. Export datasets ----------------------------------------------------
  .log("Step 4: Exporting datasets to {output_dir}...")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  exported <- list()

  for (dom in built_domains) {
    dom_data <- results[[dom]]$data
    if (is.null(dom_data)) next

    # Resolve drop_empty_perm for this domain:
    # - If drop_empty_perm is a named list, use domain-specific value if present,
    #   otherwise use the ".default" entry or FALSE
    # - If drop_empty_perm is a scalar logical, use it for all domains
    if (is.list(drop_empty_perm)) {
      dom_drop <- drop_empty_perm[[dom]] %||% drop_empty_perm[[".default"]] %||% FALSE
    } else {
      dom_drop <- isTRUE(drop_empty_perm)
    }

    # Export the main domain
    export_domain(
      data            = dom_data,
      domain          = dom,
      output_dir      = output_dir,
      formats         = export_formats,
      xpt_version     = xpt_version,
      target_meta     = target_meta,
      domain_meta     = domain_meta,
      drop_empty_perm = dom_drop
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
        xpt_version = xpt_version
      )
    }
  }

  # Export RELREC if built
  if (!is.null(relrec_data) && nrow(relrec_data) > 0L) {
    export_domain(
      data        = relrec_data,
      domain      = "RELREC",
      output_dir  = output_dir,
      formats     = export_formats,
      xpt_version = xpt_version
    )
    exported[["RELREC"]] <- TRUE
    .log("  Exported RELREC ({nrow(relrec_data)} rows)")
  }

  # ---- 8. Generate R programs ------------------------------------------------
  generated_programs <- NULL
  if (generate_programs) {
    .log("Step 5: Generating R programs to {programs_dir}...")
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
          tpt_source_var = cfg_yaml$tpt_source_var,
          export_formats = export_formats
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
  all_issues   <- list()  # collect per-domain issue tibbles
  for (dom in built_domains) {
    rpt <- results[[dom]]$report
    if (!is.null(rpt) && !is.null(rpt$findings) && nrow(rpt$findings) > 0L) {
      n_err <- sum(rpt$findings$severity == "ERROR", na.rm = TRUE)
      n_wrn <- sum(rpt$findings$severity %in% c("WARN", "WARNING"), na.rm = TRUE)
      total_errors <- total_errors + n_err
      total_warns  <- total_warns + n_wrn
      status_icon <- if (n_err > 0L) "\u26a0\ufe0f" else "\u2705"
      .log("  {status_icon} {dom}: {nrow(results[[dom]]$data)} rows | E:{n_err} W:{n_wrn}")

      # Print individual error/warning messages so user can see them
      issues <- rpt$findings[rpt$findings$severity %in% c("ERROR", "WARNING", "WARN"), ]
      if (nrow(issues) > 0L) {
        for (i in seq_len(nrow(issues))) {
          f <- issues[i, ]
          sev_label <- if (f$severity == "ERROR") "ERROR" else "WARN"
          var_label <- if (!is.na(f$variable)) paste0(" [", f$variable, "]") else ""
          if (f$severity == "ERROR") {
            cli::cli_alert_danger("    {sev_label}{var_label}: {f$message}")
          } else {
            cli::cli_alert_warning("    {sev_label}{var_label}: {f$message}")
          }
        }
        all_issues[[dom]] <- issues
      }
    } else {
      .log("  \u2705 {dom}: {nrow(results[[dom]]$data)} rows | E:0 W:0")
    }
  }

  # Print summary totals
  if (total_errors > 0L || total_warns > 0L) {
    cli::cli_alert_warning("Total: {total_errors} error(s), {total_warns} warning(s) across {length(all_issues)} domain(s)")
    cli::cli_alert_info("Tip: use study_issues(result) to inspect all issues as a table")
  } else {
    cli::cli_alert_success("All domains passed validation with no errors or warnings")
  }

  invisible(list(
    results    = results,
    target_meta = target_meta,
    ct_lib     = ct_lib,
    rule_set   = rule_set,
    config     = config,
    exported   = exported,
    programs   = generated_programs,
    relrec     = relrec_data,
    issues     = all_issues
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


#' Extract all issues (errors and warnings) from a run_study result
#'
#' @description
#' Convenience function to inspect all validation findings and derivation
#' errors from a \code{run_study()} result as a single tidy tibble.
#'
#' @param study_result The list returned by \code{run_study()}.
#' @param severity Character vector.
#'   Filter to specific severity levels.
#'   Default \code{c("ERROR", "WARNING")} — excludes NOTEs.
#' @return A tibble with columns: \code{domain}, \code{severity},
#'   \code{variable}, \code{rule_id}, \code{message}, \code{n_records},
#'   \code{example}.
#'
#' @examples
#' \dontrun{
#' out <- run_study("metadata/config.yaml")
#' study_issues(out)                    # all errors and warnings
#' study_issues(out, severity = "ERROR") # errors only
#' }
#'
#' @export
study_issues <- function(study_result, severity = c("ERROR", "WARNING")) {
  results <- study_result$results
  if (is.null(results)) {
    cli::cli_alert_info("No results found in study_result.")
    return(tibble::tibble())
  }

  all_findings <- list()
  for (dom in names(results)) {
    rpt <- results[[dom]]$report
    if (is.null(rpt) || is.null(rpt$findings) || nrow(rpt$findings) == 0L) next
    f <- rpt$findings
    f$domain <- dom
    all_findings[[dom]] <- f
  }

  if (length(all_findings) == 0L) {
    cli::cli_alert_success("No issues found across all domains!")
    return(tibble::tibble())
  }

  combined <- dplyr::bind_rows(all_findings)
  combined <- combined[combined$severity %in% severity, , drop = FALSE]

  # Re-order columns for readability
  col_order <- c("domain", "severity", "variable", "rule_id", "message",
                 "n_records", "example", "key_value")
  col_order <- intersect(col_order, names(combined))
  combined <- combined[, col_order, drop = FALSE]

  if (nrow(combined) == 0L) {
    cli::cli_alert_success("No issues matching severity: {paste(severity, collapse = ', ')}")
  } else {
    cli::cli_alert_warning("{nrow(combined)} issue(s) across {length(unique(combined$domain))} domain(s)")
  }

  combined
}
