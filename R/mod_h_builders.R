# ==============================================================================
# Module H: Domain-Level Builders
# ==============================================================================

#' Build a complete SDTM domain
#'
#' Main orchestrator. Loads relevant sources, applies joins, derives
#' variables in dependency order, finalizes, validates, and returns the
#' complete domain dataset plus build artifacts.
#'
#' @param domain Character (required).
#' @param target_meta Tibble.
#' @param source_meta Tibble or `NULL`. Optional; not used at runtime.
#' @param raw_data Named list of tibbles.
#' @param config `sdtm_config`.
#' @param rule_set `rule_set`.
#' @param dm_data Tibble or `NULL`. DM data for RFSTDTC; also accepts
#'   a full `build_result` list (auto-extracts `$data`).
#' @param sv_data Tibble or `NULL`.
#' @param create_supp Logical or `NULL`. Default `NULL`, which means the
#'   value from `config$create_supp` is used (itself defaulting to `TRUE`).
#'   Set `FALSE` to keep all variables in the main domain instead of
#'   splitting to SUPP--.
#' @param domain_meta Tibble or `NULL`. Domain-level metadata from
#'   [read_study_metadata_excel()]. If provided, used for sorting and labeling.
#' @param value_level_meta Tibble or `NULL`. Value-level metadata for
#'   per-condition validation.
#' @param return_provenance Logical. Default `TRUE`.
#' @param validate Logical. Default `TRUE`.
#' @param skip_existing Logical. Default `FALSE`. If `TRUE`, skip building
#'   this domain when its output already exists.
#' @param verbose Logical. Default `TRUE`.
#' @return Named list (build_result).
#' @export
build_domain <- function(domain, target_meta, raw_data,
                         config, rule_set,
                         source_meta = NULL,
                         dm_data = NULL, sv_data = NULL,
                         create_supp = NULL, domain_meta = NULL,
                         value_level_meta = NULL,
                         return_provenance = TRUE, validate = TRUE,
                         skip_existing = FALSE,
                         verbose = TRUE) {
  domain <- toupper(domain)
  # Resolve create_supp: explicit param > config > TRUE
  if (is.null(create_supp)) {
    create_supp <- if (!is.null(config$create_supp)) config$create_supp else TRUE
  }
  log_msgs <- character()
  derivation_errors <- list()   # collect per-variable derivation errors
  .log <- function(msg) {
    log_msgs <<- c(log_msgs, paste(Sys.time(), msg))
    if (verbose) cli::cli_alert_info(msg)
  }

  .log(glue::glue("Building domain: {domain}"))

  # Get domain-specific metadata and rules

  dom_meta <- dplyr::filter(target_meta, .data[["domain"]] == .env[["domain"]])
  if (nrow(dom_meta) == 0L) abort(glue::glue("Domain '{domain}' not found in target_meta"))

  dom_rules <- rule_set$rules[[domain]]
  if (is.null(dom_rules)) abort(glue::glue("No compiled rules for domain '{domain}'"))

  # Determine the primary source dataset for this domain
  primary_ds <- NULL
  for (var_name in names(dom_rules)) {
    r <- dom_rules[[var_name]]
    if (!is.null(r$params$dataset) && is.null(primary_ds)) {
      primary_ds <- r$params$dataset
      break
    }
  }

  if (is.null(primary_ds)) {
    # Fall back to convention: try {domain_lower} first, then {domain_lower}_raw
    dom_lc <- tolower(domain)
    if (dom_lc %in% names(raw_data)) {
      primary_ds <- dom_lc
    } else if (paste0(dom_lc, "_raw") %in% names(raw_data)) {
      primary_ds <- paste0(dom_lc, "_raw")
    } else {
      abort(glue::glue("Cannot determine primary source dataset for {domain}"))
    }
  }

  if (!primary_ds %in% names(raw_data)) {
    # Try the common `<name>_raw` suffix convention before aborting
    raw_variant <- paste0(primary_ds, "_raw")
    if (raw_variant %in% names(raw_data)) {
      primary_ds <- raw_variant
    } else {
      abort(glue::glue("Source dataset '{primary_ds}' not found in raw_data"))
    }
  }

  # Start with primary dataset and normalize column names to lowercase
  data <- raw_data[[primary_ds]]
  names(data) <- tolower(names(data))
  .log(glue::glue("Primary source: {primary_ds} ({nrow(data)} rows)"))

  # Auto-merge secondary source datasets referenced by rules
  all_ds <- unique(unlist(lapply(dom_rules, function(r) r$params$dataset)))
  all_ds <- all_ds[!is.na(all_ds) & all_ds != primary_ds]
  dom_spid <- tolower(paste0(tolower(domain), "spid"))  # e.g. "aespid"

  for (sec_ds in all_ds) {
    if (!sec_ds %in% names(raw_data)) next
    sec_data <- raw_data[[sec_ds]]
    names(sec_data) <- tolower(names(sec_data))

    # Rename secondary SPID to domain SPID if needed
    # e.g. sae has "saespid" -> rename to "aespid" to match ae primary
    sec_spid_candidates <- grep("spid$", names(sec_data), value = TRUE)
    if (dom_spid %in% names(data) && !dom_spid %in% names(sec_data)) {
      for (cand in sec_spid_candidates) {
        if (cand != dom_spid) {
          names(sec_data)[names(sec_data) == cand] <- dom_spid
          .log(glue::glue("  Renamed '{cand}' -> '{dom_spid}' in {sec_ds}"))
          break
        }
      }
    }

    # Find common join keys
    common_cols <- intersect(names(data), names(sec_data))
    join_keys <- common_cols[common_cols %in% c("subjectid", "subjid", "usubjid", dom_spid)]
    if (length(join_keys) == 0L) {
      # Fall back to subject-level merge only
      join_keys <- common_cols[common_cols %in% c("subjectid", "subjid", "usubjid")]
    }
    if (length(join_keys) > 0L) {
      # Keep only columns not already in data (except join keys)
      new_cols <- setdiff(names(sec_data), names(data))
      if (length(new_cols) > 0L) {
        sec_slim <- sec_data[, c(join_keys, new_cols), drop = FALSE]
        data <- dplyr::left_join(data, sec_slim, by = join_keys)
        .log(glue::glue("  Merged secondary source: {sec_ds} (by {paste(join_keys, collapse = ', ')})"))
      }
    }
  }

  # Join reference start date for --DY calculations; skip for DM itself.
  # Lookup priority:
  #   1. Config path (ref_start_rule$sdtm_path) — explicit file on disk
  #   2. In-memory dm_data (passed from build_all_domains) or SDTM output folder
  #   3. Raw dataset fallback (ref_start_rule$raw_dataset / raw_variable)
  if (domain != "DM") {
    rsr <- config$ref_start_rule %||%
      list(sdtm_domain = "DM", sdtm_variable = "RFSTDTC",
           raw_dataset = "dm", raw_variable = "rfstdtc")
    sdtm_var <- rsr$sdtm_variable %||% "RFSTDTC"

    # Helper: read a single dataset file from a path
    .read_dataset_file <- function(path) {
      ext <- tolower(tools::file_ext(path))
      switch(ext,
        sas7bdat = haven::read_sas(path),
        xpt      = haven::read_xpt(path),
        rda = , rdata = {
          env <- new.env(parent = emptyenv())
          load(path, envir = env)
          env[[ls(env)[1]]]
        },
        rds = readRDS(path),
        csv = readr::read_csv(path, show_col_types = FALSE),
        xlsx = , xls = readxl::read_excel(path),
        {
          .log("WARNING: Unsupported DM file format: .{ext}")
          NULL
        }
      )
    }

    # ---- Priority 1: explicit path from config ----
    if (is.null(dm_data) && !is.null(rsr$sdtm_path) && nchar(rsr$sdtm_path) > 0) {
      if (file.exists(rsr$sdtm_path)) {
        tryCatch({
          dm_data <- .read_dataset_file(rsr$sdtm_path)
          if (!is.null(dm_data)) {
            dm_data <- tibble::as_tibble(dm_data)
            .log(glue::glue("Loaded DM from config path: {rsr$sdtm_path}"))
          }
        }, error = function(e) {
          .log(glue::glue("WARNING: Failed to load DM from config path '{rsr$sdtm_path}': {e$message}"))
        })
      } else {
        .log(glue::glue("WARNING: DM config path not found: {rsr$sdtm_path}"))
      }
    }

    # ---- Priority 2: in-memory or SDTM output folder ----
    if (is.null(dm_data) && !is.null(config$output_dir)) {
      # Search output folder for dm.* in any supported format
      dm_candidates <- c(
        file.path(config$output_dir, "RDA", "dm.rda"),
        file.path(config$output_dir, "XPT", "dm.xpt"),
        file.path(config$output_dir, "dm.sas7bdat"),
        file.path(config$output_dir, "dm.rds"),
        file.path(config$output_dir, "dm.csv")
      )
      for (cand in dm_candidates) {
        if (file.exists(cand)) {
          tryCatch({
            dm_data <- .read_dataset_file(cand)
            if (!is.null(dm_data)) {
              dm_data <- tibble::as_tibble(dm_data)
              .log(glue::glue("Loaded DM from SDTM folder: {cand}"))
            }
          }, error = function(e) {
            .log(glue::glue("WARNING: Failed to load DM from '{cand}': {e$message}"))
          })
          if (!is.null(dm_data)) break
        }
      }
    }

    ref_joined <- FALSE

    if (!is.null(dm_data)) {
      # ---- Use built DM SDTM data ----
      # Auto-extract $data if a build_domain result list was passed
      if (is.list(dm_data) && "data" %in% names(dm_data) && !is.data.frame(dm_data)) {
        dm_data <- dm_data[["data"]]
      }

      dm_ref <- dm_data
      # Ensure the configured SDTM variable is accessible (case-insensitive)
      all_names <- names(dm_ref)
      all_names_uc <- toupper(all_names)
      sdtm_var_uc <- toupper(sdtm_var)
      if (!sdtm_var %in% all_names) {
        idx <- match(sdtm_var_uc, all_names_uc)
        if (!is.na(idx)) dm_ref[[sdtm_var]] <- dm_ref[[all_names[idx]]]
      }

      # Determine join key (case-insensitive matching)
      data_names_lc <- tolower(names(data))
      dm_names_lc   <- tolower(names(dm_ref))
      data_key <- NULL
      dm_key   <- NULL

      if ("subjectid" %in% data_names_lc && "subjid" %in% dm_names_lc) {
        data_key <- names(data)[match("subjectid", data_names_lc)]
        dm_key   <- names(dm_ref)[match("subjid", dm_names_lc)]
      } else if ("usubjid" %in% data_names_lc && "usubjid" %in% dm_names_lc) {
        data_key <- names(data)[match("usubjid", data_names_lc)]
        dm_key   <- names(dm_ref)[match("usubjid", dm_names_lc)]
      } else if ("subjectid" %in% data_names_lc && "subjectid" %in% dm_names_lc) {
        data_key <- names(data)[match("subjectid", data_names_lc)]
        dm_key   <- names(dm_ref)[match("subjectid", dm_names_lc)]
      }

      if (!is.null(data_key) && !is.null(dm_key) && sdtm_var %in% names(dm_ref)) {
        if (data_key != dm_key) dm_ref[[data_key]] <- dm_ref[[dm_key]]
        dm_cols <- unique(c(data_key, sdtm_var))
        dm_slim <- dplyr::distinct(dm_ref[, dm_cols, drop = FALSE])
        # Rename the reference variable to RFSTDTC so derive_dy() can find it
        if (sdtm_var != "RFSTDTC") names(dm_slim)[names(dm_slim) == sdtm_var] <- "RFSTDTC"
        data <- safe_join(data, dm_slim, by = data_key,
                          type = "left", cardinality = "m:1",
                          on_violation = "warn")
        ref_joined <- TRUE
        .log(glue::glue("Reference date: {sdtm_var} from DM SDTM"))
      }
    }

    # ---- Priority 3: raw dataset fallback ----
    if (!ref_joined) {
      raw_ds  <- rsr$raw_dataset  %||% "dm"
      raw_var <- rsr$raw_variable %||% "rfstdtc"
      # Look for the raw dataset in raw_data list
      raw_key <- if (raw_ds %in% names(raw_data)) raw_ds
                 else if (paste0(raw_ds, "_raw") %in% names(raw_data)) paste0(raw_ds, "_raw")
                 else if (tolower(raw_ds) %in% names(raw_data)) tolower(raw_ds)
                 else NULL

      if (!is.null(raw_key)) {
        dm_ref <- raw_data[[raw_key]]
        dm_names_orig <- names(dm_ref)
        names(dm_ref) <- tolower(names(dm_ref))
        raw_var_lc <- tolower(raw_var)

        # Determine join key for raw data
        data_names_lc <- tolower(names(data))
        raw_names_lc  <- names(dm_ref)  # already lowered
        data_key <- NULL
        raw_key_col <- NULL

        if ("subjectid" %in% data_names_lc && "subjectid" %in% raw_names_lc) {
          data_key    <- names(data)[match("subjectid", data_names_lc)]
          raw_key_col <- "subjectid"
        } else if ("usubjid" %in% data_names_lc && "usubjid" %in% raw_names_lc) {
          data_key    <- names(data)[match("usubjid", data_names_lc)]
          raw_key_col <- "usubjid"
        }

        if (!is.null(data_key) && !is.null(raw_key_col) && raw_var_lc %in% raw_names_lc) {
          dm_slim <- dplyr::distinct(dm_ref[, c(raw_key_col, raw_var_lc), drop = FALSE])
          dm_slim$RFSTDTC <- dm_slim[[raw_var_lc]]
          dm_slim[[data_key]] <- dm_slim[[raw_key_col]]
          data <- safe_join(data, dm_slim[, c(data_key, "RFSTDTC"), drop = FALSE],
                            by = data_key, type = "left",
                            cardinality = "m:1", on_violation = "warn")
          ref_joined <- TRUE
          .log(glue::glue("Reference date: {raw_var} from raw '{raw_key}' (DM not yet built)"))
        }
      }

      if (!ref_joined) {
        .log("WARNING: No reference start date available for --DY derivation. DM not built and raw fallback not found.")
      }
    }
  }  # end domain != "DM"

  # Build dependency order
  dep_result <- build_dependency_graph(rule_set, domain)
  var_order <- dep_result$order

  .log(glue::glue("Derivation order: {paste(var_order, collapse = ' -> ')}"))

  # Apply rules in order
  provenance <- tibble::tibble(var = character(), rule_type = character(),
                                source = character())
  for (var_name in var_order) {
    rule <- dom_rules[[var_name]]
    if (is.null(rule)) next

    # Skip if column already exists with non-NA values (user pre-derived)
    if (skip_existing && var_name %in% names(data)) {
      non_na <- sum(!is.na(data[[var_name]]))
      if (non_na > 0L) {
        .log(glue::glue("Skipping {var_name}: already derived ({non_na} non-NA values)"))
        provenance <- dplyr::bind_rows(provenance, tibble::tibble(
          var = var_name, rule_type = "pre-derived",
          source = "user"
        ))
        next
      }
    }

    tryCatch({
      data <- derive_variable(data, var_name, rule,
                               context = list(config = config,
                                              ct_lib = rule_set$ct_lib,
                                              domain = domain,
                                              raw_data = raw_data))
      provenance <- dplyr::bind_rows(provenance, tibble::tibble(
        var = var_name, rule_type = rule$type,
        source = rule$params$dataset %||% "derived"
      ))
    }, error = function(e) {
      .log(glue::glue("ERROR deriving {var_name}: {e$message}"))
      derivation_errors[[var_name]] <<- e$message
    })
  }

  .log(glue::glue("Derived {ncol(data)} columns, {nrow(data)} rows"))

  # Identify SUPP variables (non-standard vars to move to SUPP--)
  supp_vars <- character()
  if (create_supp && "to_supp" %in% names(dom_meta)) {
    supp_flags <- dom_meta$to_supp
    supp_vars <- dom_meta$var[!is.na(supp_flags) & toupper(trimws(supp_flags)) %in% c("Y", "YES", "TRUE", "1")]
  }

  # Build SUPP dataset before finalize removes extra columns
  supp_data <- NULL
  if (length(supp_vars) > 0L) {
    .log(glue::glue("Building SUPP{domain} for {length(supp_vars)} variable(s): {paste(supp_vars, collapse=', ')}"))
    supp_data <- build_supp(data, domain, dom_meta, vars_to_supp = supp_vars)
    if (!is.null(supp_data)) {
      .log(glue::glue("SUPP{domain}: {nrow(supp_data)} rows"))
    }
  }

  # Finalize (selects only non-SUPP target variables when create_supp=TRUE,
  # or keeps all target variables when create_supp=FALSE)
  final <- finalize_domain(data, domain, dom_meta, config,
                           create_seq = FALSE, create_supp = create_supp,
                           domain_meta = domain_meta)
  data <- final$data

  # Validate
  report <- new_validation_report(domain = domain)
  if (validate) {
    ct_lib <- rule_set$ct_lib
    report <- validate_domain_structure(data, dom_meta, domain, config,
                                        ct_lib = ct_lib,
                                        value_level_meta = value_level_meta,
                                        domain_meta = domain_meta)
  }

  # Add derivation errors as ERROR findings so they appear in the report
  for (vn in names(derivation_errors)) {
    report <- add_finding(report, rule_id = "derivation_error",
                          severity = "ERROR", domain = domain,
                          variable = vn,
                          message = paste0("Derivation failed for ", vn, ": ",
                                           derivation_errors[[vn]]))
  }

  result <- list(
    data       = data,
    supp       = supp_data,
    relrec     = NULL,
    report     = report,
    log        = log_msgs,
    provenance = provenance,
    artifacts  = list(dep_graph = dep_result)
  )
  structure(result, class = "build_result")
}

#' Build domain from multiple sources
#' @param domain Character.
#' @param rule_set `rule_set`.
#' @param raw_data Named list of tibbles.
#' @param config `sdtm_config`.
#' @return Tibble.
#' @export
build_domain_from_sources <- function(domain, rule_set, raw_data, config) {
  dom_rules <- rule_set$rules[[domain]]
  sources_needed <- unique(unlist(lapply(dom_rules, function(r) r$params$dataset)))
  sources_needed <- sources_needed[!is.na(sources_needed)]

  if (length(sources_needed) == 0L) return(tibble::tibble())

  primary <- sources_needed[1]
  if (!primary %in% names(raw_data)) abort(glue::glue("Source '{primary}' not in raw_data"))
  raw_data[[primary]]
}

#' Execute ordered rules on a domain
#' @param data Tibble.
#' @param domain_rules List.
#' @param context `build_context` or list.
#' @param stop_on_error Logical. Default `TRUE`.
#' @return Named list.
#' @export
apply_domain_rules <- function(data, domain_rules, context,
                               stop_on_error = TRUE) {
  for (var_name in names(domain_rules)) {
    rule <- domain_rules[[var_name]]
    tryCatch({
      data <- derive_variable(data, var_name, rule, context)
    }, error = function(e) {
      if (stop_on_error) abort(e$message)
    })
  }
  list(data = data, context = context)
}

#' Single variable derivation dispatcher
#' @param data Tibble.
#' @param var Character.
#' @param rule List.
#' @param context List or `build_context`.
#' @return Tibble.
#' @export
derive_variable <- function(data, var, rule, context) {
  type   <- rule$type
  params <- rule$params

  # If this rule has VLM branches, handle them specially
  if (isTRUE(rule$has_vlm) && !is.null(params$vlm_branches) &&
      length(params$vlm_branches) > 0L) {
    data <- .derive_with_vlm(data, var, rule, context)
    return(data)
  }

  switch(type,
    constant = {
      val <- params$value
      # Resolve config$ references (e.g. "config$studyid" -> actual value)
      if (is.character(val) && grepl("^config\\$", val)) {
        config_key <- sub("^config\\$", "", val)
        if (!is.null(context$config[[config_key]])) {
          val <- context$config[[config_key]]
        }
      }
      # Resolve "auto" for well-known constants
      if (identical(val, "auto")) {
        if (var == "STUDYID" && !is.null(context$config$studyid)) {
          val <- toupper(context$config$studyid)
        } else if (var == "DOMAIN" && !is.null(context$domain)) {
          val <- context$domain
        }
      }
      data <- derive_constant(data, var, value = val)
    },
    sourceid = {
      form_id  <- params$form_id
      ds_name  <- params$dataset  %||% "review_status"
      id_col   <- params$id_col   %||% "formid"
      name_col <- params$name_col %||% "formname"
      data <- derive_sourceid(data, var, form_id = form_id,
                              raw_data = context$raw_data,
                              dataset  = ds_name,
                              id_col   = id_col,
                              name_col = name_col)
    },
    direct_map = {
      src_col <- params$column %||% params$source_var
      src_ds  <- params$dataset

      # If the source column isn't in data, check if we can find it
      if (!src_col %in% names(data)) {
        # Try lowercase / uppercase
        if (tolower(src_col) %in% names(data)) {
          src_col <- tolower(src_col)
        } else if (toupper(src_col) %in% names(data)) {
          src_col <- toupper(src_col)
        } else if (!is.null(src_ds) && src_ds %in% names(context$raw_data)) {
          # Late-bind from raw_data when auto-merge didn't cover it
          sec <- context$raw_data[[src_ds]]
          names(sec) <- tolower(names(sec))
          src_col <- tolower(src_col)
          if (src_col %in% names(sec)) {
            join_keys <- intersect(names(data), names(sec))
            join_keys <- join_keys[join_keys %in% c("subjectid", "subjid", "usubjid")]
            if (length(join_keys) > 0L) {
              new_cols <- c(join_keys, src_col)
              data <- dplyr::left_join(data, sec[, new_cols, drop = FALSE],
                                       by = join_keys)
            }
          } else {
            abort(glue::glue("derive_variable({var}): source column '{src_col}' not found in {src_ds}"))
          }
        } else {
          abort(glue::glue("derive_variable({var}): source column '{src_col}' not found"))
        }
      }

      xform <- NULL
      if (!is.null(params$transform)) {
        xform <- switch(params$transform,
          toupper = toupper,
          tolower = tolower,
          trimws  = trimws,
          NULL
        )
      }

      # Auto-infer type from metadata DATA_TYPE when not explicitly set
      map_type <- params$type
      if (is.null(map_type) && !is.null(rule$target_type)) {
        map_type <- switch(rule$target_type,
          num  = "numeric",
          char = "character",
          NULL
        )
      }

      data <- map_direct(data, var, src_col, transform = xform, type = map_type)
    },
    ct_assign = {
      src_col     <- params$column
      codelist_id <- params$codelist_id %||% rule$codelist_id
      # Determine unknown_policy from extensibility metadata
      unknown_pol <- params$unknown_policy %||% NULL
      if (is.null(unknown_pol)) {
        is_ext <- rule$is_extensible
        if (!is.null(is_ext) && toupper(is_ext) == "YES") {
          unknown_pol <- "keep"  # extensible codelist: accept non-standard values
        } else {
          unknown_pol <- "warn_and_keep"
        }
      }

      # Build ct_lib from params$ct_map if available, else use context
      if (!is.null(params$ct_map)) {
        ct_tbl <- tibble::tibble(
          codelist_id = codelist_id,
          input_value = names(params$ct_map),
          coded_value = unname(unlist(params$ct_map))
        )
      } else if (!is.null(context$ct_lib)) {
        ct_tbl <- context$ct_lib
      } else {
        abort(glue::glue("No CT library available for {var}"))
      }

      if (!src_col %in% names(data) && tolower(src_col) %in% names(data)) {
        src_col <- tolower(src_col)
      }

      data <- assign_ct(data, var, src_col, codelist_id, ct_tbl,
                        case_sensitive = FALSE, unknown_policy = unknown_pol)
    },
    iso_dtc = {
      # Get date column reference
      date_info <- params$date_col
      date_col <- if (is.list(date_info)) date_info$column else date_info

      if (!date_col %in% names(data) && tolower(date_col) %in% names(data)) {
        date_col <- tolower(date_col)
      }

      # Get time column reference (may be NULL)
      time_col <- NULL
      if (!is.null(params$time_col)) {
        time_info <- params$time_col
        time_col <- if (is.list(time_info)) time_info$column else time_info
        if (!is.null(time_col) && !time_col %in% names(data)) {
          if (tolower(time_col) %in% names(data)) time_col <- tolower(time_col)
          else time_col <- NULL
        }
      }

      # Parse dates and format as ISO
      parsed <- parse_partial_date(data[[date_col]])
      if (!is.null(time_col)) {
        parsed <- combine_date_time(parsed, data[[time_col]])
      }
      data[[var]] <- format_iso_dtc(parsed)
    },
    dy = {
      dtc_var <- params$dtc_var
      ref_var <- params$ref_var %||% "RFSTDTC"

      # Check if DTC var exists yet
      if (!dtc_var %in% names(data)) {
        data[[var]] <- NA_real_
      } else {
        # Check if ref is available (may need to look for joined column)
        if (!ref_var %in% names(data)) {
          # Try to get from dm_raw join
          ref_ds  <- params$ref_dataset
          ref_col <- params$ref_column
          if (!is.null(ref_col) && ref_col %in% names(data)) {
            data$RFSTDTC <- data[[ref_col]]
            ref_var <- "RFSTDTC"
          } else if ("RFSTDTC" %in% names(data)) {
            ref_var <- "RFSTDTC"
          }
        }
        data <- derive_dy(data, var, dtc_var, ref_var)
      }
    },
    seq = {
      by_vars    <- unlist(params$by %||% list("USUBJID"))
      order_vars <- unlist(params$order_by %||% list())
      ties_method <- params$ties %||% "dense"

      # Map to actual column names (uppercase target vars may not exist yet,
      # but the ordered columns should be in data or as derived target vars)
      actual_by <- character()
      for (b in by_vars) {
        if (b %in% names(data)) { actual_by <- c(actual_by, b) }
        else if (tolower(b) %in% names(data)) { actual_by <- c(actual_by, tolower(b)) }
      }
      if (length(actual_by) == 0L) actual_by <- "usubjid"

      actual_order <- character()
      for (o in order_vars) {
        if (o %in% names(data)) actual_order <- c(actual_order, o)
        else if (tolower(o) %in% names(data)) actual_order <- c(actual_order, tolower(o))
      }

      data <- derive_seq(data, var, by = actual_by,
                         order_by = if (length(actual_order) > 0) actual_order else actual_by,
                         ties = ties_method)
    },
    join = {
      # Handle join-based derivation
      data[[var]] <- NA_character_
    },
    epoch = {
      dtc_var   <- params$dtc_var
      ref_var   <- params$ref_var %||% "RFSTDTC"
      epoch_map <- context$config$epoch_map
      if (!is.null(epoch_map) && length(epoch_map) > 0L) {
        data <- derive_epoch(data, var, dtc_var, epoch_map, ref_var)
      } else {
        data[[var]] <- NA_character_
      }
    },
    coalesce = {
      sources <- unlist(params$columns %||% params$sources)
      # Resolve column names
      actual <- character()
      for (s in sources) {
        if (s %in% names(data)) actual <- c(actual, s)
        else if (tolower(s) %in% names(data)) actual <- c(actual, tolower(s))
      }
      if (length(actual) > 0L) {
        data <- derive_coalesce(data, var, actual)
      } else {
        data[[var]] <- NA_character_
      }
    },
    if_else = {
      cond       <- params$condition
      true_val   <- params$true_value
      false_val  <- params$false_value
      miss_val   <- params$missing_value %||% NA
      data <- derive_if_else(data, var, cond, true_val, false_val, miss_val)
    },
    case_when = {
      conds   <- params$conditions
      default <- params$default %||% NA
      # If conditions is a string (from METHOD column), evaluate it as R expression
      if (is.character(conds) && length(conds) == 1L &&
          grepl("^list\\(", conds)) {
        conds <- tryCatch(eval(parse(text = conds)), error = function(e) {
          warn(glue::glue("case_when: cannot parse conditions for {var}: {e$message}"))
          list()
        })
      }
      data <- derive_case_when(data, var, conds, default)
    },
    ct_decode = {
      src_col     <- params$column
      codelist_id <- params$codelist_id %||% rule$codelist_id
      if (!src_col %in% names(data) && tolower(src_col) %in% names(data)) {
        src_col <- tolower(src_col)
      }
      ct_tbl <- context$ct_lib
      if (!is.null(ct_tbl)) {
        data <- decode_ct(data, var, src_col, codelist_id, ct_tbl)
      } else {
        data[[var]] <- NA_character_
      }
    },
    concat = {
      sources <- unlist(params$columns %||% params$sources)
      sep     <- params$separator %||% params$sep %||% ""
      actual  <- character()
      for (s in sources) {
        if (s %in% names(data)) actual <- c(actual, s)
        else if (tolower(s) %in% names(data)) actual <- c(actual, tolower(s))
      }
      if (length(actual) > 0L) {
        data <- derive_concat(data, var, actual, sep = sep)
      } else {
        data[[var]] <- NA_character_
      }
    },
    visit = {
      dy_var    <- params$dy_var %||% NULL
      visit_map <- context$config$visit_map
      data <- derive_visit(data, var, visit_map, dy_var = dy_var)
    },
    visitnum = {
      visit_var <- params$visit_var %||% "VISIT"
      if (!visit_var %in% names(data) && tolower(visit_var) %in% names(data)) {
        visit_var <- tolower(visit_var)
      }
      visit_map <- context$config$visit_map
      data <- derive_visitnum(data, var, visit_map, visit_var = visit_var)
    },
    baseline_flag = {
      visit_var      <- params$visit_var %||% "VISIT"
      baseline_visit <- params$baseline_visit %||% "BASELINE"
      by_vars        <- unlist(params$by %||% list("USUBJID"))
      if (!visit_var %in% names(data) && tolower(visit_var) %in% names(data)) {
        visit_var <- tolower(visit_var)
      }
      actual_by <- character()
      for (b in by_vars) {
        if (b %in% names(data)) actual_by <- c(actual_by, b)
        else if (tolower(b) %in% names(data)) actual_by <- c(actual_by, tolower(b))
      }
      if (length(actual_by) == 0L) actual_by <- "USUBJID"
      data <- derive_baseline_flag(data, var, by = actual_by,
                                   baseline_visit = baseline_visit,
                                   visit_var = visit_var)
    },
    occurrence = {
      src_var       <- params$source_var
      present_val   <- params$present_value %||% "Y"
      absent_val    <- params$absent_value %||% NA_character_
      # Allow "NA" string to represent actual NA
      if (identical(present_val, "NA")) present_val <- NA_character_
      if (identical(absent_val, "NA"))  absent_val  <- NA_character_
      if (!is.null(src_var) && !src_var %in% names(data) &&
          tolower(src_var) %in% names(data)) {
        src_var <- tolower(src_var)
      }
      data <- derive_occurrence(data, var, source_var = src_var,
                                present_value = present_val,
                                absent_value = absent_val)
    },
    status = {
      result_var    <- params$result_var
      done_val      <- params$done_value %||% NA_character_
      not_done_val  <- params$not_done_value %||% "NOT DONE"
      if (!result_var %in% names(data) && tolower(result_var) %in% names(data)) {
        result_var <- tolower(result_var)
      }
      data <- derive_status(data, var, result_var,
                            done_value = done_val, not_done_value = not_done_val)
    },
    duration = {
      start_dtc <- params$start_dtc
      end_dtc   <- params$end_dtc
      units     <- params$units %||% "days"
      data <- derive_duration(data, var, start_dtc, end_dtc, units = units)
    },
    numeric_round = {
      src_var <- params$source_var %||% params$column
      # Prefer significant_digits from rule metadata, then params, then default 0
      digits <- rule$significant_digits
      if (is.null(digits) || is.na(digits)) digits <- params$digits %||% 0L
      digits <- as.integer(digits)

      if (!is.null(src_var) && !src_var %in% names(data) &&
          tolower(src_var) %in% names(data)) {
        src_var <- tolower(src_var)
      }
      if (!is.null(src_var) && src_var %in% names(data)) {
        data <- derive_numeric_round(data, var, src_var, digits = digits)
      } else {
        # Numeric round without source = apply to the var itself if it exists
        if (var %in% names(data)) {
          data[[var]] <- round(as.numeric(data[[var]]), digits = digits)
        } else {
          data[[var]] <- NA_real_
        }
      }
    },
    unusbjid = , usubjid = {
      subjid_col <- params$subjid_col %||% "subjid"
      sep        <- params$sep %||% "-"
      if (!subjid_col %in% names(data) && tolower(subjid_col) %in% names(data)) {
        subjid_col <- tolower(subjid_col)
      }
      data <- derive_usubjid(data, studyid = context$config$studyid,
                             subjid_col = subjid_col, sep = sep)
    },
    visitdy = {
      visit_var <- params$visit_var %||% "VISIT"
      dy_var    <- params$dy_var %||% NULL
      if (!visit_var %in% names(data) && tolower(visit_var) %in% names(data)) {
        visit_var <- tolower(visit_var)
      }
      vm <- if (!is.null(context$config$visit_map)) context$config$visit_map else NULL
      data <- derive_visitdy(data, var, visit_var = visit_var,
                              dy_var = dy_var, visit_map = vm)
    },
    tpt = {
      src_var <- params$source_var %||% NULL
      tpt_map <- params$tpt_map %||% NULL
      data <- derive_tpt(data, var, source_var = src_var, tpt_map = tpt_map)
    },
    regex_extract = {
      src_col <- params$column
      pattern <- params$pattern
      grp     <- as.integer(params$group %||% 1L)
      if (!src_col %in% names(data) && tolower(src_col) %in% names(data)) {
        src_col <- tolower(src_col)
      }
      data <- derive_regex_extract(data, var, src_col, pattern, group = grp)
    },
    regex_replace = {
      src_col     <- params$column
      pattern     <- params$pattern
      replacement <- params$replacement
      all_flag    <- as.logical(params$all %||% TRUE)
      if (!src_col %in% names(data) && tolower(src_col) %in% names(data)) {
        src_col <- tolower(src_col)
      }
      data <- derive_regex_replace(data, var, src_col, pattern, replacement,
                                    all = all_flag)
    },
    trim_pad = {
      src_col <- params$column
      side    <- params$side %||% "both"
      width   <- if (!is.null(params$width)) as.integer(params$width) else NULL
      pad_chr <- params$pad %||% " "
      if (!src_col %in% names(data) && tolower(src_col) %in% names(data)) {
        src_col <- tolower(src_col)
      }
      data <- derive_trim_pad(data, var, src_col, width = width,
                               side = side, pad = pad_chr)
    },
    lastobs_flag = {
      by_vars   <- unlist(params$by %||% list("USUBJID"))
      order_var <- params$order_var %||% NULL
      actual_by <- character()
      for (b in by_vars) {
        if (b %in% names(data)) actual_by <- c(actual_by, b)
        else if (tolower(b) %in% names(data)) actual_by <- c(actual_by, tolower(b))
      }
      if (length(actual_by) == 0L) actual_by <- "USUBJID"
      if (!is.null(order_var)) {
        if (!order_var %in% names(data) && tolower(order_var) %in% names(data)) {
          order_var <- tolower(order_var)
        }
      }
      data <- derive_lastobs_flag(data, var, by = actual_by,
                                   order_var = order_var %||% actual_by[1])
    },
    seriousness = {
      flag_vars     <- unlist(params$flag_vars %||% list())
      present_val   <- params$present_value %||% "Y"
      absent_val    <- params$absent_value %||% "N"
      actual_flags <- character()
      for (f in flag_vars) {
        if (f %in% names(data)) actual_flags <- c(actual_flags, f)
        else if (tolower(f) %in% names(data)) actual_flags <- c(actual_flags, tolower(f))
      }
      if (length(actual_flags) > 0L) {
        data <- derive_seriousness(data, var, actual_flags,
                                    present_value = present_val,
                                    absent_value = absent_val)
      } else {
        data[[var]] <- absent_val
      }
    },
    ref_time_point = {
      rtpt_var   <- var
      tpt_var    <- params$tpt_var %||% NULL
      source_var <- params$source_var %||% NULL
      tpt_label  <- params$tpt_label %||% ""
      mode       <- params$mode %||% "pattern"
      mapping    <- params$mapping %||% list("BEFORE" = "before", "AFTER" = "after")

      if (!is.null(source_var)) {
        if (!source_var %in% names(data) && tolower(source_var) %in% names(data)) {
          source_var <- tolower(source_var)
        }
        if (source_var %in% names(data)) {
          data <- derive_ref_time_point(data, rtpt_var,
                                         tpt_var %||% rtpt_var,
                                         source_var, tpt_label,
                                         mode = mode, mapping = mapping)
        }
      } else {
        data[[var]] <- NA_character_
      }
    },
    dict_version = {
      dict_ds  <- params$dataset
      prefix   <- params$prefix %||% "MedDRA"
      dictvar  <- params$dictvar %||% "DictInstance"
      coded_var <- params$coded_var %||% NULL

      # Locate the source dataset in raw_data or merged data
      dict_data <- NULL
      if (!is.null(dict_ds) && !is.null(context$raw_data) && dict_ds %in% names(context$raw_data)) {
        dict_data <- context$raw_data[[dict_ds]]
      }

      version_str <- ""
      if (!is.null(dict_data)) {
        if (tolower(prefix) == "whodrug") {
          version_str <- get_whodrug_version(dict_data, dictvar = dictvar)
        } else {
          version_str <- get_meddra_version(dict_data, dictvar = dictvar)
        }
      }

      if (nzchar(version_str)) {
        full_label <- paste(prefix, version_str)
      } else {
        full_label <- ""
      }

      # If coded_var specified, only set version where coded variable is non-NA
      if (!is.null(coded_var)) {
        ref <- coded_var
        if (!ref %in% names(data) && tolower(ref) %in% names(data)) ref <- tolower(ref)
        if (ref %in% names(data)) {
          data[[var]] <- dplyr::if_else(!is.na(data[[ref]]), full_label, "")
        } else {
          data[[var]] <- full_label
        }
      } else {
        data[[var]] <- full_label
      }
    },
    {
      # Unknown rule type — set NA with warning
      warn(glue::glue("Unknown rule type '{type}' for variable {var}"))
      data[[var]] <- NA
    }
  )

  data
}

# Internal: Derive a variable using value-level metadata branches
# Each VLM branch has a condition (R expression) and may override
# codelist, significant_digits, length, or type.
# The function evaluates each condition and applies the appropriate
# derivation per branch, then assembles results using case_when logic.
.derive_with_vlm <- function(data, var, rule, context) {
  branches <- rule$params$vlm_branches
  base_type <- rule$type
  params    <- rule$params
  ct_lib    <- context$ct_lib

  n <- nrow(data)
  # Initialize result column
  result <- rep(NA, n)

  for (branch in branches) {
    cond_expr <- branch$condition
    if (is.na(cond_expr) || is.null(cond_expr)) next

    # Evaluate the WHERE condition mask
    mask <- tryCatch(
      rlang::eval_tidy(rlang::parse_expr(cond_expr), data = data),
      error = function(e) {
        warn(glue::glue("VLM condition eval error for {var}: {e$message}"))
        rep(FALSE, n)
      }
    )
    mask[is.na(mask)] <- FALSE

    if (!any(mask)) next

    # For rows matching this condition, apply derivation branch
    branch_cl_id <- branch$codelist_id %||% rule$codelist_id

    if (base_type == "ct_assign" && !is.null(branch_cl_id) && !is.null(ct_lib)) {
      # CT assignment with per-branch codelist
      src_col <- params$column %||% params$source_var
      if (!src_col %in% names(data) && tolower(src_col) %in% names(data)) {
        src_col <- tolower(src_col)
      }
      cl <- dplyr::filter(ct_lib, .data$codelist_id == !!branch_cl_id)
      if (nrow(cl) > 0L && "input_value" %in% names(cl)) {
        lookup <- stats::setNames(cl$coded_value, tolower(cl$input_value))
        # Identity pass-through for coded_value
        id_lookup <- stats::setNames(cl$coded_value, tolower(cl$coded_value))
        lookup <- c(lookup, id_lookup)
        lookup <- lookup[!duplicated(names(lookup))]
      } else if (nrow(cl) > 0L) {
        lookup <- stats::setNames(cl$coded_value, tolower(cl$coded_value))
      } else {
        lookup <- character()
      }
      raw_vals <- data[[src_col]]
      mapped_vals <- unname(lookup[tolower(raw_vals)])
      # Keep raw value for unmatched (extensible codelist behavior)
      unmapped <- !is.na(raw_vals) & is.na(mapped_vals)
      mapped_vals[unmapped] <- raw_vals[unmapped]
      result[mask] <- mapped_vals[mask]

    } else if (base_type == "direct_map") {
      src_col <- params$column %||% params$source_var
      if (!src_col %in% names(data) && tolower(src_col) %in% names(data)) {
        src_col <- tolower(src_col)
      }
      result[mask] <- as.character(data[[src_col]])[mask]

    } else {
      # Generic: use whatever the base rule type produces
      # For now, attempt direct mapping from the base rule source
      if (!is.null(params$column %||% params$source_var)) {
        src_col <- params$column %||% params$source_var
        if (!src_col %in% names(data) && tolower(src_col) %in% names(data)) {
          src_col <- tolower(src_col)
        }
        result[mask] <- as.character(data[[src_col]])[mask]
      }
    }

    # Apply per-branch significant_digits rounding
    if (!is.null(branch$significant_digits) && !is.na(branch$significant_digits)) {
      numeric_vals <- suppressWarnings(as.numeric(result[mask]))
      rounded_vals <- round(numeric_vals, digits = as.integer(branch$significant_digits))
      result[mask] <- ifelse(is.na(rounded_vals),
                              result[mask],
                              as.character(rounded_vals))
    }
  }

  # Also handle non-VLM base derivation for rows not matched by any branch
  # Apply the default (base params) rule for unmatched rows
  unmatched <- is.na(result)
  if (any(unmatched) && !is.null(params$column %||% params$source_var)) {
    src_col <- params$column %||% params$source_var
    if (!src_col %in% names(data) && tolower(src_col) %in% names(data)) {
      src_col <- tolower(src_col)
    }
    if (src_col %in% names(data)) {
      result[unmatched] <- as.character(data[[src_col]])[unmatched]
    }
  }

  data[[var]] <- result
  data
}


#' Finalize a domain dataset for export
#' @param data Tibble.
#' @param domain Character.
#' @param target_meta Tibble.
#' @param config `sdtm_config`.
#' @param create_seq Logical. Default `TRUE`.
#' @param create_supp Logical. Default `TRUE`.
#' @param strict_labels Logical. Default `TRUE`.
#' @param domain_meta Tibble or `NULL`. Domain-level metadata from
#'   [read_study_metadata_excel()]. If provided, uses `keys` column
#'   to sort rows and `description` column to set the dataset label.
#' @return Named list.
#' @export
finalize_domain <- function(data, domain, target_meta, config,
                            create_seq = TRUE, create_supp = TRUE,
                            strict_labels = TRUE, domain_meta = NULL) {
  dom_meta <- dplyr::filter(target_meta, .data[["domain"]] == .env[["domain"]])
  target_vars <- dom_meta$var

  # Exclude SUPP variables from the main dataset (only when create_supp=TRUE)
  if (isTRUE(create_supp) && "to_supp" %in% names(dom_meta)) {
    supp_flags <- dom_meta$to_supp
    non_supp <- is.na(supp_flags) | !(toupper(trimws(supp_flags)) %in% c("Y", "YES", "TRUE", "1"))
    target_vars <- dom_meta$var[non_supp]
  }

  # Ensure all target variables exist (add NA columns for missing)
  for (v in target_vars) {
    if (!v %in% names(data)) {
      data[[v]] <- NA
    }
  }

  # Select only target variables (in order)
  available <- intersect(target_vars, names(data))
  data <- data[, available, drop = FALSE]

  # Apply labels as attributes
  for (i in seq_len(nrow(dom_meta))) {
    v <- dom_meta$var[i]
    if (v %in% names(data) && !is.na(dom_meta$label[i])) {
      attr(data[[v]], "label") <- dom_meta$label[i]
    }
  }

  # Type enforcement
  for (i in seq_len(nrow(dom_meta))) {
    v <- dom_meta$var[i]
    if (!v %in% names(data)) next
    expected_type <- dom_meta$type[i]
    if (is.na(expected_type)) next
    if (expected_type == "char" && !is.character(data[[v]])) {
      data[[v]] <- as.character(data[[v]])
    }
    if (expected_type == "num" && !is.numeric(data[[v]])) {
      data[[v]] <- suppressWarnings(as.numeric(data[[v]]))
    }
  }

  # Apply significant_digits rounding for numeric variables
  if ("significant_digits" %in% names(dom_meta)) {
    for (i in seq_len(nrow(dom_meta))) {
      v <- dom_meta$var[i]
      if (!v %in% names(data)) next
      sig_d <- dom_meta$significant_digits[i]
      if (!is.na(sig_d) && is.numeric(data[[v]])) {
        data[[v]] <- round(data[[v]], digits = as.integer(sig_d))
      }
    }
  }

  # Apply length enforcement for character variables (truncate to max length)
  if ("length" %in% names(dom_meta)) {
    for (i in seq_len(nrow(dom_meta))) {
      v <- dom_meta$var[i]
      if (!v %in% names(data)) next
      max_len <- dom_meta$length[i]
      if (!is.na(max_len) && is.numeric(max_len) && is.character(data[[v]])) {
        max_len <- as.integer(max_len)
        too_long <- !is.na(data[[v]]) & nchar(data[[v]]) > max_len
        if (any(too_long)) {
          data[[v]][too_long] <- substr(data[[v]][too_long], 1L, max_len)
        }
      }
    }
  }

  # Sort by domain keys from domain_meta (if available)
  if (!is.null(domain_meta)) {
    dom_info <- dplyr::filter(domain_meta, .data[["domain"]] == .env[["domain"]])
    if (nrow(dom_info) > 0L && !is.na(dom_info$keys[1L])) {
      key_vars <- trimws(strsplit(dom_info$keys[1L], ",\\s*")[[1L]])
      valid_keys <- intersect(key_vars, names(data))
      if (length(valid_keys) > 0L) {
        data <- dplyr::arrange(data, dplyr::across(dplyr::all_of(valid_keys)))
      }
    }
    # Set dataset label from domain description
    if (nrow(dom_info) > 0L && !is.na(dom_info$description[1L])) {
      attr(data, "label") <- dom_info$description[1L]
    }
    # Store structure as comment attribute
    if (nrow(dom_info) > 0L && "structure" %in% names(dom_info) &&
        !is.na(dom_info$structure[1L])) {
      comment(data) <- dom_info$structure[1L]
    }
  }

  list(data = tibble::as_tibble(data), supp = NULL)
}

#' Derive domain default variables
#' @param data Tibble.
#' @param domain Character.
#' @param config `sdtm_config`.
#' @return Tibble.
#' @export
derive_domain_defaults <- function(data, domain, config) {
  if (!"STUDYID" %in% names(data)) data$STUDYID <- config$studyid
  if (!"DOMAIN" %in% names(data))  data$DOMAIN <- domain
  data
}

#' Build SUPP-- dataset
#' @param domain_data Tibble.
#' @param domain Character.
#' @param target_meta Tibble.
#' @param vars_to_supp Character vector or `NULL`.
#' @param idvar Character or `NULL`.
#' @param qnam_rules Named list or `NULL`.
#' @param qlabel_source Character. Default `"metadata"`.
#' @param qorig_source Character. Default `"metadata"`.
#' @param qeval Character. Default `NA_character_`.
#' @return Tibble or `NULL`.
#' @export
build_supp <- function(domain_data, domain, target_meta,
                       vars_to_supp = NULL, idvar = NULL,
                       qnam_rules = NULL, qlabel_source = "metadata",
                       qorig_source = "metadata", qeval = NA_character_) {
  if (is.null(vars_to_supp) || length(vars_to_supp) == 0L) return(NULL)
  seq_var <- paste0(domain, "SEQ")
  if (is.null(idvar)) idvar <- seq_var

  # Build label lookup from metadata
  dom_meta <- dplyr::filter(target_meta, .data[["domain"]] == .env[["domain"]])
  label_lookup <- stats::setNames(dom_meta$label, dom_meta$var)

  rows <- list()
  for (v in vars_to_supp) {
    if (!v %in% names(domain_data)) next
    qlabel <- if (!is.null(label_lookup[[v]]) && !is.na(label_lookup[[v]])) {
      label_lookup[[v]]
    } else {
      v
    }
    for (i in seq_len(nrow(domain_data))) {
      val <- domain_data[[v]][i]
      if (is.na(val)) next
      rows <- c(rows, list(tibble::tibble(
        STUDYID  = domain_data$STUDYID[i],
        RDOMAIN  = domain,
        USUBJID  = domain_data$USUBJID[i],
        IDVAR    = idvar,
        IDVARVAL = as.character(domain_data[[idvar]][i]),
        QNAM     = substr(v, 1, 8),
        QLABEL   = qlabel,
        QVAL     = as.character(val),
        QORIG    = "CRF",
        QEVAL    = qeval
      )))
    }
  }
  if (length(rows) == 0L) return(NULL)
  dplyr::bind_rows(rows)
}

#' Build RELREC dataset
#'
#' Constructs the SDTM RELREC (Related Records) dataset from relationship
#' specifications defined in `config.yaml`.  Each specification describes
#' a pair of domains linked by cross-reference columns in the raw CRF data
#' or by a domain-level structural relationship.
#'
#' Two types of relationships are supported:
#'
#' \describe{
#'   \item{record}{Subject-level links derived from raw data columns
#'     (e.g., `cmae1` in CM linking to an AE via `AESPID`).  One pair of
#'     RELREC rows is produced for each non-missing link value.}
#'   \item{domain}{Dataset-level structural relationships (e.g., EC ↔ EX
#'     via ECLNKGRP / EXLNKID).  A single pair of rows with empty USUBJID
#'     and IDVARVAL is produced.}
#' }
#'
#' @param relationship_specs List of relationship specification lists.
#'   Each element must contain:
#'   \describe{
#'     \item{rdomain1}{Character. First domain (e.g. "CM").}
#'     \item{rdomain2}{Character. Second domain (e.g. "AE").}
#'     \item{idvar1}{Character. Identifier variable in domain 1 (e.g. "CMSPID").}
#'     \item{idvar2}{Character. Identifier variable in domain 2 (e.g. "AESPID").}
#'     \item{dataset}{Character. Raw dataset containing the link columns.}
#'     \item{link_prefix}{Character. Column prefix to search for link values
#'       (e.g. "cmae").  All columns matching `^<link_prefix>[0-9]*$` are
#'       pivoted to extract cross-references.}
#'     \item{relid_prefix}{Character. Optional prefix for RELID
#'       (default: paste0(rdomain1, rdomain2)).}
#'     \item{type}{Character. Either `"record"` (default) or `"domain"`.}
#'     \item{reltype1}{Character. Optional RELTYPE for domain 1 row.}
#'     \item{reltype2}{Character. Optional RELTYPE for domain 2 row.}
#'     \item{idvar1_val_col}{Character. Optional column name to use as
#'       IDVARVAL for domain 1 (default: `<rdomain1>spid` lowercased).}
#'   }
#' @param raw_data Named list of raw data tibbles keyed by dataset name.
#' @param config `sdtm_config` object (needs `$studyid`).
#' @param built_domains Optional named list of built SDTM domain tibbles
#'   (e.g., `list(DS = ds_tibble, XS = xs_tibble)`).
#'   Required only for `type = "identity"` or `type = "seq_lookup"` specs.
#' @return A tibble with RELREC columns, or `NULL` if no relationships found.
#' @export
build_relrec <- function(relationship_specs, raw_data, config,
                         built_domains = NULL) {
  if (is.null(relationship_specs) || length(relationship_specs) == 0L) return(NULL)

  studyid <- config$studyid %||% "UNKNOWN"
  all_blocks <- list()

  for (spec in relationship_specs) {
    r1 <- toupper(spec$rdomain1)
    r2 <- toupper(spec$rdomain2)
    relid_pfx <- spec$relid_prefix %||% paste0(r1, r2)
    rel_type <- spec$type %||% "record"

    # --- Domain-level relationship (no per-subject data needed) ---
    if (rel_type == "domain") {
      block <- tibble::tibble(
        STUDYID  = rep(studyid, 2L),
        RDOMAIN  = c(r1, r2),
        USUBJID  = c("", ""),
        IDVAR    = c(spec$idvar1 %||% "", spec$idvar2 %||% ""),
        IDVARVAL = c("", ""),
        RELTYPE  = c(spec$reltype1 %||% "", spec$reltype2 %||% ""),
        RELID    = c(relid_pfx, relid_pfx)
      )
      all_blocks <- c(all_blocks, list(block))
      next
    }

    # --- Identity relationship (from built domains, 1:1 SPID match) ---
    # e.g. XS <-> AE where XSSPID = AESPID
    if (rel_type == "identity") {
      built1 <- built_domains[[r1]]
      if (is.null(built1) || nrow(built1) == 0L) next
      names(built1) <- toupper(names(built1))
      spid1_col <- toupper(spec$idvar1 %||% paste0(r1, "SPID"))
      subj_col  <- "USUBJID"
      if (!spid1_col %in% names(built1) || !subj_col %in% names(built1)) next
      vals <- built1[, c(subj_col, spid1_col), drop = FALSE]
      vals <- dplyr::distinct(vals)
      n <- nrow(vals)
      if (n == 0L) next

      d1_rows <- tibble::tibble(
        STUDYID  = studyid,
        RDOMAIN  = r1,
        USUBJID  = as.character(vals[[subj_col]]),
        IDVAR    = spid1_col,
        IDVARVAL = as.character(vals[[spid1_col]]),
        RELTYPE  = spec$reltype1 %||% "",
        RELID    = paste0(relid_pfx, as.character(vals[[spid1_col]]))
      )
      d2_rows <- tibble::tibble(
        STUDYID  = studyid,
        RDOMAIN  = r2,
        USUBJID  = as.character(vals[[subj_col]]),
        IDVAR    = toupper(spec$idvar2 %||% paste0(r2, "SPID")),
        IDVARVAL = as.character(vals[[spid1_col]]),
        RELTYPE  = spec$reltype2 %||% "",
        RELID    = paste0(relid_pfx, as.character(vals[[spid1_col]]))
      )
      block <- dplyr::bind_rows(d1_rows, d2_rows)
      all_blocks <- c(all_blocks, list(block))
      next
    }

    # --- Sequence-lookup relationship (raw link_col + built domain SEQ) ---
    # e.g. DS <-> AE where eos.etae1 links to AESPID,
    #      and DS DSSEQ is looked up from built DS domain.
    if (rel_type == "seq_lookup") {
      ds_name <- spec$dataset %||% tolower(r1)
      df <- raw_data[[ds_name]]
      if (is.null(df) || nrow(df) == 0L) next
      names(df) <- tolower(names(df))

      link_col <- tolower(spec$link_col %||% "")
      if (!nzchar(link_col) || !link_col %in% names(df)) next

      subj_col <- if ("usubjid" %in% names(df)) "usubjid" else "subjectid"
      if (!subj_col %in% names(df)) next

      # Filter to rows with non-missing link values
      df$link_val <- as.character(df[[link_col]])
      df <- df[!is.na(df$link_val) & df$link_val != "", , drop = FALSE]
      if (nrow(df) == 0L) next

      # Optional filter (e.g. DSCAT = "DISPOSITION")
      filter_col <- tolower(spec$filter_col %||% "")
      filter_val <- spec$filter_val %||% ""

      # Look up sequence from built domain
      built1 <- built_domains[[r1]]
      if (is.null(built1) || nrow(built1) == 0L) next
      names(built1) <- toupper(names(built1))
      seq_col <- toupper(spec$idvar1 %||% paste0(r1, "SEQ"))

      if (!seq_col %in% names(built1) || !"USUBJID" %in% names(built1)) next

      # Apply filter on built domain if specified
      if (nzchar(filter_col) && nzchar(filter_val)) {
        fc <- toupper(filter_col)
        if (fc %in% names(built1)) {
          built1 <- built1[built1[[fc]] == filter_val, , drop = FALSE]
        }
      }
      if (nrow(built1) == 0L) next

      # Derive USUBJID for raw data subjects
      df$usubjid_derived <- if (subj_col == "usubjid") {
        as.character(df[[subj_col]])
      } else {
        paste0(studyid, "-", as.character(df[[subj_col]]))
      }

      # Match raw subjects to built domain
      built_slim <- built1[, c("USUBJID", seq_col), drop = FALSE]
      built_slim <- dplyr::distinct(built_slim)
      names(built_slim) <- c("usubjid_derived", "seq_val")
      built_slim$seq_val <- as.character(built_slim$seq_val)

      merged <- merge(
        df[, c("usubjid_derived", "link_val"), drop = FALSE],
        built_slim,
        by = "usubjid_derived", all.x = FALSE
      )
      if (nrow(merged) == 0L) next

      d1_rows <- tibble::tibble(
        STUDYID  = studyid,
        RDOMAIN  = r1,
        USUBJID  = merged$usubjid_derived,
        IDVAR    = seq_col,
        IDVARVAL = merged$seq_val,
        RELTYPE  = spec$reltype1 %||% "",
        RELID    = paste0(relid_pfx, merged$seq_val)
      )
      d2_rows <- tibble::tibble(
        STUDYID  = studyid,
        RDOMAIN  = r2,
        USUBJID  = merged$usubjid_derived,
        IDVAR    = toupper(spec$idvar2 %||% paste0(r2, "SPID")),
        IDVARVAL = merged$link_val,
        RELTYPE  = spec$reltype2 %||% "",
        RELID    = paste0(relid_pfx, merged$seq_val)
      )
      block <- dplyr::bind_rows(d1_rows, d2_rows)
      block <- dplyr::distinct(block)
      all_blocks <- c(all_blocks, list(block))
      next
    }

    # --- Record-level relationship (from link columns in raw data) ---
    ds_name <- spec$dataset %||% tolower(r1)
    df <- raw_data[[ds_name]]
    if (is.null(df) || nrow(df) == 0L) next

    # Normalize column names to lowercase
    names(df) <- tolower(names(df))

    # Find link columns: match <link_prefix><digits>
    pfx <- tolower(spec$link_prefix %||% paste0(tolower(r1), tolower(r2)))
    link_cols <- grep(
      paste0("^", pfx, "[0-9]*$"),
      names(df), value = TRUE, ignore.case = TRUE
    )
    if (length(link_cols) == 0L) next

    # Determine IDVAR and IDVARVAL column for domain 1
    idvar1_col <- tolower(spec$idvar1_val_col %||% paste0(tolower(r1), "spid"))
    if (!idvar1_col %in% names(df)) {
      # Fallback: try the IDVAR1 name as-is
      idvar1_col_alt <- tolower(spec$idvar1 %||% paste0(r1, "SPID"))
      if (idvar1_col_alt %in% names(df)) {
        idvar1_col <- idvar1_col_alt
      } else {
        next
      }
    }

    # Ensure USUBJID or subjectid exists
    subj_col <- if ("usubjid" %in% names(df)) "usubjid" else "subjectid"
    if (!subj_col %in% names(df)) next

    # Derive full USUBJID if raw data uses subjectid
    if (subj_col == "subjectid") {
      df$usubjid_full <- paste0(studyid, "-", as.character(df[[subj_col]]))
    } else {
      df$usubjid_full <- as.character(df[[subj_col]])
    }

    # Pivot link columns to long format
    keep_cols <- intersect(c("usubjid_full", idvar1_col, link_cols), names(df))
    long <- tryCatch({
      df[, keep_cols, drop = FALSE] |>
        tidyr::pivot_longer(
          cols = dplyr::all_of(link_cols),
          names_to  = "link_col",
          values_to = "link_val"
        )
    }, error = function(e) return(NULL))
    if (is.null(long) || nrow(long) == 0L) next

    # Convert to character and filter non-missing
    long$link_val <- as.character(long$link_val)
    long <- long[!is.na(long$link_val) & long$link_val != "", , drop = FALSE]
    if (nrow(long) == 0L) next

    # Extract the numeric part of the link column name for RELID suffix
    long$link_idx <- gsub(paste0("^", pfx), "", long$link_col, ignore.case = TRUE)
    long$link_idx[long$link_idx == ""] <- "1"

    # Build RELID: combine prefix with the domain-1 SPID for uniqueness
    long$relid <- paste0(relid_pfx, as.character(long[[idvar1_col]]))

    # Extract the first token of the link value (before any '-')
    first_token <- function(x) sub("^([^-]+).*", "\\1", x)

    # Domain-1 rows (main side)
    d1_rows <- tibble::tibble(
      STUDYID  = studyid,
      RDOMAIN  = r1,
      USUBJID  = as.character(long[["usubjid_full"]]),
      IDVAR    = toupper(spec$idvar1 %||% paste0(r1, "SPID")),
      IDVARVAL = as.character(long[[idvar1_col]]),
      RELTYPE  = spec$reltype1 %||% "",
      RELID    = long$relid
    )

    # Domain-2 rows (related side)
    d2_rows <- tibble::tibble(
      STUDYID  = studyid,
      RDOMAIN  = r2,
      USUBJID  = as.character(long[["usubjid_full"]]),
      IDVAR    = toupper(spec$idvar2 %||% paste0(r2, "SPID")),
      IDVARVAL = first_token(long$link_val),
      RELTYPE  = spec$reltype2 %||% "",
      RELID    = long$relid
    )

    block <- dplyr::bind_rows(d1_rows, d2_rows)
    block <- dplyr::distinct(block)
    all_blocks <- c(all_blocks, list(block))
  }

  if (length(all_blocks) == 0L) return(NULL)

  relrec <- dplyr::bind_rows(all_blocks)
  relrec <- dplyr::distinct(relrec)
  relrec <- dplyr::arrange(relrec, .data$STUDYID, .data$RDOMAIN,
                            .data$USUBJID, .data$IDVAR,
                            .data$IDVARVAL, .data$RELID)

  # Ensure all columns are character (SDTM requirement)
  relrec[] <- lapply(relrec, as.character)

  relrec
}


#' Expand config-driven domains into raw data
#'
#' Some trial design domains (e.g. TV, TI) are not collected in a raw CRF
#' dataset — they come from the study configuration (protocol visit schedule,
#' inclusion/exclusion criteria text, etc.).
#'
#' Standard trial design domains (TV from `visit_map`, TI from
#' `ie_criteria.criteria`) are handled automatically — no explicit
#' `config_domains` section is needed.  If a `config_domains` entry is
#' present in `config.yaml`, it overrides the built-in default for that
#' domain and can also define additional config-driven domains.
#'
#' The resulting tibbles are injected into `raw_data` so that the standard
#' rule engine ([build_domain()]) can process them like any other domain.
#'
#' @section config.yaml format (optional overrides):
#' ```yaml
#' # Only needed to override defaults or add new config-driven domains
#' config_domains:
#'   TV:
#'     source: visit_map
#'     filter_field: tvstrl
#'     columns:
#'       visitnum: visitnum
#'       visit: visit
#'       visitdy: visitdy
#'       tvstrl: tvstrl
#' ```
#'
#' @param raw_data Named list of tibbles (modified in place).
#' @param config `sdtm_config` with `cfg_yaml` attached.
#' @param domains Character vector of requested domains.
#' @param verbose Logical. Default `TRUE`.
#' @return Modified `raw_data` with new tibbles injected.
#' @export
expand_config_domains <- function(raw_data, config, domains = NULL,
                                  verbose = TRUE) {
  cfg_yaml <- config$cfg_yaml
  if (is.null(cfg_yaml)) return(raw_data)

  # --- Standard defaults for trial design domains ---------------------------
  # These are always derived from standard config sections (visit_map,

  # ie_criteria) unless overridden in config_domains.
  defaults <- list(
    TV = list(
      source       = "visit_map",
      filter_field = "tvstrl",
      columns      = list(visitnum = "visitnum", visit = "visit",
                          visitdy = "visitdy", tvstrl = "tvstrl")
    ),
    TI = list(
      source  = "ie_criteria.criteria",
      columns = list(ietestcd = "ietestcd", ietest = "ietest",
                     iecat = "iecat"),
      extra   = list(tivers = "ie_criteria.tivers")
    )
  )

  # Merge: explicit config_domains entries override the defaults
  cd <- defaults
  if (!is.null(cfg_yaml$config_domains)) {
    for (nm in names(cfg_yaml$config_domains)) {
      cd[[nm]] <- cfg_yaml$config_domains[[nm]]
    }
  }

  for (dom_name in names(cd)) {
    # Only expand if this domain was actually requested
    if (!is.null(domains) && !toupper(dom_name) %in% toupper(domains)) next

    dom_lc <- tolower(dom_name)
    # Skip if raw data already exists for this domain
    if (dom_lc %in% names(raw_data)) next

    spec <- cd[[dom_name]]
    source_path <- spec$source  # e.g. "visit_map" or "ie_criteria.criteria"
    if (is.null(source_path)) next

    # Navigate the config to reach the source list
    path_parts <- strsplit(source_path, "\\.")[[1]]
    src <- cfg_yaml
    for (p in path_parts) {
      if (is.null(src[[p]])) { src <- NULL; break }
      src <- src[[p]]
    }
    if (is.null(src) || length(src) == 0L) {
      if (verbose) cli::cli_alert_warning(
        "config_domains: source '{source_path}' not found for {dom_name}")
      next
    }

    # Apply filter: only keep entries that have the filter_field non-empty
    filter_field <- spec$filter_field
    if (!is.null(filter_field)) {
      src <- Filter(function(entry) {
        val <- entry[[filter_field]]
        !is.null(val) && nzchar(as.character(val))
      }, src)
    }
    if (length(src) == 0L) next

    # Build tibble from column mapping
    col_map <- spec$columns  # named list: raw_col_name -> config_field_name
    if (is.null(col_map) || length(col_map) == 0L) next

    rows <- lapply(src, function(entry) {
      vals <- lapply(col_map, function(cfg_field) {
        v <- entry[[cfg_field]]
        if (is.null(v)) NA else v
      })
      tibble::as_tibble(vals)
    })
    tbl <- dplyr::bind_rows(rows)

    # Add extra columns from higher-level config sections
    if (!is.null(spec$extra) && length(spec$extra) > 0L) {
      for (col_name in names(spec$extra)) {
        extra_path <- strsplit(spec$extra[[col_name]], "\\.")[[1]]
        val <- cfg_yaml
        for (p in extra_path) {
          if (is.null(val[[p]])) { val <- NULL; break }
          val <- val[[p]]
        }
        if (!is.null(val)) tbl[[col_name]] <- as.character(val)
      }
    }

    # Enforce types: numeric for columns ending in 'num' or 'dy'
    for (cn in names(tbl)) {
      if (grepl("(num|dy)$", cn, ignore.case = TRUE)) {
        tbl[[cn]] <- suppressWarnings(as.numeric(tbl[[cn]]))
      } else {
        tbl[[cn]] <- as.character(tbl[[cn]])
      }
    }

    raw_data[[dom_lc]] <- tbl
    if (verbose) {
      cli::cli_alert_info(
        "  Config domain {dom_name}: {nrow(tbl)} rows from {source_path}")
    }
  }

  raw_data
}


#' Build all domains in dependency order
#'
#' Automatically determines domain build order from metadata dependencies
#' (DM is always built first, then domains that need DM data, etc.) and
#' builds each domain sequentially, passing upstream results downstream.
#'
#' When `domain_meta` is supplied (from [read_study_metadata_excel()]), the
#' build order follows the CLASS_ORDER and DOMAIN_LEVEL_ORDER from the
#' Domains sheet, and domains are logged by CLASS grouping.
#'
#' @param target_meta Tibble.
#' @param raw_data Named list of tibbles.
#' @param config `sdtm_config`.
#' @param rule_set `rule_set`.
#' @param source_meta Tibble or `NULL`. Optional; not used at runtime.
#' @param domains Character vector or `NULL`. If `NULL`, builds all domains
#'   in the rule\_set. Otherwise builds only the listed domains.
#' @param domain_meta Tibble or `NULL`. Domain-level metadata from
#'   [read_study_metadata_excel()]. Controls build order and provides
#'   keys/description/structure for finalization.
#' @param sources_meta Tibble or `NULL`. Domain-level preprocessing
#'   metadata from the "Sources" sheet. When provided, replaces manual hooks.
#' @param source_cols_meta Tibble or `NULL`. Column-level preprocessing
#'   metadata from the "Source Columns" sheet.
#' @param value_level_meta Tibble or `NULL`. Value-level metadata for
#'   per-condition validation.
#' @param create_supp Logical or `NULL`. Default `NULL` (inherits from
#'   `config$create_supp`, itself defaulting to `TRUE`). Set `FALSE` to
#'   keep all variables in the main domain instead of splitting to SUPP--.
#' @param validate Logical. Default `TRUE`.
#' @param verbose Logical. Default `TRUE`.
#' @return Named list of `build_result` objects, keyed by domain.
#' @export
build_all_domains <- function(target_meta, raw_data,
                              config, rule_set,
                              source_meta = NULL,
                              domains = NULL,
                              domain_meta = NULL,
                              value_level_meta = NULL,
                              sources_meta = NULL,
                              source_cols_meta = NULL,
                              create_supp = NULL,
                              validate = TRUE,
                              verbose = TRUE) {
  all_doms <- names(rule_set$rules)
  if (!is.null(domains)) {
    all_doms <- intersect(toupper(domains), all_doms)
  }
  if (length(all_doms) == 0L) {
    abort("No domains found to build.")
  }

  # Determine build order:
  # If domain_meta is provided, use its order (already sorted by
  # class_order + domain_level_order from read_study_metadata_excel)
  if (!is.null(domain_meta) && nrow(domain_meta) > 0L) {
    meta_order <- domain_meta$domain
    # Filter to domains that actually have rules
    ordered <- intersect(meta_order, all_doms)
    # Add any remaining domains not in domain_meta
    ordered <- c(ordered, setdiff(all_doms, ordered))

    if (verbose) {
      # Log by CLASS groups
      classes <- unique(domain_meta$class)
      for (cls in classes) {
        cls_doms <- dplyr::filter(domain_meta, .data$class == cls)$domain
        cls_doms <- intersect(cls_doms, ordered)
        if (length(cls_doms) > 0L) {
          cli::cli_alert_info("  {cls}: {paste(cls_doms, collapse = ', ')}")
        }
      }
    }
  } else {
    # Fallback: DM first, then everything else
    ordered <- character()
    if ("DM" %in% all_doms) ordered <- "DM"
    ordered <- c(ordered, setdiff(all_doms, ordered))
  }

  results <- list()
  dm_built <- NULL

  # ---- Expand config-driven domains ----
  # Trial design domains (TV, TI, ...) may not have raw CRF datasets.
  # expand_config_domains() reads config_domains from config.yaml and
  # generates raw_data tibbles so they flow through build_domain().
  raw_data <- expand_config_domains(raw_data, config, domains = ordered,
                                     verbose = verbose)

  # ---- Metadata-driven preprocessing ----
  # Replaces manual hooks: reads Sources + Source Columns metadata and
  # applies standardized preprocessing (filter, derive, stack, merge).
  has_preprocessing <- !is.null(sources_meta) && nrow(sources_meta) > 0L
  if (has_preprocessing && verbose) {
    n_src_doms <- length(unique(toupper(sources_meta$domain)))
    cli::cli_alert_info("  Preprocessing: {n_src_doms} domains from Sources metadata")
  }

  # Fallback to hooks directory if no Sources metadata
  hooks_dir <- config$hooks_dir
  use_hooks <- !has_preprocessing && !is.null(hooks_dir) && dir.exists(hooks_dir)
  if (use_hooks && verbose) {
    hook_files <- list.files(hooks_dir, pattern = "\\.R$", full.names = FALSE)
    if (length(hook_files) > 0L) {
      cli::cli_alert_info("  Hooks directory: {hooks_dir} ({length(hook_files)} hooks)")
    }
  }

  for (dom in ordered) {
    if (verbose) cli::cli_alert_info("Building {dom}...")

    result <- NULL

    # ---- 1. Preprocess domain (metadata-driven or hook fallback) ----
    if (has_preprocessing) {
      dom_has_sources <- any(toupper(sources_meta$domain) == dom)
      if (dom_has_sources) {
        if (verbose) cli::cli_alert_info("  Preprocessing: {dom}")
        tryCatch({
          raw_data <- preprocess_domain(
            domain          = dom,
            raw_data        = raw_data,
            config          = config,
            sources_meta    = sources_meta,
            source_cols_meta = source_cols_meta %||% data.frame(
              block_id = character(), target_column = character(),
              method = character(), col_order = numeric(),
              stringsAsFactors = FALSE
            ),
            built_domains   = results,
            verbose         = verbose
          )
        }, error = function(e) {
          if (verbose) cli::cli_alert_danger("  Preprocessing error ({dom}): {e$message}")
        })
      }
    } else if (use_hooks) {
      hook_path <- file.path(hooks_dir, paste0(tolower(dom), ".R"))
      if (file.exists(hook_path)) {
        hook_env <- new.env(parent = globalenv())
        tryCatch({
          source(hook_path, local = hook_env)
          fn_name <- paste0("preprocess_", tolower(dom))
          if (exists(fn_name, envir = hook_env, inherits = FALSE)) {
            if (verbose) cli::cli_alert_info("  Running hook: {basename(hook_path)}")
            raw_data <- hook_env[[fn_name]](raw_data, config, results)
          }
        }, error = function(e) {
          if (verbose) cli::cli_alert_danger("  Hook error ({dom}): {e$message}")
        })
      }
    }

    # ---- 2. Build the domain via the rule engine ----
    result <- tryCatch(
      build_domain(
        domain      = dom,
        target_meta = target_meta,
        raw_data    = raw_data,
        config      = config,
        rule_set    = rule_set,
        dm_data     = dm_built,
        create_supp = create_supp,
        domain_meta = domain_meta,
        value_level_meta = value_level_meta,
        validate    = validate,
        verbose     = verbose
      ),
      error = function(e) {
        if (verbose) cli::cli_alert_danger("ERROR building {dom}: {e$message}")
        return(NULL)
      }
    )

    if (!is.null(result)) {
      results[[dom]] <- result
      if (dom == "DM") dm_built <- result$data
    }
  }

  results
}
