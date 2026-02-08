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
#' @param source_meta Tibble.
#' @param raw_data Named list of tibbles.
#' @param config `sdtm_config`.
#' @param rule_set `rule_set`.
#' @param dm_data Tibble or `NULL`.
#' @param sv_data Tibble or `NULL`.
#' @param return_provenance Logical. Default `TRUE`.
#' @param validate Logical. Default `TRUE`.
#' @param verbose Logical. Default `TRUE`.
#' @return Named list (build_result).
#' @export
build_domain <- function(domain, target_meta, source_meta, raw_data,
                         config, rule_set, dm_data = NULL, sv_data = NULL,
                         return_provenance = TRUE, validate = TRUE,
                         verbose = TRUE) {
  domain <- toupper(domain)
  log_msgs <- character()
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
    abort(glue::glue("Cannot determine primary source dataset for {domain}"))
  }

  if (!primary_ds %in% names(raw_data)) {
    abort(glue::glue("Source dataset '{primary_ds}' not found in raw_data"))
  }

  # Start with primary dataset
  data <- raw_data[[primary_ds]]
  .log(glue::glue("Primary source: {primary_ds} ({nrow(data)} rows)"))

  # Join DM data for RFSTDTC (needed for DY calculations) - skip for DM itself
  if (domain != "DM") {
    if (!is.null(dm_data)) {
      # Add RFSTDTC from DM
      dm_ref <- dm_data
      if (!"RFSTDTC" %in% names(dm_ref) && "rfstdtc" %in% names(dm_ref)) {
        dm_ref$RFSTDTC <- dm_ref$rfstdtc
      }
      # Determine join key — built DM has UPPERCASE names, raw data lowercase
      data_key <- if ("usubjid" %in% names(data)) "usubjid" else
                  if ("USUBJID" %in% names(data)) "USUBJID" else NULL
      dm_key   <- if ("usubjid" %in% names(dm_ref)) "usubjid" else
                  if ("USUBJID" %in% names(dm_ref)) "USUBJID" else NULL
      if (!is.null(data_key) && !is.null(dm_key)) {
        # Normalise dm_ref key to match data
        if (data_key != dm_key) dm_ref[[data_key]] <- dm_ref[[dm_key]]
        dm_cols <- intersect(names(dm_ref), c(data_key, "RFSTDTC", "rfstdtc"))
        dm_slim <- dplyr::distinct(dm_ref[, dm_cols, drop = FALSE])
        data <- safe_join(data, dm_slim, by = data_key,
                          type = "left", cardinality = "m:1",
                          on_violation = "warn")
      }
    } else if ("dm_raw" %in% names(raw_data)) {
      dm_ref <- raw_data[["dm_raw"]]
      if ("usubjid" %in% names(data) && "usubjid" %in% names(dm_ref)) {
        dm_slim <- dplyr::distinct(dm_ref[, c("usubjid", "rfstdtc"), drop = FALSE])
        dm_slim$RFSTDTC <- dm_slim$rfstdtc
        data <- safe_join(data, dm_slim[, c("usubjid", "RFSTDTC")],
                          by = "usubjid", type = "left",
                          cardinality = "m:1", on_violation = "warn")
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

    tryCatch({
      data <- derive_variable(data, var_name, rule,
                               context = list(config = config, ct_lib = NULL,
                                              domain = domain))
      provenance <- dplyr::bind_rows(provenance, tibble::tibble(
        var = var_name, rule_type = rule$type,
        source = rule$params$dataset %||% "derived"
      ))
    }, error = function(e) {
      .log(glue::glue("ERROR deriving {var_name}: {e$message}"))
    })
  }

  .log(glue::glue("Derived {ncol(data)} columns, {nrow(data)} rows"))

  # Identify SUPP variables (non-standard vars to move to SUPP--)
  supp_vars <- character()
  if ("to_supp" %in% names(dom_meta)) {
    supp_flags <- dom_meta$to_supp
    supp_vars <- dom_meta$var[!is.na(supp_flags) & toupper(supp_flags) == "Y"]
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

  # Finalize (selects only non-SUPP target variables)
  final <- finalize_domain(data, domain, dom_meta, config,
                           create_seq = FALSE, create_supp = FALSE)
  data <- final$data

  # Validate
  report <- new_validation_report(domain = domain)
  if (validate) {
    ct_lib <- rule_set$ct_lib
    report <- validate_domain_structure(data, dom_meta, domain, config,
                                        ct_lib = ct_lib)
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
#' @param source_meta Tibble.
#' @param config `sdtm_config`.
#' @return Tibble.
#' @export
build_domain_from_sources <- function(domain, rule_set, raw_data,
                                      source_meta, config) {
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

  switch(type,
    constant = {
      data <- derive_constant(data, var, value = params$value)
    },
    direct_map = {
      src_col <- params$column
      src_ds  <- params$dataset

      # If the source column isn't in data, check if we can find it
      if (!src_col %in% names(data)) {
        # Try lowercase
        if (tolower(src_col) %in% names(data)) {
          src_col <- tolower(src_col)
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

      data <- map_direct(data, var, src_col, transform = xform)
    },
    ct_assign = {
      src_col     <- params$column
      codelist_id <- params$codelist_id %||% rule$codelist_id
      unknown_pol <- params$unknown_policy %||% "warn_and_keep"

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
      epoch_map <- context$config$epoch_map  # from config.yaml
      if (!is.null(epoch_map) && length(epoch_map) > 0L) {
        data <- derive_epoch(data, var, dtc_var, epoch_map, ref_var)
      } else {
        data[[var]] <- NA_character_
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

#' Finalize a domain dataset for export
#' @param data Tibble.
#' @param domain Character.
#' @param target_meta Tibble.
#' @param config `sdtm_config`.
#' @param create_seq Logical. Default `TRUE`.
#' @param create_supp Logical. Default `TRUE`.
#' @param strict_labels Logical. Default `TRUE`.
#' @return Named list.
#' @export
finalize_domain <- function(data, domain, target_meta, config,
                            create_seq = TRUE, create_supp = TRUE,
                            strict_labels = TRUE) {
  dom_meta <- dplyr::filter(target_meta, .data[["domain"]] == .env[["domain"]])
  target_vars <- dom_meta$var

  # Exclude SUPP variables from the main dataset
  if ("to_supp" %in% names(dom_meta)) {
    supp_flags <- dom_meta$to_supp
    non_supp <- is.na(supp_flags) | toupper(supp_flags) != "Y"
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
#' @param relationship_specs List.
#' @param domain_data Named list.
#' @param config `sdtm_config`.
#' @return Tibble or `NULL`.
#' @export
build_relrec <- function(relationship_specs, domain_data, config) {
  if (is.null(relationship_specs) || length(relationship_specs) == 0L) return(NULL)
  rows <- lapply(relationship_specs, function(spec) {
    tibble::tibble(
      STUDYID  = config$studyid,
      RDOMAIN  = spec$rdomain1,
      USUBJID  = "",
      IDVAR    = spec$idvar1 %||% "",
      IDVARVAL = "",
      RELTYPE  = spec$reltype %||% "",
      RELID    = spec$relid %||% ""
    )
  })
  dplyr::bind_rows(rows)
}

#' DM domain plugin
#' @param target_meta Tibble.
#' @param source_meta Tibble.
#' @param raw_data Named list.
#' @param config `sdtm_config`.
#' @param rule_set `rule_set`.
#' @return `build_result` for DM.
#' @export
build_dm_plugin <- function(target_meta, source_meta, raw_data,
                            config, rule_set) {
  build_domain("DM", target_meta, source_meta, raw_data, config, rule_set)
}

#' Trial design domain plugins
#' @param domains Character vector.
#' @param target_meta Tibble.
#' @param config `sdtm_config`.
#' @param rule_set `rule_set`.
#' @param trial_design_data Named list.
#' @return Named list of `build_result` objects.
#' @export
build_ta_tv_te_ts_plugins <- function(domains = c("TA","TV","TE","TS"),
                                       target_meta, config, rule_set,
                                       trial_design_data = list()) {
  # Placeholder for non-MVP domains
  list()
}

#' SV domain plugin
#' @param target_meta Tibble.
#' @param source_meta Tibble.
#' @param raw_data Named list.
#' @param config `sdtm_config`.
#' @param rule_set `rule_set`.
#' @return `build_result` for SV.
#' @export
build_sv_plugin <- function(target_meta, source_meta, raw_data,
                            config, rule_set) {
  # Placeholder
  list()
}
