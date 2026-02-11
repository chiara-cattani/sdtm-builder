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
    # Fall back to convention: {domain_lower}_raw
    convention_ds <- paste0(tolower(domain), "_raw")
    if (convention_ds %in% names(raw_data)) {
      primary_ds <- convention_ds
    } else {
      abort(glue::glue("Cannot determine primary source dataset for {domain}"))
    }
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
      # Auto-extract $data if a build_domain result list was passed
      if (is.list(dm_data) && "data" %in% names(dm_data) && !is.data.frame(dm_data)) {
        dm_data <- dm_data[["data"]]
      }
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
                                        value_level_meta = value_level_meta)
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
    direct_map = {
      src_col <- params$column
      src_ds  <- params$dataset

      # If the source column isn't in data, check if we can find it
      if (!src_col %in% names(data)) {
        # Try lowercase
        if (tolower(src_col) %in% names(data)) {
          src_col <- tolower(src_col)
        } else if (toupper(src_col) %in% names(data)) {
          src_col <- toupper(src_col)
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
      src_col <- params$column
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
      src_col <- params$column
      if (!src_col %in% names(data) && tolower(src_col) %in% names(data)) {
        src_col <- tolower(src_col)
      }
      result[mask] <- as.character(data[[src_col]])[mask]

    } else {
      # Generic: use whatever the base rule type produces
      # For now, attempt direct mapping from the base rule source
      if (!is.null(params$column)) {
        src_col <- params$column
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
  if (any(unmatched) && !is.null(params$column)) {
    src_col <- params$column
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
#' @param raw_data Named list.
#' @param config `sdtm_config`.
#' @param rule_set `rule_set`.
#' @return `build_result` for DM.
#' @export
build_dm_plugin <- function(target_meta, raw_data, config, rule_set) {
  build_domain("DM", target_meta, raw_data, config, rule_set)
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
  rlang::inform("Trial design domains (TA/TV/TE/TS) are not yet implemented.")
  list()
}

#' SV domain plugin
#' @param target_meta Tibble.
#' @param raw_data Named list.
#' @param config `sdtm_config`.
#' @param rule_set `rule_set`.
#' @return `build_result` for SV.
#' @export
build_sv_plugin <- function(target_meta, raw_data, config, rule_set) {
  rlang::inform("SV domain plugin is not yet implemented.")
  list()
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

  for (dom in ordered) {
    if (verbose) cli::cli_alert_info("Building {dom}...")
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
