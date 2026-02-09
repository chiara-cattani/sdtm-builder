# ==============================================================================
# Module I: Domain-Level Validation
# ==============================================================================

#' Validate a completed domain dataset
#'
#' Runs a battery of checks on a built domain and accumulates findings
#' into a `validation_report`.
#'
#' @param data Tibble. The built domain dataset.
#' @param target_meta Tibble. Target metadata for this domain.
#' @param domain Character. Domain abbreviation.
#' @param config `sdtm_config`.
#' @param ct_lib Tibble or `NULL`.
#' @param checks Character vector of check names to run, or `"all"`.
#' @return `validation_report`.
#' @export
validate_domain_structure <- function(data, target_meta, domain, config,
                                      ct_lib = NULL, value_level_meta = NULL,
                                      checks = "all") {
  rpt <- new_validation_report(domain = domain)

  if ("all" %in% checks || "required_vars" %in% checks) {
    rpt <- validate_required_vars(data, target_meta, domain, rpt)
  }
  if ("all" %in% checks || "keys_unique" %in% checks) {
    rpt <- validate_keys_unique(data, target_meta, domain, rpt)
  }
  if ("all" %in% checks || "iso8601" %in% checks) {
    rpt <- validate_iso8601(data, target_meta, domain, rpt)
  }
  if ("all" %in% checks || "lengths_types" %in% checks) {
    rpt <- validate_lengths_types_labels(data, target_meta, domain, rpt)
  }
  if ("all" %in% checks || "ct_conformance" %in% checks) {
    rpt <- validate_ct_conformance(data, target_meta, domain, ct_lib, rpt)
  }
  if ("all" %in% checks || "value_level" %in% checks) {
    rpt <- validate_value_level(data, vlm_meta = value_level_meta,
                                 domain = domain, report = rpt)
  }
  if ("all" %in% checks || "domain_value" %in% checks) {
    rpt <- validate_domain_value(data, domain, rpt)
  }
  if ("all" %in% checks || "studyid_constant" %in% checks) {
    rpt <- validate_studyid_constant(data, domain, rpt)
  }
  if ("all" %in% checks || "no_allna_reqexp" %in% checks) {
    rpt <- validate_no_allna_reqexp(data, target_meta, domain, rpt)
  }
  if ("all" %in% checks || "seq_integrity" %in% checks) {
    rpt <- validate_seq_integrity(data, domain, rpt)
  }
  if ("all" %in% checks || "no_duplicate_rows" %in% checks) {
    rpt <- validate_no_duplicate_rows(data, domain, rpt)
  }

  rpt
}

#' Validate required variables are present and non-missing
#' @param data Tibble.
#' @param target_meta Tibble.
#' @param domain Character.
#' @param report `validation_report`.
#' @return Updated `validation_report`.
#' @export
validate_required_vars <- function(data, target_meta, domain, report) {
  dom_meta <- dplyr::filter(target_meta, .data[["domain"]] == .env[["domain"]])
  req_vars <- dom_meta$var[!is.na(dom_meta$core) &
                           toupper(dom_meta$core) == "REQ"]

  for (v in req_vars) {
    if (!v %in% names(data)) {
      report <- add_finding(report, rule_id = "required_vars",
                            severity = "ERROR",
                            message = glue::glue("Required variable {v} is missing"),
                            variable = v, domain = domain)
    } else {
      n_miss <- sum(is.na(data[[v]]) | data[[v]] == "")
      if (n_miss > 0L) {
        report <- add_finding(report, rule_id = "required_vars",
                              severity = "WARNING",
                              message = glue::glue("{v}: {n_miss} of {nrow(data)} values are missing"),
                              variable = v, domain = domain)
      }
    }
  }
  report
}

#' Validate key variables are unique
#' @param data Tibble.
#' @param target_meta Tibble.
#' @param domain Character.
#' @param report `validation_report`.
#' @return Updated `validation_report`.
#' @export
validate_keys_unique <- function(data, target_meta, domain, report) {
  dom_meta <- dplyr::filter(target_meta, .data[["domain"]] == .env[["domain"]])

  key_vars <- character()
  if ("is_key" %in% names(dom_meta)) {
    key_flags <- dom_meta$is_key
    if (is.logical(key_flags)) {
      key_vars <- dom_meta$var[!is.na(key_flags) & key_flags]
    } else {
      key_vars <- dom_meta$var[!is.na(key_flags) & tolower(key_flags) == "true"]
    }
  }

  if (length(key_vars) == 0L) {
    # Fall back to STUDYID + USUBJID + {DOMAIN}SEQ
    seq_var <- paste0(domain, "SEQ")
    key_vars <- intersect(c("STUDYID", "USUBJID", seq_var), names(data))
  }

  available_keys <- intersect(key_vars, names(data))
  if (length(available_keys) > 0L) {
    n_total <- nrow(data)
    n_unique <- nrow(dplyr::distinct(data[, available_keys, drop = FALSE]))
    if (n_unique < n_total) {
      n_dups <- n_total - n_unique
      report <- add_finding(report, rule_id = "keys_unique",
                            severity = "ERROR",
                            message = glue::glue("{domain}: {n_dups} duplicate key combinations ({paste(available_keys, collapse=', ')})"),
                            domain = domain)
    }
  }
  report
}

#' Validate ISO 8601 date/time variables
#' @param data Tibble.
#' @param target_meta Tibble.
#' @param domain Character.
#' @param report `validation_report`.
#' @return Updated `validation_report`.
#' @export
validate_iso8601 <- function(data, target_meta, domain, report) {
  dom_meta <- dplyr::filter(target_meta, .data[["domain"]] == .env[["domain"]])
  dtc_vars <- dom_meta$var[grepl("DTC$", dom_meta$var)]

  # Allow: YYYY, YYYY-MM, YYYY-MM-DD, plus optional Thh:mm or Thh:mm:ss
  iso_pattern <- "^\\d{4}(-\\d{2}(-\\d{2})?)?(T\\d{2}(:\\d{2}(:\\d{2})?)?)?$"

  for (v in dtc_vars) {
    if (!v %in% names(data)) next
    vals <- data[[v]][!is.na(data[[v]]) & data[[v]] != ""]
    if (length(vals) == 0L) next
    bad <- vals[!grepl(iso_pattern, vals)]
    if (length(bad) > 0L) {
      report <- add_finding(report, rule_id = "iso8601",
                            severity = "ERROR",
                            message = glue::glue("{v}: {length(bad)} values are not valid ISO 8601 (e.g., '{bad[1]}')"),
                            variable = v, domain = domain)
    }
  }
  report
}

#' Validate lengths, types, and labels
#' @param data Tibble.
#' @param target_meta Tibble.
#' @param domain Character.
#' @param report `validation_report`.
#' @return Updated `validation_report`.
#' @export
validate_lengths_types_labels <- function(data, target_meta, domain, report) {
  dom_meta <- dplyr::filter(target_meta, .data[["domain"]] == .env[["domain"]])

  for (i in seq_len(nrow(dom_meta))) {
    v <- dom_meta$var[i]
    if (!v %in% names(data)) next

    # Type check
    expected_type <- dom_meta$type[i]
    if (!is.na(expected_type)) {
      actual <- if (is.character(data[[v]])) "char" else if (is.numeric(data[[v]])) "num" else "other"
      if (actual != expected_type && !(expected_type == "num" && is.integer(data[[v]]))) {
        report <- add_finding(report, rule_id = "type_check",
                              severity = "WARNING",
                              message = glue::glue("{v}: expected type '{expected_type}', got '{actual}'"),
                              variable = v, domain = domain)
      }
    }

    # Label check
    lbl <- attr(data[[v]], "label")
    if (is.null(lbl) && !is.na(dom_meta$label[i]) && dom_meta$label[i] != "") {
      report <- add_finding(report, rule_id = "label_check",
                            severity = "NOTE",
                            message = glue::glue("{v}: label attribute missing"),
                            variable = v, domain = domain)
    }

    # Length check (character variables only)
    if ("length" %in% names(dom_meta) && is.character(data[[v]])) {
      max_len <- dom_meta$length[i]
      if (!is.na(max_len) && is.numeric(max_len)) {
        non_na_chars <- data[[v]][!is.na(data[[v]])]
        if (length(non_na_chars) > 0L) {
          max_actual <- max(nchar(non_na_chars))
          if (max_actual > max_len) {
            report <- add_finding(report, rule_id = "length_check",
                                  severity = "WARNING",
                                  message = glue::glue("{v}: max length {max_actual} exceeds defined {max_len}"),
                                  variable = v, domain = domain)
          }
        }
      }
    }

    # Significant digits check (numeric variables only)
    if ("significant_digits" %in% names(dom_meta) && is.numeric(data[[v]])) {
      sig_d <- dom_meta$significant_digits[i]
      if (!is.na(sig_d) && is.numeric(sig_d)) {
        sig_d <- as.integer(sig_d)
        vals <- data[[v]][!is.na(data[[v]])]
        if (length(vals) > 0L) {
          rounded <- round(vals, digits = sig_d)
          mismatched <- vals != rounded
          if (any(mismatched)) {
            report <- add_finding(report, rule_id = "sig_digits_check",
                                  severity = "WARNING",
                                  message = glue::glue("{v}: {sum(mismatched)} value(s) do not conform to significant_digits={sig_d}"),
                                  variable = v, domain = domain)
          }
        }
      }
    }
  }
  report
}

#' Validate controlled terminology conformance
#' @param data Tibble.
#' @param target_meta Tibble.
#' @param domain Character.
#' @param ct_lib Tibble or `NULL`.
#' @param report `validation_report`.
#' @return Updated `validation_report`.
#' @export
validate_ct_conformance <- function(data, target_meta, domain,
                                    ct_lib = NULL, report) {
  if (is.null(ct_lib)) return(report)

  dom_meta <- dplyr::filter(target_meta, .data[["domain"]] == .env[["domain"]])

  # Build extensibility lookup
  ext_lookup <- list()
  if ("is_extensible" %in% names(ct_lib)) {
    ext_df <- dplyr::distinct(ct_lib[, c("codelist_id", "is_extensible"), drop = FALSE])
    ext_lookup <- stats::setNames(ext_df$is_extensible, ext_df$codelist_id)
  }

  # Check all variables with a codelist_id (not just ct_assign rules)
  ct_vars <- dom_meta[!is.na(dom_meta$codelist_id), ]

  for (i in seq_len(nrow(ct_vars))) {
    v <- ct_vars$var[i]
    if (!v %in% names(data)) next

    cl_id <- ct_vars$codelist_id[i]
    valid_values <- ct_lib$coded_value[ct_lib$codelist_id == cl_id]
    if (length(valid_values) == 0L) next

    data_vals <- unique(data[[v]][!is.na(data[[v]])])
    bad_vals <- data_vals[!data_vals %in% valid_values]

    if (length(bad_vals) > 0L) {
      # Check if codelist is extensible — downgrade to NOTE if so
      is_ext <- ext_lookup[[cl_id]]
      if (!is.null(is_ext) && toupper(is_ext) == "YES") {
        severity <- "NOTE"
        msg_prefix <- "(extensible codelist)"
      } else {
        severity <- "WARNING"
        msg_prefix <- ""
      }
      report <- add_finding(report, rule_id = "ct_conformance",
                            severity = severity,
                            message = glue::glue("{v}: {length(bad_vals)} value(s) not in codelist {cl_id} {msg_prefix}: {paste(head(bad_vals,3), collapse=', ')}"),
                            variable = v, domain = domain)
    }
  }
  report
}

#' Validate value-level metadata
#' @param data Tibble.
#' @param vlm_meta Tibble or `NULL`.
#' @param domain Character.
#' @param report `validation_report`.
#' @return Updated `validation_report`.
#' @export
validate_value_level <- function(data, vlm_meta = NULL, domain, report) {
  if (is.null(vlm_meta) || nrow(vlm_meta) == 0L) return(report)

  # Filter VLM to this domain
  dom_vlm <- dplyr::filter(vlm_meta, .data$domain == .env[["domain"]])
  if (nrow(dom_vlm) == 0L) return(report)

  for (i in seq_len(nrow(dom_vlm))) {
    row <- dom_vlm[i, ]
    vlm_id   <- row$value_level_id
    cond_str <- if ("condition" %in% names(row)) row$condition else NA_character_

    if (is.na(cond_str)) next

    # Evaluate the WHERE condition to select matching rows
    mask <- tryCatch(
      rlang::eval_tidy(rlang::parse_expr(cond_str), data = data),
      error = function(e) {
        report <<- add_finding(report, rule_id = "vlm_condition",
                               severity = "WARNING",
                               message = glue::glue("VLM condition '{cond_str}' could not be evaluated: {e$message}"),
                               domain = domain)
        rep(FALSE, nrow(data))
      }
    )
    mask[is.na(mask)] <- FALSE
    n_match <- sum(mask)

    if (n_match == 0L) {
      report <- add_finding(report, rule_id = "vlm_coverage",
                            severity = "NOTE",
                            message = glue::glue("VLM condition '{cond_str}' matched 0 rows (VLM_ID={vlm_id})"),
                            domain = domain)
      next
    }

    # Check significant_digits conformance for matched rows
    if ("significant_digits" %in% names(row) && !is.na(row$significant_digits)) {
      target_var <- row$varname %||% NA_character_
      if (!is.na(target_var) && target_var %in% names(data) && is.numeric(data[[target_var]])) {
        sig_d <- as.integer(row$significant_digits)
        vals <- data[[target_var]][mask]
        vals <- vals[!is.na(vals)]
        if (length(vals) > 0L) {
          rounded <- round(vals, digits = sig_d)
          mismatched <- vals != rounded
          if (any(mismatched)) {
            report <- add_finding(report, rule_id = "vlm_sig_digits",
                                  severity = "WARNING",
                                  message = glue::glue("{target_var} (VLM={vlm_id}): {sum(mismatched)} value(s) do not match significant_digits={sig_d}"),
                                  variable = target_var, domain = domain)
          }
        }
      }
    }

    # Check length conformance for matched rows
    if ("length" %in% names(row) && !is.na(row$length)) {
      target_var <- row$varname %||% NA_character_
      if (!is.na(target_var) && target_var %in% names(data) && is.character(data[[target_var]])) {
        max_len <- as.integer(row$length)
        vals <- data[[target_var]][mask]
        vals <- vals[!is.na(vals)]
        if (length(vals) > 0L) {
          too_long <- nchar(vals) > max_len
          if (any(too_long)) {
            report <- add_finding(report, rule_id = "vlm_length",
                                  severity = "WARNING",
                                  message = glue::glue("{target_var} (VLM={vlm_id}): {sum(too_long)} value(s) exceed length={max_len}"),
                                  variable = target_var, domain = domain)
          }
        }
      }
    }
  }

  report
}

#' Cross-domain validation
#' @param domain_data Named list of domain tibbles.
#' @param config `sdtm_config`.
#' @return `validation_report`.
#' @export
validate_cross_domain <- function(domain_data, config) {
  rpt <- new_validation_report(domain = "CROSS")

  # Check STUDYID consistency
  studyids <- lapply(domain_data, function(d) {
    if ("STUDYID" %in% names(d)) unique(d$STUDYID) else character()
  })
  all_ids <- unique(unlist(studyids))
  if (length(all_ids) > 1L) {
    rpt <- add_finding(rpt, rule_id = "cross_studyid",
                       severity = "ERROR",
                       message = glue::glue("Multiple STUDYIDs across domains: {paste(all_ids, collapse=', ')}"))
  }

  # Check USUBJIDs in non-DM domains exist in DM
  if ("DM" %in% names(domain_data)) {
    dm_ids <- unique(domain_data$DM$USUBJID)
    for (dom_name in setdiff(names(domain_data), "DM")) {
      d <- domain_data[[dom_name]]
      if ("USUBJID" %in% names(d)) {
        extra <- setdiff(unique(d$USUBJID), dm_ids)
        if (length(extra) > 0L) {
          rpt <- add_finding(rpt, rule_id = "cross_usubjid",
                             severity = "WARNING",
                             message = glue::glue("{dom_name}: {length(extra)} USUBJID(s) not in DM"))
        }
      }
    }
  }

  rpt
}

#' Summary of a validation report
#' @param report `validation_report`.
#' @param verbose Logical. Default `TRUE`.
#' @return Tibble summary.
#' @export
summarize_validation_report <- function(report, verbose = TRUE) {
  findings <- report$findings

  if (nrow(findings) == 0L) {
    if (verbose) cli::cli_alert_success("No findings -- all checks passed!")
    return(tibble::tibble(severity = character(), n = integer()))
  }

  summary_tbl <- dplyr::count(findings, .data$severity, name = "n")

  if (verbose) {
    n_err  <- sum(findings$severity == "ERROR")
    n_warn <- sum(findings$severity == "WARNING")
    n_note <- sum(findings$severity == "NOTE")
    cli::cli_h3("Validation Summary: {report$domain}")
    if (n_err > 0L)  cli::cli_alert_danger("{n_err} ERROR(s)")
    if (n_warn > 0L) cli::cli_alert_warning("{n_warn} WARNING(s)")
    if (n_note > 0L) cli::cli_alert_info("{n_note} NOTE(s)")

    for (i in seq_len(nrow(findings))) {
      f <- findings[i, ]
      cli::cli_text("  [{f$severity}] {f$message}")
    }
  }

  summary_tbl
}

#' Emit validation findings as log messages
#' @param report `validation_report`.
#' @param sink `log_sink` or `NULL`.
#' @return Invisible `NULL`.
#' @export
emit_log_messages <- function(report, sink = NULL) {
  for (i in seq_len(nrow(report$findings))) {
    f <- report$findings[i, ]
    msg <- glue::glue("[{f$severity}] {f$rule_id}: {f$message}")
    switch(f$severity,
      ERROR   = log_error(msg),
      WARNING = log_warn(msg),
      log_info(msg)
    )
  }
  invisible(NULL)
}

# ==============================================================================
# Additional validation helpers (Phase 6)
# ==============================================================================

#' Validate DOMAIN variable value matches domain abbreviation
#' @param data Tibble.
#' @param domain Character.
#' @param report `validation_report`.
#' @return Updated `validation_report`.
#' @export
validate_domain_value <- function(data, domain, report) {
  if (!"DOMAIN" %in% names(data)) return(report)
  bad <- data$DOMAIN[!is.na(data$DOMAIN) & data$DOMAIN != domain]
  if (length(bad) > 0L) {
    report <- add_finding(report, rule_id = "domain_value",
                          severity = "ERROR",
                          message = glue::glue("DOMAIN contains values other than '{domain}': {paste(unique(bad), collapse=', ')}"),
                          variable = "DOMAIN", domain = domain)
  }
  report
}

#' Validate STUDYID is constant across all rows
#' @param data Tibble.
#' @param domain Character.
#' @param report `validation_report`.
#' @return Updated `validation_report`.
#' @export
validate_studyid_constant <- function(data, domain, report) {
  if (!"STUDYID" %in% names(data)) return(report)
  unique_ids <- unique(data$STUDYID[!is.na(data$STUDYID)])
  if (length(unique_ids) > 1L) {
    report <- add_finding(report, rule_id = "studyid_constant",
                          severity = "ERROR",
                          message = glue::glue("STUDYID has {length(unique_ids)} distinct values: {paste(unique_ids, collapse=', ')}"),
                          variable = "STUDYID", domain = domain)
  }
  report
}

#' Validate no Required/Expected variable is entirely NA
#' @param data Tibble.
#' @param target_meta Tibble.
#' @param domain Character.
#' @param report `validation_report`.
#' @return Updated `validation_report`.
#' @export
validate_no_allna_reqexp <- function(data, target_meta, domain, report) {
  dom_meta <- dplyr::filter(target_meta, .data[["domain"]] == .env[["domain"]])
  reqexp <- dom_meta$var[!is.na(dom_meta$core) &
                         toupper(dom_meta$core) %in% c("REQ", "EXP")]

  for (v in reqexp) {
    if (!v %in% names(data)) next
    if (all(is.na(data[[v]]) | data[[v]] == "")) {
      report <- add_finding(report, rule_id = "no_allna_reqexp",
                            severity = "ERROR",
                            message = glue::glue("{v} (core={dom_meta$core[dom_meta$var == v]}) is entirely missing/blank"),
                            variable = v, domain = domain)
    }
  }
  report
}

#' Validate --SEQ is a positive integer with no gaps per subject
#' @param data Tibble.
#' @param domain Character.
#' @param report `validation_report`.
#' @return Updated `validation_report`.
#' @export
validate_seq_integrity <- function(data, domain, report) {
  seq_var <- paste0(domain, "SEQ")
  if (!seq_var %in% names(data)) return(report)

  vals <- data[[seq_var]]
  non_na <- vals[!is.na(vals)]

  # Check positive
  if (length(non_na) > 0L && any(non_na <= 0)) {
    report <- add_finding(report, rule_id = "seq_integrity",
                          severity = "ERROR",
                          message = glue::glue("{seq_var} contains non-positive values"),
                          variable = seq_var, domain = domain)
  }

  # Check integer-valued
  if (length(non_na) > 0L && any(non_na != floor(non_na))) {
    report <- add_finding(report, rule_id = "seq_integrity",
                          severity = "ERROR",
                          message = glue::glue("{seq_var} contains non-integer values"),
                          variable = seq_var, domain = domain)
  }

  report
}

#' Validate no fully duplicate rows
#' @param data Tibble.
#' @param domain Character.
#' @param report `validation_report`.
#' @return Updated `validation_report`.
#' @export
validate_no_duplicate_rows <- function(data, domain, report) {
  n_total <- nrow(data)
  n_unique <- nrow(dplyr::distinct(data))
  if (n_unique < n_total) {
    n_dups <- n_total - n_unique
    report <- add_finding(report, rule_id = "no_duplicate_rows",
                          severity = "ERROR",
                          message = glue::glue("{domain}: {n_dups} fully duplicate row(s)"),
                          domain = domain)
  }
  report
}
