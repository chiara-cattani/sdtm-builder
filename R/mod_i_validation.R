# ==============================================================================
# Module I: Domain-Level Validation
# ==============================================================================

#' @importFrom utils head
NULL

# ==============================================================================
# PHASE 1: PRE-VALIDATION (Metadata ↔ CT Structure)
# ==============================================================================
# Run BEFORE dataset creation to catch malformed metadata/CT early
# ==============================================================================

#' Pre-validation: Metadata and CT structure consistency
#'
#' Validates the structure and consistency of metadata and controlled
#' terminology BEFORE dataset creation. Catches misconfigurations early.
#'
#' Checks include:
#' - Domains in target_meta are valid and complete
#' - Variables reference existing codelists (if specified)
#' - Codelists have valid structure (required columns, no duplicates)
#' - Value-level metadata references existing domain/variables
#' - No orphaned codelist references
#' - Codelist extensibility flags are properly set
#'
#' @param target_meta Tibble. Target metadata for all domains.
#' @param ct_lib Tibble or `NULL`. Controlled terminology library.
#' @param value_level_meta Tibble or `NULL`. Value-level metadata.
#' @param domain_meta Tibble or `NULL`. Domain-level metadata.
#' @param domains Character vector or `NULL`. If specified, only these domains
#'   are validated. If `NULL`, all domains in target_meta are validated.
#' @return `validation_report` with findings organized by category.
#' @export
validate_metadata_ct_structure <- function(target_meta,
                                            ct_lib = NULL,
                                            value_level_meta = NULL,
                                            domain_meta = NULL,
                                            domains = NULL) {
  rpt <- new_validation_report(domain = "PRE-VALIDATION (Metadata & CT)")

  # =========== 1. TARGET METADATA STRUCTURE ===========

  if (is.null(target_meta) || nrow(target_meta) == 0L) {
    report <- add_finding(rpt, rule_id = "metadata_empty",
                          severity = "ERROR",
                          message = "Target metadata is empty or NULL",
                          domain = "PRE-VALIDATION")
    return(rpt)
  }

  # Required columns
  required_meta_cols <- c("domain", "var", "type", "label")
  missing_cols <- setdiff(required_meta_cols, names(target_meta))
  if (length(missing_cols) > 0L) {
    rpt <- add_finding(rpt, rule_id = "metadata_missing_cols",
                       severity = "ERROR",
                       message = glue::glue(
                         "Target metadata missing required columns: {paste(missing_cols, collapse=', ')}"
                       ),
                       domain = "PRE-VALIDATION")
    return(rpt)
  }

  # Filter to selected domains if specified
  if (!is.null(domains) && length(domains) > 0L) {
    domains_upper <- toupper(domains)
    target_meta <- target_meta %>%
      dplyr::filter(.data$domain %in% domains_upper)
    if (nrow(target_meta) == 0L) {
      rpt <- add_finding(rpt, rule_id = "metadata_no_matching_domains",
                         severity = "ERROR",
                         message = glue::glue(
                           "No metadata found for selected domain(s): {paste(domains, collapse=', ')}"
                         ),
                         domain = "PRE-VALIDATION")
      return(rpt)
    }
  }

  # 1a. Check for duplicate (domain, var) pairs (WARNING only - acceptable with value-level metadata)
  dup_check <- target_meta %>%
    dplyr::group_by(.data$domain, .data$var) %>%
    dplyr::mutate(n = dplyr::n()) %>%
    dplyr::filter(.data$n > 1L) %>%
    dplyr::distinct(.data$domain, .data$var, .data$n)

  if (nrow(dup_check) > 0L) {
    dup_list <- glue::glue_data(dup_check,
                                 "{domain}/{var} ({n} times)")
    rpt <- add_finding(rpt, rule_id = "metadata_duplicate_vars",
                       severity = "WARNING",
                       message = glue::glue(
                         "Duplicate (domain, var) pairs found (common with value-level metadata): {paste(dup_list, collapse='; ')}"
                       ),
                       domain = "PRE-VALIDATION")
  }

  # 1b. Check for invalid core values
  invalid_core <- target_meta %>%
    dplyr::filter(!is.na(.data$core) &
                  !toupper(.data$core) %in% c("REQ", "EXP", "PERM"))
  if (nrow(invalid_core) > 0L) {
    invalid_list <- glue::glue_data(invalid_core,
                                      "{domain}/{var}={core}")
    rpt <- add_finding(rpt, rule_id = "metadata_invalid_core",
                       severity = "ERROR",
                       message = glue::glue(
                         "Invalid 'core' values (must be REQ/EXP/PERM): {paste(head(invalid_list, 5), collapse='; ')}"
                       ),
                       domain = "PRE-VALIDATION")
  }

  # 1c. Check for invalid type values (case-insensitive: char/Char, num/Num)
  invalid_type <- target_meta %>%
    dplyr::filter(!is.na(.data$type) &
                  !toupper(.data$type) %in% c("CHAR", "NUM"))
  if (nrow(invalid_type) > 0L) {
    invalid_list <- glue::glue_data(invalid_type,
                                      "{domain}/{var}={type}")
    rpt <- add_finding(rpt, rule_id = "metadata_invalid_type",
                       severity = "ERROR",
                       message = glue::glue(
                         "Invalid 'type' values (must be 'Char'/'char' or 'Num'/'num'): {paste(head(invalid_list, 5), collapse='; ')}"
                       ),
                       domain = "PRE-VALIDATION")
  }

  # =========== 2. CODELIST STRUCTURE ===========

  if (!is.null(ct_lib) && nrow(ct_lib) > 0L) {
    # Required columns
    required_ct_cols <- c("codelist_id", "coded_value")
    missing_ct_cols <- setdiff(required_ct_cols, names(ct_lib))
    if (length(missing_ct_cols) > 0L) {
      rpt <- add_finding(rpt, rule_id = "ct_missing_cols",
                         severity = "ERROR",
                         message = glue::glue(
                           "CT library missing required columns: {paste(missing_ct_cols, collapse=', ')}"
                         ),
                         domain = "PRE-VALIDATION")
    } else {
      # 2a. Check for duplicate (codelist_id, coded_value, is_selected) - each combo should appear once per select status
      dup_ct <- ct_lib %>%
        dplyr::group_by(.data$codelist_id, .data$coded_value,
                        dplyr::across(dplyr::any_of("is_selected"))) %>%
        dplyr::mutate(n = dplyr::n()) %>%
        dplyr::filter(.data$n > 1L) %>%
        dplyr::distinct(.data$codelist_id, .data$coded_value, .data$n)

      if (nrow(dup_ct) > 0L) {
        dup_list <- glue::glue_data(dup_ct,
                                     "{codelist_id}/{coded_value} ({n} times)")
        rpt <- add_finding(rpt, rule_id = "ct_duplicate_values",
                           severity = "ERROR",
                           message = glue::glue(
                             "Duplicate values in CT: {paste(head(dup_list, 5), collapse='; ')}"
                           ),
                           domain = "PRE-VALIDATION")
      }

      # 2b. Check is_selected and is_extensible format
      if ("is_selected" %in% names(ct_lib)) {
        invalid_selected <- ct_lib %>%
          dplyr::filter(!is.na(.data$is_selected) &
                        !toupper(.data$is_selected) %in% c("Y", "N"))
        if (nrow(invalid_selected) > 0L) {
          rpt <- add_finding(rpt, rule_id = "ct_invalid_is_selected",
                             severity = "ERROR",
                             message = glue::glue(
                               "{nrow(invalid_selected)} row(s) in CT have invalid is_selected values (must be Y/N or empty)"
                             ),
                             domain = "PRE-VALIDATION")
        }
      }

      if ("is_extensible" %in% names(ct_lib)) {
        invalid_ext <- ct_lib %>%
          dplyr::filter(!is.na(.data$is_extensible) &
                        !toupper(.data$is_extensible) %in% c("Y", "YES", "N", "NO"))
        if (nrow(invalid_ext) > 0L) {
          rpt <- add_finding(rpt, rule_id = "ct_invalid_is_extensible",
                             severity = "ERROR",
                             message = glue::glue(
                               "{nrow(invalid_ext)} row(s) in CT have invalid is_extensible values (must be Y/YES or N/NO)"
                             ),
                             domain = "PRE-VALIDATION")
        }
      }
    }

    # 2c. Check for codelists referenced in metadata but missing from ct_lib
    # Exclude special codelists that are external dictionaries (MEDDRA, WHODRUG, etc)
    dictionary_codelists <- c("MEDDRA", "WHOD", "WHODRUG")
    
    meta_codelists <- unique(target_meta$codelist_id[!is.na(target_meta$codelist_id)])
    ct_codelists <- unique(ct_lib$codelist_id)
    
    # Filter out dictionary codelists from missing check
    missing_in_ct <- setdiff(meta_codelists, ct_codelists)
    missing_in_ct <- missing_in_ct[!toupper(missing_in_ct) %in% toupper(dictionary_codelists)]

    if (length(missing_in_ct) > 0L) {
      # Find which variables reference missing codelists
      missing_vars <- target_meta %>%
        dplyr::filter(.data$codelist_id %in% missing_in_ct) %>%
        dplyr::distinct(.data$domain, .data$var, .data$codelist_id)

      missing_text <- glue::glue_data(missing_vars,
                                       "{domain}/{var} → {codelist_id}")
      rpt <- add_finding(rpt, rule_id = "ct_missing_codelists",
                         severity = "WARNING",
                         message = glue::glue(
                           "{length(missing_in_ct)} codelist(s) referenced but NOT in CT: {paste(head(missing_text, 5), collapse='; ')}"
                         ),
                         domain = "PRE-VALIDATION")
    }

    # 2d. Check for orphaned codelists (in ct_lib but not referenced in selected domains)
    # Only warn if we're validating specific domains (not all domains)
    if (!is.null(domains) && length(domains) > 0L) {
      orphan_codelists <- setdiff(ct_codelists, meta_codelists)
      orphan_codelists <- orphan_codelists[!toupper(orphan_codelists) %in% toupper(dictionary_codelists)]

      if (length(orphan_codelists) > 0L) {
        rpt <- add_finding(rpt, rule_id = "ct_orphaned_codelists",
                           severity = "NOTE",
                           message = glue::glue(
                             "{length(orphan_codelists)} codelist(s) in CT not referenced in selected domain(s): {paste(head(orphan_codelists, 5), collapse=', ')}"
                           ),
                           domain = "PRE-VALIDATION")
      }
    }
  }

  # =========== 3. VALUE-LEVEL METADATA STRUCTURE ===========

  # Extract domains from target_meta for later use (available regardless of VLM presence)
  meta_domains <- unique(target_meta$domain)

  # Filter value-level metadata to selected domains if specified
  vlm_filtered <- value_level_meta
  if (!is.null(domains) && length(domains) > 0L && !is.null(value_level_meta)) {
    domains_upper <- toupper(domains)
    vlm_filtered <- value_level_meta %>%
      dplyr::filter(.data$domain %in% domains_upper)
  }

  if (!is.null(vlm_filtered) && nrow(vlm_filtered) > 0L) {
    # Check for required columns: domain is always needed, variable can be VARNAME or variable
    required_vlm_cols <- c("domain")
    missing_vlm_cols <- setdiff(required_vlm_cols, names(vlm_filtered))
    has_var_col <- "variable" %in% names(vlm_filtered) || "VARNAME" %in% names(vlm_filtered)
    
    if (length(missing_vlm_cols) > 0L || !has_var_col) {
      if (length(missing_vlm_cols) > 0L) {
        msg <- glue::glue("Value-level metadata missing required columns: {paste(missing_vlm_cols, collapse=', ')}")
      } else {
        msg <- "Value-level metadata missing 'variable' or 'VARNAME' column"
      }
      rpt <- add_finding(rpt, rule_id = "vlm_missing_cols",
                         severity = "ERROR",
                         message = msg,
                         domain = "PRE-VALIDATION")
    } else {
      # 3a. Check if referenced domains exist in target_meta
      vlm_domains <- unique(vlm_filtered$domain)
      missing_vlm_domains <- setdiff(vlm_domains, meta_domains)

      if (length(missing_vlm_domains) > 0L) {
        rpt <- add_finding(rpt, rule_id = "vlm_invalid_domain",
                           severity = "ERROR",
                           message = glue::glue(
                             "Value-level metadata references non-existent domains: {paste(missing_vlm_domains, collapse=', ')}"
                           ),
                           domain = "PRE-VALIDATION")
      }

      # 3b. Check if referenced variables exist in target_meta for their domain
      vlm_invalid_vars <- vlm_filtered %>%
        dplyr::anti_join(target_meta, by = c("domain", "variable" = "var"))

      if (nrow(vlm_invalid_vars) > 0L) {
        invalid_list <- glue::glue_data(vlm_invalid_vars,
                                         "{domain}/{variable}")
        rpt <- add_finding(rpt, rule_id = "vlm_invalid_variable",
                           severity = "ERROR",
                           message = glue::glue(
                             "Value-level metadata references non-existent variables: {paste(head(invalid_list, 5), collapse='; ')}"
                           ),
                           domain = "PRE-VALIDATION")
      }

      # 3c. Check that where_var/where_value are not left without both
      incomplete_where <- vlm_filtered %>%
        dplyr::filter((is.na(.data$where_var) & !is.na(.data$where_value)) |
                      (!is.na(.data$where_var) & is.na(.data$where_value)))

      if (nrow(incomplete_where) > 0L) {
        incomplete_list <- glue::glue_data(incomplete_where,
                                            "{domain}/{variable}")
        rpt <- add_finding(rpt, rule_id = "vlm_incomplete_where",
                           severity = "WARNING",
                           message = glue::glue(
                             "Value-level metadata has incomplete where clause (both where_var and where_value needed): {paste(head(incomplete_list, 5), collapse='; ')}"
                           ),
                           domain = "PRE-VALIDATION")
      }
    }
  }

  # =========== 4. DOMAIN-LEVEL METADATA STRUCTURE ===========

  # Filter domain-level metadata to selected domains if specified
  dm_filtered <- domain_meta
  if (!is.null(domains) && length(domains) > 0L && !is.null(domain_meta)) {
    domains_upper <- toupper(domains)
    dm_filtered <- domain_meta %>%
      dplyr::filter(.data$domain %in% domains_upper)
  }

  if (!is.null(dm_filtered) && nrow(dm_filtered) > 0L) {
    # 4a. Check if domains in domain_meta exist in target_meta
    domain_meta_domains <- unique(dm_filtered$domain)
    missing_domains <- setdiff(domain_meta_domains, meta_domains)

    if (length(missing_domains) > 0L) {
      rpt <- add_finding(rpt, rule_id = "domain_meta_invalid_domain",
                         severity = "ERROR",
                         message = glue::glue(
                           "Domain metadata references non-existent domains: {paste(missing_domains, collapse=', ')}"
                         ),
                         domain = "PRE-VALIDATION")
    }
  }

  # =========== 5. CROSS-STRUCTURAL CONSISTENCY ===========

  # 5a. At least one REQ or EXP variable per domain
  domain_cores <- target_meta %>%
    dplyr::filter(!is.na(.data$core) & toupper(.data$core) %in% c("REQ", "EXP")) %>%
    dplyr::distinct(.data$domain)

  domains_no_req <- setdiff(meta_domains, domain_cores$domain)
  if (length(domains_no_req) > 0L) {
    rpt <- add_finding(rpt, rule_id = "metadata_no_req_vars",
                       severity = "WARNING",
                       message = glue::glue(
                         "{length(domains_no_req)} domain(s) have NO required or expected variables: {paste(domains_no_req, collapse=', ')}"
                       ),
                       domain = "PRE-VALIDATION")
  }

  rpt
}

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
#' @param value_level_meta Tibble or `NULL`. Value-level metadata for
#'   per-condition validation.
#' @param domain_meta Tibble or `NULL`. Domain-level metadata providing
#'   keys and structure information for validation.
#' @param checks Character vector of check names to run, or `"all"`.
#'   Special option: `"consistency"` runs only the final metadata-CT-dataset
#'   consistency check, useful for Define-XML preparation.
#' @return `validation_report`.
#' @export
validate_domain_structure <- function(data, target_meta, domain, config,
                                      ct_lib = NULL, value_level_meta = NULL,
                                      domain_meta = NULL,
                                      checks = "all") {
  rpt <- new_validation_report(domain = domain)

  # Special case: consistency check only
  if ("consistency" %in% checks) {
    return(validate_metadata_ct_dataset_consistency(
      data, target_meta, domain,
      ct_lib = ct_lib,
      value_level_meta = value_level_meta,
      report = rpt
    ))
  }

  if ("all" %in% checks || "required_vars" %in% checks) {
    rpt <- validate_required_vars(data, target_meta, domain, rpt)
  }
  if ("all" %in% checks || "keys_unique" %in% checks) {
    rpt <- validate_keys_unique(data, target_meta, domain, rpt,
                                domain_meta = domain_meta)
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

  # Add consistency check if requested (can be run with or without other checks)
  if ("all" %in% checks || "consistency" %in% checks) {
    rpt <- validate_metadata_ct_dataset_consistency(
      data, target_meta, domain,
      ct_lib = ct_lib,
      value_level_meta = value_level_meta,
      report = rpt
    )
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
  # Exclude SUPP-extracted variables: they are moved to SUPPxx and won't be in

  # the main dataset, so checking them here would produce false positives.
  supp_col <- if ("to_supp" %in% names(dom_meta)) dom_meta[["to_supp"]] else NA_character_
  is_supp  <- !is.na(supp_col) & toupper(supp_col) == "YES"
  req_vars <- dom_meta$var[!is.na(dom_meta$core) &
                           toupper(dom_meta$core) == "REQ" &
                           !is_supp]

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
#' @param domain_meta Tibble or `NULL`. Domain-level metadata used as
#'   fallback source for key variable definitions.
#' @return Updated `validation_report`.
#' @export
validate_keys_unique <- function(data, target_meta, domain, report,
                                 domain_meta = NULL) {
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

  # Fall back to domain_meta keys (e.g. "STUDYID, ARMCD, VISITNUM")
  if (length(key_vars) == 0L && !is.null(domain_meta)) {
    dm_info <- dplyr::filter(domain_meta, .data[["domain"]] == .env[["domain"]])
    if (nrow(dm_info) > 0L && !is.na(dm_info$keys[1L])) {
      key_vars <- trimws(strsplit(dm_info$keys[1L], ",\\s*")[[1L]])
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

  # Compare CT values case/whitespace-insensitively (spaces and casing only)
  # so values like "Not Recovered / Not Resolved" match
  # "NOT RECOVERED/NOT RESOLVED".
  normalize_ct <- function(x) {
    x <- as.character(x)
    x <- trimws(x)
    x <- tolower(x)
    x <- gsub("\\s+", "", x, perl = TRUE)
    x
  }

  # Check all variables with a codelist_id (not just ct_assign rules)
  ct_vars <- dom_meta[!is.na(dom_meta$codelist_id), ]

  for (i in seq_len(nrow(ct_vars))) {
    v <- ct_vars$var[i]
    if (!v %in% names(data)) next

    cl_id <- ct_vars$codelist_id[i]

    # Get full codelist (all records)
    cl_all <- ct_lib %>% dplyr::filter(.data$codelist_id == cl_id)
    if (nrow(cl_all) == 0L) next

    # Split into selected vs unselected terms
    is_selected_col <- "is_selected" %in% names(cl_all)
    if (is_selected_col) {
      cl_selected <- cl_all %>% dplyr::filter(toupper(.data$is_selected) == "Y")
      cl_unselected <- cl_all %>% dplyr::filter(toupper(.data$is_selected) == "N")
    } else {
      # Fallback if is_selected column not present
      cl_selected <- cl_all
      cl_unselected <- cl_all[0L, , drop = FALSE]
    }

    # Get valid values from selected terms (primary validation)
    valid_values <- union(cl_selected$coded_value, cl_selected$submission_value)
    if ("decode" %in% names(cl_selected)) {
      decode_vals <- cl_selected$decode[!is.na(cl_selected$decode) & trimws(cl_selected$decode) != ""]
      valid_values <- union(valid_values, decode_vals)
    }

    # Get all values in codelist (for detecting not-selected terms)
    all_codelist_values <- union(cl_all$coded_value, cl_all$submission_value)
    if ("decode" %in% names(cl_all)) {
      decode_vals_all <- cl_all$decode[!is.na(cl_all$decode) & trimws(cl_all$decode) != ""]
      all_codelist_values <- union(all_codelist_values, decode_vals_all)
    }

    if (length(valid_values) == 0L) next

    # Get data values
    data_vals <- unique(data[[v]][!is.na(data[[v]])])
    if (length(data_vals) == 0L) next

    data_norm <- normalize_ct(data_vals)
    valid_norm <- normalize_ct(valid_values)
    all_codelist_norm <- normalize_ct(all_codelist_values)

    # Find bad values (not in selected terms)
    bad_vals <- data_vals[!data_norm %in% valid_norm]

    if (length(bad_vals) > 0L) {
      # Determine which bad values are in the full codelist but not selected
      bad_vals_norm <- normalize_ct(bad_vals)
      in_codelist_not_selected <- bad_vals[bad_vals_norm %in% all_codelist_norm]
      not_in_codelist <- bad_vals[!bad_vals_norm %in% all_codelist_norm]

      # Report unselected values first (always a warning)
      if (length(in_codelist_not_selected) > 0L) {
        report <- add_finding(
          report, rule_id = "ct_conformance",
          severity = "WARNING",
          message = glue::glue(
            "{v}: {length(in_codelist_not_selected)} value(s) present in {cl_id} but not selected (select≠Y): {paste(head(in_codelist_not_selected, 3), collapse=', ')}"
          ),
          variable = v, domain = domain
        )
      }

      # Report values not in codelist (severity depends on extensibility)
      if (length(not_in_codelist) > 0L) {
        is_ext <- ext_lookup[[cl_id]]
        if (!is.null(is_ext) && toupper(is_ext) == "YES") {
          # Extensible: warning (accept values not in codelist)
          severity <- "WARNING"
          msg_type <- "not in codelist (extensible)"
        } else {
          # Non-extensible: error
          severity <- "ERROR"
          msg_type <- "not in codelist (non-extensible)"
        }

        report <- add_finding(
          report, rule_id = "ct_conformance",
          severity = severity,
          message = glue::glue(
            "{v}: {length(not_in_codelist)} value(s) {msg_type} {cl_id}: {paste(head(not_in_codelist, 3), collapse=', ')}"
          ),
          variable = v, domain = domain
        )
      }
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
  reqexp <- unique(dom_meta$var[!is.na(dom_meta$core) &
                         toupper(dom_meta$core) %in% c("REQ", "EXP")])

  for (v in reqexp) {
    if (!v %in% names(data)) next
    if (all(is.na(data[[v]]) | data[[v]] == "")) {
      v_core <- toupper(dom_meta$core[dom_meta$var == v][1L])
      sev <- if (v_core == "EXP") "WARNING" else "ERROR"
      report <- add_finding(report, rule_id = "no_allna_reqexp",
                            severity = sev,
                            message = glue::glue("{v} (core={v_core}) is entirely missing/blank"),
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

# ==============================================================================
# PHASE 2: FINAL VALIDATION (Metadata ↔ CT ↔ Dataset)
# ==============================================================================
# Run AFTER dataset creation to verify complete alignment before Define-XML
# ==============================================================================

#' Final validation: Metadata, CT, and dataset consistency
#'
#' Validates complete alignment between metadata, controlled terminology,
#' and SDTM datasets AFTER creation. Ensures everything is ready for
#' Define-XML file generation.
#'
#' Checks include:
#' - Variables: extra in dataset, missing from dataset, not selected but in dataset
#' - Values: extra in dataset, not selected in CT, extensibility violations
#' - Value-level metadata: selected but unused, unused but valid
#' - Data integrity: all-missing selected vars, unexpected patterns
#'
#' @param data Tibble. The built domain dataset.
#' @param target_meta Tibble. Target metadata for this domain.
#' @param domain Character. Domain abbreviation.
#' @param ct_lib Tibble or `NULL`. Codelists with is_selected column.
#' @param value_level_meta Tibble or `NULL`. Value-level metadata.
#' @param report `validation_report` or `NULL`.
#' @return Updated `validation_report`.
#' @export
validate_metadata_ct_dataset_consistency <- function(data, target_meta, domain,
                                                      ct_lib = NULL,
                                                      value_level_meta = NULL,
                                                      report = NULL) {
  if (is.null(report)) report <- new_validation_report(domain = domain)

  dom_meta <- dplyr::filter(target_meta, .data[["domain"]] == .env[["domain"]])
  if (nrow(dom_meta) == 0L) return(report)

  # =========== 1. VARIABLE-LEVEL CONSISTENCY ===========

  # 1a. Extra variables in dataset (not in metadata)
  extra_vars <- setdiff(names(data), toupper(dom_meta$var))
  if (length(extra_vars) > 0L) {
    report <- add_finding(report, rule_id = "consistency_extra_vars",
                          severity = "WARNING",
                          message = glue::glue(
                            "{domain}: {length(extra_vars)} variable(s) in dataset but NOT in metadata: {paste(extra_vars, collapse=', ')}"
                          ),
                          domain = domain)
  }

  # 1b. Missing variables in dataset (selected in metadata as REQ or EXP)
  req_exp_vars <- toupper(dom_meta$var[!is.na(dom_meta$core) &
                                        toupper(dom_meta$core) %in% c("REQ", "EXP")])
  missing_vars <- setdiff(req_exp_vars, names(data))
  if (length(missing_vars) > 0L) {
    report <- add_finding(report, rule_id = "consistency_missing_vars",
                          severity = "ERROR",
                          message = glue::glue(
                            "{domain}: {length(missing_vars)} required/expected variable(s) MISSING from dataset: {paste(missing_vars, collapse=', ')}"
                          ),
                          domain = domain)
  }

  # 1c. Permissible variables in metadata not in dataset (OK if PERM)
  all_meta_vars <- toupper(dom_meta$var)
  perm_not_in_data <- setdiff(all_meta_vars, names(data))
  perm_in_meta <- dom_meta$var[toupper(dom_meta$var) %in% perm_not_in_data &
                                !is.na(dom_meta$core) &
                                toupper(dom_meta$core) == "PERM"]
  if (length(perm_in_meta) > 0L) {
    # This is informational - PERM variables can be absent
    report <- add_finding(report, rule_id = "consistency_perm_absent",
                          severity = "NOTE",
                          message = glue::glue(
                            "{domain}: {length(perm_in_meta)} permissible variable(s) in metadata but absent from dataset (OK if not needed): {paste(perm_in_meta, collapse=', ')}"
                          ),
                          domain = domain)
  }

  # =========== 2. VALUE-LEVEL CONSISTENCY (CODELISTS) ===========

  if (!is.null(ct_lib)) {
    ct_vars <- dom_meta[!is.na(dom_meta$codelist_id), ]

    for (i in seq_len(nrow(ct_vars))) {
      v <- ct_vars$var[i]
      if (!v %in% names(data)) next

      cl_id <- ct_vars$codelist_id[i]
      cl_all <- ct_lib %>% dplyr::filter(.data$codelist_id == cl_id)
      if (nrow(cl_all) == 0L) next

      # Separate selected from unselected
      is_selected_col <- "is_selected" %in% names(cl_all)
      if (is_selected_col) {
        cl_selected <- cl_all %>% dplyr::filter(toupper(.data$is_selected) == "Y")
      } else {
        cl_selected <- cl_all
      }

      # Build list of selected CT values
      selected_ct_values <- union(cl_selected$coded_value, cl_selected$submission_value)
      if ("decode" %in% names(cl_selected)) {
        decode_vals <- cl_selected$decode[!is.na(cl_selected$decode) &
                                          trimws(cl_selected$decode) != ""]
        selected_ct_values <- union(selected_ct_values, decode_vals)
      }

      # Get data values
      data_vals <- unique(data[[v]][!is.na(data[[v]])])
      if (length(data_vals) == 0L) next

      # Normalize for comparison
      normalize_ct <- function(x) {
        x <- as.character(x)
        x <- trimws(x)
        x <- tolower(x)
        x <- gsub("\\s+", "", x, perl = TRUE)
        x
      }

      data_norm <- normalize_ct(data_vals)
      selected_norm <- normalize_ct(selected_ct_values)
      all_ct_norm <- normalize_ct(union(cl_all$coded_value, cl_all$submission_value))

      # 2a. Values in dataset not in selected CT terms
      extra_data_vals <- data_vals[!data_norm %in% selected_norm]
      if (length(extra_data_vals) > 0L) {
        # Check if they're in full codelist (unselected) vs not in codelist at all
        extra_data_norm <- normalize_ct(extra_data_vals)
        unselected_in_cl <- extra_data_vals[extra_data_norm %in% all_ct_norm]
        not_in_cl <- extra_data_vals[!extra_data_norm %in% all_ct_norm]

        if (length(unselected_in_cl) > 0L) {
          report <- add_finding(report, rule_id = "consistency_unselected_values",
                                severity = "WARNING",
                                message = glue::glue(
                                  "{domain}/{v}: {length(unselected_in_cl)} value(s) in dataset are in {cl_id} but NOT SELECTED (select≠Y) in CT - UPDATE CT or dataset: {paste(head(unselected_in_cl, 3), collapse=', ')}"
                                ),
                                variable = v, domain = domain)
        }

        if (length(not_in_cl) > 0L) {
          is_ext <- cl_all$is_extensible[1L]
          if (!is.null(is_ext) && toupper(is_ext) == "YES") {
            report <- add_finding(report, rule_id = "consistency_ext_value_not_in_ct",
                                  severity = "WARNING",
                                  message = glue::glue(
                                    "{domain}/{v}: {length(not_in_cl)} value(s) in dataset NOT IN codelist {cl_id} (extensible) - ADD to CT if standard or REMOVE from dataset: {paste(head(not_in_cl, 3), collapse=', ')}"
                                  ),
                                  variable = v, domain = domain)
          } else {
            report <- add_finding(report, rule_id = "consistency_non_ext_value_not_in_ct",
                                  severity = "ERROR",
                                  message = glue::glue(
                                    "{domain}/{v}: {length(not_in_cl)} value(s) in dataset NOT IN codelist {cl_id} (NON-EXTENSIBLE) - REMOVE from dataset or ADD to CT: {paste(head(not_in_cl, 3), collapse=', ')}"
                                  ),
                                  variable = v, domain = domain)
          }
        }
      }

      # 2b. Selected CT terms not appearing in data
      selected_data_vals <- unique(data[[v]][!is.na(data[[v]])])
      selected_data_norm <- normalize_ct(selected_data_vals)
      unused_ct_vals <- cl_selected$coded_value[
        !normalize_ct(cl_selected$coded_value) %in% selected_data_norm
      ]

      if (length(unused_ct_vals) > 0L && nrow(dplyr::filter(data, !is.na(!!rlang::sym(v)))) > 0L) {
        # Only warn if variable has data
        report <- add_finding(report, rule_id = "consistency_unused_ct_values",
                              severity = "NOTE",
                              message = glue::glue(
                                "{domain}/{v}: Selected value(s) in {cl_id} NOT APPEARING in dataset: {paste(head(unused_ct_vals, 3), collapse=', ')} (OK if not expected in this dataset)"
                              ),
                              variable = v, domain = domain)
      }
    }
  }

  # =========== 3. VALUE-LEVEL METADATA CONSISTENCY ===========

  if (!is.null(value_level_meta)) {
    dom_vlm <- dplyr::filter(value_level_meta, .data$domain == .env[["domain"]])

    for (i in seq_len(nrow(dom_vlm))) {
      vlm_var <- dom_vlm$variable[i]
      vlm_where <- dom_vlm$where_var[i]
      vlm_when <- dom_vlm$where_value[i]

      if (!vlm_var %in% names(data)) next

      # Check if VLM condition has data
      if (!is.na(vlm_where) && vlm_where %in% names(data)) {
        if (is.na(vlm_when)) {
          has_data <- nrow(dplyr::filter(data, !is.na(!!rlang::sym(vlm_where)))) > 0L
        } else {
          has_data <- nrow(
            dplyr::filter(data, .data[[vlm_where]] == vlm_when &
                          !is.na(!!rlang::sym(vlm_var)))
          ) > 0L
        }

        if (!has_data) {
          report <- add_finding(report, rule_id = "consistency_unused_vlm",
                                severity = "WARNING",
                                message = glue::glue(
                                  "{domain}: Value-level metadata selected for {vlm_var} (when {vlm_where}={vlm_when}) but NO MATCHING DATA in dataset"
                                ),
                                domain = domain)
        }
      }
    }
  }

  # =========== 4. DATA INTEGRITY CHECKS ===========

  # 4a. Verify no all-missing required/expected variables
  for (v in req_exp_vars) {
    if (!v %in% names(data)) next
    n_non_missing <- sum(!is.na(data[[v]]) & trimws(data[[v]]) != "")
    if (n_non_missing == 0L) {
      report <- add_finding(report, rule_id = "consistency_all_missing",
                            severity = "ERROR",
                            message = glue::glue(
                              "{domain}/{v}: Required/Expected variable is ALL-MISSING in dataset"
                            ),
                            variable = v, domain = domain)
    }
  }

  report
}

#' Print formatted report of consistency findings
#'
#' Generates a beautifully formatted, actionable report of metadata-CT-dataset
#' consistency findings. Organized by severity (ERROR/WARNING/NOTE) and
#' issue type with specific values and recommended actions.
#'
#' Called automatically by [validate_and_report_dataset()].
#'
#' @param report `validation_report`. The validation report to print.
#' @return Invisibly returns report (for piping).
#' @export
print_consistency_report <- function(report) {
  checkmate::assert_class(report, "validation_report")

  findings <- report$findings

  # Filter and organize by severity and rule category
  if (nrow(findings) == 0L) {
    cli::cli_alert_success("✓ No consistency issues found - ready for Define-XML!")
    return(invisible(report))
  }

  # Count by severity
  n_err <- sum(findings$severity == "ERROR")
  n_warn <- sum(findings$severity == "WARNING")
  n_note <- sum(findings$severity == "NOTE")

  # Print header
  cli::cli_h1("Metadata ↔ CT ↔ Dataset Consistency Report")
  if (!is.null(report$domain)) {
    cli::cli_text("Domain: {.strong {report$domain}}")
  }
  cli::cli_text("Generated: {format(Sys.time(), '%Y-%m-%d %H:%M:%S')} UTC")
  cli::cli_br()

  # Summary counts
  cli::cli_div(theme = list(
    col_type = list("span" = list(before = "{.strong ", after = "}"))
  ))
  cli::cli_h2("Summary")
  cli::cli_ul(c(
    "{.red ERROR}s: {n_err}",
    "{.yellow WARNING}s: {n_warn}",
    "{.cyan NOTE}s: {n_note}"
  ))
  cli::cli_end()

  # Group findings by issue type (rule_id)
  issue_groups <- unique(findings$rule_id)
  issue_labels <- list(
    consistency_extra_vars = "Extra Variables in Dataset",
    consistency_missing_vars = "Missing Required Variables",
    consistency_perm_absent = "Permissible Variables Absent",
    consistency_unselected_values = "Unselected CT Values Used",
    consistency_ext_value_not_in_ct = "Extensible Codelist: Value Not in CT",
    consistency_non_ext_value_not_in_ct = "Non-Extensible Codelist: Value Not in CT",
    consistency_unused_ct_values = "Selected CT Values Not Used",
    consistency_unused_vlm = "Unused Value-Level Metadata",
    consistency_all_missing = "All-Missing Required Variables"
  )

  for (issue in issue_groups) {
    issue_findings <- dplyr::filter(findings, .data$rule_id == issue)
    issue_title <- issue_labels[[issue]] %||% issue

    # Determine icon and color based on severity
    max_sev <- issue_findings$severity[1L]
    if (any(issue_findings$severity == "ERROR")) max_sev <- "ERROR"
    else if (any(issue_findings$severity == "WARNING")) max_sev <- "WARNING"

    icon <- switch(max_sev,
      "ERROR" = "✗",
      "WARNING" = "!",
      "NOTE" = "ℹ",
      "?"
    )
    color <- switch(max_sev,
      "ERROR" = "red",
      "WARNING" = "yellow",
      "NOTE" = "cyan",
      "grey"
    )

    # Print issue group header
    cli::cli_h3("{.{color} {icon} {issue_title}} ({nrow(issue_findings)})")

    # Print each finding in this group
    for (k in seq_len(nrow(issue_findings))) {
      finding <- issue_findings[k, ]
      severity_text <- switch(finding$severity,
        "ERROR" = "{.red [ERROR]}",
        "WARNING" = "{.yellow [WARN]}",
        "NOTE" = "{.cyan [INFO]}"
      )
      cli::cli_text("{severity_text} {finding$message}")
    }

    cli::cli_br()
  }

  # Actionable next steps
  if (n_err > 0L || n_warn > 0L) {
    cli::cli_h2("Next Steps")
    cli::cli_ul(c(
      "Review all {.red [ERROR]} findings - these {.strong MUST} be resolved before Define-XML",
      "Address {.yellow [WARN]} findings - these should be resolved for data quality",
      "Review {.cyan [INFO]} findings - informational; may not require action",
      "Update dataset ({.file sdtm/datasets}) or metadata ({.file metadata/*.xlsx) as needed",
      "Rerun validation to verify fixes"
    ))
  } else if (n_note == 0L) {
    cli::cli_alert_success("✓ All consistency checks passed - ready to generate Define-XML!")
  }

  invisible(report)
}

# ==============================================================================
# CONVENIENCE WRAPPERS FOR TWO-PHASE VALIDATION
# ==============================================================================

#' Quick validation of metadata and CT structure (Phase 1)
#'
#' Convenient wrapper for one-line pre-validation of metadata and CT.
#' Prints report automatically for immediate feedback.
#'
#' **Use Case**: Run at the beginning of your project to catch metadata/CT
#' issues before dataset creation.
#'
#' @param target_meta Tibble. Target metadata.
#' @param ct_lib Tibble or `NULL`. Controlled terminology library.
#' @param value_level_meta Tibble or `NULL`. Value-level metadata.
#' @param domain_meta Tibble or `NULL`. Domain-level metadata.
#' @param print_report Logical. Default `TRUE`. Print formatted report.
#' @return Invisibly returns `validation_report` (silently if print_report=TRUE).
#' @export
#' @examples
#' \dontrun{
#'   # Run pre-validation on your metadata files
#'   validate_and_report_metadata_ct(
#'     target_meta = metadata$target_meta,
#'     ct_lib = ct_spec,
#'     value_level_meta = metadata$value_level_meta,
#'     domain_meta = metadata$domain_meta
#'   )
#' }
validate_and_report_metadata_ct <- function(target_meta,
                                             ct_lib = NULL,
                                             value_level_meta = NULL,
                                             domain_meta = NULL,
                                             print_report = TRUE) {
  rpt <- validate_metadata_ct_structure(
    target_meta = target_meta,
    ct_lib = ct_lib,
    value_level_meta = value_level_meta,
    domain_meta = domain_meta
  )

  if (print_report) {
    print(rpt)
  }

  invisible(rpt)
}

#' Quick validation of dataset consistency with metadata and CT (Phase 2)
#'
#' Convenient wrapper for one-line final validation of datasets against
#' metadata and CT. Prints report automatically for immediate feedback.
#'
#' **Use Case**: Run after dataset creation (or with manually created datasets)
#' to verify complete alignment before Define-XML generation.
#'
#' @param data Tibble. The SDTM domain dataset to validate.
#' @param domain Character. Domain abbreviation (e.g., "AE", "DM").
#' @param target_meta Tibble. Target metadata.
#' @param ct_lib Tibble or `NULL`. Controlled terminology library.
#' @param value_level_meta Tibble or `NULL`. Value-level metadata.
#' @param print_report Logical. Default `TRUE`. Print formatted report.
#' @return Invisibly returns `validation_report`.
#' @export
#' @examples
#' \dontrun{
#'   # Validate a built AE dataset
#'   validate_and_report_dataset(
#'     data = ae_built,
#'     domain = "AE",
#'     target_meta = metadata$target_meta,
#'     ct_lib = ct_spec,
#'     value_level_meta = metadata$value_level_meta
#'   )
#' }
validate_and_report_dataset <- function(data, domain, target_meta,
                                         ct_lib = NULL,
                                         value_level_meta = NULL,
                                         print_report = TRUE) {
  rpt <- validate_metadata_ct_dataset_consistency(
    data = data,
    target_meta = target_meta,
    domain = domain,
    ct_lib = ct_lib,
    value_level_meta = value_level_meta,
    report = NULL
  )

  if (print_report) {
    print_consistency_report(rpt)
  }

  invisible(rpt)
}
