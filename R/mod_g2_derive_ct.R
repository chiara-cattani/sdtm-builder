# ==============================================================================
# Module G2: Controlled Terminology & Dictionary Derivations
# ==============================================================================
# Functions: assign_ct, decode_ct, map_yes_no, map_unknown, validate_ct_values,
#            get_meddra_version, get_whodrug_version, derive_dict_version
# ==============================================================================

#' Assign controlled terminology coded values
#' @param data Tibble.
#' @param target_var Character.
#' @param source_var Character.
#' @param codelist_id Character.
#' @param ct_lib Tibble.
#' @param case_sensitive Logical. Default `FALSE`.
#' @param unknown_policy Character. Default `"warn_and_keep"`.
#' @param custom_map Named character vector or `NULL`.
#' @return Tibble.
#' @export
assign_ct <- function(data, target_var, source_var, codelist_id, ct_lib,
                      case_sensitive = FALSE, unknown_policy = "warn_and_keep",
                      custom_map = NULL) {
  if (!source_var %in% names(data)) {
    abort(glue::glue("assign_ct: source column '{source_var}' not found"))
  }

  # Build lookup from ct_lib
  cl <- dplyr::filter(ct_lib, .data$codelist_id == !!codelist_id)
  if (nrow(cl) == 0L) {
    abort(glue::glue("assign_ct: codelist '{codelist_id}' not found in CT library"))
  }

  # Build mapping: input_value -> coded_value
  if ("input_value" %in% names(cl)) {
    lookup_keys <- cl$input_value
    lookup_vals <- cl$coded_value
  } else {
    lookup_keys <- cl$coded_value
    lookup_vals <- cl$coded_value
  }

  # Also add identity mappings (coded_value -> coded_value) so that raw data

  # already containing CDISC submission values passes through correctly.
  lookup_keys <- c(lookup_keys, cl$coded_value)
  lookup_vals <- c(lookup_vals, cl$coded_value)

  if (!case_sensitive) {
    lookup <- stats::setNames(lookup_vals, tolower(lookup_keys))
  } else {
    lookup <- stats::setNames(lookup_vals, lookup_keys)
  }

  # Apply custom map first (overrides CT)
  if (!is.null(custom_map)) {
    if (!case_sensitive) {
      cm <- stats::setNames(custom_map, tolower(names(custom_map)))
    } else {
      cm <- custom_map
    }
    lookup <- c(cm, lookup)
  }

  # Deduplicate lookup (first wins)
  lookup <- lookup[!duplicated(names(lookup))]

  raw_vals <- data[[source_var]]
  match_keys <- if (!case_sensitive) tolower(raw_vals) else raw_vals

  mapped <- lookup[match_keys]
  mapped <- unname(mapped)

  # Handle unknowns
  unknown_mask <- !is.na(raw_vals) & is.na(mapped)
  if (any(unknown_mask)) {
    unknown_vals <- unique(raw_vals[unknown_mask])
    msg <- glue::glue("assign_ct: {sum(unknown_mask)} value(s) not in codelist '{codelist_id}': {paste(unknown_vals, collapse = ', ')}")
    switch(unknown_policy,
      error          = abort(msg),
      warn_and_keep  = { warn(msg); mapped[unknown_mask] <- raw_vals[unknown_mask] },
      warn_and_na    = { warn(msg) },
      keep           = { mapped[unknown_mask] <- raw_vals[unknown_mask] },
      na             = { },
      { warn(msg); mapped[unknown_mask] <- raw_vals[unknown_mask] }
    )
  }

  data[[target_var]] <- mapped
  data
}

#' Decode controlled terminology
#' @param data Tibble.
#' @param target_var Character.
#' @param source_var Character.
#' @param codelist_id Character.
#' @param ct_lib Tibble.
#' @param missing_decode Character. Default `NA_character_`.
#' @return Tibble.
#' @export
decode_ct <- function(data, target_var, source_var, codelist_id, ct_lib,
                      missing_decode = NA_character_) {
  cl <- dplyr::filter(ct_lib, .data$codelist_id == !!codelist_id)
  if (nrow(cl) == 0L) {
    warn(glue::glue("decode_ct: codelist '{codelist_id}' not found"))
    data[[target_var]] <- missing_decode
    return(data)
  }
  if (!"decode" %in% names(cl)) {
    data[[target_var]] <- data[[source_var]]
    return(data)
  }
  lookup <- stats::setNames(cl$decode, cl$coded_value)
  lookup <- lookup[!duplicated(names(lookup))]

  coded <- data[[source_var]]
  decoded <- unname(lookup[coded])
  decoded[is.na(decoded) & !is.na(coded)] <- missing_decode
  data[[target_var]] <- decoded
  data
}

#' Map values to Y/N format
#' @param data Tibble.
#' @param target_var Character.
#' @param source_var Character.
#' @param yes_values Character vector.
#' @param no_values Character vector.
#' @param na_policy Character. Default `"na"`.
#' @return Tibble.
#' @export
map_yes_no <- function(data, target_var, source_var,
                       yes_values = c("Y","YES","1","TRUE","X","CHECKED"),
                       no_values = c("N","NO","0","FALSE","UNCHECKED"),
                       na_policy = "na") {
  raw <- toupper(as.character(data[[source_var]]))
  result <- dplyr::case_when(
    raw %in% toupper(yes_values) ~ "Y",
    raw %in% toupper(no_values) ~ "N",
    is.na(raw) ~ NA_character_,
    TRUE ~ switch(na_policy, na = NA_character_, keep = data[[source_var]], error = {
      abort(glue::glue("map_yes_no: unmapped values found"))
    })
  )
  data[[target_var]] <- result
  data
}

#' Map unknown indicators to SDTM conventions
#' @param data Tibble.
#' @param target_var Character.
#' @param source_var Character.
#' @param unknown_tokens Character vector.
#' @param sdtm_value Character. Default `"UNKNOWN"`.
#' @return Tibble.
#' @export
map_unknown <- function(data, target_var, source_var,
                        unknown_tokens = c("UNK","UNKNOWN","NK","U","N/A"),
                        sdtm_value = "UNKNOWN") {
  raw <- data[[source_var]]
  is_unk <- !is.na(raw) & toupper(raw) %in% toupper(unknown_tokens)
  result <- raw
  result[is_unk] <- sdtm_value
  data[[target_var]] <- result
  data
}

#' Validate CT values
#' @param data Tibble.
#' @param var Character.
#' @param codelist_id Character.
#' @param ct_lib Tibble.
#' @param check_type Character. Default `"coded"`.
#' @param allow_na Logical. Default `TRUE`.
#' @param allow_other Character vector.
#' @return Tibble of findings.
#' @export
validate_ct_values <- function(data, var, codelist_id, ct_lib,
                               check_type = "coded", allow_na = TRUE,
                               allow_other = character()) {
  cl <- dplyr::filter(ct_lib, .data$codelist_id == !!codelist_id)
  if (check_type == "coded") {
    valid <- unique(cl$coded_value)
  } else {
    valid <- unique(cl$decode)
  }
  valid <- c(valid, allow_other)
  vals <- data[[var]]
  if (allow_na) vals <- vals[!is.na(vals)]
  invalid <- setdiff(unique(vals), valid)
  if (length(invalid) == 0L) return(tibble::tibble())
  tibble::tibble(
    rule_id  = paste0("CT_", codelist_id),
    severity = "WARNING",
    variable = var,
    message  = paste("Values not in CT:", paste(invalid, collapse = ", ")),
    n_records = sum(vals %in% invalid),
    example  = invalid[1]
  )
}


#' Get MedDRA dictionary version from a dataset
#'
#' Extracts the MedDRA version string from a column (typically `DictInstance`
#' or `version`). Returns a character scalar such as `"27.1"`.
#'
#' @param indata A data frame containing the dictionary version column.
#' @param dictvar Character. Column name to inspect. Default `"DictInstance"`
#'   (matched case-insensitively).
#' @param compare Optional character. If provided, a message is emitted
#'   when the detected version does not match.
#' @return Character scalar with the MedDRA version(s), or `""`.
#' @export
get_meddra_version <- function(indata,
                               dictvar = "DictInstance",
                               compare = NULL) {
  if (missing(indata) || is.null(indata)) {
    message("get_meddra_version: No dataset provided.")
    return("")
  }
  if (!is.data.frame(indata)) {
    message("get_meddra_version: Input is not a data frame.")
    return("")
  }

  dictvar_match <- names(indata)[tolower(names(indata)) == tolower(dictvar)]
  if (length(dictvar_match) == 0L) {
    dictvar_match <- names(indata)[tolower(names(indata)) == "version"]
  }
  if (length(dictvar_match) == 0L) {
    message("get_meddra_version: Column '", dictvar,
            "' (or 'version') not found in dataset.")
    return("")
  }

  dictvar <- dictvar_match[1L]
  vals <- as.character(indata[[dictvar]])
  vals <- vals[!is.na(vals) & trimws(vals) != ""]
  if (length(vals) == 0L) {
    message("get_meddra_version: No non-empty values in '", dictvar, "'.")
    return("")
  }

  extracted <- stringr::str_extract(vals, "\\d{2}\\.\\d")
  versions <- unique(extracted[!is.na(extracted) & extracted != ""])
  if (length(versions) == 0L) {
    message("get_meddra_version: Could not extract version pattern.")
    return("")
  }

  result <- paste(sort(versions), collapse = "#")
  if (grepl("#", result, fixed = TRUE)) {
    message("get_meddra_version: Multiple MedDRA versions found: ",
            gsub("#", ", ", result))
  }
  if (!is.null(compare) && nzchar(compare) && result != compare) {
    message("get_meddra_version: Detected version (", result,
            ") does not match expected (", compare, ").")
  }
  result
}


#' Get WHODrug dictionary version from a dataset
#'
#' Extracts the WHODrug version string from a column (typically
#' `DictInstance` or `version`). Returns a character scalar such as
#' `"2023Q1"`.
#'
#' @param indata A data frame containing the dictionary version column.
#' @param dictvar Character. Column name to inspect. Default `"DictInstance"`
#'   (matched case-insensitively).
#' @param compare Optional character. If provided, a message is emitted
#'   when the detected version does not match.
#' @return Character scalar with the WHODrug version(s), or `""`.
#' @export
get_whodrug_version <- function(indata,
                                dictvar = "DictInstance",
                                compare = NULL) {
  if (missing(indata) || is.null(indata)) {
    message("get_whodrug_version: No dataset provided.")
    return("")
  }
  if (!is.data.frame(indata)) {
    message("get_whodrug_version: Input is not a data frame.")
    return("")
  }

  dictvar_match <- names(indata)[tolower(names(indata)) == tolower(dictvar)]
  if (length(dictvar_match) == 0L) {
    dictvar_match <- names(indata)[tolower(names(indata)) == "version"]
  }
  if (length(dictvar_match) == 0L) {
    message("get_whodrug_version: Column '", dictvar,
            "' (or 'version') not found in dataset.")
    return("")
  }

  dictvar <- dictvar_match[1L]
  vals <- as.character(indata[[dictvar]])
  vals <- vals[!is.na(vals) & trimws(vals) != ""]
  if (length(vals) == 0L) {
    message("get_whodrug_version: No non-empty values in '", dictvar, "'.")
    return("")
  }

  whodrug_versions <- vapply(vals, function(v) {
    v <- as.character(v)
    year <- stringr::str_extract(v, "20\\d{2}")
    if (is.na(year)) return(NA_character_)
    v_upper <- toupper(v)
    quarter <- dplyr::case_when(
      grepl("MAR|Q1", v_upper) ~ "Q1",
      grepl("SEP|Q3", v_upper) ~ "Q3",
      TRUE ~ NA_character_
    )
    if (is.na(quarter)) return(NA_character_)
    paste0(year, quarter)
  }, character(1), USE.NAMES = FALSE)

  versions <- unique(whodrug_versions[!is.na(whodrug_versions)])
  if (length(versions) == 0L) {
    raw_versions <- unique(trimws(vals))
    if (length(raw_versions) == 1L) return(raw_versions)
    message("get_whodrug_version: Could not determine WHODrug version.")
    return("")
  }

  result <- paste(sort(versions), collapse = "#")
  if (grepl("#", result, fixed = TRUE)) {
    message("get_whodrug_version: Multiple WHODrug versions found: ",
            gsub("#", ", ", result))
  }
  if (!is.null(compare) && nzchar(compare) && result != compare) {
    message("get_whodrug_version: Detected version (", result,
            ") does not match expected (", compare, ").")
  }
  result
}


#' Derive dictionary version string for a domain variable
#'
#' Sets `--DICT` to e.g. `"MedDRA 27.1"` or `"WHODrug 2023Q1"` when
#' the decoded variable is non-missing.
#'
#' @param data Tibble.
#' @param target_var Character. Target dictionary variable (e.g., `"AEDICT"`).
#' @param decode_var Character. The decoded variable to check for non-missing
#'   values (e.g., `"AEDECOD"`).
#' @param dict_type Character. `"meddra"` or `"whodrug"`.
#' @param version Character. The version string (e.g., `"27.1"`).
#' @return Tibble with `target_var` populated.
#' @export
derive_dict_version <- function(data, target_var, decode_var,
                                dict_type = c("meddra", "whodrug"),
                                version) {
  dict_type <- match.arg(dict_type)
  prefix <- switch(dict_type, meddra = "MedDRA", whodrug = "WHODrug")
  dict_label <- paste(prefix, version)

  if (!decode_var %in% names(data)) {
    data[[target_var]] <- NA_character_
    return(data)
  }

  data[[target_var]] <- dplyr::if_else(
    !is.na(data[[decode_var]]) & nchar(trimws(data[[decode_var]])) > 0L,
    dict_label,
    ""
  )
  data
}
