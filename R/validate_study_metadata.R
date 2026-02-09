# ==============================================================================
# Validation Functions for Study Metadata & CT Excel Workbooks
# ==============================================================================
# Cross-sheet consistency checks run after reading Study_Metadata.xlsx and
# Study_CT.xlsx. Called from within the reader functions or standalone.
# ==============================================================================

#' Validate study metadata cross-references
#'
#' @description
#' Runs a suite of consistency checks across the components returned by
#' [read_study_metadata_excel()] and [read_study_ct_excel()]:
#'
#' 1. **Domain consistency** — every domain in Variables exists in Domains sheet.
#' 2. **Where clause references** — every `WHERE_CLAUSE_ID` in Value Level
#'    exists in Where Clauses.
#' 3. **VLM references** — every `VLM_ID` in Variables exists in Value Level.
#' 4. **CT references** — every `codelist_id` in Variables/Value Level exists
#'    in the CT library.
#' 5. **Extensibility** — warns if a term would need to be added to a
#'    non-extensible codelist.
#'
#' @param study_meta Named list returned by [read_study_metadata_excel()].
#' @param ct_lib Tibble returned by [read_study_ct_excel()], or `NULL`.
#' @param strict Logical. If `TRUE`, errors on any failure. If `FALSE`
#'   (default), issues warnings.
#'
#' @return Invisible list of validation results:
#'   \describe{
#'     \item{domain_check}{Tibble of domains in Variables but not in Domains sheet.}
#'     \item{where_clause_check}{Tibble of missing WHERE_CLAUSE_IDs.}
#'     \item{vlm_check}{Tibble of VLM_IDs in Variables with no Value Level entry.}
#'     \item{ct_check}{Tibble of codelist_ids not found in CT library.}
#'     \item{passed}{Logical. TRUE if all checks passed.}
#'   }
#'
#' @export
validate_study_metadata <- function(study_meta, ct_lib = NULL, strict = FALSE) {
  target_meta      <- study_meta$target_meta
  domain_meta      <- study_meta$domain_meta
  value_level_meta <- study_meta$value_level_meta
  results <- list(passed = TRUE)

  report <- function(msg, items = NULL) {
    if (strict) abort(msg)
    warn(msg)
    results$passed <<- FALSE
  }

  # 1. Domain consistency: all domains in Variables exist in Domains sheet
  var_domains <- unique(target_meta$domain)
  dom_domains <- unique(domain_meta$domain)
  missing_doms <- setdiff(var_domains, dom_domains)
  results$domain_check <- tibble::tibble(domain = missing_doms)
  if (length(missing_doms) > 0L) {
    report(glue::glue(
      "Variables sheet contains domains not in Domains sheet: ",
      "{paste(missing_doms, collapse = ', ')}"
    ))
  }

  # 2. Where clause references: WHERE_CLAUSE_IDs in Value Level exist in Where Clauses
  if (!is.null(value_level_meta) && nrow(value_level_meta) > 0L &&
      "where_clause_id" %in% names(value_level_meta)) {
    vlm_wc_ids <- unique(value_level_meta$where_clause_id)
    vlm_wc_ids <- vlm_wc_ids[!is.na(vlm_wc_ids)]
    # Check if WHERE clauses were successfully joined (wc_varname populated)
    if ("wc_varname" %in% names(value_level_meta)) {
      unresolved <- value_level_meta %>%
        dplyr::filter(!is.na(.data$where_clause_id) & is.na(.data$wc_varname))
      missing_wc <- unique(unresolved$where_clause_id)
    } else {
      missing_wc <- character()
    }
    results$where_clause_check <- tibble::tibble(where_clause_id = missing_wc)
    if (length(missing_wc) > 0L) {
      report(glue::glue(
        "Value Level sheet references WHERE_CLAUSE_IDs not found in Where Clauses: ",
        "{paste(missing_wc, collapse = ', ')}"
      ))
    }
  } else {
    results$where_clause_check <- tibble::tibble(where_clause_id = character())
  }

  # 3. VLM references: VLM_IDs in Variables exist in Value Level
  if ("value_level_id" %in% names(target_meta)) {
    var_vlm_ids <- unique(target_meta$value_level_id)
    var_vlm_ids <- var_vlm_ids[!is.na(var_vlm_ids)]
    if (!is.null(value_level_meta) && nrow(value_level_meta) > 0L &&
        "value_level_id" %in% names(value_level_meta)) {
      vlm_ids <- unique(value_level_meta$value_level_id)
      missing_vlm <- setdiff(var_vlm_ids, vlm_ids)
    } else {
      missing_vlm <- var_vlm_ids
    }
    results$vlm_check <- tibble::tibble(value_level_id = missing_vlm)
    if (length(missing_vlm) > 0L) {
      report(glue::glue(
        "Variables sheet VLM_IDs not found in Value Level sheet: ",
        "{paste(missing_vlm, collapse = ', ')}"
      ))
    }
  } else {
    results$vlm_check <- tibble::tibble(value_level_id = character())
  }

  # 4. CT references: codelist_ids exist in CT library
  if (!is.null(ct_lib) && nrow(ct_lib) > 0L) {
    ct_ids <- unique(ct_lib$codelist_id)

    # From target_meta
    var_ct_ids <- unique(target_meta$codelist_id)
    var_ct_ids <- var_ct_ids[!is.na(var_ct_ids)]

    # From value_level_meta
    vlm_ct_ids <- character()
    if (!is.null(value_level_meta) && "codelist_id" %in% names(value_level_meta)) {
      vlm_ct_ids <- unique(value_level_meta$codelist_id)
      vlm_ct_ids <- vlm_ct_ids[!is.na(vlm_ct_ids)]
    }

    all_ct_refs <- unique(c(var_ct_ids, vlm_ct_ids))
    missing_ct <- setdiff(all_ct_refs, ct_ids)
    results$ct_check <- tibble::tibble(codelist_id = missing_ct)
    if (length(missing_ct) > 0L) {
      report(glue::glue(
        "Codelist IDs referenced in metadata but not found in CT library: ",
        "{paste(missing_ct, collapse = ', ')}"
      ))
    }
  } else {
    results$ct_check <- tibble::tibble(codelist_id = character())
  }

  invisible(results)
}


#' Check codelist extensibility constraints
#'
#' @description
#' Checks whether any data values would need to be added to non-extensible
#' codelists. This is an informational check — does not error by default.
#'
#' @param data Tibble. Domain data with columns that have codelist_id assignments.
#' @param target_meta Tibble. Variable metadata with codelist_id column.
#' @param ct_lib Tibble. CT library with codelist_id, coded_value, is_extensible.
#' @param domain Character. Domain abbreviation.
#'
#' @return Tibble of violations (variable, value, codelist_id) or empty tibble.
#'
#' @export
check_extensibility <- function(data, target_meta, ct_lib, domain) {
  if (is.null(ct_lib) || nrow(ct_lib) == 0L) return(tibble::tibble())
  if (!"is_extensible" %in% names(ct_lib)) return(tibble::tibble())

  dom_meta <- dplyr::filter(target_meta, .data$domain == !!toupper(domain))
  ct_vars <- dplyr::filter(dom_meta, !is.na(.data$codelist_id))
  if (nrow(ct_vars) == 0L) return(tibble::tibble())

  violations <- list()
  for (i in seq_len(nrow(ct_vars))) {
    v <- ct_vars$var[i]
    cl_id <- ct_vars$codelist_id[i]
    if (!v %in% names(data)) next

    cl_terms <- dplyr::filter(ct_lib, .data$codelist_id == cl_id)
    if (nrow(cl_terms) == 0L) next

    is_ext <- any(toupper(cl_terms$is_extensible) == "YES", na.rm = TRUE)
    if (is_ext) next  # extensible codelists are fine

    allowed <- unique(cl_terms$coded_value)
    actual <- unique(data[[v]])
    actual <- actual[!is.na(actual)]
    not_allowed <- setdiff(actual, allowed)

    if (length(not_allowed) > 0L) {
      for (bad in not_allowed) {
        violations <- c(violations, list(tibble::tibble(
          domain      = domain,
          variable    = v,
          value       = bad,
          codelist_id = cl_id
        )))
      }
    }
  }

  if (length(violations) == 0L) return(tibble::tibble())
  dplyr::bind_rows(violations)
}
