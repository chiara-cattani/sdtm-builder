# ******************************************************************************
# Danone Nutricia Research
# ******************************************************************************
# STUDY/PROJECT : SONA / 23REX0061265
# PROGRAM PATH  : SAS/SONA/Files/SDTM Conversion/01 Macros
# PROGRAM NAME  : copy_import.R
# PURPOSE       : Import a CRF dataset, derive USUBJID, filter/sort, add OAK vars.
# ------------------------------------------------------------------------------
# NOTES :
#   - If the source SAS file does NOT exist, a message is logged and an EMPTY
#     dataset is returned (0 rows) with `usubjid` and OAK ID vars (no stop/error).
#   - If `subjectid` is missing, USUBJID derivation is skipped (kept as NA).
#   - Sorting by `by` is skipped if requested columns are absent (with a message).
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2025-09-29 - cattanch - Initial program
# ******************************************************************************

#' @title Import CRF data, derive USUBJID, and add OAK ID variables
#'
#' @description
#' Reads a SAS dataset, normalizes blanks and column case, derives **USUBJID**
#' from `subjectid` when available, optionally applies a filter and sort,
#' removes SAS formats, and finally calls `generate_oak_id_vars()` using `usubjid`.
#' If the source file does **not** exist, it logs a message and returns an **empty**
#' dataset (0 rows) with `usubjid` and OAK ID variables (no error).
#'
#' @param inset     Character. Dataset name without extension (e.g., `"dm"`).
#' @param inlib     Character. Directory path containing the SAS file.
#' @param studyid   Character. Study ID prefix used to build `USUBJID`.
#' @param by        Character vector or comma-separated string of columns to sort by.
#' @param where     Character. R expression (as a string) used to filter rows
#'                  (e.g., `"country == 'NL' & !is.na(subjectid)"`).
#' @param noformats Logical. If `TRUE`, drop SAS formats.
#' @param raw_src   Character. Source name to pass to `generate_oak_id_vars()`.
#'
#' @return A tibble with `usubjid` (if derivable) and OAK ID variables appended.
#'         If the source is missing, returns an **empty** tibble (0 rows) with
#'         `usubjid` + OAK ID vars.
#'
#' @details
#' - Column names are lower-cased after import.
#' - USUBJID is derived from `subjectid` formatted like `STUDY-SITE-SUBJ`.
#'
#' @examples
#' \dontrun{
#' dm1 <- copy_import(
#'   inset     = "dm",
#'   inlib     = "../04 Import Data",
#'   studyid   = "SONA",
#'   by        = "usubjid",
#'   where     = NULL,
#'   noformats = TRUE,
#'   raw_src   = "dm"
#' )
#' }
#'
#' @import dplyr haven stringr rlang admiral sdtm.oak

copy_import <- function(inset,
                        inlib     = "../04 Import Data",
                        studyid   = study,
                        by        = "usubjid",
                        where     = NULL,
                        noformats = TRUE,
                        raw_src   = inset) {
  src_path <- file.path(inlib, paste0(inset, ".sas7bdat"))
  
  # Read SAS file with fallback to empty structure if file missing or unreadable
  ds <- if (!file.exists(src_path)) {
    message("copy_import: source file not found: ", src_path,
            " — returning empty dataset with full structure.")
    haven::read_sas(src_path, n_max = 0)
  } else {
    tryCatch(
      haven::read_sas(src_path),
      error = function(e) {
        message("copy_import: failed to read ", src_path, " — ", conditionMessage(e),
                ". Returning empty dataset with full structure.")
        haven::read_sas(src_path, n_max = 0)
      }
    )
  }
  
  # Normalize and lowercase names
  ds <- ds %>%
    convert_blanks_to_na() %>%
    dplyr::rename_with(tolower)
  
  # Coerce all date-like variables to character to avoid create_iso8601 errors
  ds <- ds %>%
    mutate(across(where(lubridate::is.Date), as.character)) %>%
    mutate(across(where(is.factor), as.character))
  
  # Derive USUBJID from subjectid if present
  if ("subjectid" %in% names(ds)) {
    cap <- stringr::str_match(ds$subjectid, "([^-]+)-([^-]+)-([^-]+)$")
    ds <- ds %>%
      dplyr::mutate(
        usubjid = ifelse(
          is.na(cap[, 1]),
          NA_character_,
          paste(
            studyid,
            cap[, 2],
            cap[, 3],
            sprintf("%04d", suppressWarnings(as.integer(cap[, 4]))),
            sep = "-"
          )
        )
      )
  } else if (!"usubjid" %in% names(ds)) {
    message("copy_import: 'subjectid' not found and no existing 'usubjid' — USUBJID left as NA.")
    ds$usubjid <- NA_character_
  }
  
  # Optional filter
  if (!is.null(where) && nzchar(where)) {
    ds <- dplyr::filter(ds, !!rlang::parse_expr(where))
  }
  
  # Optional sort (skip if 'by' columns are missing)
  if (!is.null(by) && length(by)) {
    if (length(by) == 1L) by <- strsplit(by, ",\\s*")[[1]]
    by <- trimws(by)
    miss_by <- setdiff(by, names(ds))
    if (length(miss_by)) {
      message("copy_import: skip sorting — missing 'by' column(s): ",
              paste(miss_by, collapse = ", "))
    } else {
      ds <- dplyr::arrange(ds, dplyr::across(dplyr::all_of(by)))
    }
  }
  
  # Drop SAS formats if requested
  if (isTRUE(noformats)) {
    ds <- haven::zap_formats(ds)
  }
  
  # Add OAK ID vars
  ds <- generate_oak_id_vars(
    ds,
    pat_var = "usubjid",
    raw_src = raw_src
  )
  
  # Ensure correct types for OAK ID vars if dataset is empty
  if (nrow(ds) == 0L) {
    expected_types <- list(
      patient_number = character(),
      oak_id         = integer(),
      raw_source     = character()
    )
    for (var in names(expected_types)) {
      if (var %in% names(ds)) {
        ds[[var]] <- expected_types[[var]]
      }
    }
  }
  
  return(ds)
}