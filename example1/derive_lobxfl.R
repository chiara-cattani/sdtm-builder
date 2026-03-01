# ******************************************************************************
# Danone Nutricia Research
# ******************************************************************************
# STUDY/PROJECT : SONA / 23REX0061265
# PROGRAM PATH  : R/SONA/Files/SDTM Conversion/01 Macros
# PROGRAM NAME  : derive_lobxfl.R
# PURPOSE       : Derive --LOBXFL flag for last pre-exposure observation
# ------------------------------------------------------------------------------
# NOTES :
#   - Automatically applies `!is.na(--ORRES)` as base condition for VALID.
#   - Optionally accepts an additional condition via `add_val_rule` to refine VALID.
#   - Joins with DM to get RFXSTDTC and compares against domain datetime.
#   - Works for any domain with standard --DTC and --ORRES structure.
#   - Gracefully handles empty input datasets and type mismatches.
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2025-10-16 - cattanch - Robust version with empty input handling
# ******************************************************************************

#' @title Derive --LOBXFL flag for last pre-exposure observation
#'
#' @description
#' Flags the last valid observation before the reference start date (`RFXSTDTC`)
#' for each subject and test code, using the SDTM convention for `--LOBXFL`.
#' The function always checks for `!is.na(--ORRES)` and optionally combines it
#' with a user-defined condition via `add_val_rule`.
#'
#' @param domain_data  A tibble containing the SDTM domain data (e.g., `qs2`, `vs1`).
#' @param dm_data      A tibble containing the DM dataset with `USUBJID` and `RFXSTDTC`.
#' @param sdtm_domain  Character. Two-letter SDTM domain code (e.g., `"QS"`, `"VS"`).
#' @param add_val_rule Optional. A quoted expression (e.g., `expr(...)`) that adds
#'                     extra conditions to the default `!is.na(--ORRES)` rule.
#'
#' @return A tibble with the derived `--LOBXFL` flag added to the input domain data.
#'         The flag is `"Y"` for the last valid pre-exposure record per subject/test.
#'
#' @details
#' - Parses `--DTC` and `RFXSTDTC` into POSIX datetime using `lubridate::parse_date_time`.
#' - Applies `VALID` condition as `!is.na(--ORRES)` AND `add_val_rule` if provided.
#' - Flags the latest pre-exposure record (`--DTC < RFXSTDTC`) per `USUBJID` and `--TESTCD`.
#' - Drops intermediate variables (`DTC_PARSED`, `RFXSTDTM`, `VALID`, `PRE_EXP`) before return.
#'
#' @examples
#' \dontrun{
#' # QS domain: default rule only
#' qs3 <- derive_lobxfl(qs2, dm1, "QS")
#'
#' # VS domain: add VSSTAT exclusion
#' vs3 <- derive_lobxfl(
#'   vs1,
#'   dm1,
#'   "VS",
#'   add_val_rule = expr(toupper(coalesce(VSSTAT, "")) != "NOT DONE")
#' )
#' }
#'
#' @import dplyr lubridate stringr rlang

derive_lobxfl <- function(domain_data, dm_data, sdtm_domain, add_val_rule = NULL) {
  library(dplyr)
  library(lubridate)
  library(stringr)
  library(rlang)
  
  # Define domain-specific variables
  date_var   <- paste0(sdtm_domain, "DTC")
  testcd_var <- paste0(sdtm_domain, "TESTCD")
  orres_var  <- paste0(sdtm_domain, "ORRES")
  lobxfl_var <- paste0(sdtm_domain, "LOBXFL")
  
  # If domain_data is empty, add empty --LOBXFL column and return
  if (nrow(domain_data) == 0L) {
    domain_data[[lobxfl_var]] <- character(0)
    return(domain_data)
  }
  
  # If dm_data is empty, still add empty --LOBXFL column and return
  if (nrow(dm_data) == 0L) {
    domain_data[[lobxfl_var]] <- ""
    return(domain_data)
  }
  
  # Ensure USUBJID is character in both datasets
  domain_data <- domain_data %>% mutate(USUBJID = as.character(USUBJID))
  dm_data     <- dm_data %>% mutate(USUBJID = as.character(USUBJID))
  
  # Join with DM and parse dates
  merged <- domain_data %>%
    left_join(dm_data %>% select(USUBJID, RFXSTDTC), by = "USUBJID") %>%
    mutate(
      DTC_PARSED = parse_date_time(!!sym(date_var), orders = c("ymd HMS", "ymd HM", "ymd H", "ymd")),
      RFXSTDTM   = parse_date_time(RFXSTDTC, orders = c("ymd HMS", "ymd HM", "ymd H", "ymd")),
      DTC_PARSED = if_else(is.na(DTC_PARSED),
                           parse_date_time(str_replace(!!sym(date_var), "T", " "), orders = c("ymd HMS", "ymd")),
                           DTC_PARSED),
      RFXSTDTM   = if_else(is.na(RFXSTDTM),
                           parse_date_time(str_replace(RFXSTDTC, "T", " "), orders = c("ymd HMS", "ymd")),
                           RFXSTDTM)
    )
  
  # Define validity condition
  base_valid <- expr(!is.na(!!sym(orres_var)))
  full_valid <- if (!is.null(add_val_rule)) expr(!!base_valid & !!add_val_rule) else base_valid
  # Apply flags
  merged <- merged %>%
    mutate(
      VALID   = !!full_valid,
      PRE_EXP = VALID & !is.na(DTC_PARSED) & !is.na(RFXSTDTM) & DTC_PARSED < RFXSTDTM
    ) %>%
    group_by(USUBJID, !!sym(testcd_var)) %>%
    mutate(!!lobxfl_var := if (any(PRE_EXP)) {
      if_else(DTC_PARSED == max(DTC_PARSED[PRE_EXP], na.rm = TRUE), "Y", "")
    } else {
      ""
    }) %>%
    ungroup() %>%
    select(-DTC_PARSED, -RFXSTDTM, -VALID, -PRE_EXP, -RFXSTDTC)
  
  return(merged)
}