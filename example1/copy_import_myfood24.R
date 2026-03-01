# ******************************************************************************
# Danone Nutricia Research
# ******************************************************************************
# STUDY/PROJECT : SONA / 23REX0061265
# PROGRAM PATH  : SAS/SONA/Files/SDTM Conversion/01 Macros
# PROGRAM NAME  : copy_import_myfood24.R
# PURPOSE       : Import myfood24 data, derive SUBJID/USUBJID, optional filter/sort.
# ------------------------------------------------------------------------------
# NOTES :
# - Adapted from SAS macro copy_import_myfood24.
# - USUBJID is derived from respondent_id with the same rules as SAS:
# * Case 1: starts with "SONA-NL-" -> normalize
# * Case 2: starts with "NL-" -> prepend SONA-
# * Case 3: starts with "SONA-" -> insert NL
# * Else: assume XXX-YYY-ZZZ, with YYY defaulting to "101" if missing.
# - Formats/informats are removed via haven::zap_formats() when noformats = TRUE.
# - By default this function does NOT add OAK ID variables (SAS macro didn’t).
# You can wrap the result in generate_oak_id_vars() if needed.
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2025-12-08 - cattanch - Initial program
# ******************************************************************************

library(dplyr)
library(haven)
library(stringr)
library(rlang)
library(tibble)

library(dplyr)
library(stringr)
library(rlang)
library(tibble)
library(haven)

# ----------------------------------------------------------------------
# Helper: derive USUBJID from RESPONDENT_ID (myfood24 version)
# ----------------------------------------------------------------------
derive_usubjid_myfood24 <- function(respondent_id, debug = FALSE) {
  id <- as.character(respondent_id)
  id_trim <- trimws(id)
  is_miss <- is.na(id_trim) | id_trim == ""
  
  # Pattern for already correct USUBJID (case-insensitive)
  pat_ok <- regex("^SONA-NL-[0-9]{3}-[0-9]{4}$", ignore_case = TRUE)
  
  res <- rep(NA_character_, length(id_trim))
  
  # Case 0: already correct -> just uppercase
  cond_ok <- !is_miss & str_detect(id_trim, pat_ok)
  res[cond_ok] <- toupper(id_trim[cond_ok])
  
  # For all others, apply SAS-like logic
  idx <- which(!cond_ok & !is_miss)
  if (length(idx) > 0) {
    id_sub <- id_trim[idx]
    
    # Split in parts for SITE logic
    parts <- str_split(id_sub, "-", simplify = TRUE)
    n_cols <- ncol(parts)
    
    get_part <- function(j) {
      if (n_cols >= j) parts[, j] else rep(NA_character_, length(idx))
    }
    part1 <- get_part(1)
    part2 <- get_part(2)
    part3 <- get_part(3)
    
    # Numeric subject: *last run of digits at the end of the string*
    last_digits <- str_extract(id_sub, "(\\d+)$")
    subj_num <- suppressWarnings(as.integer(last_digits))
    subj_fmt <- ifelse(is.na(subj_num), NA_character_,
                       sprintf("%04d", subj_num))
    
    # Default site (like coalescec(scan(...,2), '101'))
    site_default <- ifelse(is.na(part2) | part2 == "", "101", part2)
    
    # SAS-like cases on the original string
    cond1 <- str_detect(id_sub, regex("^SONA-NL-", ignore_case = TRUE))
    cond2 <- str_detect(id_sub, regex("^NL-", ignore_case = TRUE))
    cond3 <- str_detect(id_sub, regex("^SONA-", ignore_case = TRUE)) & !cond1
    
    sub_res <- rep(NA_character_, length(idx))
    
    # Case 1: SONA-NL-<SITE>-<SUBJ>
    sub_res[cond1] <- paste("SONA", "NL", part3[cond1], subj_fmt[cond1], sep = "-")
    
    # Case 2: NL-<SITE>-<SUBJ> -> prepend SONA-
    sub_res[cond2] <- paste("SONA", "NL", part2[cond2], subj_fmt[cond2], sep = "-")
    
    # Case 3: SONA-<SITE>-<SUBJ> (missing NL)
    sub_res[cond3] <- paste("SONA", "NL", part2[cond3], subj_fmt[cond3], sep = "-")
    
    # Default: assume XXX-YYY-ZZZ -> site = YYY (or 101), subj = last digits
    cond_else <- !(cond1 | cond2 | cond3)
    sub_res[cond_else] <- paste("SONA", "NL",
                                site_default[cond_else],
                                subj_fmt[cond_else],
                                sep = "-")
    
    res[idx] <- toupper(sub_res)
    
    # ---- Debug info: show problematic rows where subj_fmt is NA ----
    if (debug) {
      bad <- which(!is.na(id_sub) & is.na(subj_fmt))
      if (length(bad)) {
        message("derive_usubjid_myfood24: could not derive subject number for these RESPONDENT_ID values:")
        print(head(
          data.frame(
            ROW = idx[bad],
            RESPONDENT_ID = id_sub[bad],
            PART1 = part1[bad],
            PART2 = part2[bad],
            PART3 = part3[bad],
            LAST_DIGITS = last_digits[bad],
            stringsAsFactors = FALSE
          ),
          30
        ))
      }
    }
  }
  
  res
}

# ----------------------------------------------------------------------
# Main: import myfood24 SAS dataset and derive SUBJID / USUBJID
# ----------------------------------------------------------------------
copy_import_myfood24 <- function(inset = "myfood24",
                                 inlib = "../04 Import Data",
                                 by = "USUBJID",
                                 where = NULL,
                                 noformats = TRUE,
                                 debug_usubjid = TRUE) {
  
  src_path <- file.path(inlib, paste0(inset, ".sas7bdat"))
  
  if (!file.exists(src_path)) {
    message("copy_import_myfood24: source file not found: ", src_path,
            " — returning empty dataset.")
    return(tibble(SUBJID = character(), USUBJID = character()))
  }
  
  ds <- tryCatch(
    haven::read_sas(src_path),
    error = function(e) {
      message("copy_import_myfood24: failed to read ", src_path,
              " — ", conditionMessage(e), ". Returning empty dataset.")
      tibble(SUBJID = character(), USUBJID = character())
    }
  )
  
  # Keep SAS-style upper-case column names
  ds <- ds %>% rename_with(toupper)
  
  if (!"RESPONDENT_ID" %in% names(ds)) {
    message("copy_import_myfood24: 'RESPONDENT_ID' missing — cannot derive USUBJID.")
    ds$SUBJID <- NA_character_
    ds$USUBJID <- NA_character_
  } else {
    ds <- ds %>%
      mutate(
        RESPONDENT_ID = as.character(RESPONDENT_ID),
        SUBJID = RESPONDENT_ID,
        USUBJID = derive_usubjid_myfood24(RESPONDENT_ID,
                                          debug = debug_usubjid)
      )
  }
  
  # Optional WHERE filter
  if (!is.null(where) && nzchar(where)) {
    ds <- ds %>% filter(!!parse_expr(where))
  }
  
  # Optional sort
  if (!is.null(by) && length(by)) {
    if (length(by) == 1L) by <- trimws(strsplit(by, ",")[[1]])
    miss <- setdiff(by, names(ds))
    if (length(miss)) {
      message("copy_import_myfood24: skip sorting — missing columns: ",
              paste(miss, collapse = ", "))
    } else {
      ds <- ds %>% arrange(across(all_of(by)))
    }
  }
  
  # Drop SAS formats if requested
  if (isTRUE(noformats)) {
    ds <- haven::zap_formats(ds)
  }
  
  # Put SUBJID / USUBJID first for convenience
  ds <- ds %>% relocate(any_of(c("SUBJID", "USUBJID")), .before = everything())
  
  ds
}
