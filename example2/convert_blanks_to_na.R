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
# 2025-10-09 - cattanch - Initial program
# ******************************************************************************

#' @title Convert Blank Strings Into NAs
#'
#' @description
#' Turn SAS blank strings into proper R `NA`s.
#'
#' @param x Any R object
#'
#' @details
#' The default method simply returns its input unchanged. The `character` method
#' turns every instance of `""` into `NA_character_` while preserving *all* attributes.
#' When given a data frame as input the function keeps all non-character columns
#' as is and applies the just described logic to `character` columns. Once again
#' all attributes such as labels are preserved.
#'
#' @return An object of the same class as the input
#'
#' @family utils_fmt
#' @keywords utils_fmt
#' @export
convert_blanks_to_na <- function(x) {
  UseMethod("convert_blanks_to_na")
}

#' @export
#' @rdname convert_blanks_to_na
convert_blanks_to_na.default <- function(x) {
  x
}

#' @export
#' @rdname convert_blanks_to_na
convert_blanks_to_na.character <- function(x) {
  do.call(structure, c(list(if_else(x == "", NA_character_, x)), attributes(x))) # nolint
}

#' @export
#' @rdname convert_blanks_to_na
convert_blanks_to_na.list <- function(x) {
  lapply(x, convert_blanks_to_na)
}

#' @export
#' @rdname convert_blanks_to_na
convert_blanks_to_na.data.frame <- function(x) { # nolint
  x[] <- lapply(x, convert_blanks_to_na)
  x
}