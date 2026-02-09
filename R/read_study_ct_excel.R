# ==============================================================================
# Read Study Controlled Terminology from Multi-Sheet Excel Workbook
# ==============================================================================
# Reads Study_CT.xlsx with sheets: Codelists, Codelists_terms.
# Transforms into flat CT library structure for meta_bundle.
# ==============================================================================

#' Read study controlled terminology from a multi-sheet Excel workbook
#'
#' @description
#' Parses `Study_CT.xlsx` containing sheets:
#' \itemize{
#'   \item **Codelists** — codelist definitions (names, IDs, extensibility)
#'   \item **Codelists_terms** — individual terms per codelist (submission
#'     values, decodes, term codes)
#' }
#'
#' Each sheet uses a `Select` column (`"Y"`) to filter active rows.
#'
#' The result is a flat tibble where each row = one coded term, suitable for
#' use as the `ct_lib` component of [new_meta_bundle()].
#'
#' @param path Character. Path to the `.xlsx` file.
#' @return Tibble with columns:
#'   \describe{
#'     \item{codelist_id}{Codelist identifier (e.g., `"YN"`, `"SEV"`, `"ROUTE"`).}
#'     \item{codelist_name}{Human-readable codelist name.}
#'     \item{coded_value}{The CDISC submission value (term as it appears in SDTM).}
#'     \item{input_value}{The collected/source value to match against. If
#'       `DECODE` is populated, uses that; otherwise same as `coded_value`.}
#'     \item{decode}{The decode value (may be `NA`).}
#'     \item{case_sensitive}{Always `"N"` (case-insensitive matching).}
#'     \item{term_code}{CDISC term code (e.g., `"C100435"`), may be `NA`.}
#'     \item{is_extensible}{`"Yes"` or `"No"` — whether the codelist allows
#'       sponsor-defined additions.}
#'   }
#'
#' @export
read_study_ct_excel <- function(path) {
  checkmate::assert_string(path, min.chars = 1L)
  if (!file.exists(path)) abort(glue::glue("File not found: {path}"))
  ext <- tolower(tools::file_ext(path))
  if (!ext %in% c("xlsx", "xls")) abort("CT file must be an Excel file (.xlsx/.xls)")

  # Validate expected sheets exist
  available_sheets <- readxl::excel_sheets(path)
  required_sheets <- c("Codelists", "Codelists_terms")
  missing_sheets <- setdiff(required_sheets, available_sheets)
  if (length(missing_sheets) > 0L) {
    abort(glue::glue(
      "CT file is missing required sheets: {paste(missing_sheets, collapse = ', ')}\n",
      "Available sheets: {paste(available_sheets, collapse = ', ')}"
    ))
  }

  # --- Read Codelists sheet ---------------------------------------------------
  codelists <- .read_codelists_sheet(path)

  # --- Read Codelists_terms sheet ---------------------------------------------
  terms <- .read_codelists_terms_sheet(path)

  # --- Join codelist-level info to terms --------------------------------------
  # Bring is_extensible from Codelists to each term row
  cl_info <- dplyr::select(codelists,
    "codelist_id",
    cl_name = "codelist_name",
    "is_extensible"
  )

  ct_lib <- dplyr::left_join(terms, cl_info, by = "codelist_id",
                              relationship = "many-to-one")

  # Use codelist_name from terms if available, else from codelists join
  if ("codelist_name" %in% names(ct_lib) && "cl_name" %in% names(ct_lib)) {
    ct_lib$codelist_name <- dplyr::coalesce(ct_lib$codelist_name, ct_lib$cl_name)
    ct_lib$cl_name <- NULL
  } else if ("cl_name" %in% names(ct_lib)) {
    ct_lib$codelist_name <- ct_lib$cl_name
    ct_lib$cl_name <- NULL
  }

  # Ensure is_extensible comes through even if terms sheet didn't have it
  if (!"is_extensible" %in% names(ct_lib)) ct_lib$is_extensible <- NA_character_

  ct_lib
}


# ==============================================================================
# Internal Sheet Readers — Controlled Terminology
# ==============================================================================

#' Read and parse the Codelists sheet
#' @param path Path to Excel file.
#' @return Tibble with codelist definitions.
#' @noRd
.read_codelists_sheet <- function(path) {
  df <- readxl::read_excel(path, sheet = "Codelists")
  df <- tibble::as_tibble(df)
  names(df) <- tolower(names(df))

  # Filter selected rows
  if ("select" %in% names(df)) {
    df <- dplyr::filter(df, toupper(.data$select) == "Y")
    df$select <- NULL
  }
  if (nrow(df) == 0L) abort("Codelists sheet has no rows with Select = 'Y'.")

  # Validate required columns
  required <- c("codelist_id")
  miss <- setdiff(required, names(df))
  if (length(miss)) abort(paste("Codelists sheet missing columns:", paste(miss, collapse = ", ")))

  # Ensure optional columns exist
  if (!"codelist_name" %in% names(df)) df$codelist_name <- NA_character_
  if (!"codelist_code" %in% names(df)) df$codelist_code <- NA_character_
  if (!"is_extensible" %in% names(df)) df$is_extensible <- NA_character_

  # Trim whitespace
  chr_cols <- names(df)[vapply(df, is.character, logical(1))]
  for (col in chr_cols) df[[col]] <- trimws(df[[col]])

  df
}


#' Read and parse the Codelists_terms sheet
#' @param path Path to Excel file.
#' @return Tibble with flat CT terms: codelist_id, coded_value, input_value, decode, etc.
#' @noRd
.read_codelists_terms_sheet <- function(path) {
  df <- readxl::read_excel(path, sheet = "Codelists_terms")
  df <- tibble::as_tibble(df)
  names(df) <- tolower(names(df))

  # Filter selected rows
  if ("select" %in% names(df)) {
    df <- dplyr::filter(df, toupper(.data$select) == "Y")
    df$select <- NULL
  }
  if (nrow(df) == 0L) abort("Codelists_terms sheet has no rows with Select = 'Y'.")

  # Validate required columns
  required <- c("codelist_id", "submission_value")
  miss <- setdiff(required, names(df))
  if (length(miss)) abort(paste("Codelists_terms sheet missing columns:", paste(miss, collapse = ", ")))

  # Ensure optional columns exist
  if (!"codelist_name" %in% names(df)) df$codelist_name <- NA_character_
  if (!"term_code" %in% names(df)) df$term_code <- NA_character_
  if (!"decode" %in% names(df)) df$decode <- NA_character_

  # Trim whitespace
  chr_cols <- names(df)[vapply(df, is.character, logical(1))]
  for (col in chr_cols) df[[col]] <- trimws(df[[col]])

  # Transform to internal CT library format:
  #   coded_value  = SUBMISSION_VALUE (the SDTM standard term)
  #   input_value  = DECODE if present, else SUBMISSION_VALUE
  #   decode       = DECODE as-is
  #   case_sensitive = "N" (default)
  df$coded_value    <- df$submission_value
  df$input_value    <- dplyr::coalesce(df$decode, df$submission_value)
  df$case_sensitive <- "N"

  # Select and order output columns
  out_cols <- c("codelist_id", "codelist_name", "coded_value", "input_value",
                "decode", "case_sensitive", "term_code")
  out_cols <- intersect(out_cols, names(df))
  df[, out_cols, drop = FALSE]
}
