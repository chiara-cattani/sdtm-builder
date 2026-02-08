# ==============================================================================
# Module E: Raw Data Access & Standardization
# ==============================================================================

#' Load raw datasets from files
#' @param paths Named character vector.
#' @param formats Named character vector or `NULL`.
#' @param encoding Character. Default `"UTF-8"`.
#' @param db_con DBI connection or `NULL`.
#' @param db_tables Named character vector.
#' @return Named list of tibbles.
#' @export
load_raw_data <- function(paths, formats = NULL, encoding = "UTF-8",
                          db_con = NULL, db_tables = NULL) {
  result <- list()
  for (nm in names(paths)) {
    p <- paths[[nm]]
    if (!file.exists(p)) abort(glue::glue("File not found for dataset '{nm}': {p}"))
    ext <- if (!is.null(formats) && nm %in% names(formats)) formats[[nm]]
           else tolower(tools::file_ext(p))
    df <- switch(ext,
      csv  = readr::read_csv(p, show_col_types = FALSE),
      xpt  = haven::read_xpt(p),
      rds  = readRDS(p),
      xlsx = , xls = readxl::read_excel(p),
      abort(glue::glue("Unsupported format '{ext}' for dataset '{nm}'"))
    )
    result[[nm]] <- tibble::as_tibble(df)
  }
  result
}

#' Standardize column names
#' @param data Tibble.
#' @param dataset_name Character.
#' @param source_meta Tibble or `NULL`.
#' @param to_lower Logical. Default `TRUE`.
#' @param replace_special Logical. Default `TRUE`.
#' @return Tibble with standardized names.
#' @export
standardize_names <- function(data, dataset_name = NA_character_,
                              source_meta = NULL, to_lower = TRUE,
                              replace_special = TRUE) {
  nms <- names(data)
  if (to_lower) nms <- tolower(nms)
  if (replace_special) nms <- gsub("[^a-zA-Z0-9_]", "_", nms)
  if (anyDuplicated(nms) > 0L) {
    abort(glue::glue("Duplicate column names after standardization in '{dataset_name}'"))
  }
  names(data) <- nms
  data
}

#' Coerce column types to match metadata expectations
#' @param data Tibble.
#' @param source_meta Tibble.
#' @param dataset_name Character.
#' @param coerce_factors Logical. Default `TRUE`.
#' @param strip_labels Logical. Default `TRUE`.
#' @return Tibble with corrected types.
#' @export
standardize_types <- function(data, source_meta, dataset_name = NA_character_,
                              coerce_factors = TRUE, strip_labels = TRUE) {
  if (coerce_factors) {
    for (col in names(data)) {
      if (is.factor(data[[col]])) data[[col]] <- as.character(data[[col]])
    }
  }
  if (strip_labels) {
    for (col in names(data)) {
      if (inherits(data[[col]], "haven_labelled")) {
        data[[col]] <- as.vector(data[[col]])
      }
    }
  }
  data
}

#' Apply missing value conventions
#' @param data Tibble.
#' @param config `sdtm_config` or `NULL`.
#' @param blank_to_na Logical. Default `TRUE`.
#' @param unk_tokens Character vector.
#' @param unk_to_na Logical. Default `FALSE`.
#' @return Tibble with standardized missings.
#' @export
apply_missing_conventions <- function(data, config = NULL, blank_to_na = TRUE,
                                      unk_tokens = c("UNK","UNKNOWN","NK","N/A","."),
                                      unk_to_na = FALSE) {
  if (blank_to_na) {
    chr_cols <- names(data)[vapply(data, is.character, logical(1))]
    for (col in chr_cols) {
      data[[col]] <- dplyr::if_else(data[[col]] == "" | is.na(data[[col]]),
                                     NA_character_, data[[col]])
    }
  }
  if (unk_to_na) {
    chr_cols <- names(data)[vapply(data, is.character, logical(1))]
    for (col in chr_cols) {
      data[[col]] <- dplyr::if_else(toupper(data[[col]]) %in% toupper(unk_tokens),
                                     NA_character_, data[[col]])
    }
  }
  data
}

#' Derive core identification keys (STUDYID, USUBJID)
#' @param data Tibble.
#' @param config `sdtm_config`.
#' @param subjid_col Character. Default `"subjid"`.
#' @param sep Character. Default `"-"`.
#' @return Tibble with STUDYID and USUBJID columns.
#' @export
derive_core_keys <- function(data, config, subjid_col = "subjid", sep = "-") {
  data$STUDYID <- config$studyid
  if (!"USUBJID" %in% names(data) && "usubjid" %in% names(data)) {
    data$USUBJID <- data$usubjid
  } else if (!subjid_col %in% names(data) && !"USUBJID" %in% names(data)) {
    abort(glue::glue("Cannot derive USUBJID: column '{subjid_col}' not found"))
  } else if (!"USUBJID" %in% names(data)) {
    data$USUBJID <- paste(config$studyid, data[[subjid_col]], sep = sep)
  }
  data
}

#' Get subject-level data
#' @param raw_data Named list of tibbles.
#' @param config `sdtm_config`.
#' @param dm_dataset Character. Default `"dm"`.
#' @param key_vars Character vector.
#' @return Tibble: one row per subject.
#' @export
get_subject_level <- function(raw_data, config, dm_dataset = "dm",
                              key_vars = c("USUBJID","RFSTDTC","RFENDTC",
                                           "ARM","ACTARM")) {
  # Try dm_raw first, then dm
  dm_name <- NULL
  for (cand in c("dm_raw", "dm", dm_dataset)) {
    if (cand %in% names(raw_data)) { dm_name <- cand; break }
  }
  if (is.null(dm_name)) abort("DM dataset not found in raw_data")
  dm <- raw_data[[dm_name]]
  # Standardize key column names
  nms <- tolower(names(dm))
  names(dm) <- nms
  # Return relevant columns
  avail <- intersect(tolower(key_vars), nms)
  dm[, unique(c("usubjid", avail)), drop = FALSE]
}
