# ==============================================================================
# Module E: Raw Data Access & Standardization
# ==============================================================================

#' Infer source metadata from raw data
#'
#' Scans each dataset in `raw_data` and extracts column names, types, and
#' (where available) labels. This replaces the need for a manually curated
#' `source_meta.csv` in most workflows.
#'
#' @param raw_data Named list of data frames / tibbles.
#' @return A tibble with columns `dataset`, `column`, `type`, `label`.
#' @export
infer_source_meta <- function(raw_data) {
  checkmate::assert_list(raw_data, min.len = 1L, names = "named")
  rows <- vector("list", length = 0L)
  for (ds in names(raw_data)) {
    df <- raw_data[[ds]]
    for (col in names(df)) {
      # Determine R type
      cls <- class(df[[col]])[1L]
      rtype <- dplyr::case_when(
        cls %in% c("numeric", "double", "integer") ~ "numeric",
        cls %in% c("character", "factor")           ~ "character",
        cls %in% c("Date", "POSIXct", "POSIXlt")   ~ "character",
        cls == "logical"                             ~ "character",
        cls == "haven_labelled"                      ~ "character",
        TRUE                                         ~ cls
      )
      # Try to extract label (SAS/haven datasets have them)
      lbl <- attr(df[[col]], "label")
      if (is.null(lbl)) lbl <- NA_character_
      rows[[length(rows) + 1L]] <- tibble::tibble(
        dataset = ds,
        column  = col,
        type    = rtype,
        label   = as.character(lbl)
      )
    }
  }
  dplyr::bind_rows(rows)
}

#' Load raw datasets from a directory
#'
#' Scans a directory for data files (`.sas7bdat`, `.xlsx`, `.xls`, `.csv`,
#' `.rds`, `.rda`, `.xpt`) and loads each into a named tibble. The dataset
#' name is derived from the file name (without extension), lowercased.
#'
#' @param dir Character. Path to a directory containing raw data files.
#' @param pattern Character. Regex pattern for file selection. Default matches
#'   all supported extensions.
#' @param recursive Logical. Search sub-directories? Default `FALSE`.
#' @param verbose Logical. Print progress. Default `TRUE`.
#' @return Named list of tibbles.
#'
#' @details
#' Supported formats:
#' - **SAS**: `.sas7bdat` (via [haven::read_sas()])
#' - **XPT**: `.xpt` (via [haven::read_xpt()])
#' - **Excel**: `.xlsx`, `.xls` (via [readxl::read_excel()])
#' - **CSV**: `.csv` (via [readr::read_csv()])
#' - **RDS**: `.rds` (via [readRDS()])
#' - **RDA**: `.rda`, `.rdata` (via [load()])
#'
#' @export
load_raw_datasets <- function(dir, pattern = "\\.(sas7bdat|xlsx|xls|csv|rds|rda|rdata|xpt)$",
                              recursive = FALSE, verbose = TRUE) {
  checkmate::assert_directory_exists(dir)
  files <- list.files(dir, pattern = pattern, full.names = TRUE,
                      recursive = recursive, ignore.case = TRUE)
  if (length(files) == 0L) {
    abort(glue::glue("No data files found in '{dir}' matching pattern '{pattern}'"))
  }

  result <- list()
  for (f in files) {
    ds_name <- tolower(tools::file_path_sans_ext(basename(f)))
    ext <- tolower(tools::file_ext(f))
    if (verbose) cli::cli_alert_info("Loading {ds_name} from {basename(f)}...")

    df <- tryCatch(
      switch(ext,
        sas7bdat = haven::read_sas(f),
        xpt      = haven::read_xpt(f),
        xlsx = , xls = readxl::read_excel(f),
        csv      = readr::read_csv(f, show_col_types = FALSE),
        rds      = readRDS(f),
        rda = , rdata = {
          env <- new.env(parent = emptyenv())
          load(f, envir = env)
          # Return the first object found
          obj_names <- ls(env)
          if (length(obj_names) == 0L) abort(glue::glue("No objects in {basename(f)}"))
          get(obj_names[1L], envir = env)
        },
        abort(glue::glue("Unsupported format: .{ext}"))
      ),
      error = function(e) {
        cli::cli_alert_danger("Failed to load {basename(f)}: {e$message}")
        return(NULL)
      }
    )

    if (!is.null(df)) {
      result[[ds_name]] <- tibble::as_tibble(df)
      if (verbose) {
        cli::cli_alert_success("  {ds_name}: {nrow(df)} rows x {ncol(df)} cols")
      }
    }
  }

  if (length(result) == 0L) abort("No datasets could be loaded successfully.")
  result
}

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
      csv      = readr::read_csv(p, show_col_types = FALSE),
      xpt      = haven::read_xpt(p),
      rds      = readRDS(p),
      sas7bdat = haven::read_sas(p),
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
#' @param to_lower Logical. Default `TRUE`.
#' @param replace_special Logical. Default `TRUE`.
#' @return Tibble with standardized names.
#' @export
standardize_names <- function(data, dataset_name = NA_character_,
                              to_lower = TRUE,
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

#' Coerce column types
#'
#' Converts factors to character and strips haven labels.
#'
#' @param data Tibble.
#' @param dataset_name Character.
#' @param coerce_factors Logical. Default `TRUE`.
#' @param strip_labels Logical. Default `TRUE`.
#' @return Tibble with corrected types.
#' @export
standardize_types <- function(data, dataset_name = NA_character_,
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
