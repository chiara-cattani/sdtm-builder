# ==============================================================================
# Module E: Raw Data Access & Standardization
# ==============================================================================
# Functions: infer_source_meta, load_raw_datasets, standardize_names,
#            standardize_types, apply_missing_conventions, convert_blanks_to_na,
#            derive_core_keys, get_subject_level
# ==============================================================================

#' Infer source metadata from raw data
#'
#' Scans each dataset in `raw_data` and extracts column names, types, and
#' (where available) labels. This replaces the need for a manually curated
#' source metadata file in most workflows.
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
        csv      = suppressWarnings(readr::read_csv(f, show_col_types = FALSE)),
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

#' Extract source datasets needed from compiled rule set
#'
#' Scans a compiled rule_set object to identify which raw datasets are referenced
#' in the rule_params (derivation rules). This enables ultra-selective data loading
#' based on the actual variables being derived. Also examines target_meta derivation
#' fields to identify dataset dependencies from the raw derivation expressions.
#'
#' @param rule_set A rule_set object (result from compile_rules).
#' @param target_meta Tibble. The target metadata (for context).
#' @param sources_meta Tibble. The sources metadata (for validation).
#' @return Character vector of required raw dataset names (lowercased), extracted
#'   from the compiled rules.
#' @keywords internal
#' @export
get_source_datasets_from_rules <- function(rule_set, target_meta = NULL, sources_meta = NULL) {
  required <- character()
  
  # Iterate through all domains and variables in the rule_set
  for (dom in names(rule_set$rules)) {
    dom_rules <- rule_set$rules[[dom]]
    
    for (var_name in names(dom_rules)) {
      rule_info <- dom_rules[[var_name]]
      
      # Extract datasets from rule_params if present
      if (!is.null(rule_info$rule_params)) {
        params <- rule_info$rule_params
        
        # Check for dataset references in params
        for (param_name in names(params)) {
          param_val <- params[[param_name]]
          
          # If a parameter value looks like "dataset.column", extract the dataset name
          if (is.character(param_val) && length(param_val) == 1L && 
              grepl("^[a-zA-Z_][a-zA-Z0-9_]*(\\.[a-zA-Z_][a-zA-Z0-9_]*)?$", param_val)) {
            if (grepl("\\.", param_val)) {
              ds_name <- sub("\\..*", "", param_val)
              required <- c(required, tolower(ds_name))
            }
          }
        }
      }
      
      # Check the derivation field for dataset references
      # This catches cases like "ae_meddra.pt_name" in derivation expressions
      if (!is.null(target_meta)) {
        var_meta <- target_meta %>% 
          dplyr::filter(.data$domain == toupper(dom), .data$var == var_name) %>%
          dplyr::slice(1L)
        
        if (nrow(var_meta) > 0L) {
          if (!is.na(var_meta$derivation[1L])) {
            deriv <- var_meta$derivation[1L]
            # Extract dataset.column patterns: look for word.word sequences
            # Use regex to find potential dataset.column references
            matches <- gregexpr("[a-zA-Z_][a-zA-Z0-9_]*\\.[a-zA-Z_][a-zA-Z0-9_]*", deriv, perl = TRUE)
            if (matches[[1]][1] != -1) {
              found_refs <- regmatches(deriv, matches)[[1]]
              for (ref in found_refs) {
                ds_name <- sub("\\..*", "", ref)
                # Filter out common non-dataset prefixes
                if (!ds_name %in% c("data", "meta", "config", "raw", "rule", "param")) {
                  required <- c(required, tolower(ds_name))
                }
              }
            }
          }
        }
      }
    }
  }
  
  # Remove duplicates and return
  unique(required)
}

#' Determine required raw datasets for specified domains
#'
#' Given a set of target domains and sources metadata, returns the list of raw
#' dataset names that must be loaded to build those domains.
#'
#' @param domains Character vector. The domains to build (e.g., c("AE", "CM")).
#'   If `NULL`, returns all datasets referenced in sources_meta.
#' @param sources_meta Tibble or `NULL`. The Sources metadata sheet. Must contain
#'   at least a `domain` column (or `DOMAIN`) and a `source` column (or `source`).
#'   If `NULL`, a minimal heuristic is used: assumes dataset names match domain names.
#' @return Character vector of required raw dataset names (lowercased).
#' @keywords internal
#' @export
get_required_datasets <- function(domains = NULL, sources_meta = NULL) {
  required <- character()

  # If sources_meta is available, use it to find dependencies
  if (!is.null(sources_meta) && nrow(sources_meta) > 0L) {
    # Normalize column names (could be "DOMAIN" or "domain", "source" or "SOURCE")
    col_domain <- if ("DOMAIN" %in% names(sources_meta)) "DOMAIN" else "domain"
    col_source <- if ("SOURCE" %in% names(sources_meta)) "SOURCE" else "source"

    if (col_domain %in% names(sources_meta) && col_source %in% names(sources_meta)) {
      if (is.null(domains)) {
        # Get all unique datasets referenced
        required <- unique(tolower(sources_meta[[col_source]]))
      } else {
        # Get datasets for specific domains
        domains_upper <- toupper(domains)
        for (dom in domains_upper) {
          dom_rows <- sources_meta[[col_domain]] == dom | 
                      tolower(sources_meta[[col_domain]]) == tolower(dom)
          if (any(dom_rows)) {
            ds_in_dom <- tolower(sources_meta[[col_source]][dom_rows])
            required <- c(required, ds_in_dom)
          }
        }
        required <- unique(required)
      }
    }
  }

  # If no sources_meta or no results found, use heuristic:
  # assume dataset name = domain name (lowercased)
  if (length(required) == 0L && !is.null(domains)) {
    required <- tolower(domains)
  }

  # Remove NAs and empty strings
  required <- required[!is.na(required) & nchar(required) > 0L]
  unique(required)
}

#' Load only required raw datasets
#'
#' Loads raw datasets from a directory, but only those needed for the specified
#' domains (based on sources metadata). This is more efficient than loading all
#' datasets when building only a subset of domains.
#'
#' @param dir Character. Path to the raw data directory.
#' @param domains Character vector or `NULL`. The domains to build. If `NULL`,
#'   loads all datasets in the directory.
#' @param sources_meta Tibble or `NULL`. The Sources metadata. If provided, used
#'   to determine dataset dependencies.
#' @param pattern Character. Regex pattern for file selection.
#' @param recursive Logical. Search subdirectories?
#' @param verbose Logical. Print progress messages?
#' @return Named list of tibbles (only those needed).
#' @export
load_raw_datasets_selective <- function(dir, domains = NULL, sources_meta = NULL,
                                       pattern = "\\.(sas7bdat|xlsx|xls|csv|rds|rda|rdata|xpt)$",
                                       recursive = FALSE, verbose = TRUE) {
  checkmate::assert_directory_exists(dir)
  
  # Determine which datasets are needed
  required <- get_required_datasets(domains = domains, sources_meta = sources_meta)

  # List all available files
  files <- list.files(dir, pattern = pattern, full.names = TRUE,
                      recursive = recursive, ignore.case = TRUE)
  
  # Filter to only required datasets
  result <- list()
  for (f in files) {
    ds_name <- tolower(tools::file_path_sans_ext(basename(f)))
    
    # Skip if not in required list
    if (length(required) > 0L && !(ds_name %in% required)) {
      next
    }

    ext <- tolower(tools::file_ext(f))
    if (verbose) cli::cli_alert_info("Loading {ds_name} from {basename(f)}...")

    df <- tryCatch(
      switch(ext,
        sas7bdat = haven::read_sas(f),
        xpt      = haven::read_xpt(f),
        xlsx = , xls = readxl::read_excel(f),
        csv      = suppressWarnings(readr::read_csv(f, show_col_types = FALSE)),
        rds      = readRDS(f),
        rda = , rdata = {
          env <- new.env(parent = emptyenv())
          load(f, envir = env)
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

  # If nothing was loaded and domains were specified, fall back to loading everything
  if (length(result) == 0L && !is.null(domains)) {
    if (verbose) cli::cli_alert_warning("  No datasets found for domains {paste(domains, collapse=', ')}; loading all available")
    result <- load_raw_datasets(dir, pattern = pattern, recursive = recursive, verbose = verbose)
  } else if (length(result) == 0L) {
    abort("No datasets could be loaded successfully.")
  }

  result
}

#' Load raw datasets by explicit source names
#'
#' Loads raw datasets from a directory by specifying exact dataset names
#' (sources). This is useful when you know precisely which datasets are needed,
#' such as when loading only datasets required by compiled derivation rules.
#'
#' @param dir Character. Path to the raw data directory.
#' @param sources Character vector. Names of specific datasets to load
#'   (e.g., c("ae", "ae_meddra", "sae")). Names are lowercased for matching.
#' @param pattern Character. Regex pattern for file selection.
#' @param recursive Logical. Search subdirectories?
#' @param verbose Logical. Print progress messages?
#' @return Named list of tibbles (only those that exist and are loadable).
#' @export
load_raw_datasets_with_sources <- function(dir, sources = NULL,
                                          pattern = "\\.(sas7bdat|xlsx|xls|csv|rds|rda|rdata|xpt)$",
                                          recursive = FALSE, verbose = TRUE) {
  checkmate::assert_directory_exists(dir)
  checkmate::assert_character(sources, min.len = 1L)
  
  # Normalize source names (lowercase)
  sources_lower <- tolower(sources)
  
  # List all available files
  files <- list.files(dir, pattern = pattern, full.names = TRUE,
                      recursive = recursive, ignore.case = TRUE)
  
  # Load only the requested sources
  result <- list()
  for (f in files) {
    ds_name <- tolower(tools::file_path_sans_ext(basename(f)))
    
    # Skip if not in requested sources
    if (!(ds_name %in% sources_lower)) {
      next
    }

    ext <- tolower(tools::file_ext(f))
    if (verbose) cli::cli_alert_info("Loading {ds_name} from {basename(f)}...")

    df <- tryCatch(
      switch(ext,
        sas7bdat = haven::read_sas(f),
        xpt      = haven::read_xpt(f),
        xlsx = , xls = readxl::read_excel(f),
        csv      = suppressWarnings(readr::read_csv(f, show_col_types = FALSE)),
        rds      = readRDS(f),
        rda = , rdata = {
          env <- new.env(parent = emptyenv())
          load(f, envir = env)
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

  if (length(result) == 0L) {
    if (verbose) {
      missing_sources <- setdiff(sources_lower, names(result))
      cli::cli_alert_warning("  Could not load requested sources: {paste(missing_sources, collapse=', ')}")
    }
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


#' Convert blanks to NA in a data frame
#'
#' Turns empty strings (`""`) into proper R `NA` values. Preserves
#' all attributes (labels, etc.). This replicates the SAS concept of
#' "missing" for character variables.
#'
#' @param x Any R object (method dispatch).
#' @return Object of the same class with blanks converted to `NA`.
#'
#' @examples
#' convert_blanks_to_na(c("hello", "", "world"))
#' # [1] "hello" NA      "world"
#'
#' @export
convert_blanks_to_na <- function(x) {
  UseMethod("convert_blanks_to_na")
}

#' @export
#' @rdname convert_blanks_to_na
convert_blanks_to_na.default <- function(x) x

#' @export
#' @rdname convert_blanks_to_na
convert_blanks_to_na.character <- function(x) {
  x[!is.na(x) & x == ""] <- NA_character_
  x
}

#' @export
#' @rdname convert_blanks_to_na
convert_blanks_to_na.data.frame <- function(x) {
  x[] <- lapply(x, convert_blanks_to_na)
  x
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
