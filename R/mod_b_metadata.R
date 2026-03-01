# ==============================================================================
# Module B: Metadata Ingestion, Validation & Normalization
# ==============================================================================

# --- Section 1: Read Study_Metadata.xlsx ------------------------------------


#' Read study metadata from a multi-sheet Excel workbook
#'
#' @description
#' Parses `Study_Metadata.xlsx` containing sheets:
#' \itemize{
#'   \item **Meta** — study-level information (study name, select flag)
#'   \item **Standards** — (ignored)
#'   \item **Domains** — domain definitions with build order, keys, labels
#'   \item **Variables** — per-variable definitions with ordering, types, roles
#'   \item **Value Level** — value-level metadata with WHERE clause references
#'   \item **Where Clauses** — condition definitions for value-level metadata
#'   \item **Method** — derivation method descriptions (informational).
#'         The Variables sheet uses a **DERIVATION** column for derivation
#'         rules (the legacy METHOD column is kept empty for reference).
#' }
#'
#' Each sheet uses a `Select` column (`"Y"`) to filter active rows.
#'
#' @param path Character. Path to the `.xlsx` file.
#' @return Named list with components:
#'   \describe{
#'     \item{study_name}{Character. Study name from the Meta sheet.}
#'     \item{target_meta}{Tibble. Variable-level metadata in standard format
#'       (columns: domain, var, type, label, role, core, codelist_id, order,
#'       length, significant_digits, value_level_id, rule_type, var_model, ...).}
#'     \item{domain_meta}{Tibble. Domain-level metadata with build order
#'       (columns: domain, class, class_order, domain_level_order, keys,
#'       description, structure).}
#'     \item{value_level_meta}{Tibble or NULL. Value-level metadata with
#'       WHERE clause conditions expanded.}
#'   }
#'
#' @details
#' ## Column Mappings (Variables sheet)
#' | Excel Column         | Internal Column      |
#' |----------------------|----------------------|
#' | VARNAME              | var                  |
#' | VARLABEL             | label                |
#' | DATA_TYPE            | type (mapped)        |
#' | LENGTH               | length               |
#' | SIGNIFICANT_DIGITS   | significant_digits   |
#' | CODELIST_ID          | codelist_id          |
#' | ROLE                 | role                 |
#' | CORE                 | core                 |
#' | VLM_ID               | value_level_id       |
#' | DERIVATION           | method / rule_type   |
#' | VAR_MODEL            | var_model            |
#'
#' ## Data Type Mappings
#' | Excel DATA_TYPE      | Internal type |
#' |----------------------|---------------|
#' | text                 | char          |
#' | integer              | num           |
#' | float                | num           |
#' | datetime             | char          |
#' | durationDatetime     | char          |
#'
#' @export
read_study_metadata_excel <- function(path) {
  checkmate::assert_string(path, min.chars = 1L)
  if (!file.exists(path)) abort(glue::glue("File not found: {path}"))
  ext <- tolower(tools::file_ext(path))
  if (!ext %in% c("xlsx", "xls")) abort("Study metadata must be an Excel file (.xlsx/.xls)")

  # Validate expected sheets exist
  available_sheets <- readxl::excel_sheets(path)
  required_sheets <- c("Meta", "Domains", "Variables", "Value Level", "Where Clauses")
  missing_sheets <- setdiff(required_sheets, available_sheets)
  if (length(missing_sheets) > 0L) {
    abort(glue::glue(
      "Study metadata file is missing required sheets: {paste(missing_sheets, collapse = ', ')}\n",
      "Available sheets: {paste(available_sheets, collapse = ', ')}"
    ))
  }

  # --- Read Meta sheet --------------------------------------------------------
  study_name <- .read_meta_sheet(path)

  # --- Read Domains sheet -----------------------------------------------------
  domain_meta <- .read_domains_sheet(path)

  # --- Read Variables sheet ---------------------------------------------------
  target_meta <- .read_variables_sheet(path)

  # --- Read Where Clauses sheet -----------------------------------------------
  where_clauses <- .read_where_clauses_sheet(path)

  # --- Read Value Level sheet -------------------------------------------------
  value_level_meta <- .read_value_level_sheet(path, where_clauses)

  # --- Read Sources & Source Columns sheets (optional) ------------------------
  sources_meta      <- NULL
  source_cols_meta  <- NULL
  if ("Sources" %in% available_sheets) {
    sources_meta <- .read_sources_sheet(path)
  }
  if ("Source Columns" %in% available_sheets) {
    source_cols_meta <- .read_source_columns_sheet(path)
  }

  list(
    study_name       = study_name,
    target_meta      = target_meta,
    domain_meta      = domain_meta,
    value_level_meta = value_level_meta,
    sources_meta     = sources_meta,
    source_cols_meta = source_cols_meta
  )
}


# ==============================================================================
# Internal Sheet Readers
# ==============================================================================

#' Read and parse the Meta sheet
#' @param path Path to Excel file.
#' @return Character scalar: the study name.
#' @noRd
.read_meta_sheet <- function(path) {
  df <- readxl::read_excel(path, sheet = "Meta")
  df <- tibble::as_tibble(df)
  names(df) <- tolower(names(df))

  if (!"select" %in% names(df)) abort("Meta sheet missing 'Select' column.")
  if (!"study_name" %in% names(df)) abort("Meta sheet missing 'STUDY_NAME' column.")

  selected <- dplyr::filter(df, toupper(.data$select) == "Y")
  if (nrow(selected) == 0L) abort("Meta sheet has no rows with Select = 'Y'.")

  study_name <- selected$study_name[1L]
  if (is.na(study_name) || nchar(trimws(study_name)) == 0L) {
    abort("STUDY_NAME is empty in the selected Meta row.")
  }
  trimws(study_name)
}


#' Read and parse the Domains sheet
#' @param path Path to Excel file.
#' @return Tibble with domain-level metadata, sorted by build order.
#' @noRd
.read_domains_sheet <- function(path) {
  df <- readxl::read_excel(path, sheet = "Domains")
  df <- tibble::as_tibble(df)
  names(df) <- tolower(names(df))
  # Remove duplicate columns (keep first occurrence)
  df <- df[, !duplicated(names(df)), drop = FALSE]

  # Filter selected domains
  if ("select" %in% names(df)) {
    df <- dplyr::filter(df, toupper(.data$select) == "Y")
    df$select <- NULL
  }
  if (nrow(df) == 0L) abort("Domains sheet has no rows with Select = 'Y'.")

  # Validate required columns
  required <- c("domain", "class", "class_order", "domain_level_order")
  miss <- setdiff(required, names(df))
  if (length(miss)) abort(paste("Domains sheet missing columns:", paste(miss, collapse = ", ")))

  # Sort by build order
  df <- dplyr::arrange(df, .data$class_order, .data$domain_level_order)

  # Ensure standard columns
  if (!"keys" %in% names(df)) df$keys <- NA_character_
  if (!"description" %in% names(df)) df$description <- NA_character_
  if (!"structure" %in% names(df)) df$structure <- NA_character_

  # Uppercase domain names

  df$domain <- toupper(df$domain)
  df$class  <- toupper(df$class)

  # Trim whitespace in character columns
  chr_cols <- names(df)[vapply(df, is.character, logical(1))]
  for (col in chr_cols) df[[col]] <- trimws(df[[col]])

  df
}


#' Read and parse the Variables sheet
#' @param path Path to Excel file.
#' @return Tibble with variable-level metadata in standard format.
#' @noRd
.read_variables_sheet <- function(path) {
  df <- readxl::read_excel(path, sheet = "Variables")
  df <- tibble::as_tibble(df)
  names(df) <- tolower(names(df))

  # Filter selected variables
  if ("select" %in% names(df)) {
    df <- dplyr::filter(df, toupper(.data$select) == "Y")
    df$select <- NULL
  }
  if (nrow(df) == 0L) abort("Variables sheet has no rows with Select = 'Y'.")

  # Validate required columns
  required <- c("domain", "varname", "varlabel", "data_type")
  miss <- setdiff(required, names(df))
  if (length(miss)) abort(paste("Variables sheet missing columns:", paste(miss, collapse = ", ")))

  # Rename columns to internal convention
  df <- dplyr::rename(df,
    var   = "varname",
    label = "varlabel"
  )

  # Map DATA_TYPE → internal type
  df$type <- dplyr::case_when(
    tolower(df$data_type) == "text"             ~ "char",
    tolower(df$data_type) == "integer"          ~ "num",
    tolower(df$data_type) == "float"            ~ "num",
    tolower(df$data_type) == "datetime"         ~ "char",
    tolower(df$data_type) == "durationdatetime" ~ "char",
    TRUE ~ tolower(df$data_type)
  )

  # Map DERIVATION → method (internal) → rule_type
  # Excel now uses DERIVATION for derivation rules; METHOD is kept empty.
  # For backward compatibility, fall back to METHOD if DERIVATION is absent.
  if ("derivation" %in% names(df)) {
    df$method    <- df$derivation
    df$rule_type <- map_method_to_rule_type(df$method)
  } else if ("method" %in% names(df)) {
    df$rule_type <- map_method_to_rule_type(df$method)
  } else {
    df$method    <- NA_character_
    df$rule_type <- NA_character_
  }

  # Rename VLM_ID → value_level_id
  if ("vlm_id" %in% names(df)) {
    df <- dplyr::rename(df, value_level_id = "vlm_id")
  } else {
    df$value_level_id <- NA_character_
  }

  # Ensure standard columns exist
  standard_cols <- c("codelist_id", "role", "core", "length",
                     "significant_digits", "var_model", "seqorder",
                     "class_order", "domain_level_order",
                     "role_order", "var_model_order")
  for (col in standard_cols) {
    if (!col %in% names(df)) df[[col]] <- NA
  }

  # Sort variables: class_order → domain_level_order → role_order →
  #                 var_model_order → seqorder
  sort_cols <- c("class_order", "domain_level_order", "role_order",
                 "var_model_order", "seqorder")
  existing_sort <- intersect(sort_cols, names(df))
  if (length(existing_sort) > 0L) {
    df <- dplyr::arrange(df, dplyr::across(dplyr::all_of(existing_sort)))
  }

  # Assign final ordering number per domain
  df <- df %>%
    dplyr::group_by(.data$domain) %>%
    dplyr::mutate(order = dplyr::row_number()) %>%
    dplyr::ungroup()

  # Uppercase domain
  df$domain <- toupper(df$domain)

  # Rename SUPPFL -> to_supp if present (Excel uses SUPPFL, internal uses to_supp)
  if ("suppfl" %in% names(df) && !"to_supp" %in% names(df)) {
    df <- dplyr::rename(df, to_supp = "suppfl")
  }

  # Add missing standard target_meta columns
  if (!"is_key" %in% names(df)) df$is_key <- NA
  if (!"to_supp" %in% names(df)) df$to_supp <- NA
  if (!"rule_params" %in% names(df)) df$rule_params <- NA_character_
  if (!"depends_on" %in% names(df)) df$depends_on <- NA_character_

  # Trim whitespace in character columns
  chr_cols <- names(df)[vapply(df, is.character, logical(1))]
  for (col in chr_cols) df[[col]] <- trimws(df[[col]])

  # Validate no duplicate (domain, var) pairs
  dupes <- df %>% dplyr::count(.data$domain, .data$var) %>% dplyr::filter(.data$n > 1L)
  if (nrow(dupes) > 0L) {
    abort(paste("Duplicate (domain, var) in Variables sheet:",
                paste0(dupes$domain, ".", dupes$var, collapse = ", ")))
  }

  df
}


#' Read and parse the Where Clauses sheet
#' @param path Path to Excel file.
#' @return Tibble with parsed WHERE clause definitions.
#' @noRd
.read_where_clauses_sheet <- function(path) {
  df <- readxl::read_excel(path, sheet = "Where Clauses")
  df <- tibble::as_tibble(df)
  names(df) <- tolower(names(df))

  # Filter selected rows
  if ("select" %in% names(df)) {
    df <- dplyr::filter(df, toupper(.data$select) == "Y")
    df$select <- NULL
  }
  if (nrow(df) == 0L) {
    return(tibble::tibble(
      where_clause_id = character(),
      domain          = character(),
      varname         = character(),
      comparator      = character(),
      value           = character(),
      values_list     = list()
    ))
  }

  # Validate required columns
  required <- c("where_clause_id", "domain", "varname", "comparator", "value")
  miss <- setdiff(required, names(df))
  if (length(miss)) abort(paste("Where Clauses sheet missing columns:", paste(miss, collapse = ", ")))

  # Trim whitespace
  chr_cols <- names(df)[vapply(df, is.character, logical(1))]
  for (col in chr_cols) df[[col]] <- trimws(df[[col]])

  # Parse pipe-separated values into list column
  df$values_list <- lapply(df$value, function(v) {
    if (is.na(v) || nchar(v) == 0L) return(character())
    strsplit(v, "\\|")[[1L]]
  })

  # Uppercase domain
  df$domain <- toupper(df$domain)

  df
}


#' Read and parse the Value Level sheet
#'
#' Joins value-level rows with WHERE clause conditions to create expanded
#' metadata. Each VLM_ID + WHERE_CLAUSE_ID combination becomes a separate
#' condition branch.
#'
#' @param path Path to Excel file.
#' @param where_clauses Tibble from `.read_where_clauses_sheet()`.
#' @return Tibble with value-level metadata and expanded conditions, or NULL.
#' @noRd
.read_value_level_sheet <- function(path, where_clauses) {
  df <- readxl::read_excel(path, sheet = "Value Level")
  df <- tibble::as_tibble(df)
  names(df) <- tolower(names(df))

  # Filter selected rows
  if ("select" %in% names(df)) {
    df <- dplyr::filter(df, toupper(.data$select) == "Y")
    df$select <- NULL
  }
  if (nrow(df) == 0L) return(NULL)

  # Validate required columns
  required <- c("domain", "varname", "vlm_id", "where_clause_id")
  miss <- setdiff(required, names(df))
  if (length(miss)) abort(paste("Value Level sheet missing columns:", paste(miss, collapse = ", ")))

  # Trim whitespace
  chr_cols <- names(df)[vapply(df, is.character, logical(1))]
  for (col in chr_cols) df[[col]] <- trimws(df[[col]])

  # Uppercase domain
  df$domain <- toupper(df$domain)

  # Rename vlm_id → value_level_id for consistency with target_meta
  df <- dplyr::rename(df, value_level_id = "vlm_id")

  # Ensure optional columns exist
  if (!"data_type" %in% names(df)) df$data_type <- NA_character_
  if (!"length" %in% names(df)) df$length <- NA
  if (!"significant_digits" %in% names(df)) df$significant_digits <- NA
  if (!"codelist_id" %in% names(df)) df$codelist_id <- NA_character_

  # Map data_type to internal type conventions (same as variables sheet)
  df$vlm_type <- dplyr::case_when(
    tolower(df$data_type) == "text"             ~ "char",
    tolower(df$data_type) == "integer"          ~ "num",
    tolower(df$data_type) == "float"            ~ "num",
    tolower(df$data_type) == "datetime"         ~ "char",
    tolower(df$data_type) == "durationdatetime" ~ "char",
    is.na(df$data_type)                         ~ NA_character_,
    TRUE ~ tolower(df$data_type)
  )

  # Join with WHERE clauses to build condition expressions
  if (nrow(where_clauses) > 0L) {
    # Select relevant WHERE clause columns for join
    wc_slim <- dplyr::select(where_clauses,
      wc_id       = "where_clause_id",
      wc_domain   = "domain",
      wc_varname  = "varname",
      wc_comparator = "comparator",
      wc_value    = "value",
      wc_values   = "values_list"
    )

    df <- dplyr::left_join(df, wc_slim,
      by = c("where_clause_id" = "wc_id"),
      relationship = "many-to-many"
    )

    # Build condition expression: "{VARNAME} {EQ|IN} {VALUE}"
    df$condition <- dplyr::case_when(
      toupper(df$wc_comparator) == "EQ" ~
        paste0(df$wc_varname, " == \"", df$wc_value, "\""),
      toupper(df$wc_comparator) == "IN" ~
        paste0(df$wc_varname, " %in% c(",
               vapply(df$wc_values, function(vals) {
                 paste0("\"", vals, "\"", collapse = ", ")
               }, character(1L)),
               ")"),
      toupper(df$wc_comparator) == "NE" ~
        paste0(df$wc_varname, " != \"", df$wc_value, "\""),
      toupper(df$wc_comparator) == "NOTIN" ~
        paste0("!", df$wc_varname, " %in% c(",
               vapply(df$wc_values, function(vals) {
                 paste0("\"", vals, "\"", collapse = ", ")
               }, character(1L)),
               ")"),
      TRUE ~ NA_character_
    )
  } else {
    df$wc_domain    <- NA_character_
    df$wc_varname   <- NA_character_
    df$wc_comparator <- NA_character_
    df$wc_value     <- NA_character_
    df$wc_values    <- list(character())
    df$condition    <- NA_character_
  }

  df
}


#' Read and parse the Sources sheet (optional)
#'
#' Defines domain-level preprocessing: which raw datasets feed each domain,
#' how they combine, filtering, visit/timepoint mapping.
#'
#' @param path Path to Excel file.
#' @return Tibble with source block definitions, or NULL.
#' @noRd
.read_sources_sheet <- function(path) {
  df <- readxl::read_excel(path, sheet = "Sources")
  df <- tibble::as_tibble(df)
  names(df) <- tolower(names(df))

  # Filter selected rows
  if ("select" %in% names(df)) {
    df <- dplyr::filter(df, toupper(.data$select) == "Y")
    df$select <- NULL
  }
  if (nrow(df) == 0L) return(NULL)

  # Validate required columns
  required <- c("domain", "source", "block_id", "step_order", "merge_type")
  miss <- setdiff(required, names(df))
  if (length(miss)) abort(paste("Sources sheet missing columns:", paste(miss, collapse = ", ")))

  # Ensure optional columns
  for (col in c("join_by", "filter", "map_visit", "map_timepoint",
                "tpt_normalize", "sourceid", "pivot_pattern")) {
    if (!col %in% names(df)) df[[col]] <- NA_character_
  }

  df$domain <- toupper(df$domain)
  df$step_order <- as.numeric(df$step_order)
  df
}


#' Read and parse the Source Columns sheet (optional)
#'
#' Defines column-level derivations within each source block.
#'
#' @param path Path to Excel file.
#' @return Tibble with column derivation definitions, or NULL.
#' @noRd
.read_source_columns_sheet <- function(path) {
  df <- readxl::read_excel(path, sheet = "Source Columns")
  df <- tibble::as_tibble(df)
  names(df) <- tolower(names(df))

  if (nrow(df) == 0L) return(NULL)

  # Validate required columns
  required <- c("block_id", "target_column", "method")
  miss <- setdiff(required, names(df))
  if (length(miss)) abort(paste("Source Columns sheet missing columns:", paste(miss, collapse = ", ")))

  # Ensure col_order
  if (!"col_order" %in% names(df)) df$col_order <- seq_len(nrow(df))
  df$col_order <- as.numeric(df$col_order)
  df
}


# --- Section 2: Read Study_CT.xlsx ------------------------------------------


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
#' @return Tibble with flat CT terms: codelist_id, coded_value, input_value, decode, etc.,
#'   plus is_selected column indicating if the term was marked with select='Y'.
#' @noRd
.read_codelists_terms_sheet <- function(path) {
  df <- readxl::read_excel(path, sheet = "Codelists_terms")
  df <- tibble::as_tibble(df)
  names(df) <- tolower(names(df))

  # Track which rows are selected
  is_selected <- "Y"
  if ("select" %in% names(df)) {
    is_selected <- toupper(df$select) == "Y"
    df$select <- NULL
  }

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
  #   input_value  = DECODE if present (non-blank), else SUBMISSION_VALUE
  #   decode       = DECODE as-is
  #   case_sensitive = "N" (default)
  decode_clean <- ifelse(is.na(df$decode), NA_character_, trimws(df$decode))
  decode_clean[decode_clean == ""] <- NA_character_

  df$coded_value       <- df$submission_value
  df$submission_value  <- df$submission_value
  df$input_value       <- dplyr::coalesce(decode_clean, df$submission_value)
  df$case_sensitive    <- "N"
  df$is_selected       <- ifelse(is_selected, "Y", "N")

  # Select and order output columns
  out_cols <- c("codelist_id", "codelist_name", "coded_value", "submission_value",
                "input_value", "decode", "case_sensitive", "term_code", "is_selected")
  out_cols <- intersect(out_cols, names(df))
  df[, out_cols, drop = FALSE]
}

# --- Section 3: Structural Validation (target_meta, ct_lib) -----------------


#' Validate target metadata
#'
#' Checks that required columns exist and no duplicate `(domain, var)` pairs
#' are present.
#'
#' @param target_meta Tibble. Target metadata to validate.
#' @param strict Logical. Reserved for future stricter checks. Default `FALSE`.
#' @return Invisible `target_meta` (for piping).
#' @export
validate_target_meta <- function(target_meta, strict = FALSE) {
  checkmate::assert_tibble(target_meta, min.rows = 1L)
  required <- c("domain", "var", "type", "label")
  miss <- setdiff(required, names(target_meta))
  if (length(miss)) abort(paste("Missing required columns:", paste(miss, collapse = ", ")))
  dupes <- target_meta %>% dplyr::count(.data$domain, .data$var) %>% dplyr::filter(.data$n > 1L)
  if (nrow(dupes) > 0L) abort(paste("Duplicate (domain,var):", paste0(dupes$domain,".",dupes$var, collapse=", ")))
  invisible(target_meta)
}

#' Validate controlled terminology library
#'
#' Checks that required columns (`codelist_id`, `coded_value`) exist.
#'
#' @param ct_lib Tibble. CT library to validate.
#' @return Invisible `ct_lib` (for piping).
#' @export
validate_ct_library <- function(ct_lib) {
  checkmate::assert_tibble(ct_lib, min.rows = 1L)
  miss <- setdiff(c("codelist_id", "coded_value"), names(ct_lib))
  if (length(miss)) abort(paste("CT library missing columns:", paste(miss, collapse = ", ")))
  invisible(ct_lib)
}

#' Normalize target metadata
#'
#' Trims whitespace, standardizes `type` to `"char"`/`"num"`, normalizes
#' `core` to `"Req"`/`"Exp"`/`"Perm"`, and uppercases domain names.
#'
#' @param target_meta Tibble. Target metadata to normalize.
#' @param config `sdtm_config` or `NULL`. Currently unused.
#' @return Normalized tibble.
#' @export
normalize_target_meta <- function(target_meta, config = NULL) {
  df <- target_meta
  chr_cols <- names(df)[vapply(df, is.character, logical(1))]
  for (col in chr_cols) df[[col]] <- trimws(df[[col]])
  if ("type" %in% names(df)) {
    df$type <- dplyr::case_when(
      tolower(df$type) %in% c("char","character","text") ~ "char",
      tolower(df$type) %in% c("num","numeric","float","integer","int") ~ "num",
      TRUE ~ tolower(df$type)
    )
  }
  if ("core" %in% names(df)) {
    df$core <- dplyr::case_when(
      tolower(df$core) %in% c("req","required") ~ "Req",
      tolower(df$core) %in% c("exp","expected") ~ "Exp",
      tolower(df$core) %in% c("perm","permissible") ~ "Perm",
      TRUE ~ df$core
    )
  }
  if ("is_key" %in% names(df)) df$is_key <- toupper(as.character(df$is_key)) %in% c("Y","YES","TRUE","1")
  if ("to_supp" %in% names(df)) df$to_supp <- toupper(as.character(df$to_supp)) %in% c("Y","YES","TRUE","1")
  if ("order" %in% names(df)) df$order <- as.integer(df$order)
  if ("domain" %in% names(df)) df$domain <- toupper(df$domain)
  df
}

#' Expand value-level metadata
#'
#' Joins value-level metadata rows into the target metadata when a
#' `value_level_id` column is present. Each VLM_ID + WHERE_CLAUSE_ID
#' combination creates a separate condition branch (case_when style).
#'
#' When the value-level metadata contains `condition` and `wc_varname`
#' columns (from [read_study_metadata_excel()]), these are parsed into
#' R expressions suitable for `case_when` derivation rules.
#'
#' @param target_meta Tibble. Target metadata with optional `value_level_id`.
#' @param value_level_meta Tibble or `NULL`. Value-level metadata to join.
#'   May contain columns: `value_level_id`, `condition`, `wc_varname`,
#'   `wc_comparator`, `wc_value`, `vlm_type`, `codelist_id`, `length`,
#'   `significant_digits`.
#' @return Expanded tibble.
#' @export
expand_value_level_meta <- function(target_meta, value_level_meta) {
  if (is.null(value_level_meta) || nrow(value_level_meta) == 0L) return(target_meta)
  if (!"value_level_id" %in% names(target_meta)) return(target_meta)
  vlm <- dplyr::filter(target_meta, !is.na(.data$value_level_id))
  non <- dplyr::filter(target_meta, is.na(.data$value_level_id))
  if (nrow(vlm) == 0L) return(target_meta)

  # Select relevant VLM columns for joining (avoid column collisions)
  vlm_join_cols <- c("value_level_id")
  vlm_extra_cols <- intersect(
    c("condition", "wc_varname", "wc_comparator", "wc_value",
      "vlm_type", "where_clause_id"),
    names(value_level_meta)
  )

  # Override type/length/codelist from VLM if present
  vlm_override_cols <- intersect(
    c("codelist_id", "length", "significant_digits"),
    names(value_level_meta)
  )

  vlm_select <- unique(c(vlm_join_cols, vlm_extra_cols, vlm_override_cols))
  vlm_data <- value_level_meta[, intersect(vlm_select, names(value_level_meta)), drop = FALSE]

  # Deduplicate VLM data for joining
  vlm_data <- dplyr::distinct(vlm_data)

  expanded <- dplyr::left_join(vlm, vlm_data, by = "value_level_id",
                                relationship = "many-to-many",
                                suffix = c("", ".vlm"))

  # Override target columns with VLM-specific values where available
  if ("vlm_type" %in% names(expanded)) {
    expanded$type <- dplyr::coalesce(expanded$vlm_type, expanded$type)
    expanded$vlm_type <- NULL
  }
  # Handle codelist_id override from VLM
  if ("codelist_id.vlm" %in% names(expanded)) {
    expanded$codelist_id <- dplyr::coalesce(expanded$codelist_id.vlm, expanded$codelist_id)
    expanded$codelist_id.vlm <- NULL
  }
  # Handle length override from VLM
  if ("length.vlm" %in% names(expanded)) {
    expanded$length <- dplyr::coalesce(expanded$length.vlm, expanded$length)
    expanded$length.vlm <- NULL
  }
  # Handle significant_digits override from VLM
  if ("significant_digits.vlm" %in% names(expanded)) {
    expanded$significant_digits <- dplyr::coalesce(
      expanded$significant_digits.vlm, expanded$significant_digits
    )
    expanded$significant_digits.vlm <- NULL
  }

  dplyr::bind_rows(non, expanded)
}

#' Apply sponsor overrides to target metadata
#'
#' Patches target metadata fields (e.g., label, codelist_id) based on
#' `config$sponsor_overrides`.
#'
#' @param target_meta Tibble. Target metadata to patch.
#' @param config `sdtm_config` with `sponsor_overrides` list.
#' @return Updated tibble.
#' @export
apply_study_overrides <- function(target_meta, config) {
  if (is.null(config$sponsor_overrides) || length(config$sponsor_overrides) == 0L) return(target_meta)
  for (ovr in config$sponsor_overrides) {
    if (!is.null(ovr$domain) && !is.null(ovr$var)) {
      idx <- which(target_meta$domain == ovr$domain & target_meta$var == ovr$var)
      if (length(idx) == 0L) { warn(glue::glue("Override for unknown var: {ovr$domain}.{ovr$var}")); next }
      for (f in setdiff(names(ovr), c("domain","var"))) {
        if (f %in% names(target_meta)) target_meta[[f]][idx] <- ovr[[f]]
      }
    }
  }
  target_meta
}

#' Resolve domain model from metadata
#'
#' Extracts and optionally sorts the metadata rows for a single domain.
#'
#' @param domain Character. Domain abbreviation (e.g., `"AE"`).
#' @param target_meta Tibble. Full target metadata.
#' @param config `sdtm_config`.
#' @param ig_version Character. SDTM IG version. Default `"3.4"`.
#' @return Tibble filtered and ordered for the domain.
#' @export
resolve_domain_model <- function(domain, target_meta, config, ig_version = "3.4") {
  dm <- dplyr::filter(target_meta, .data$domain == !!toupper(domain))
  if (nrow(dm) == 0L) { warn(glue::glue("Domain '{domain}' not in metadata.")); return(tibble::tibble()) }
  if ("order" %in% names(dm) && !all(is.na(dm$order))) dm <- dplyr::arrange(dm, .data$order)
  dm
}

# --- Section 4: Cross-sheet Validation & Pre-build Checks -------------------


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


#' Pre-build validation of metadata rules
#'
#' @description
#' Performs comprehensive checks on study metadata **before** domain building
#' to catch common errors early with actionable messages:
#'
#' 1. **DERIVATION syntax** — every DERIVATION value can be parsed as a function call.
#' 2. **Unknown functions** — function names match the FUNCTION_REGISTRY.
#' 3. **Required parameters** — required params for each function are present.
#' 4. **Source column existence** — columns referenced in DERIVATION exist in raw data.
#' 5. **Codelist availability** — CODELIST_IDs referenced by ct_assign/ct_decode
#'    exist in the CT library.
#' 6. **Circular dependencies** — DEPENDS_ON does not create cycles.
#'
#' @param study_meta Named list from [read_study_metadata_excel()] or
#'   [make_dummy_study()].
#' @param ct_lib Tibble. CT library (optional).
#' @param raw_data Named list of raw data frames (optional).
#'
#' @return A tibble of issues with columns: `severity`, `domain`, `variable`,
#'   `check`, `message`. Empty tibble if all checks pass.
#'
#' @export
validate_prebuild <- function(study_meta, ct_lib = NULL, raw_data = NULL) {
  target_meta <- study_meta$target_meta
  issues <- list()

  add_issue <- function(sev, dom, var, chk, msg) {
    issues[[length(issues) + 1L]] <<- tibble::tibble(
      severity = sev, domain = dom, variable = var, check = chk, message = msg
    )
  }

  # Build known function registry
  known_fns <- tryCatch(names(FUNCTION_REGISTRY), error = function(e) character())
  if (length(known_fns) == 0L) {
    known_fns <- c(
      "map_direct", "derive_constant", "assign_ct", "derive_usubjid",
      "derive_seq", "format_iso_dtc", "derive_dy", "derive_duration",
      "derive_epoch", "derive_visitnum", "derive_visit", "derive_visitdy",
      "derive_tpt", "derive_numeric_round", "derive_baseline_flag",
      "derive_lastobs_flag", "decode_ct", "derive_coalesce", "derive_concat",
      "derive_if_else", "derive_case_when", "derive_regex_extract",
      "derive_regex_replace", "derive_trim_pad", "derive_occurrence",
      "derive_status"
    )
  }

  # Required parameters per function
  required_params <- list(
    map_direct      = "column",
    assign_ct       = "column",
    decode_ct       = "column",
    derive_if_else  = c("condition", "true_value", "false_value"),
    derive_case_when = "conditions",
    derive_coalesce  = "columns",
    derive_concat    = "columns",
    derive_regex_extract = c("column", "pattern"),
    derive_regex_replace = c("column", "pattern", "replacement"),
    derive_trim_pad  = "column",
    derive_numeric_round = "column",
    derive_dy       = "dtc_var",
    derive_duration = c("start_dtc", "end_dtc"),
    derive_epoch    = "dtc_var",
    format_iso_dtc  = "date_col",
    derive_seq      = "by",
    derive_baseline_flag = "by",
    derive_lastobs_flag  = "by",
    derive_status   = "result_var",
    derive_occurrence = "source_var"
  )

  # Build raw data column lookup per dataset
  raw_cols <- list()
  if (!is.null(raw_data)) {
    for (nm in names(raw_data)) {
      raw_cols[[nm]] <- tolower(names(raw_data[[nm]]))
    }
  }

  # CT codelist lookup
  ct_ids <- character()
  if (!is.null(ct_lib) && nrow(ct_lib) > 0L) {
    ct_ids <- unique(ct_lib$codelist_id)
  }

  # Check each variable with a DERIVATION rule
  for (i in seq_len(nrow(target_meta))) {
    method <- target_meta$method[i]
    if (is.na(method) || method == "") next

    dom <- target_meta$domain[i]
    var <- target_meta$var[i]

    # 1. Parse DERIVATION syntax
    parsed <- tryCatch(parse_method_call(method), error = function(e) NULL)
    if (is.null(parsed)) {
      add_issue("ERROR", dom, var, "method_syntax",
                paste0("Cannot parse DERIVATION: '", method, "'"))
      next
    }

    fn_name  <- parsed$fn
    params   <- parsed$params

    # 2. Unknown function
    if (!fn_name %in% known_fns) {
      add_issue("ERROR", dom, var, "unknown_function",
                paste0("Unknown function '", fn_name, "' in DERIVATION. ",
                       "Known functions: ", paste(known_fns, collapse = ", ")))
      next
    }

    # 3. Required parameters
    req <- required_params[[fn_name]]
    if (!is.null(req)) {
      missing_p <- setdiff(req, names(params))
      if (length(missing_p) > 0L) {
        add_issue("WARN", dom, var, "missing_params",
                  paste0(fn_name, "() missing required parameter(s): ",
                         paste(missing_p, collapse = ", ")))
      }
    }

    # 4. Source column: check 'column' param against raw data
    col_param <- params$column %||% params$source_var
    if (!is.null(col_param) && length(col_param) == 1L && length(raw_cols) > 0L) {
      ds_name <- paste0(tolower(dom), "_raw")
      if (ds_name %in% names(raw_cols)) {
        src_cols <- raw_cols[[ds_name]]
        in_raw <- tolower(col_param) %in% src_cols
        # Column might reference a derived variable — skip those
        derived <- target_meta$var[target_meta$domain == dom]
        in_derived <- col_param %in% derived || toupper(col_param) %in% derived
        if (!in_raw && !in_derived) {
          add_issue("NOTE", dom, var, "source_column",
                    paste0("Column '", col_param, "' not found in ",
                           ds_name, " raw data columns (",
                           paste(head(src_cols, 5), collapse = ", "), "...)"))
        }
      }
    }

    # 5. CT codelist for ct_assign / ct_decode
    if (fn_name %in% c("assign_ct", "decode_ct")) {
      cl_id <- target_meta$codelist_id[i]
      if (is.na(cl_id) || cl_id == "") {
        add_issue("WARN", dom, var, "missing_codelist",
                  paste0(fn_name, "() has no CODELIST_ID specified in metadata"))
      } else if (length(ct_ids) > 0L && !cl_id %in% ct_ids) {
        add_issue("WARN", dom, var, "codelist_not_found",
                  paste0("CODELIST_ID '", cl_id, "' not found in CT library"))
      }
    }
  }

  # 6. Circular dependency check
  for (dom in unique(target_meta$domain)) {
    dom_meta <- target_meta[target_meta$domain == dom, ]
    edges <- tibble::tibble(from_var = character(), to_var = character())
    for (j in seq_len(nrow(dom_meta))) {
      deps <- dom_meta$depends_on[j]
      if (is.na(deps) || deps == "") next
      dep_list <- unlist(strsplit(deps, ";"))
      for (d in dep_list) {
        d <- trimws(d)
        if (d != "" && d %in% dom_meta$var) {
          edges <- dplyr::bind_rows(edges, tibble::tibble(
            from_var = d, to_var = dom_meta$var[j]
          ))
        }
      }
    }
    if (nrow(edges) > 0L) {
      has_cycle <- tryCatch({
        g <- igraph::graph_from_data_frame(edges, directed = TRUE)
        !igraph::is_dag(g)
      }, error = function(e) FALSE)
      if (isTRUE(has_cycle)) {
        add_issue("ERROR", dom, NA_character_, "circular_dependency",
                  paste0("Circular dependency detected in domain ", dom))
      }
    }
  }

  if (length(issues) == 0L) {
    return(tibble::tibble(
      severity = character(), domain = character(), variable = character(),
      check = character(), message = character()
    ))
  }
  dplyr::bind_rows(issues)
}
