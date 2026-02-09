# ==============================================================================
# Read Study Metadata from Multi-Sheet Excel Workbook
# ==============================================================================
# Reads Study_Metadata.xlsx with sheets: Meta, Domains, Variables,
# Value Level, Where Clauses (and optionally Standards, Method).
# ==============================================================================

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
#'   \item **Method** — derivation method descriptions (informational)
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
#' | METHOD               | rule_type (mapped)   |
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

  list(
    study_name       = study_name,
    target_meta      = target_meta,
    domain_meta      = domain_meta,
    value_level_meta = value_level_meta
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

  # Map METHOD → rule_type using fixed mapping
  if ("method" %in% names(df)) {
    df$rule_type <- map_method_to_rule_type(df$method)
  } else {
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
