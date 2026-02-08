# ==============================================================================
# Module B: Metadata Ingestion & Normalization
# ==============================================================================

#' Read target (SDTM) metadata
#'
#' Reads a CSV or Excel file containing one row per SDTM variable per domain.
#' Column names are lowercased automatically. Duplicate `(domain, var)` pairs
#' cause an error.
#'
#' @param path Character. Path to `.csv` or `.xlsx` file.
#' @param sheet Integer. Sheet index for Excel files. Default `1`.
#' @param domain Character or `NULL`. If supplied, filter to this domain.
#' @param colmap Named list or `NULL`. Column rename mapping.
#' @param encoding Character. File encoding. Default `"UTF-8"`.
#' @return A tibble with standard target metadata columns.
#' @export
read_target_meta <- function(path, sheet = 1L, domain = NULL,
                             colmap = NULL, encoding = "UTF-8") {
  checkmate::assert_string(path, min.chars = 1L)
  ext <- tolower(tools::file_ext(path))
  df <- switch(ext,
    csv  = utils::read.csv(path, stringsAsFactors = FALSE,
                           fileEncoding = encoding, na.strings = ""),
    xlsx = , xls = readxl::read_excel(path, sheet = sheet),
    abort(glue::glue("Unsupported file extension: .{ext}"))
  )
  df <- tibble::as_tibble(df)
  if (nrow(df) == 0L) abort("Target metadata file is empty.")
  names(df) <- tolower(names(df))
  if (!is.null(colmap)) {
    for (nm in names(colmap)) {
      idx <- match(tolower(nm), names(df))
      if (!is.na(idx)) names(df)[idx] <- tolower(colmap[[nm]])
    }
  }
  required <- c("domain", "var", "type", "label")
  miss <- setdiff(required, names(df))
  if (length(miss)) abort(paste("Target metadata missing columns:", paste(miss, collapse = ", ")))
  dupes <- df %>% dplyr::count(.data$domain, .data$var) %>% dplyr::filter(.data$n > 1L)
  if (nrow(dupes) > 0L) {
    abort(paste("Duplicate (domain, var):", paste0(dupes$domain, ".", dupes$var, collapse = ", ")))
  }
  if (!is.null(domain)) df <- dplyr::filter(df, .data$domain == !!domain)
  standard <- c("domain","var","type","length","label","role","core",
                "codelist_id","order","is_key","to_supp","rule_type","rule_params","depends_on")
  for (col in standard) if (!col %in% names(df)) df[[col]] <- NA
  df
}

#' Read source (raw) metadata
#'
#' Reads a CSV or Excel file describing raw-data columns: dataset, column, type.
#'
#' @param path Character. Path to `.csv` or `.xlsx` file.
#' @param sheet Integer. Sheet index for Excel files. Default `1`.
#' @param colmap Named list or `NULL`. Column rename mapping.
#' @param encoding Character. File encoding. Default `"UTF-8"`.
#' @return A tibble with source metadata columns.
#' @export
read_source_meta <- function(path, sheet = 1L, colmap = NULL, encoding = "UTF-8") {
  checkmate::assert_string(path, min.chars = 1L)
  ext <- tolower(tools::file_ext(path))
  df <- switch(ext,
    csv  = utils::read.csv(path, stringsAsFactors = FALSE, fileEncoding = encoding, na.strings = ""),
    xlsx = , xls = readxl::read_excel(path, sheet = sheet),
    abort(glue::glue("Unsupported file extension: .{ext}"))
  )
  df <- tibble::as_tibble(df)
  names(df) <- tolower(names(df))
  required <- c("dataset", "column", "type")
  miss <- setdiff(required, names(df))
  if (length(miss)) abort(paste("Source metadata missing columns:", paste(miss, collapse = ", ")))
  dupes <- df %>% dplyr::count(.data$dataset, .data$column) %>% dplyr::filter(.data$n > 1L)
  if (nrow(dupes) > 0L) {
    abort(paste("Duplicate (dataset, column):", paste0(dupes$dataset, ".", dupes$column, collapse = ", ")))
  }
  df
}

#' Read controlled terminology library
#'
#' Reads a CSV or Excel file containing codelist-level mappings. Required
#' columns: `codelist_id`, `coded_value`.
#'
#' @param path Character. Path to `.csv` or `.xlsx` file.
#' @param sheet Integer. Sheet index for Excel files. Default `1`.
#' @param colmap Named list or `NULL`. Column rename mapping.
#' @param version Character or `NULL`. CT version tag to attach.
#' @param sponsor_extension Data frame or `NULL`. Extra sponsor terms to append.
#' @return A tibble with CT library rows.
#' @export
read_ct_library <- function(path, sheet = 1L, colmap = NULL,
                            version = NULL, sponsor_extension = NULL) {
  checkmate::assert_string(path, min.chars = 1L)
  ext <- tolower(tools::file_ext(path))
  df <- switch(ext,
    csv  = utils::read.csv(path, stringsAsFactors = FALSE, na.strings = ""),
    xlsx = , xls = readxl::read_excel(path, sheet = sheet),
    abort(glue::glue("Unsupported file extension: .{ext}"))
  )
  df <- tibble::as_tibble(df)
  names(df) <- tolower(names(df))
  required <- c("codelist_id", "coded_value")
  miss <- setdiff(required, names(df))
  if (length(miss)) abort(paste("CT library missing columns:", paste(miss, collapse = ", ")))
  if (!is.null(version)) df$version <- version
  if (!is.null(sponsor_extension)) {
    sp <- tibble::as_tibble(sponsor_extension)
    names(sp) <- tolower(names(sp))
    df <- dplyr::bind_rows(df, sp)
  }
  df
}

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

#' Validate source metadata
#'
#' Checks that required columns (`dataset`, `column`, `type`) exist.
#'
#' @param source_meta Tibble. Source metadata to validate.
#' @param strict Logical. Reserved for future stricter checks. Default `FALSE`.
#' @return Invisible `source_meta` (for piping).
#' @export
validate_source_meta <- function(source_meta, strict = FALSE) {
  checkmate::assert_tibble(source_meta, min.rows = 1L)
  required <- c("dataset", "column", "type")
  miss <- setdiff(required, names(source_meta))
  if (length(miss)) abort(paste("Missing required columns:", paste(miss, collapse = ", ")))
  invisible(source_meta)
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

#' Normalize source metadata
#'
#' Trims whitespace, lowercases dataset/column names, and standardizes
#' type strings.
#'
#' @param source_meta Tibble. Source metadata to normalize.
#' @param config `sdtm_config` or `NULL`. Currently unused.
#' @return Normalized tibble.
#' @export
normalize_source_meta <- function(source_meta, config = NULL) {
  df <- source_meta
  chr_cols <- names(df)[vapply(df, is.character, logical(1))]
  for (col in chr_cols) df[[col]] <- trimws(df[[col]])
  if ("dataset" %in% names(df)) df$dataset <- tolower(df$dataset)
  if ("column" %in% names(df))  df$column  <- tolower(df$column)
  if ("type" %in% names(df)) {
    df$type <- dplyr::case_when(
      tolower(df$type) %in% c("char","character","text","string") ~ "character",
      tolower(df$type) %in% c("num","numeric","float","double") ~ "numeric",
      tolower(df$type) %in% c("int","integer") ~ "integer",
      TRUE ~ tolower(df$type)
    )
  }
  df
}

#' Expand value-level metadata
#'
#' Joins value-level metadata rows into the target metadata when a
#' `value_level_id` column is present.
#'
#' @param target_meta Tibble. Target metadata with optional `value_level_id`.
#' @param value_level_meta Tibble or `NULL`. Value-level metadata to join.
#' @return Expanded tibble.
#' @export
expand_value_level_meta <- function(target_meta, value_level_meta) {
  if (is.null(value_level_meta) || nrow(value_level_meta) == 0L) return(target_meta)
  if (!"value_level_id" %in% names(target_meta)) return(target_meta)
  vlm <- dplyr::filter(target_meta, !is.na(.data$value_level_id))
  non <- dplyr::filter(target_meta, is.na(.data$value_level_id))
  if (nrow(vlm) == 0L) return(target_meta)
  expanded <- dplyr::left_join(vlm, value_level_meta, by = "value_level_id",
                                relationship = "many-to-many")
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
