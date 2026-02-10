# ==============================================================================
# Module B: Metadata Ingestion & Normalization
# ==============================================================================
# Primary readers are now read_study_metadata_excel() and read_study_ct_excel().
# This module retains validation, normalization, and expansion helpers.
# ==============================================================================

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
