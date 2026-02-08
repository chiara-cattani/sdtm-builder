# ==============================================================================
# Module G5: Identifiers & Sequences
# ==============================================================================

#' Derive USUBJID
#' @param data Tibble.
#' @param studyid Character.
#' @param subjid_col Character. Default `"subjid"`.
#' @param sep Character. Default `"-"`.
#' @param validate_existing Logical. Default `TRUE`.
#' @return Tibble.
#' @export
derive_usubjid <- function(data, studyid, subjid_col = "subjid",
                           sep = "-", validate_existing = TRUE) {
  if ("USUBJID" %in% names(data) && validate_existing) return(data)
  if ("usubjid" %in% names(data)) {
    data$USUBJID <- data$usubjid
    return(data)
  }
  if (!subjid_col %in% names(data)) {
    abort(glue::glue("derive_usubjid: '{subjid_col}' not found"))
  }
  data$USUBJID <- paste(studyid, data[[subjid_col]], sep = sep)
  data
}

#' Derive STUDYID and DOMAIN constants
#' @param data Tibble.
#' @param domain Character.
#' @param config `sdtm_config`.
#' @param idvar Character or `NULL`.
#' @param idvarval_col Character or `NULL`.
#' @return Tibble.
#' @export
derive_domain_keys <- function(data, domain, config,
                               idvar = NULL, idvarval_col = NULL) {
  data$STUDYID <- config$studyid
  data$DOMAIN  <- domain
  if (!is.null(idvar)) data$IDVAR <- idvar
  if (!is.null(idvarval_col) && idvarval_col %in% names(data)) {
    data$IDVARVAL <- as.character(data[[idvarval_col]])
  }
  data
}

#' Derive sequence number (--SEQ)
#' @param data Tibble.
#' @param target_var Character.
#' @param by Character vector. Default `"USUBJID"`.
#' @param order_by Character vector.
#' @param ties Character. Default `"dense"`.
#' @param start_at Integer. Default `1L`.
#' @return Tibble.
#' @export
derive_seq <- function(data, target_var, by = "USUBJID", order_by,
                       ties = "dense", start_at = 1L) {
  # Handle missing order_by columns gracefully
  available_order <- intersect(order_by, names(data))
  if (length(available_order) == 0L) available_order <- by

  # Sort then assign sequence within groups
  data <- dplyr::arrange(data, dplyr::across(dplyr::all_of(c(by, available_order))))
  data <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) %>%
    dplyr::mutate(!!target_var := dplyr::row_number() + start_at - 1L) %>%
    dplyr::ungroup()
  data
}

#' Derive Sponsor-Defined Identifier
#' @param data Tibble.
#' @param target_var Character.
#' @param source_cols Character vector.
#' @param sep Character. Default `"-"`.
#' @param prefix Character or `NULL`.
#' @return Tibble.
#' @export
derive_spid <- function(data, target_var, source_cols, sep = "-",
                        prefix = NULL) {
  parts <- lapply(source_cols, function(c) as.character(data[[c]]))
  val <- do.call(paste, c(parts, sep = sep))
  if (!is.null(prefix)) val <- paste(prefix, val, sep = sep)
  data[[target_var]] <- val
  data
}

#' Derive Group Identifier
#' @param data Tibble.
#' @param target_var Character.
#' @param group_by Character vector.
#' @param method Character. Default `"sequential"`.
#' @return Tibble.
#' @export
derive_grpid <- function(data, target_var, group_by, method = "sequential") {
  data <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_by))) %>%
    dplyr::mutate(!!target_var := dplyr::cur_group_id()) %>%
    dplyr::ungroup()
  data
}
