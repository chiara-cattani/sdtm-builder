# ******************************************************************************
# Danone Nutricia Research
# ******************************************************************************
# STUDY/PROJECT : SONA / 23REX0061265
# PROGRAM PATH  : SAS/SONA/Files/SDTM Conversion/01 Macros
# PROGRAM NAME  : merge_inputs.R
# PURPOSE       : Merge any number of domain datasets by subject to build DM.
# ------------------------------------------------------------------------------
# NOTES :
# - Accepts any number of inputs and any selection of domains.
# - Detects a subject key among: patient_number, USUBJID, usubjid.
# - Standardizes the key to `patient_number` and full-joins left→right.
# - Keeps `oak_id`/`raw_source` only from the first dataset; drops from others.
# - If an input is EMPTY but has a recognizable key, its variables are still
#   added to the output as empty columns.
# - Datasets lacking any recognizable key are skipped (logged).
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2025-09-29 - cattanch - Initial program
# ******************************************************************************

#' @title Merge multiple SDTM inputs by subject.
#'
#' @description
#' Merge an arbitrary set of domain data frames by subject (`patient_number`),
#' preserving columns from empty inputs as empty columns in the result.
#' Uses caller-provided argument names in diagnostics (e.g., `"ae2"` instead of `"df2"`).
#'
#' @param ... One or more data frames/tibbles to merge (may be empty).
#' @param key_candidates Character vector of possible subject keys (priority order).
#'
#' @return A tibble: full join of all inputs by `patient_number`.
#'
#' @examples
#' \dontrun{
#' dm3 <- merge_inputs(dm2, ae2, ie2, eos2, ic2, ex2)
#' }
#'
#' @import dplyr rlang

merge_inputs <- function(...,
                         key_candidates = c("patient_number", "USUBJID", "usubjid")) {
  inputs <- list(...)
  if (length(inputs) == 0) return(dplyr::tibble())
  # caller-provided names for clearer messages
  call_syms <- as.list(substitute(list(...)))[-1]
  arg_names <- vapply(call_syms, function(e)
    if (is.name(e) || is.symbol(e)) as.character(e) else paste(deparse(e), collapse = ""),
    character(1)
  )
  nm <- names(inputs); if (is.null(nm)) nm <- rep("", length(inputs))
  nm[nm == ""] <- ifelse(nzchar(arg_names), arg_names[nm == ""], paste0("df", seq_len(sum(nm == ""))))
  nm <- make.unique(nm, sep = "_")
  names(inputs) <- nm
  # normalize each dataset
  normalize_key <- function(df, src_name) {
    if (!is.data.frame(df)) {
      message("merge_inputs: skipping '", src_name, "' (not a data.frame).")
      return(NULL)
    }
    hits <- intersect(key_candidates, names(df))
    if (length(hits) == 0) {
      message("merge_inputs: '", src_name, "' has no recognizable key (",
              paste(key_candidates, collapse = ", "), "); skipping dataset.")
      return(NULL)
    }
    key_col <- hits[1]
    # rename key to patient_number (even if empty)
    if (!"patient_number" %in% names(df)) {
      df <- dplyr::rename(df, patient_number = !!rlang::sym(key_col))
    }
    if (nrow(df) == 0L) {
      # >>> drop usubjid columns from EMPTY datasets before joining <<<
      df <- dplyr::select(df, -dplyr::any_of(c("USUBJID", "usubjid")))
      message("merge_inputs: '", src_name, "' is empty; columns will be added as empty.")
      return(df)  # keep empty df so its (cleaned) columns are preserved
    }
    # non-empty: keep valid keys only
    dplyr::filter(df, !is.na(patient_number) & patient_number != "")
  }
  datasets <- mapply(normalize_key, inputs, names(inputs), SIMPLIFY = FALSE)
  keep_idx <- !vapply(datasets, is.null, logical(1))
  if (!any(keep_idx)) {
    all_cols <- unique(unlist(lapply(inputs, names)))
    message("merge_inputs: no input had a recognizable key; returning 0-row dataset with union of columns (best effort).")
    if (is.null(all_cols)) return(dplyr::tibble())
    empty_list <- setNames(rep(list(logical(0)), length(all_cols)), all_cols)
    return(tibble::as_tibble(empty_list))
  }
  datasets  <- datasets[keep_idx]
  src_names <- names(datasets)
  # keep oak columns only from first kept dataset
  base_name <- src_names[1]
  datasets <- lapply(names(datasets), function(nm_i) {
    df <- datasets[[nm_i]]
    if (nm_i != base_name) {
      df <- dplyr::select(df, -dplyr::any_of(c("oak_id", "raw_source")))
    }
    df
  })
  names(datasets) <- src_names
  # warn on duplicates (only for non-empty inputs)
  for (nm_i in names(datasets)) {
    df <- datasets[[nm_i]]
    if (nrow(df) > 0L) {
      dupes <- df$patient_number[duplicated(df$patient_number)]
      if (length(dupes) > 0) {
        warning(sprintf("Dataset '%s' has multiple records for patient_number(s): %s",
                        nm_i, paste(unique(dupes), collapse = ", ")))
      }
    }
  }
  # full-join left -> right by patient_number
  out <- Reduce(function(x, y) dplyr::full_join(x, y, by = "patient_number"), datasets)
  # Safety net for EMPTY inputs that had a key:
  # add their columns as NA **but do not add 'usubjid'/'USUBJID'**
  empty_inputs <- names(inputs)[vapply(inputs, function(df) is.data.frame(df) && nrow(df) == 0L, logical(1))]
  empty_inputs <- intersect(empty_inputs, src_names)  # only those kept (had a key)
  if (length(empty_inputs)) {
    for (src in empty_inputs) {
      cols <- setdiff(names(inputs[[src]]), c("patient_number", "USUBJID", "usubjid"))
      missing_cols <- setdiff(cols, names(out))
      if (length(missing_cols)) {
        for (cc in missing_cols) out[[cc]] <- NA_character_
      }
    }
  }
  out
}