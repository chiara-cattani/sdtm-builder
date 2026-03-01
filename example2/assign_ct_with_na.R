# ******************************************************************************
# Danone Nutricia Research
# ******************************************************************************
# STUDY/PROJECT : SONA / 23REX0061265
# PROGRAM PATH  : SAS/SONA/Files/SDTM Conversion/01 Macros
# PROGRAM NAME  : sdtm_assign.R
# PURPOSE       : Map collected raw values to SDTM target values using
#                 controlled terminology (CT), with precise handling of:
#                   - true empties (remain empty, not mapped)
#                   - literal "NA" (text) mapped via CT (e.g., "NOT APPLICABLE")
#                 Includes standalone helpers to avoid package-internal deps.
# ------------------------------------------------------------------------------
# NOTES :
#   * Case-insensitive matching on collected_value and term_synonyms.
#   * Whitespace is trimmed before matching.
#   * If ct_clst is provided, mapping is restricted to that codelist.
#   * Silent on unmapped tokens (no warnings are emitted).
#   * IMPORTANT: For a given codelist, ensure every raw token you expect to map
#     is listed consistently in the *same* CT column—either all under
#     `collected_value` or all under `term_synonyms`. Do not split tokens across
#     columns for the same codelist.
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2025-10-03 - cattanch - Initial version
# ******************************************************************************

# ------------------------------------------------------------------------------
# Helpers (ID vars, CT validation, Join)
# ------------------------------------------------------------------------------
#' Does a vector contain the Oak identifier variables?
#'
#' @param x A character vector.
#' @return Logical scalar: TRUE if all `oak_id_vars()` are present in `x`.
#' @keywords internal
contains_oak_id_vars <- function(x) {
  all(oak_id_vars() %in% x)
}
#' Assert a controlled terminology specification
#'
#' @param ct_spec A data frame to be asserted as a valid controlled terminology
#'   data set.
#' @param optional Logical. If TRUE, allows `ct_spec = NULL`.
#'
#' @return Invisibly returns `ct_spec` if valid; otherwise throws an error.
#' @details
#' Required columns: `codelist_code`, `collected_value`, `term_synonyms`,
#' `term_value`. The table must be non-empty and cannot contain `NA` in
#' `codelist_code` or `term_value`.
#' @keywords internal
assert_ct_spec <- function(ct_spec, optional = FALSE) {
  if (optional && is.null(ct_spec)) return(invisible(ct_spec))
  stopifnot(is.data.frame(ct_spec))
  required_cols <- c("codelist_code", "collected_value", "term_synonyms", "term_value")
  missing <- setdiff(required_cols, names(ct_spec))
  if (length(missing) > 0) {
    stop("`ct_spec` is missing required columns: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  if (nrow(ct_spec) == 0) {
    stop("`ct_spec` must not be empty.", call. = FALSE)
  }
  if (any(is.na(ct_spec$codelist_code))) {
    stop("`ct_spec$codelist_code` contains NA values.", call. = FALSE)
  }
  if (any(is.na(ct_spec$term_value))) {
    stop("`ct_spec$term_value` contains NA values.", call. = FALSE)
  }
  invisible(ct_spec)
}
#' Assert a codelist code
#'
#' @param ct_spec A data frame encoding a controlled terminology data set, or
#'   `NULL`.
#' @param ct_clst A single string codelist code to assert, or `NULL`.
#' @param optional Logical. If TRUE, allows `ct_clst = NULL`.
#'
#' @return Invisibly returns `ct_clst` if valid; otherwise throws an error.
#' @details
#' If `ct_spec` is supplied, its column `codelist_code` must contain the given
#' `ct_clst`.
#' @keywords internal
assert_ct_clst <- function(ct_spec, ct_clst, optional = FALSE) {
  if (optional && is.null(ct_clst)) return(invisible(ct_clst))
  if (is.null(ct_clst)) {
    stop("`ct_clst` must not be NULL.", call. = FALSE)
  }
  if (!is.character(ct_clst) || length(ct_clst) != 1) {
    stop("`ct_clst` must be a single string.", call. = FALSE)
  }
  if (!is.null(ct_spec)) {
    stopifnot(is.data.frame(ct_spec))
    if (!"codelist_code" %in% names(ct_spec)) {
      stop("`ct_spec` must contain a column `codelist_code`.", call. = FALSE)
    }
    if (!ct_clst %in% unique(ct_spec$codelist_code)) {
      stop("`ct_clst` (", ct_clst, ") not found in `ct_spec$codelist_code`.", call. = FALSE)
    }
  }
  invisible(ct_clst)
}
#' SDTM join (left join by id_vars)
#'
#' @param raw_dat Raw dataset (data.frame). Must include variables in `id_vars`.
#' @param tgt_dat Target dataset (data.frame) to be joined; can be `NULL`.
#' @param id_vars Character vector of key variables for the join.
#'
#' @return A data.frame with all rows from `raw_dat` and matching columns
#'   from `tgt_dat`.
#' @keywords internal
sdtm_join <- function(raw_dat, tgt_dat = NULL, id_vars = oak_id_vars()) {
  stopifnot(is.data.frame(raw_dat))
  stopifnot(all(id_vars %in% names(raw_dat)))
  if (is.null(tgt_dat)) return(raw_dat)
  stopifnot(is.data.frame(tgt_dat))
  stopifnot(all(id_vars %in% names(tgt_dat)))
  dplyr::left_join(raw_dat, tgt_dat, by = id_vars)
}

# ------------------------------------------------------------------------------
# Local CT mapping (silent, robust, case-insensitive)
# ------------------------------------------------------------------------------
#' Prepare a normalized CT lookup table
#'
#' @param ct_spec Controlled terminology data.frame.
#' @param ct_clst Optional codelist code to filter `ct_spec`.
#'
#' @return A data.frame with columns:
#'   - `key`: normalized matching key (upper-trimmed)
#'   - `term_value`: mapped target value
#' @details
#' Keys are built from **collected_value** and **term_synonyms**.
#' Synonyms are split on commas, semicolons, or pipes.
#' @note
#' **Consistency requirement:** For any given codelist, list *all* raw tokens
#' you expect to map under the **same** CT column—either all in
#' `collected_value` or all in `term_synonyms`. Do **not** split tokens across
#' the two columns for the same codelist, otherwise some values may not map.
#' @keywords internal
ct_prepare_lookup <- function(ct_spec, ct_clst = NULL) {
  stopifnot(is.data.frame(ct_spec))
  req <- c("codelist_code", "collected_value", "term_synonyms", "term_value")
  stopifnot(all(req %in% names(ct_spec)))
  ct <- ct_spec
  if (!is.null(ct_clst)) {
    ct <- ct[ct$codelist_code == ct_clst, , drop = FALSE]
  }
  norm <- function(x) trimws(toupper(as.character(x)))
  # Collected values as keys
  keys <- data.frame(
    key = norm(ct$collected_value),
    term_value = as.character(ct$term_value),
    stringsAsFactors = FALSE
  )
  # Synonyms as keys
  has_syn <- !is.na(ct$term_synonyms) & nzchar(ct$term_synonyms)
  if (any(has_syn)) {
    syn_list <- strsplit(ct$term_synonyms[has_syn], "[,;|]", perl = TRUE)
    syn_df <- data.frame(
      key = norm(unlist(syn_list)),
      term_value = rep(as.character(ct$term_value[has_syn]), lengths(syn_list)),
      stringsAsFactors = FALSE
    )
    keys <- rbind(keys, syn_df)
  }
  # Drop empty keys and duplicates (keep first occurrence)
  keys <- keys[nzchar(keys$key), , drop = FALSE]
  keys <- keys[!duplicated(keys$key), , drop = FALSE]
  keys
}
#' Map values using a local CT lookup (silent)
#'
#' @param x Character vector to map.
#' @param ct_spec Controlled terminology data.frame (see `assert_ct_spec`).
#' @param ct_clst Optional codelist code to restrict mapping.
#'
#' @return Character vector of mapped values. True empties (NA or "") remain NA.
#' @details
#' Matching is case-insensitive and trims whitespace. Literal "NA" (text) is
#' treated as a normal value and mapped if present in CT. No warnings for
#' unmapped tokens.
#' @keywords internal
ct_map_local <- function(x, ct_spec, ct_clst = NULL) {
  if (is.null(ct_spec)) return(as.character(x))
  look <- ct_prepare_lookup(ct_spec, ct_clst)
  norm <- function(v) trimws(toupper(as.character(v)))
  x_chr  <- as.character(x)
  x_trim <- trimws(x_chr)
  is_empty <- is.na(x_trim) | x_trim == ""
  out <- rep(NA_character_, length(x_chr))
  if (any(!is_empty)) {
    nx <- norm(x_trim[!is_empty])
    idx <- match(nx, look$key)
    out[!is_empty] <- ifelse(!is.na(idx), look$term_value[idx], NA_character_)
  }
  out
}

# ------------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------------
#' Assign SDTM target values from raw using Controlled Terminology (CT)
#'
#' @description
#' Maps `raw_var` from `raw_dat` to `tgt_var` in a joined dataset using CT.
#' **True empties** (`NA` or `""`) are left empty (not mapped); any non-empty
#' text (including the literal `"NA"`) is matched case-insensitively against
#' CT (`collected_value` and `term_synonyms`) and replaced by `term_value`.
#' No “unmapped terms” messages are emitted.
#'
#' @param tgt_dat Target dataset (data.frame) that may already contain `tgt_var`;
#'   can be `NULL`.
#' @param tgt_var Name of the target variable to create/fill (string).
#' @param raw_dat Raw dataset (data.frame) containing `raw_var` and `id_vars`.
#' @param raw_var Name of the raw variable to map (string).
#' @param ct_spec Controlled terminology data.frame with columns:
#'   `codelist_code`, `collected_value`, `term_synonyms`, `term_value`. Can be `NULL`
#'   (in which case values pass through unchanged).
#' @param ct_clst Optional single codelist code to restrict mapping.
#' @param id_vars Character vector of join keys; must include `oak_id_vars()`.
#'
#' @return A data.frame equal to `raw_dat` left-joined to `tgt_dat` on `id_vars`,
#'   with `tgt_var` appended as the rightmost column. Existing non-missing
#'   values in `tgt_var` are preserved.
#'
#' @details
#' Steps:
#' 1) Validate inputs and CT.
#' 2) Join `raw_dat` to `tgt_dat` by `id_vars`.
#' 3) Build a mapped vector using `ct_map_local()` for non-empty inputs;
#'    leave true empties as `NA`.
#' 4) Fill `tgt_var` preferring existing target values, else mapped.
#' 5) Place `tgt_var` as the last column; drop the source column if it would
#'    duplicate `tgt_var`.
#'
#' @note
#' **Consistency requirement (again):** For each codelist, list all raw tokens
#' to be mapped in the same CT column—either exclusively in `collected_value`
#' or exclusively in `term_synonyms`—to ensure deterministic mapping.
#'
#' @examples
#' \dontrun{
#' # Minimal example
#' id_vars <- c("STUDYID","DOMAIN","USUBJID")
#' oak_id_vars <- function() id_vars
#'
#' raw_dat <- data.frame(STUDYID="S1", DOMAIN="QS", USUBJID="01",
#'                       ANS=c("NA","amount reduced",""), stringsAsFactors = FALSE)
#' ct_spec <- data.frame(
#'   codelist_code = c("YESNO","YESNO"),
#'   collected_value = c("", ""),                 # keep tokens in one column
#'   term_synonyms   = c("NA|N/A", "AMOUNT REDUCED"),   # e.g., all tokens placed here
#'   term_value      = c("NOT APPLICABLE","amount reduced"),
#'   stringsAsFactors = FALSE
#' )
#'
#' out <- assign_ct_with_na(
#'   tgt_dat = NULL, tgt_var = "QSTESTCD",
#'   raw_dat = raw_dat, raw_var = "ANS",
#'   ct_spec = ct_spec, ct_clst = "YESNO",
#'   id_vars = id_vars
#' )
#' # Expected: "NOT APPLICABLE", "amount reduced", "" (empty)
#' }
#' @export
assign_ct_with_na <- function(tgt_dat = NULL,
                              tgt_var,
                              raw_dat,
                              raw_var,
                              ct_spec = NULL,
                              ct_clst = NULL,
                              id_vars = oak_id_vars()) {
  admiraldev::assert_character_scalar(raw_var)
  admiraldev::assert_character_scalar(tgt_var)
  admiraldev::assert_character_vector(id_vars)
  assertthat::assert_that(
    contains_oak_id_vars(id_vars),
    msg = "`id_vars` must include the oak id vars."
  )
  admiraldev::assert_data_frame(
    raw_dat,
    required_vars = rlang::syms(c(id_vars, raw_var))
  )
  admiraldev::assert_data_frame(
    tgt_dat,
    required_vars = rlang::syms(id_vars),
    optional = TRUE
  )
  assert_ct_spec(ct_spec, optional = TRUE)
  assert_ct_clst(ct_spec = ct_spec, ct_clst = ct_clst, optional = TRUE)
  join_dat <-
    raw_dat |>
    dplyr::select(dplyr::all_of(c(id_vars, raw_var))) |>
    sdtm_join(tgt_dat = tgt_dat, id_vars = id_vars)
  # Source values
  raw_chr  <- as.character(join_dat[[raw_var]])
  raw_trim <- trimws(raw_chr)
  # True empties (NA or "")
  is_empty <- is.na(raw_trim) | raw_trim == ""
  # Map only non-empty values (literal "NA" is treated as text and mapped)
  mapped <- rep(NA_character_, length(raw_trim))
  if (any(!is_empty) && !is.null(ct_spec)) {
    mapped[!is_empty] <- ct_map_local(raw_trim[!is_empty], ct_spec = ct_spec, ct_clst = ct_clst)
  } else if (any(!is_empty) && is.null(ct_spec)) {
    mapped[!is_empty] <- raw_trim[!is_empty] # passthrough if no CT provided
  }
  # Preserve existing target values if present
  cur_tgt_val <- if (!is.null(join_dat[[tgt_var]])) {
    as.character(join_dat[[tgt_var]])
  } else {
    rep(NA_character_, length(mapped))
  }
  join_dat |>
    dplyr::mutate("{tgt_var}" := dplyr::coalesce(cur_tgt_val, mapped)) |>
    dplyr::select(-dplyr::any_of(setdiff(raw_var, tgt_var))) |>
    dplyr::relocate(dplyr::all_of(tgt_var), .after = dplyr::last_col())
}