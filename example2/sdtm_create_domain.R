# ******************************************************************************
# Danone Nutricia Research
# ******************************************************************************
# STUDY/PROJECT : SONA / 23REX0061265
# PROGRAM PATH  : SAS/SONA/Files/SDTM Conversion/01 Macros
# PROGRAM NAME  : sdtm_create_domain.R
# PURPOSE       : Finalize an SDTM domain dataset using metadata-driven rules.
#                 - Sort rows by KEYS from `sdtm_dom`
#                 - Enforce required/expected variables
#                 - Assign variable labels from `sdtm_var`
#                 - Conditionally drop PERM variables that are all empty
#                 - Drop variables not in metadata
#                 - Preserve numeric types (no global character coercion)
#                 - Replace NA with blanks only for character columns
#                 - Set dataset label to "SDTM domain <DOMAIN>"
#                 - Save finalized dataset as lower-case <domain> in ../70 QC:
#                     * RDA/<domain>.rds
#                     * CSV/<domain>.csv
#                     * XPT/<domain>.xpt (SAS XPORT v8, variable labels preserved)
# ------------------------------------------------------------------------------
# NOTES :
#   * Column types: numeric columns remain numeric; only character columns get
#     NA -> "" replacement for CSV friendliness.
#   * KEYS can be provided or read from `sdtm_dom$KEYS` for the given DOMAIN.
#   * Variable labels are taken from `sdtm_var` (first non-empty label per var).
#   * When `delperm = TRUE`, PERM variables that are entirely empty are removed.
#   * Output filenames are forced to lower-case domain names.
#   * Output root defaults to "../70 QC"; subfolders RDA/ CSV/ XPT/ are created
#     on the fly if missing.
#   * XPT export uses `haven::write_xpt(..., version = 8)`.
#   * The object returned in R is identical to the RDS saved to RDA/.
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2025-10-01 - cattanch - Initial program
# 2025-10-06 - cattanch - Added multi-format export (RDS/CSV/XPT),
#                         new parameter `out_root`, lower-case file naming,
#                         automatic creation of ../70 QC/{RDA,CSV,XPT}/
#                       - Preserve numeric types; dataset label set to
#                         "SDTM domain <DOMAIN>"
# 2025-10-09 - cattanch - Renamed output datasets to qc_xx
# 2025-10-14 - cattanch - Fixed sorting when capital letters present.
# 2025-12-08 - cattanch - Added option addsuppvars.
# ******************************************************************************
#' Finalize and export an SDTM domain (RDS/CSV/XPT) without forcing all-character.
#'
#' Finalizes an SDTM domain dataset (`inset`) using metadata from `sdtm_var`
#' and `sdtm_dom`: orders rows by KEYS, enforces REQ/EXP variables, applies
#' labels, optionally removes PERM variables that are entirely empty, and drops
#' variables not listed in metadata **except any explicitly listed in
#' `addsuppvars`**. The finalized dataset is returned and also written under
#' `out_root` (default: `../70 QC`) into:
#' - `RDA/<domain>.rds` (R serialized object),
#' - `CSV/<domain>.csv` (comma-separated; NA -> "" only for character columns),
#' - `XPT/<domain>.xpt` (SAS XPORT v8; variable labels preserved).
#'
#' Dataset label is set to `"SDTM domain <DOMAIN>"`, where `<DOMAIN>` is upper-case.
#' Filenames use the lower-case domain (e.g., `co.csv`, `co.rds`, `co.xpt`).
#' Subdirectories `RDA/`, `CSV/`, `XPT/` are created if they do not exist.
#'
#' @param inset A `data.frame`. The domain dataset to finalize.
#' @param keys Optional character vector or comma-separated string of KEYS
#' to use for sorting. If omitted, KEYS are taken from
#' `sdtm_dom` for the current domain.
#' @param delperm Logical. If `TRUE` (default), drop permissible (PERM)
#' variables that are entirely empty. If `FALSE`, keep PERM
#' variables even when all values are missing/blank.
#' @param out_root Character scalar. Root output directory where the function
#' writes `RDA/`, `CSV/`, and `XPT/` subfolders. Default:
#' `"../70 QC"`.
#' @param addsuppvars Optional character vector or string of **additional
#' variables** (not present in `sdtm_var`) that should be kept
#' in the final dataset. Names are matched case-insensitively
#' to columns in `inset` and are appended after the metadata
#' variables. Can be:
#' - a character vector: `c("USUBJID_SUPP", "VISITNUM_SUPP")`
#' - or a single string: `"USUBJID_SUPP, VISITNUM_SUPP"`
#'
#' @return A tibble with:
#' - Required and expected variables enforced.
#' - Labels assigned from `sdtm_var`.
#' - PERM variables removed only when `delperm = TRUE` and entirely empty.
#' - Columns ordered as in `sdtm_var` for the domain, followed by any
#' `addsuppvars` columns in their original input order.
#' - **Numeric columns remain numeric**; character columns have `NA` replaced by `""`.
#'
#' **Side effects** (files written under `out_root`):
#' - `RDA/<domain>.rds`
#' - `CSV/<domain>.csv`
#' - `XPT/<domain>.xpt` (XPORT v8)
#' 
#' @details
#' Processing steps:
#' 0. **Uppercase input names** (ensures KEYS recognition).
#' 1. Resolve DOMAIN from global `sdtm_domain`; derive lower-case `<domain>`.
#' 2. Determine KEYS (from `keys` if provided; else from `sdtm_dom$KEYS`) and
#'    sort rows lexicographically by those variables.
#' 3. Select and order variables per `sdtm_var` for the domain; validate that
#'    REQ/EXP variables are present.
#' 4. Assign variable labels using the first non-empty label found in metadata.
#' 5. If `delperm = TRUE`, drop PERM variables that are entirely empty (using
#'    type-aware emptiness checks).
#' 6. Drop variables not present in metadata; **preserve numeric types**; replace
#'    `NA` with `""` **only** for character columns.
#' 7. Re-sort by KEYS.
#' 8. Write outputs under `out_root`:
#'    - **RDS** to `RDA/<domain>.rds` via `saveRDS()`.
#'    - **CSV** to `CSV/<domain>.csv` via `utils::write.csv(..., na = "")`.
#'    - **XPT v8** to `XPT/<domain>.xpt` via `haven::write_xpt(..., version = 8)`.
#'
#' @section Dependencies:
#' Requires the **haven** package for XPT export. If not installed, the function
#' attempts to install it from CRAN.
#'
#' @examples
#' \dontrun{
#' sdtm_domain <- "CO"
#' out <- sdtm_create_domain(
#'   inset    = co_input,
#'   keys     = c("STUDYID", "USUBJID", "COSEQ"),
#'   delperm  = TRUE,
#'   out_root = "../70 QC"
#' )
#' # Files created:
#' #   ../70 QC/RDA/co.rds
#' #   ../70 QC/CSV/co.csv
#' #   ../70 QC/XPT/co.xpt
#' # Dataset label: "SDTM domain CO"
#' }
suppressPackageStartupMessages({
  if (!requireNamespace("haven", quietly = TRUE)) {
    message("Installing 'haven' ...")
    install.packages("haven", repos = "https://cloud.r-project.org")
  }
})

sdtm_create_domain <- function(inset,
                               keys = NULL,
                               delperm = TRUE,
                               out_root = "../70 QC",
                               addsuppvars = NULL) {
  
  stopifnot(is.data.frame(inset), is.data.frame(sdtm_var), is.data.frame(sdtm_dom))
  
  # --- helpers ---
  `%||%` <- function(a, b) if (is.null(a)) b else a
  .mkdir <- function(p) if (!dir.exists(p)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
  .clean <- function(x) toupper(trimws(gsub("\u00A0", " ", as.character(x))))
  .all_emp_chr <- function(x) all(is.na(x) | trimws(x) == "")
  .all_emp_num <- function(x) all(is.na(x))
  .split_keys <- function(x) {
    if (is.null(x) || length(x) == 0) return(character(0))
    if (length(x) > 1L) x <- paste(x, collapse = ",")
    toks <- unlist(strsplit(x, "\\s*,\\s*"))
    toks <- gsub("[^A-Za-z0-9_]", "", toks)
    toupper(trimws(toks[nzchar(toks)]))
  }
  # more permissive splitter for addsuppvars: commas or spaces
  .split_vars <- function(x) {
    if (is.null(x) || length(x) == 0) return(character(0))
    if (length(x) > 1L) {
      toks <- x
    } else {
      toks <- unlist(strsplit(x, "[,\\s]+"))
    }
    toks <- gsub("[^A-Za-z0-9_]", "", toks)
    toupper(trimws(toks[nzchar(toks)]))
  }
  
  # 0) uppercase input names
  names(inset) <- toupper(names(inset))
  
  # resolve DOMAIN from global sdtm_domain
  if (is.null(sdtm_domain)) {
    if (!exists("sdtm_domain", inherits = TRUE))
      stop("sdtm_create_domain: 'sdtm_domain' must be provided or defined globally.")
    sdtm_domain <- get("sdtm_domain", inherits = TRUE)
  }
  dom_u <- toupper(sdtm_domain)
  dom_lc <- tolower(dom_u)
  
  # 1) keys
  dom_row <- sdtm_dom[.clean(sdtm_dom$DOMAIN) == dom_u, , drop = FALSE]
  if (!nrow(dom_row)) stop("sdtm_create_domain: no row in sdtm_dom for DOMAIN=", dom_u)
  
  if (!is.null(keys) && length(.split_keys(keys))) {
    keys_vec <- .split_keys(keys)
    source_keys <- "override"
  } else {
    keys_raw <- if ("KEYS" %in% names(dom_row)) as.character(dom_row$KEYS[1]) else NA_character_
    if (!nzchar(keys_raw)) stop("sdtm_create_domain: KEYS missing/empty for DOMAIN=", dom_u)
    keys_vec <- .split_keys(keys_raw)
    source_keys <- "metadata"
  }
  
  nm_in_clean <- .clean(names(inset))
  key_cols <- names(inset)[match(.clean(keys_vec), nm_in_clean)]
  if (anyNA(key_cols)) {
    miss <- keys_vec[is.na(key_cols)]
    stop("sdtm_create_domain: KEYS not found in inset: ", paste(miss, collapse = ", "))
  }
  
  # 1b) additional supplemental variables (addsuppvars)
  addsupp_clean <- .split_vars(addsuppvars)
  addsupp_cols <- character(0)
  if (length(addsupp_clean)) {
    idx_add <- match(addsupp_clean, nm_in_clean)
    if (anyNA(idx_add)) {
      miss_add <- addsupp_clean[is.na(idx_add)]
      warning(
        "sdtm_create_domain: addsuppvars not found in inset and will be ignored: ",
        paste(miss_add, collapse = ", ")
      )
      idx_add <- idx_add[!is.na(idx_add)]
    }
    if (length(idx_add)) {
      # keep them in the order specified by addsuppvars, de-duplicated
      addsupp_cols <- unique(names(inset)[idx_add])
    }
  }
  
  # 2) variable metadata
  mv0 <- sdtm_var[.clean(sdtm_var$DOMAIN) == dom_u, , drop = FALSE]
  if (!nrow(mv0)) stop("sdtm_create_domain: no variable metadata for ", dom_u)
  
  need <- c("VARNAME", "VARLABEL", "CORE")
  miss_cols <- setdiff(need, names(mv0))
  if (length(miss_cols)) stop("sdtm_create_domain: sdtm_var missing: ", paste(miss_cols, collapse = ", "))
  
  mv0$VARNAME_C <- .clean(mv0$VARNAME)
  mv0$VARLABEL <- trimws(as.character(mv0$VARLABEL))
  mv0$CORE_C <- .clean(mv0$CORE)
  
  # expected type from metadata (if ATTR_LENGTH present: '$' -> char; else num)
  has_len <- "ATTR_LENGTH" %in% names(mv0)
  if (has_len) {
    mv0$ATTR_LENGTH <- as.character(mv0$ATTR_LENGTH)
    mv0$TYPE_META <- ifelse(grepl("^\\s*\\$", mv0$ATTR_LENGTH %||% ""), "char", "num")
  } else {
    mv0$TYPE_META <- NA_character_
  }
  
  allowed_order <- mv0$VARNAME_C[!duplicated(mv0$VARNAME_C)]
  
  lab_map <- tapply(seq_len(nrow(mv0)), mv0$VARNAME_C, function(idx) {
    labs <- mv0$VARLABEL[idx]
    ne <- which(nzchar(labs))
    if (length(ne)) labs[ne[1]] else labs[1]
  })
  core_map <- tapply(seq_len(nrow(mv0)), mv0$VARNAME_C, function(idx) mv0$CORE_C[idx][1])
  type_map <- tapply(seq_len(nrow(mv0)), mv0$VARNAME_C, function(idx) mv0$TYPE_META[idx][1])
  
  # 3) REQ/EXP check
  inset_names_clean <- .clean(names(inset))
  req_exp <- names(core_map)[core_map %in% c("REQ", "EXP")]
  miss_req <- setdiff(req_exp, inset_names_clean)
  if (length(miss_req)) stop("sdtm_create_domain: missing REQ/EXP var(s): ", paste(miss_req, collapse = ", "))
  
  # 4) drop PERM all-empty (type-aware)
  if (isTRUE(delperm)) {
    perm_c <- names(core_map)[core_map == "PERM"]
    perm_idx <- match(perm_c, inset_names_clean)
    perm_idx <- perm_idx[!is.na(perm_idx)]
    if (length(perm_idx)) {
      drop_mask <- logical(length(perm_idx))
      for (ii in seq_along(perm_idx)) {
        v <- names(inset)[perm_idx[ii]]
        drop_mask[ii] <- if (is.character(inset[[v]])) .all_emp_chr(inset[[v]]) else .all_emp_num(inset[[v]])
      }
      if (any(drop_mask)) {
        dropped_perm <- names(inset)[perm_idx[drop_mask]]
        inset[perm_idx[drop_mask]] <- NULL
        message(
          "sdtm_create_domain: dropped ", length(dropped_perm),
          " PERM all-empty var(s): ", paste(dropped_perm, collapse = ", ")
        )
        inset_names_clean <- .clean(names(inset))
        # nm_in_clean is only used for addsuppvars, which refer to original inset;
        # PERM vars are metadata vars, so this is fine.
      }
    }
  }
  
  # 5) enforce variable set & order from metadata + addsuppvars
  inset_names_clean <- .clean(names(inset)) # refresh after possible drops
  
  keep_meta_idx <- match(allowed_order, inset_names_clean)
  keep_meta_idx <- keep_meta_idx[!is.na(keep_meta_idx)]
  if (!length(keep_meta_idx)) {
    stop("sdtm_create_domain: after metadata filter, no metadata variables remain.")
  }
  
  addsupp_idx <- integer(0)
  if (length(addsupp_cols)) {
    addsupp_idx <- match(addsupp_cols, names(inset))
    addsupp_idx <- addsupp_idx[!is.na(addsupp_idx)]
  }
  
  # final keep index: metadata vars (ordered by metadata) + extra addsuppvars (after, no duplicates)
  keep_idx <- c(keep_meta_idx, setdiff(addsupp_idx, keep_meta_idx))
  
  if (length(addsupp_cols)) {
    message(
      "sdtm_create_domain: kept additional supplemental var(s) from addsuppvars: ",
      paste(unique(addsupp_cols), collapse = ", ")
    )
  }
  
  not_kept_idx <- setdiff(seq_along(inset), keep_idx)
  if (length(not_kept_idx)) {
    not_kept <- names(inset)[not_kept_idx]
    message(
      "sdtm_create_domain: removed ", length(not_kept),
      " non-metadata var(s) not in addsuppvars: ",
      paste(not_kept, collapse = ", ")
    )
  }
  
  inset <- inset[, keep_idx, drop = FALSE]
  
  # 6) apply types & labels (no global character coercion; NA->"" only for chars)
  unlabeled <- character(0)
  final_clean_names <- .clean(names(inset))
  
  for (i in seq_along(inset)) {
    nm_clean <- final_clean_names[i]
    
    # Safe lookup: addsuppvars are not in lab_map/type_map
    if (nm_clean %in% names(type_map)) {
      tpe <- type_map[[nm_clean]] # "char" / "num" / NA
    } else {
      tpe <- NA_character_
    }
    
    if (nm_clean %in% names(lab_map)) {
      lbl <- lab_map[[nm_clean]]
    } else {
      lbl <- NULL
    }
    
    # Only coerce when we have metadata type information
    if (!is.na(tpe) && tpe == "char" && !is.character(inset[[i]])) {
      inset[[i]] <- as.character(inset[[i]])
    } else if (!is.na(tpe) && tpe == "num" && !is.numeric(inset[[i]])) {
      suppressWarnings(inset[[i]] <- as.numeric(inset[[i]]))
    } # else: leave as-is (includes addsuppvars, which have tpe = NA)
    
    if (is.character(inset[[i]])) {
      x <- inset[[i]]
      x[is.na(x)] <- ""
      inset[[i]] <- x
    }
    
    if (!is.null(lbl) && nzchar(lbl)) {
      attr(inset[[i]], "label") <- lbl
    } else {
      unlabeled <- c(unlabeled, names(inset)[i])
    }
  }
  
  # 7) sort by KEYS using SAS-like alphabetical order
  ord1 <- do.call(order, lapply(key_cols, function(k) {
    x <- as.character(inset[[k]])
    # SAS-like sorting: lowercase, trimmed, then by length
    order(tolower(trimws(x)), nchar(trimws(x)))
  }))
  inset <- inset[ord1, , drop = FALSE]
  
  # 8) DATASET LABEL: "SDTM domain <DOMAIN_U>"
  dlabel <- paste("SDTM domain", dom_u)
  attr(inset, "label") <- dlabel
  attr(inset, "domain") <- dom_u
  
  # 9) multi-export: RDS, CSV, XPT(v8)
  out_dir_rda <- file.path(out_root, "RDA")
  out_dir_csv <- file.path(out_root, "CSV")
  out_dir_xpt <- file.path(out_root, "XPT")
  .mkdir(out_dir_rda); .mkdir(out_dir_csv); .mkdir(out_dir_xpt)
  
  f_rds <- file.path(out_dir_rda, paste0("qc_", dom_lc, ".rds"))
  f_csv <- file.path(out_dir_csv, paste0("qc_", dom_lc, ".csv"))
  f_xpt <- file.path(out_dir_xpt, paste0("qc_", dom_lc, ".xpt"))
  
  # RDS
  try({
    saveRDS(inset, f_rds)
    message("RDS written: ", f_rds)
  }, silent = TRUE)
  
  # CSV: NA -> "" ONLY for characters (already applied); numeric NA stay NA (CSV blanks)
  try({
    utils::write.csv(inset, f_csv, row.names = FALSE, na = "")
    message("CSV written: ", f_csv)
  }, silent = TRUE)
  
  # XPT v8 (variable labels preserved).
  try({
    haven::write_xpt(inset, path = f_xpt, name = toupper(dom_u), version = 8)
    message("XPT v8 written: ", f_xpt)
  }, silent = TRUE)
  
  message(sprintf(
    "sdtm_create_domain: finalized %s | vars=%d | rows=%d | KEYS used (%s): %s | saved to RDA/CSV/XPT.",
    dom_lc, ncol(inset), nrow(inset), source_keys, paste(key_cols, collapse = ", ")
  ))
  
  return(inset)
}