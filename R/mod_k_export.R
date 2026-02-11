# ==============================================================================
# Module K: Export
# ==============================================================================
# Functions: export_domain, write_define_support, write_codelist_support,
#            write_value_level_support, write_origin_support
# ==============================================================================

#' Export an SDTM domain dataset
#'
#' Single, unified export function that finalizes a domain dataset using
#' metadata (ordering, labelling, type enforcement) and writes it to one
#' or more formats.
#'
#' @param data Data frame / tibble. The domain dataset to export.
#' @param domain Character. SDTM domain code (e.g., \code{"AE"}).
#' @param output_dir Character. Root output directory. Sub-directories
#'   \code{XPT/}, \code{RDA/}, \code{CSV/} are created when the
#'   corresponding format is requested.
#' @param formats Character vector. Which output formats to produce.
#'   Any combination of \code{"xpt"}, \code{"rds"}, \code{"csv"},
#'   \code{"rda"}. Default \code{c("xpt")}.
#' @param xpt_version Integer. SAS transport version: 5 or 8. Default 8.
#' @param target_meta Tibble or \code{NULL}. Variable-level metadata with
#'   \code{domain}, \code{var}, \code{label}, \code{core} (and optionally
#'   \code{type}, \code{length}, \code{significant_digits}). Used to
#'   enforce column order, labels, widths, rounding, and to drop all-empty
#'   PERM variables.
#' @param domain_meta Tibble or \code{NULL}. Dataset-level metadata with
#'   \code{domain} and \code{description}.
#' @param keys Character vector or \code{NULL}. Sort keys. If \code{NULL},
#'   uses \code{STUDYID}, \code{USUBJID}, \code{<domain>SEQ}.
#' @param drop_empty_perm Logical. If \code{TRUE} (default), drop PERM
#'   variables that are entirely empty (all NA / blank).
#' @param max_label_length Integer. Maximum label length. Default 40.
#' @return Invisible tibble: the finalized dataset.
#'
#' @examples
#' \dontrun{
#' # Export to XPT v8 (default)
#' export_domain(ae, "AE", "sdtm/datasets", target_meta = target_meta)
#'
#' # Export to XPT v8 + CSV
#' export_domain(ae, "AE", "sdtm/datasets", formats = c("xpt", "csv"))
#'
#' # Export to all formats with XPT v5
#' export_domain(ae, "AE", "sdtm/datasets",
#'               formats = c("xpt", "rds", "csv", "rda"),
#'               xpt_version = 5L)
#' }
#'
#' @export
export_domain <- function(data, domain, output_dir,
                          formats = c("xpt"),
                          xpt_version = 8L,
                          target_meta = NULL,
                          domain_meta = NULL,
                          keys = NULL,
                          drop_empty_perm = TRUE,
                          max_label_length = 40L) {
  domain <- toupper(domain)
  dom_lc <- tolower(domain)


  # Uppercase column names
  names(data) <- toupper(names(data))

  # 1. Variable metadata for this domain
  dom_meta <- NULL
  if (!is.null(target_meta)) {
    dom_meta <- dplyr::filter(target_meta, toupper(.data[["domain"]]) == domain)
    if (nrow(dom_meta) == 0L) {
      cli::cli_alert_warning(
        "export_domain: no variable metadata for {domain}; skipping metadata enforcement."
      )
      dom_meta <- NULL
    }
  }

  # 2. Resolve keys
  if (is.null(keys) || length(keys) == 0L) {
    seq_var <- paste0(domain, "SEQ")
    keys <- c("STUDYID", "USUBJID", seq_var)
  }
  keys <- toupper(keys)
  avail_keys <- intersect(keys, names(data))

  # 3. Sort by keys
  if (length(avail_keys) > 0L) {
    data <- dplyr::arrange(data, dplyr::across(dplyr::all_of(avail_keys)))
  }

  # 4. Enforce metadata variable order, labels, and core checks
  if (!is.null(dom_meta)) {
    core_map <- stats::setNames(toupper(dom_meta$core), toupper(dom_meta$var))

    # Check REQ/EXP
    req_exp_vars <- names(core_map)[core_map %in% c("REQ", "EXP")]
    missing_req <- setdiff(req_exp_vars, names(data))
    if (length(missing_req) > 0L) {
      cli::cli_alert_warning(
        "export_domain: missing REQ/EXP var(s): {paste(missing_req, collapse=', ')}"
      )
    }

    # Drop all-empty PERM variables
    if (isTRUE(drop_empty_perm)) {
      perm_vars <- names(core_map)[core_map == "PERM"]
      perm_in_data <- intersect(perm_vars, names(data))
      if (length(perm_in_data) > 0L) {
        all_empty <- vapply(perm_in_data, function(v) {
          col <- data[[v]]
          if (is.character(col)) all(is.na(col) | trimws(col) == "")
          else all(is.na(col))
        }, logical(1))
        drop_cols <- perm_in_data[all_empty]
        if (length(drop_cols) > 0L) {
          data <- data[, setdiff(names(data), drop_cols), drop = FALSE]
          cli::cli_alert_info(
            "Dropped {length(drop_cols)} all-empty PERM var(s): {paste(drop_cols, collapse=', ')}"
          )
        }
      }
    }

    # Reorder columns per metadata + any extra columns at end
    meta_order <- toupper(dom_meta$var)
    meta_in_data <- intersect(meta_order, names(data))
    extra_cols <- setdiff(names(data), meta_order)
    data <- data[, c(meta_in_data, extra_cols), drop = FALSE]

    # Apply labels, widths, and rounding
    for (i in seq_len(nrow(dom_meta))) {
      v <- toupper(dom_meta$var[i])
      if (!v %in% names(data)) next
      lbl <- dom_meta$label[i]
      if (!is.na(lbl) && nzchar(lbl)) {
        attr(data[[v]], "label") <- substr(lbl, 1, max_label_length)
      }
      # Width for character variables
      if ("length" %in% names(dom_meta) && is.character(data[[v]])) {
        w <- dom_meta$length[i]
        if (!is.na(w) && is.numeric(w)) {
          attr(data[[v]], "width") <- as.integer(w)
        }
      }
      # Rounding for numeric variables
      if ("significant_digits" %in% names(dom_meta) && is.numeric(data[[v]])) {
        sig_d <- dom_meta$significant_digits[i]
        if (!is.na(sig_d) && is.numeric(sig_d)) {
          data[[v]] <- round(data[[v]], digits = as.integer(sig_d))
        }
      }
    }
  }

  # 5. Replace NA with "" for character columns
  for (v in names(data)) {
    if (is.character(data[[v]])) {
      data[[v]][is.na(data[[v]])] <- ""
    }
  }

  # 6. Dataset label
  ds_label <- paste("SDTM domain", domain)
  if (!is.null(domain_meta)) {
    dom_info <- dplyr::filter(domain_meta, toupper(.data[["domain"]]) == domain)
    if (nrow(dom_info) > 0L && "description" %in% names(dom_info) &&
        !is.na(dom_info$description[1L])) {
      ds_label <- substr(dom_info$description[1L], 1, max_label_length)
    }
  }
  attr(data, "label") <- ds_label

  # 7. Multi-format export
  if ("xpt" %in% formats) {
    xpt_dir <- file.path(output_dir, "XPT")
    dir.create(xpt_dir, recursive = TRUE, showWarnings = FALSE)
    xpt_path <- file.path(xpt_dir, paste0(dom_lc, ".xpt"))
    haven::write_xpt(data, path = xpt_path, version = xpt_version)
    cli::cli_alert_success("XPT v{xpt_version} written: {xpt_path}")
  }

  if ("rds" %in% formats) {
    rda_dir <- file.path(output_dir, "RDA")
    dir.create(rda_dir, recursive = TRUE, showWarnings = FALSE)
    rds_path <- file.path(rda_dir, paste0(dom_lc, ".rds"))
    saveRDS(data, rds_path)
    cli::cli_alert_success("RDS written: {rds_path}")
  }

  if ("csv" %in% formats) {
    csv_dir <- file.path(output_dir, "CSV")
    dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)
    csv_path <- file.path(csv_dir, paste0(dom_lc, ".csv"))
    utils::write.csv(data, csv_path, row.names = FALSE, na = "")
    cli::cli_alert_success("CSV written: {csv_path}")
  }

  if ("rda" %in% formats) {
    rda_dir <- file.path(output_dir, "RDA")
    dir.create(rda_dir, recursive = TRUE, showWarnings = FALSE)
    rda_path <- file.path(rda_dir, paste0(dom_lc, ".rda"))
    assign(domain, data)
    save(list = domain, file = rda_path)
    cli::cli_alert_success("RDA written: {rda_path}")
  }

  cli::cli_alert_success(
    "export_domain: {domain} | {ncol(data)} vars | {nrow(data)} rows | formats: {paste(formats, collapse=', ')}"
  )

  invisible(data)
}


# --- Support file writers -----------------------------------------------------

#' Write define.xml support file
#'
#' Generates a CSV that maps variables to metadata for define.xml generation.
#'
#' @param domains Named list of built domain tibbles.
#' @param target_meta Tibble.
#' @param config \code{sdtm_config}.
#' @param output_path Character.
#' @return Invisible file path.
#' @export
write_define_support <- function(domains, target_meta, config, output_path) {
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

  rows <- list()
  for (dom_name in names(domains)) {
    dom_data <- domains[[dom_name]]
    dom_meta <- dplyr::filter(target_meta, .data$domain == dom_name)
    for (i in seq_len(nrow(dom_meta))) {
      v <- dom_meta$var[i]
      rows <- c(rows, list(tibble::tibble(
        Domain    = dom_name,
        Variable  = v,
        Label     = dom_meta$label[i],
        Type      = dom_meta$type[i],
        Length    = if ("length" %in% names(dom_meta)) dom_meta$length[i] else NA_integer_,
        SignificantDigits = if ("significant_digits" %in% names(dom_meta)) dom_meta$significant_digits[i] else NA_integer_,
        Core      = dom_meta$core[i],
        Origin    = dom_meta$origin[i] %||% NA_character_,
        Codelist  = if ("codelist_id" %in% names(dom_meta)) dom_meta$codelist_id[i] else NA_character_,
        HasData   = v %in% names(dom_data),
        MaxLength = if (v %in% names(dom_data) && is.character(dom_data[[v]])) {
          max(nchar(dom_data[[v]][!is.na(dom_data[[v]])]), 0, na.rm = TRUE)
        } else NA_integer_
      )))
    }
  }

  define_df <- dplyr::bind_rows(rows)
  readr::write_csv(define_df, output_path)
  cli::cli_alert_success("Define support written to {output_path}")
  invisible(output_path)
}

#' Write codelist support file
#' @param ct_lib Tibble.
#' @param output_path Character.
#' @return Invisible file path.
#' @export
write_codelist_support <- function(ct_lib, output_path) {
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(ct_lib, output_path)
  cli::cli_alert_success("Codelist support written to {output_path}")
  invisible(output_path)
}

#' Write value-level metadata support file
#' @param vlm Tibble or \code{NULL}.
#' @param output_path Character.
#' @return Invisible file path or \code{NULL}.
#' @export
write_value_level_support <- function(vlm = NULL, output_path) {
  if (is.null(vlm) || nrow(vlm) == 0L) return(invisible(NULL))
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(vlm, output_path)
  invisible(output_path)
}

#' Write origin metadata support file
#' @param target_meta Tibble.
#' @param output_path Character.
#' @return Invisible file path.
#' @export
write_origin_support <- function(target_meta, output_path) {
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

  origins <- target_meta[, c("domain", "var", "rule_type"), drop = FALSE]
  origins$origin <- dplyr::case_when(
    origins$rule_type == "constant"   ~ "Assigned",
    origins$rule_type == "direct_map" ~ "CRF",
    origins$rule_type == "ct_assign"  ~ "CRF",
    origins$rule_type == "iso_dtc"    ~ "CRF",
    origins$rule_type == "dy"         ~ "Derived",
    origins$rule_type == "seq"        ~ "Derived",
    TRUE                              ~ "Other"
  )

  readr::write_csv(origins, output_path)
  cli::cli_alert_success("Origin support written to {output_path}")
  invisible(output_path)
}
