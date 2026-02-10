# ==============================================================================
# Module K: Export
# ==============================================================================

#' Export a domain dataset to XPT (SAS transport v5) format
#'
#' Writes a properly labelled SDTM domain to `.xpt` using `haven::write_xpt()`.
#'
#' @param data Tibble. The domain dataset.
#' @param domain Character. Domain code.
#' @param output_dir Character. Directory for output.
#' @param target_meta Tibble or `NULL`. If provided, labels/types are enforced.
#' @param max_label_length Integer. Default 40.
#' @param version Integer. SAS transport version (5 or 8). Default 5.
#' @return Invisible file path.
#' @export
export_xpt <- function(data, domain, output_dir,
                       target_meta = NULL, domain_meta = NULL,
                       max_label_length = 40L,
                       version = 5L) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  domain <- toupper(domain)
  fname  <- paste0(tolower(domain), ".xpt")
  fpath  <- file.path(output_dir, fname)

  # Apply labels, type enforcement, and width from target_meta if provided
  if (!is.null(target_meta)) {
    dom_meta <- dplyr::filter(target_meta, .data[["domain"]] == .env[["domain"]])
    for (i in seq_len(nrow(dom_meta))) {
      v <- dom_meta$var[i]
      if (!v %in% names(data)) next

      # Apply label
      if (!is.na(dom_meta$label[i])) {
        lbl <- substr(dom_meta$label[i], 1, max_label_length)
        attr(data[[v]], "label") <- lbl
      }

      # Set width attribute for character variables (used by haven for xpt)
      if ("length" %in% names(dom_meta) && is.character(data[[v]])) {
        max_len <- dom_meta$length[i]
        if (!is.na(max_len) && is.numeric(max_len)) {
          attr(data[[v]], "width") <- as.integer(max_len)
        }
      }

      # Apply significant_digits rounding for numeric variables
      if ("significant_digits" %in% names(dom_meta) && is.numeric(data[[v]])) {
        sig_d <- dom_meta$significant_digits[i]
        if (!is.na(sig_d) && is.numeric(sig_d)) {
          data[[v]] <- round(data[[v]], digits = as.integer(sig_d))
        }
      }
    }
  }

  # Set dataset label from domain_meta (if available) or fallback to domain code
  if (!is.null(domain_meta)) {
    dom_info <- dplyr::filter(domain_meta, .data[["domain"]] == .env[["domain"]])
    if (nrow(dom_info) > 0L && "description" %in% names(dom_info) &&
        !is.na(dom_info$description[1L])) {
      attr(data, "label") <- substr(dom_info$description[1L], 1, max_label_length)
    } else {
      attr(data, "label") <- domain
    }
  } else {
    attr(data, "label") <- domain
  }

  haven::write_xpt(data, fpath, version = version)
  cli::cli_alert_success("Exported {domain} -> {fpath} ({nrow(data)} rows)")
  invisible(fpath)
}

#' Export a domain to RDS and CSV
#'
#' @param data Tibble. Domain dataset.
#' @param domain Character.
#' @param output_dir Character.
#' @param formats Character vector. Subset of `c("rds","csv","rda")`.
#' @return Invisible named list of paths.
#' @export
export_rds_csv <- function(data, domain, output_dir,
                           formats = c("rds", "csv")) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  domain <- toupper(domain)
  paths <- list()

  if ("rds" %in% formats) {
    p <- file.path(output_dir, paste0(tolower(domain), ".rds"))
    saveRDS(data, p)
    paths$rds <- p
  }
  if ("csv" %in% formats) {
    p <- file.path(output_dir, paste0(tolower(domain), ".csv"))
    readr::write_csv(data, p)
    paths$csv <- p
  }
  if ("rda" %in% formats) {
    p <- file.path(output_dir, paste0(tolower(domain), ".rda"))
    # Save with the domain name as the variable name (e.g., AE, DM)
    assign(domain, data)
    save(list = domain, file = p)
    paths$rda <- p
  }

  fmts_done <- intersect(formats, names(paths))
  cli::cli_alert_success("Exported {domain}: {paste(fmts_done, collapse=', ')}")
  invisible(paths)
}

#' Write define.xml support file
#'
#' Generates a CSV that maps variables to metadata for define.xml generation.
#'
#' @param domains Named list of built domain tibbles.
#' @param target_meta Tibble.
#' @param config `sdtm_config`.
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
#' @param vlm Tibble or `NULL`.
#' @param output_path Character.
#' @return Invisible file path or `NULL`.
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
