# ==============================================================================
# Module J: Code Generation
# ==============================================================================

#' Generate a standalone R script for a single domain
#'
#' Produces a fully commented, self-contained R program that reproduces the
#' derivation for one domain.
#'
#' @param domain Character.
#' @param rule_set `rule_set`.
#' @param target_meta Tibble.
#' @param config `sdtm_config`.
#' @param style Character. `"tidyverse"` or `"base"`.
#' @param include_comments Logical. Default `TRUE`.
#' @param output_path Character or `NULL`.
#' @return Character string of the generated script (invisibly if written).
#' @export
gen_domain_script <- function(domain, rule_set, target_meta,
                              config, style = "tidyverse",
                              include_comments = TRUE,
                              output_path = NULL) {
  domain <- toupper(domain)
  dom_rules <- rule_set$rules[[domain]]
  dom_meta  <- dplyr::filter(target_meta, .data[["domain"]] == .env[["domain"]])

  if (is.null(dom_rules)) abort(glue::glue("No rules for domain {domain}"))

  lines <- character()
  .add <- function(...) lines <<- c(lines, paste0(...))

  studyid_hdr <- if (!is.null(config$studyid)) toupper(config$studyid) else "UNKNOWN"
  .add("# ============================================================")
  .add(glue::glue("# Study:     {studyid_hdr}"))
  .add(glue::glue("# Domain:    {domain}"))
  .add(glue::glue("# Purpose:   SDTM {domain} Domain Build Script"))
  .add(glue::glue("# Generated: sdtmbuilder v{utils::packageVersion('sdtmbuilder')}"))
  .add(glue::glue("# Date:      {Sys.Date()}"))
  .add("# ============================================================")
  .add("")
  .add("library(dplyr)")
  .add("library(sdtmbuilder)")
  .add("")
  .add("# --- Load source data ---")

  # Determine source datasets
  sources <- unique(unlist(lapply(dom_rules, function(r) r$params$dataset)))
  sources <- sources[!is.na(sources)]
  for (src in sources) {
    .add(glue::glue('{src} <- read.csv("data/raw/{src}.csv", stringsAsFactors = FALSE)'))
  }
  .add("")

  .add("# --- Load metadata and config ---")
  .add('config      <- yaml::read_yaml("metadata/config.yaml")')
  .add('study_meta  <- read_study_metadata_excel("metadata/Study_Metadata.xlsx")')
  .add('ct_lib      <- read_study_ct_excel("metadata/Study_CT.xlsx")')
  .add('target_meta <- study_meta$target_meta')
  .add('domain_meta <- study_meta$domain_meta')
  .add('value_level_meta <- study_meta$value_level_meta')
  .add('')
  .add("# Expand value-level metadata into target_meta")
  .add("if (nrow(value_level_meta) > 0L) {")
  .add("  target_meta <- expand_value_level_meta(target_meta, value_level_meta)")
  .add("}")
  .add("")

  # Build dependency order
  dep <- build_dependency_graph(rule_set, domain)

  .add(glue::glue("# --- Derive variables ({length(dep$order)} variables) ---"))
  .add("")
  .add(glue::glue("data <- {sources[1]}"))
  .add("")

  for (var_name in dep$order) {
    rule <- dom_rules[[var_name]]
    if (is.null(rule)) next

    if (include_comments) {
      # Show the METHOD call from Excel (if available)
      method_str <- rule$method_string
      if (is.null(method_str) || is.na(method_str)) {
        method_str <- reconstruct_method_string(rule$type, rule$params)
      }
      .add(glue::glue("# {var_name}: {method_str}"))
      if (!is.null(rule$params$dataset)) {
        .add(glue::glue("#   source: {rule$params$dataset}.{rule$params$column %||% ''}"))
      }
    }

    .add(render_rule_code(var_name, rule, style))
    .add("")
  }

  .add("# --- Finalize: apply significant digits, length, variable order ---")
  target_vars <- paste0('"', dom_meta$var, '"', collapse = ", ")
  .add(glue::glue("target_vars <- c({target_vars})"))
  .add("data <- data[, intersect(target_vars, names(data)), drop = FALSE]")
  .add("")

  # Add sig digits and length enforcement comments
  sig_rows <- dplyr::filter(dom_meta, !is.na(.data$significant_digits))
  if (nrow(sig_rows) > 0L) {
    .add("# Apply significant digits rounding")
    for (i in seq_len(nrow(sig_rows))) {
      v <- sig_rows$var[i]
      d <- sig_rows$significant_digits[i]
      .add(glue::glue('if ("{v}" %in% names(data)) data[["{v}"]] <- signif(data[["{v}"]], {d})'))
    }
    .add("")
  }

  len_rows <- dplyr::filter(dom_meta, !is.na(.data$length))
  char_len <- dplyr::filter(len_rows, .data$type %in% c("Char", "char", "character", "text"))
  if (nrow(char_len) > 0L) {
    .add("# Enforce character variable lengths")
    for (i in seq_len(nrow(char_len))) {
      v <- char_len$var[i]
      l <- char_len$length[i]
      .add(glue::glue('if ("{v}" %in% names(data)) data[["{v}"]] <- substr(data[["{v}"]], 1, {l})'))
    }
    .add("")
  }

  .add(glue::glue("# --- Export ---"))
  .add(glue::glue('export_domain(data, domain = "{domain}", output_dir = "output",'))
  .add(glue::glue('              formats = "xpt", target_meta = target_meta)'))

  script <- paste(lines, collapse = "\n")

  if (!is.null(output_path)) {
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    writeLines(script, output_path)
    cli::cli_alert_success("Script written to {output_path}")
    return(invisible(script))
  }

  script
}

#' Render R code for a single rule
#' @param var Character. Variable name.
#' @param rule List. Rule object.
#' @param style Character.
#' @return Character.
#' @keywords internal
render_rule_code <- function(var, rule, style = "tidyverse") {
  p <- rule$params
  switch(rule$type,
    constant = {
      val <- if (is.character(p$value)) paste0('"', p$value, '"') else p$value
      glue::glue('data${var} <- {val}')
    },
    direct_map = {
      xform <- if (!is.null(p$transform)) paste0(p$transform, "(") else ""
      xform_close <- if (!is.null(p$transform)) ")" else ""
      glue::glue('data${var} <- {xform}data${p$column}{xform_close}')
    },
    ct_assign = {
      glue::glue('data <- assign_ct(data, "{var}", "{p$column}", "{p$codelist_id}", ct_lib)')
    },
    iso_dtc = {
      dc <- if (is.list(p$date_col)) p$date_col$column else p$date_col
      glue::glue('data${var} <- format_iso_dtc(parse_partial_date(data${dc}))')
    },
    dy = {
      glue::glue('data <- derive_dy(data, "{var}", "{p$dtc_var}", "{p$ref_var %||% "RFSTDTC"}")')
    },
    seq = {
      by_str <- paste0('"', unlist(p$by), '"', collapse = ", ")
      glue::glue('data <- derive_seq(data, "{var}", by = c({by_str}))')
    },
    epoch = {
      glue::glue('data <- derive_epoch(data, "{var}", "{p$dtc_var}", config$epoch_map, "{p$ref_var %||% "RFSTDTC"}")')
    },
    visitnum = {
      glue::glue('data <- derive_visitnum(data, "{var}", "{p$visit_var %||% "VISIT"}")')
    },
    visitdy = {
      glue::glue('data <- derive_visitdy(data, "{var}", "{p$dtc_var}", "{p$ref_var %||% "RFSTDTC"}")')
    },
    tpt = {
      glue::glue('data <- derive_tpt(data, "{var}",',
                  ' tpt_col = "{p$tpt_col %||% ""}", tptnum_col = "{p$tptnum_col %||% ""}")')
    },
    numeric_round = {
      digits <- rule$significant_digits %||% p$significant_digits %||% 3L
      glue::glue('data${var} <- signif(as.numeric(data${p$column}), {digits})')
    },
    duration = {
      glue::glue('data <- derive_duration(data, "{var}", "{p$start_dtc}", "{p$end_dtc}")')
    },
    usubjid = ,
    unusbjid = {
      glue::glue('data <- derive_usubjid(data, studyid = "{p$studyid %||% "STUDYID"}")')
    },
    baseline_flag = {
      glue::glue('data <- derive_baseline_flag(data, "{var}")')
    },
    lastobs_flag = {
      glue::glue('data <- derive_lastobs_flag(data, "{var}")')
    },
    case_when = {
      glue::glue('# [case_when] Derive {var} (see rule params for branch logic)')
    },
    coalesce = {
      cols <- paste0('"', unlist(p$columns), '"', collapse = ", ")
      glue::glue('data <- derive_coalesce(data, "{var}", c({cols}))')
    },
    concat = {
      cols <- paste0('"', unlist(p$columns), '"', collapse = ", ")
      sep <- p$separator %||% " "
      glue::glue('data <- derive_concat(data, "{var}", c({cols}), sep = "{sep}")')
    },
    if_else = {
      glue::glue('# [if_else] Derive {var} (see rule params for condition)')
    },
    regex_extract = {
      glue::glue('data <- derive_regex_extract(data, "{var}", "{p$column}", "{p$pattern}")')
    },
    regex_replace = {
      glue::glue('data <- derive_regex_replace(data, "{var}", "{p$column}", "{p$pattern}", "{p$replacement}")')
    },
    glue::glue('# [MANUAL] Derive {var} (rule_type: {rule$type})')
  )
}

#' Generate shared utility script
#' @param config `sdtm_config`.
#' @param output_path Character or `NULL`.
#' @return Character.
#' @export
gen_shared_utils_script <- function(config, output_path = NULL) {
  script <- paste(
    "# Shared utilities for SDTM build",
    glue::glue("# Study: {config$studyid}"),
    "",
    "library(sdtmbuilder)",
    "library(dplyr)",
    "",
    glue::glue('STUDYID <- "{config$studyid}"'),
    "",
    sep = "\n"
  )

  if (!is.null(output_path)) {
    writeLines(script, output_path)
    return(invisible(script))
  }
  script
}

#' Generate a Quarto domain report
#' @param domain Character.
#' @param rule_set `rule_set`.
#' @param target_meta Tibble.
#' @param build_result Named list.
#' @param output_path Character or `NULL`.
#' @return Character.
#' @export
gen_qmd_domain <- function(domain, rule_set, target_meta,
                           build_result = NULL, output_path = NULL) {
  domain <- toupper(domain)
  dom_meta <- dplyr::filter(target_meta, .data[["domain"]] == .env[["domain"]])

  lines <- character()
  .add <- function(...) lines <<- c(lines, paste0(...))

  .add("---")
  .add(glue::glue('title: "SDTM {domain} Domain"'))
  .add(glue::glue('date: "{Sys.Date()}"'))
  .add("format: html")
  .add("---")
  .add("")
  .add(glue::glue("## {domain} Overview"))
  .add("")
  .add(glue::glue("Variables: {nrow(dom_meta)}"))
  .add("")
  .add("| Variable | Label | Type | Core | Rule |")
  .add("|----------|-------|------|------|------|")
  for (i in seq_len(nrow(dom_meta))) {
    m <- dom_meta[i, ]
    .add(glue::glue("| {m$var} | {m$label} | {m$type} | {m$core} | {m$rule_type} |"))
  }
  .add("")

  if (!is.null(build_result)) {
    .add("## Build Summary")
    .add("")
    .add(glue::glue("Rows: {nrow(build_result$data)}"))
    .add(glue::glue("Columns: {ncol(build_result$data)}"))
    .add("")
  }

  qmd <- paste(lines, collapse = "\n")

  if (!is.null(output_path)) {
    writeLines(qmd, output_path)
    return(invisible(qmd))
  }
  qmd
}

#' Generate project scaffold (directories and template files)
#'
#' Creates the standard directory structure for an SDTM study project and
#' copies the `Study_Metadata.xlsx` and `Study_CT.xlsx` templates into
#' the `metadata/` directory.
#'
#' @param output_dir Character. Root directory for the new project.
#' @param config `sdtm_config`.
#' @param domains Character vector.
#' @return Invisible `NULL`.
#' @export
gen_project_scaffold <- function(output_dir, config, domains) {
  dirs <- c("data/raw", "data/sdtm", "programs", "output", "metadata", "docs")
  for (d in dirs) {
    dir.create(file.path(output_dir, d), recursive = TRUE, showWarnings = FALSE)
  }

  # Copy Study_Metadata.xlsx and Study_CT.xlsx templates to metadata/
  template_dir <- system.file("extdata", "starter_kit", package = "sdtmbuilder")
  if (nchar(template_dir) > 0L) {
    meta_src <- file.path(template_dir, "Study_Metadata.xlsx")
    ct_src   <- file.path(template_dir, "Study_CT.xlsx")
    meta_dst <- file.path(output_dir, "metadata")

    if (file.exists(meta_src)) {
      file.copy(meta_src, file.path(meta_dst, "Study_Metadata.xlsx"),
                overwrite = FALSE)
      cli::cli_alert_info("Copied Study_Metadata.xlsx to metadata/")
    }
    if (file.exists(ct_src)) {
      file.copy(ct_src, file.path(meta_dst, "Study_CT.xlsx"),
                overwrite = FALSE)
      cli::cli_alert_info("Copied Study_CT.xlsx to metadata/")
    }

    # Copy config.yaml into metadata/
    config_src <- file.path(template_dir, "config.yaml")
    if (file.exists(config_src)) {
      file.copy(config_src, file.path(meta_dst, "config.yaml"),
                overwrite = FALSE)
    }
  }

  cli::cli_alert_success("Project scaffold created at {output_dir}")
  invisible(NULL)
}

#' Render rule as code comment
#' @param rule List.
#' @return Character.
#' @export
render_rule_comments <- function(rule) {
  paste0("# Rule: ", rule$type,
         if (!is.null(rule$params$dataset))
           paste0(" | source: ", rule$params$dataset, ".", rule$params$column %||% "") else "",
         if (!is.null(rule$codelist_id))
           paste0(" | codelist: ", rule$codelist_id) else "")
}

#' Serialize rules to YAML
#' @param rule_set `rule_set`.
#' @param output_path Character or `NULL`.
#' @return Character.
#' @export
serialize_rules_to_yaml <- function(rule_set, output_path = NULL) {
  txt <- yaml::as.yaml(rule_set$rules)
  if (!is.null(output_path)) {
    writeLines(txt, output_path)
    return(invisible(txt))
  }
  txt
}

#' Serialize rules to JSON
#' @param rule_set `rule_set`.
#' @param output_path Character or `NULL`.
#' @return Character.
#' @export
serialize_rules_to_json <- function(rule_set, output_path = NULL) {
  txt <- jsonlite::toJSON(rule_set$rules, auto_unbox = TRUE, pretty = TRUE)
  if (!is.null(output_path)) {
    writeLines(txt, output_path)
    return(invisible(txt))
  }
  txt
}
