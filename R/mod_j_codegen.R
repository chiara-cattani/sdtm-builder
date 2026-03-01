# ==============================================================================
# Module J: Code Generation
# ==============================================================================
# Produces clean, piped R programs for SDTM domains.
# Generated programs use only sdtmbuilder functions (no sdtm.oak).
# All internal helpers (.categorize_var, .render_mutate_expr, etc.) are
# consolidated in this single file.
# ==============================================================================

# --- Internal helpers: variable categorization --------------------------------

#' Assign a semantic category to a variable based on name and rule type
#' @keywords internal
.categorize_var <- function(var, rule_type, domain) {
  dom <- toupper(domain)
  if (var %in% c("STUDYID", "DOMAIN", "USUBJID", "SUBJID"))   return("identifiers")
  if (grepl("SPID$", var) || var == "SOURCEID")                return("identifiers")
  if (rule_type == "seq")                                      return("sequence")
  if (rule_type == "dy")                                       return("study_day")
  if (rule_type == "iso_dtc")                                  return("dates")
  if (grepl("DICT$|DICTVER$", var))                            return("dict_version")
  if (grepl(paste0("DECOD$|LLT$|LLTCD$|PT$|PTCD$|HLT$|HLTCD$",
                   "|HLGT$|HLGTCD$|BODSYS$|BDSYCD$|SOC$",
                   "|SOCCD$|SOCLST$"), var))                   return("coded_fields")
  if (grepl("TERM$|TRT$|MODIFY$", var))                        return("topics")
  if (grepl("CAT$|SCAT$", var))                                return("categories")
  if (var %in% c("VISIT", "VISITNUM", "VISITDY") ||
      rule_type %in% c("visit", "visitnum", "visitdy"))        return("visits")
  if (var == "EPOCH" || rule_type == "epoch")                   return("epoch")
  if (grepl("TPT$|TPTNUM$|TPTREF$|ELTM$", var) ||
      rule_type %in% c("tpt", "ref_time_point"))               return("time_points")
  if (rule_type %in% c("ct_assign", "ct_decode"))              return("ct_mapped")
  if (rule_type %in% c("baseline_flag", "lastobs_flag"))       return("flags")
  "other"
}

#' Return a human-readable section label for a category tag
#' @keywords internal
.section_label <- function(category) {
  switch(category,
    identifiers  = "Identifiers",
    topics       = "Topic / Term",
    categories   = "Categories",
    coded_fields = "Coded Fields",
    dict_version = "Dictionary Version",
    ct_mapped    = "Controlled Terminology",
    dates        = "Dates",
    time_points  = "Time Points",
    visits       = "Visit Variables",
    epoch        = "Epoch",
    study_day    = "Study Day",
    flags        = "Flags",
    sequence     = "Sequence",
    other        = "Derived Variables",
    category
  )
}

# --- Internal helpers: pipe / mutate classification ---------------------------

#' Test whether a rule type requires its own pipe step
#' @keywords internal
.is_pipe_step <- function(rule_type) {
  rule_type %in% c("ct_assign", "ct_decode", "dy", "seq", "epoch",
                    "visitnum", "visit", "visitdy", "tpt",
                    "duration", "baseline_flag", "lastobs_flag",
                    "seriousness", "ref_time_point", "usubjid")
}

# --- Internal helpers: expression rendering -----------------------------------

#' Quote a scalar value for code rendering
#' @keywords internal
.quote_val <- function(x) {
  if (is.null(x) || identical(x, NA)) return("NA_character_")
  if (is.character(x)) return(paste0('"', x, '"'))
  as.character(x)
}

#' Render a mutate-compatible expression for a rule
#' @keywords internal
.render_mutate_expr <- function(var, rule, config) {
  p    <- rule$params
  type <- rule$type

  switch(type,
    constant = {
      val <- p$value
      if (identical(val, "auto")) {
        if (var == "STUDYID") return(paste0(var, " = study"))
        if (var == "DOMAIN")  return(paste0(var, " = sdtm_domain"))
      }
      if (is.character(val)) paste0(var, ' = "', val, '"')
      else                   paste0(var, " = ", val)
    },
    sourceid = {
      fid <- p$form_id
      paste0(var, ' = paste("CRF:", sources$formname[match("', fid,
             '", toupper(sources$formid))])')
    },
    direct_map = {
      col   <- p$column
      xform <- p$transform
      # Auto-infer type cast from metadata target_type
      target_t <- rule$target_type
      type_wrap <- ""
      type_close <- ""
      if (!is.null(target_t) && is.null(xform)) {
        if (target_t == "num") {
          type_wrap  <- "as.numeric("
          type_close <- ")"
        } else if (target_t == "char") {
          type_wrap  <- "as.character("
          type_close <- ")"
        }
      }
      if (!is.null(xform) && !is.na(xform) && nchar(xform) > 0) {
        paste0(var, " = ", xform, "(", col, ")")
      } else if (nchar(type_wrap) > 0) {
        paste0(var, " = ", type_wrap, col, type_close)
      } else {
        paste0(var, " = ", col)
      }
    },
    iso_dtc = {
      dc <- if (is.list(p$date_col)) p$date_col$column else p$date_col
      tc <- NULL
      if (!is.null(p$time_col)) {
        tc <- if (is.list(p$time_col)) p$time_col$column else p$time_col
      }
      if (!is.null(tc) && !is.na(tc) && nchar(tc) > 0) {
        paste0(var, " = format_iso_dtc(combine_date_time(", dc, ", ", tc, "))")
      } else {
        paste0(var, " = format_iso_dtc(", dc, ")")
      }
    },
    coalesce = {
      cols <- paste(unlist(p$columns %||% p$sources), collapse = ", ")
      paste0(var, " = dplyr::coalesce(", cols, ")")
    },
    concat = {
      cols <- paste0('"', unlist(p$columns %||% p$sources), '"', collapse = ", ")
      sep  <- p$separator %||% p$sep %||% ""
      paste0(var, ' = purrr::pmap_chr(list(', gsub('"', '', cols),
             '), function(...) { p <- c(...); p <- p[!is.na(p) & trimws(p) != ""]; paste(p, collapse = "', sep, '") })')
    },
    numeric_round = {
      digits <- rule$significant_digits %||% p$significant_digits %||% 3L
      paste0(var, " = signif(as.numeric(", p$column, "), ", digits, ")")
    },
    if_else = {
      cond <- p$condition  %||% "TRUE"
      tv   <- .quote_val(p$true_value)
      fv   <- .quote_val(p$false_value)
      paste0(var, " = dplyr::if_else(", cond, ", ", tv, ", ", fv, ")")
    },
    case_when = {
      conds <- p$conditions
      dflt  <- p$default
      if (is.list(conds) && length(conds) > 0L) {
        branches <- vapply(conds, function(c) {
          paste0(c$condition, " ~ ", .quote_val(c$value))
        }, character(1))
        if (!is.null(dflt) && !identical(dflt, NA)) {
          branches <- c(branches, paste0("TRUE ~ ", .quote_val(dflt)))
        }
        paste0(var, " = dplyr::case_when(\n",
               paste0("      ", branches, collapse = ",\n"),
               "\n    )")
      } else {
        paste0(var, " = NA_character_  # TODO: case_when derivation")
      }
    },
    join = {
      paste0(var, " = NA_character_  # TODO: join-based derivation")
    },
    regex_extract = {
      paste0(var, ' = stringr::str_extract(', p$column,
             ', "', p$pattern %||% "", '")')
    },
    regex_replace = {
      paste0(var, ' = stringr::str_replace_all(', p$column, ', "',
             p$pattern %||% "", '", "', p$replacement %||% "", '")')
    },
    trim_pad = {
      paste0(var, " = trimws(", p$column, ")")
    },
    occurrence = {
      paste0(var, " = ", p$source_var %||% "NA_character_",
             "  # TODO: occurrence derivation")
    },
    status = {
      paste0(var, " = ", p$result_var %||% "NA_character_",
             "  # TODO: status derivation")
    },
    dict_version = {
      ds_name   <- p$dataset %||% "ae_meddra"
      prefix    <- p$prefix  %||% "MedDRA"
      dictvar   <- p$dictvar %||% "DictInstance"
      coded_var <- p$coded_var %||% NULL
      ver_fn <- if (tolower(prefix) == "whodrug") "get_whodrug_version" else "get_meddra_version"
      ver_expr <- paste0(ver_fn, '(', ds_name, ', dictvar = "', dictvar, '")')
      if (!is.null(coded_var)) {
        paste0(var, ' = dplyr::if_else(!is.na(', coded_var, '), paste("', prefix, '", ', ver_expr, '), "")')
      } else {
        paste0(var, ' = paste("', prefix, '", ', ver_expr, ')')
      }
    },
    # Default
    paste0(var, " = NA_character_  # TODO: ", type, " derivation")
  )
}

#' Render a pipe-step function call for a rule
#' @keywords internal
.render_pipe_step <- function(var, rule, config) {
  p    <- rule$params
  type <- rule$type

  switch(type,
    ct_assign = {
      col   <- p$column
      cl_id <- p$codelist_id %||% rule$codelist_id %||% "UNKNOWN"
      glue::glue('assign_ct("{var}", "{col}", "{cl_id}", ct_spec)')
    },
    ct_decode = {
      col   <- p$column
      cl_id <- p$codelist_id %||% rule$codelist_id %||% "UNKNOWN"
      glue::glue('decode_ct("{var}", "{col}", "{cl_id}", ct_spec)')
    },
    dy = {
      dtc_var <- p$dtc_var
      ref_var <- p$ref_var %||% "RFSTDTC"
      glue::glue('derive_dy("{var}", "{dtc_var}", "{ref_var}")')
    },
    seq = {
      by_vars    <- unlist(p$by %||% list("USUBJID"))
      order_vars <- unlist(p$order_by %||% list())
      by_str <- paste0('"', by_vars, '"', collapse = ", ")
      if (length(order_vars) > 0L) {
        ord_str <- paste0('"', order_vars, '"', collapse = ", ")
        glue::glue('derive_seq("{var}", by = c({by_str}), order_by = c({ord_str}))')
      } else {
        glue::glue('derive_seq("{var}", by = c({by_str}))')
      }
    },
    epoch = {
      dtc_var <- p$dtc_var
      ref_var <- p$ref_var %||% "RFSTDTC"
      glue::glue('derive_epoch("{var}", "{dtc_var}", epoch_map, "{ref_var}")')
    },
    visitnum = {
      visit_var <- p$visit_var %||% "VISIT"
      glue::glue('derive_visitnum("{var}", "{visit_var}")')
    },
    visit = {
      glue::glue('derive_visit("{var}")')
    },
    visitdy = {
      dtc_var <- p$dtc_var %||% "VISITDTC"
      ref_var <- p$ref_var %||% "RFSTDTC"
      glue::glue('derive_visitdy("{var}", "{dtc_var}", "{ref_var}")')
    },
    tpt = {
      tpt_col <- p$tpt_col %||% p$source_var %||% ""
      glue::glue('derive_tpt("{var}", tpt_col = "{tpt_col}")')
    },
    duration = {
      glue::glue('derive_duration("{var}", "{p$start_dtc}", "{p$end_dtc}")')
    },
    baseline_flag = {
      glue::glue('derive_baseline_flag("{var}")')
    },
    lastobs_flag = {
      glue::glue('derive_lastobs_flag("{var}")')
    },

    ref_time_point = {
      src_var <- p$source_var %||% ""
      label   <- p$tpt_label %||% ""
      glue::glue('derive_ref_time_point("{var}", "{src_var}", "{label}")')
    },
    usubjid = {
      subjid_col <- p$subjid_col %||% "subjid"
      glue::glue('derive_usubjid(study, subjid_col = "{subjid_col}")')
    },
    # Default
    glue::glue('mutate({var} = NA_character_)  # TODO: {type} pipe step')
  )
}

# --- Internal helpers: source detection and chain assembly --------------------

#' Detect unique source datasets referenced by domain rules
#' @keywords internal
.detect_source_datasets <- function(dom_rules) {
  sources <- unique(unlist(lapply(dom_rules, function(r) {
    # For sourceid rules, the dataset is a lookup (review_status), not a data source
    if (identical(r$type, "sourceid")) return(NULL)
    r$params$dataset
  })))
  sources[!is.na(sources)]
}

#' Build the piped derivation chain
#' @keywords internal
.build_derivation_chain <- function(var_order, dom_rules, domain, config,
                                    include_comments) {
  steps   <- list()
  mut_buf <- character()
  pending <- character()
  cur_cat <- ""

  flush_buf <- function() {
    if (length(mut_buf) == 0L) return()
    code <- character()
    if (length(mut_buf) == 1L) {
      code <- paste0("  mutate(", mut_buf, ")")
    } else {
      code <- "  mutate("
      for (k in seq_along(mut_buf)) {
        sfx <- if (k < length(mut_buf)) "," else ""
        expr <- mut_buf[k]
        # If expression ends with a # comment, insert comma before the comment
        if (nzchar(sfx) && grepl("\\s+#\\s+", expr)) {
          cmt   <- regmatches(expr, regexpr("\\s+#.*$", expr))
          base  <- sub("\\s+#.*$", "", expr)
          expr  <- paste0(base, sfx, cmt)
          sfx   <- ""  # already appended
        }
        code <- c(code, paste0("    ", expr, sfx))
      }
      code <- c(code, "  )")
    }
    steps[[length(steps) + 1L]] <<- c(pending, code)
    pending <<- character()
    mut_buf <<- character()
  }

  for (v in var_order) {
    rl <- dom_rules[[v]]
    if (is.null(rl)) next

    cat <- .categorize_var(v, rl$type, domain)
    if (include_comments && cat != cur_cat) {
      flush_buf()
      lbl <- .section_label(cat)
      pending <- c(pending, paste0("  # --- ", lbl, " ---"))
      cur_cat <- cat
    }

    if (.is_pipe_step(rl$type)) {
      flush_buf()
      code <- paste0("  ", .render_pipe_step(v, rl, config))
      steps[[length(steps) + 1L]] <- c(pending, code)
      pending <- character()
    } else {
      mut_buf <- c(mut_buf, .render_mutate_expr(v, rl, config))
    }
  }
  flush_buf()

  # Join steps with %>%
  result <- character()
  for (i in seq_along(steps)) {
    chunk <- steps[[i]]
    if (i < length(steps)) {
      chunk[length(chunk)] <- paste0(chunk[length(chunk)], " %>%")
    }
    result <- c(result, chunk)
  }
  result
}

# --- Internal helpers: TPT timepoint code generation --------------------------

#' Generate TPT derivation code from config.yaml timepoint_map
#'
#' If the config contains a `timepoint_map`, returns lines of code that derive
#' planned time point variables (--TPT, --TPTNUM, --TPTREF, --ELTM) using
#' `case_when()` inside `mutate()`.
#'
#' @param config Config list. Must contain `$timepoint_map`.
#' @param domain Character. Domain code.
#' @param source_var Character. Source variable (e.g. "eventid").
#' @return Character vector of code lines, or `character(0)` if no map.
#' @keywords internal
.gen_tpt_code <- function(config, domain, source_var = "eventid") {
  tpt_map <- config$timepoint_map
  if (is.null(tpt_map) || length(tpt_map) == 0L) return(character(0))

  pfx <- toupper(domain)
  lines <- character()
  .add  <- function(...) lines <<- c(lines, ...)

  tpt_branches    <- character()
  tptnum_branches <- character()
  tptref_branches <- character()
  eltm_branches   <- character()

  for (tp in tpt_map) {
    code_val   <- tp$code
    tpt_val    <- tp$tpt     %||% NA_character_
    tptnum_val <- tp$tptnum  %||% NA
    tptref_val <- tp$tptref  %||% NA_character_
    eltm_val   <- tp$eltm    %||% NA_character_

    if (!is.na(tpt_val)) {
      tpt_branches <- c(tpt_branches,
        paste0('    ', source_var, ' == "', code_val, '" ~ "', tpt_val, '"'))
    }
    if (!is.na(tptnum_val)) {
      tptnum_branches <- c(tptnum_branches,
        paste0('    ', source_var, ' == "', code_val, '" ~ ', tptnum_val))
    }
    if (!is.na(tptref_val)) {
      tptref_branches <- c(tptref_branches,
        paste0('    ', source_var, ' == "', code_val, '" ~ "', tptref_val, '"'))
    }
    if (!is.na(eltm_val)) {
      eltm_branches <- c(eltm_branches,
        paste0('    ', source_var, ' == "', code_val, '" ~ "', eltm_val, '"'))
    }
  }

  # Count how many variables we have
  has_vars <- c(length(tpt_branches), length(tptnum_branches),
                length(tptref_branches), length(eltm_branches))
  n_vars <- sum(has_vars > 0L)
  if (n_vars == 0L) return(character(0))

  .add("  # --- Time Points (from config) ---")
  .add("  mutate(")

  var_idx <- 0L

  if (length(tpt_branches) > 0L) {
    var_idx <- var_idx + 1L
    tpt_branches <- c(tpt_branches, '    TRUE ~ NA_character_')
    .add(paste0("    ", pfx, "TPT = case_when("))
    .add(paste(tpt_branches, collapse = ",\n"))
    .add(paste0("    )", if (var_idx < n_vars) "," else ""))
  }

  if (length(tptnum_branches) > 0L) {
    var_idx <- var_idx + 1L
    tptnum_branches <- c(tptnum_branches, '    TRUE ~ NA_real_')
    .add(paste0("    ", pfx, "TPTNUM = case_when("))
    .add(paste(tptnum_branches, collapse = ",\n"))
    .add(paste0("    )", if (var_idx < n_vars) "," else ""))
  }

  if (length(tptref_branches) > 0L) {
    var_idx <- var_idx + 1L
    tptref_branches <- c(tptref_branches, '    TRUE ~ NA_character_')
    .add(paste0("    ", pfx, "TPTREF = case_when("))
    .add(paste(tptref_branches, collapse = ",\n"))
    .add(paste0("    )", if (var_idx < n_vars) "," else ""))
  }

  if (length(eltm_branches) > 0L) {
    var_idx <- var_idx + 1L
    eltm_branches <- c(eltm_branches, '    TRUE ~ NA_character_')
    .add(paste0("    ", pfx, "ELTM = case_when("))
    .add(paste(eltm_branches, collapse = ",\n"))
    .add("    )")
  }

  .add("  )")

  lines
}

# ==============================================================================
# Exported functions
# ==============================================================================

#' Generate a standalone R script for a single domain
#'
#' Produces a fully commented, piped R program that reproduces the derivation
#' for one domain.  The generated script uses only `sdtmbuilder` functions
#' (no sdtm.oak), follows a professional layout with a header, working-directory
#' setup, metadata and raw-data import, a grouped piped derivation chain, and
#' an [export_domain()] call.
#'
#' If the `config` contains a `timepoint_map`, planned time point variables
#' are derived from that map using `case_when()`.
#'
#' @param domain Character.
#' @param rule_set `rule_set`.
#' @param target_meta Tibble.
#' @param config `sdtm_config` or list.
#' @param domain_meta Tibble or `NULL`.
#' @param style Character. `"tidyverse"` (default).
#' @param include_comments Logical. Default `TRUE`.
#' @param output_path Character or `NULL`. If non-`NULL`, writes the script.
#' @param metadata_path Character or `NULL`.
#' @param ct_path Character or `NULL`.
#' @param raw_dir Character or `NULL`.
#' @param output_dir Character or `NULL`.
#' @param tpt_source_var Character or `NULL`. Source variable for TPT
#'   derivation (e.g., `"eventid"`). Defaults to `"eventid"`.
#' @param export_formats Character vector. Output formats for the generated
#'   export step. Default `c("xpt", "rda")`.
#' @return Character string of the generated script (invisibly if written).
#' @export
gen_domain_script <- function(domain, rule_set, target_meta,
                              config,
                              domain_meta = NULL,
                              style = "tidyverse",
                              include_comments = TRUE,
                              output_path = NULL,
                              metadata_path = NULL,
                              ct_path = NULL,
                              raw_dir = NULL,
                              output_dir = NULL,
                              tpt_source_var = NULL,
                              export_formats = c("xpt", "rda")) {
  domain <- toupper(domain)
  dom_lc <- tolower(domain)

  dom_rules <- rule_set$rules[[domain]]
  dom_meta  <- dplyr::filter(target_meta, .data[["domain"]] == .env[["domain"]])
  if (is.null(dom_rules)) abort(glue::glue("No rules for domain {domain}"))

  # --- Resolve paths ----------------------------------------------------------
  metadata_path  <- metadata_path %||% "metadata/Study_Metadata.xlsx"
  ct_path        <- ct_path       %||% "metadata/Study_CT.xlsx"
  raw_dir        <- raw_dir       %||% "raw"
  output_dir     <- output_dir    %||% "sdtm/datasets"
  studyid        <- toupper(config$studyid %||% "UNKNOWN")
  tpt_source_var <- tpt_source_var %||% "eventid"

  # --- Domain description -----------------------------------------------------
  dom_desc <- NULL
  if (!is.null(domain_meta)) {
    desc_row <- dplyr::filter(domain_meta,
                              toupper(.data[["domain"]]) == .env[["domain"]])
    if (nrow(desc_row) > 0L && "description" %in% names(desc_row) &&
        !is.na(desc_row$description[1])) {
      dom_desc <- desc_row$description[1]
    }
  }
  if (is.null(dom_desc)) dom_desc <- paste0(domain, " Domain")

  # --- Dependency order -------------------------------------------------------
  dep     <- build_dependency_graph(rule_set, domain)
  sources <- .detect_source_datasets(dom_rules)
  primary <- if (length(sources) > 0L) sources[1] else paste0(dom_lc, "_raw")

  needs_dm <- any(vapply(dom_rules, function(r) r$type == "dy", logical(1))) &&
              domain != "DM"

  # --- Domain keys ------------------------------------------------------------
  keys <- NULL
  if (!is.null(domain_meta)) {
    key_row <- dplyr::filter(domain_meta,
                             toupper(.data[["domain"]]) == .env[["domain"]])
    if (nrow(key_row) > 0L && "keys" %in% names(key_row) &&
        !is.na(key_row$keys[1])) {
      keys <- trimws(strsplit(key_row$keys[1], ",")[[1]])
    }
  }
  if (is.null(keys)) keys <- c("STUDYID", "USUBJID", paste0(domain, "SEQ"))

  # --- Check for timepoint map ------------------------------------------------
  has_tpt <- !is.null(config$timepoint_map) && length(config$timepoint_map) > 0L

  # --- Build script -----------------------------------------------------------
  lines <- character()
  .add  <- function(...) lines <<- c(lines, ...)
  .blk  <- function() lines <<- c(lines, "")

  # ---- 1. Header -------------------------------------------------------------
  src_label <- paste(gsub("_raw$", "", sources), collapse = ", ")
  .add("# ******************************************************************************")
  .add(glue::glue("# {studyid}"))
  .add("# ******************************************************************************")
  .add(glue::glue("# PROGRAM NAME  : {domain}.R"))
  .add(glue::glue("# PURPOSE       : SDTM {domain} Domain - {dom_desc}"))
  .add("# ------------------------------------------------------------------------------")
  .add("# NOTES :")
  .add(glue::glue("#   Raw datasets : {src_label}"))
  .add("#   Dependencies : sdtmbuilder package")
  .add("# ------------------------------------------------------------------------------")
  .add("# PROGRAM HISTORY :")
  .add(glue::glue("# {Sys.Date()} - sdtmbuilder - Auto-generated program"))
  .add("# ******************************************************************************")
  .blk()

  # ---- 2. Configuration ------------------------------------------------------
  .add("# Configuration ----")
  .add("library(sdtmbuilder)")
  .add("library(dplyr)")
  .blk()
  .add(glue::glue('study       <- "{studyid}"'))
  .add(glue::glue('sdtm_domain <- "{domain}"'))
  .blk()

  # ---- 3. Working directory --------------------------------------------------
  # Navigate to study root (2 levels up from sdtm/programs/)
  .add("# Set working directory to study root ----")
  .add("if (requireNamespace(\"rstudioapi\", quietly = TRUE) &&")
  .add("    rstudioapi::isAvailable()) {")
  .add("  prog_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)")
  .add("  setwd(file.path(prog_dir, \"..\", \"..\"))")
  .add("} else if (exists(\"progdir\")) {")
  .add("  setwd(file.path(progdir, \"..\", \"..\"))")
  .add("}")
  .blk()

  # ---- 4. Metadata import ----------------------------------------------------
  .add("# Import metadata ----")
  .add(glue::glue('study_meta <- read_study_metadata_excel("{metadata_path}")'))
  .add(glue::glue('ct_spec    <- read_study_ct_excel("{ct_path}")'))
  .blk()
  .add("target_meta      <- study_meta$target_meta")
  .add("domain_meta      <- study_meta$domain_meta")
  .add("value_level_meta <- study_meta$value_level_meta")
  .blk()

  # ---- 5. Data import --------------------------------------------------------
  .add("# Import data ----")
  .add(glue::glue('all_raw <- load_raw_datasets("{raw_dir}")'))
  .add("for (nm in names(all_raw)) {")
  .add("  all_raw[[nm]] <- all_raw[[nm]] %>% standardize_names() %>% convert_blanks_to_na()")
  .add("}")
  .blk()

  # Source variable assignments
  assigned <- character()
  for (src in sources) {
    src_clean <- gsub("_raw$", "", src)
    .add(glue::glue('{src_clean} <- all_raw[["{src_clean}"]]'))
    assigned <- c(assigned, src_clean)
  }
  if (needs_dm && !"dm" %in% assigned) {
    .add('dm1 <- all_raw[["dm"]]')
  }
  .blk()

  # ---- Sources lookup for SOURCEID derivation ----
  has_sourceid <- any(vapply(dom_rules, function(r) identical(r$type, "sourceid"), logical(1)))
  if (has_sourceid) {
    # Find the review_status dataset name (default "review_status")
    rs_ds <- "review_status"
    for (r in dom_rules) {
      if (identical(r$type, "sourceid") && !is.null(r$params$dataset)) {
        rs_ds <- r$params$dataset
        break
      }
    }
    .add("# Sources lookup (for SOURCEID) ----")
    .add(glue::glue('review_status <- all_raw[["{rs_ds}"]]'))
    .add("sources <- review_status %>%")
    .add('  dplyr::filter(trimws(formname) != "") %>%')
    .add("  dplyr::distinct(formid, formname)")
    .blk()
  }

  # ---- Auto-merge secondary source datasets into primary ----
  prim_clean <- gsub("_raw$", "", primary)
  secondary  <- setdiff(gsub("_raw$", "", sources), prim_clean)
  dom_spid   <- tolower(paste0(dom_lc, "spid"))  # e.g. "aespid"
  if (length(secondary) > 0L) {
    .add("# Merge secondary source datasets ----")
    for (sec in secondary) {
      # Check if secondary has its own SPID that needs renaming
      # e.g. sae has "saespid" -> rename to "aespid"
      sec_spid <- paste0(sec, "spid")
      if (sec_spid != dom_spid) {
        .add(glue::glue('{sec} <- dplyr::rename({sec}, dplyr::any_of(c({dom_spid} = "{sec_spid}")))'))
      }
      # Keep only non-overlapping columns from secondary (+ join keys)
      .add(glue::glue('{sec}_new <- setdiff(names({sec}), names({prim_clean}))'))
      .add(glue::glue('{sec}_slim <- {sec}[, c("subjectid", "{dom_spid}", {sec}_new), drop = FALSE]'))
      .add(glue::glue('{prim_clean} <- {prim_clean} %>%'))
      .add(glue::glue('  dplyr::left_join({sec}_slim, by = c("subjectid", "{dom_spid}"))'))
    }
    .blk()
  }

  # ---- 6. Pre-merge DM for RFSTDTC -------------------------------------------
  if (needs_dm) {
    # Detect RFSTDTC source from config
    ref_col <- config$ref_start_rule$var %||% "rfstdtc"
    ref_src <- config$ref_start_rule$source %||% "dm"
    ref_src_clean <- gsub("_raw$", "", ref_src)
    dm_var <- if (ref_src_clean %in% assigned) ref_src_clean else "dm1"
    .add("# Prepare DM for RFSTDTC ----")
    .add("# Load DM SDTM dataset (has RFSTDTC); fall back to raw DM")
    .add('dm_rda <- "sdtm/datasets/RDA/dm.rda"')
    .add('dm_xpt <- "sdtm/datasets/XPT/dm.xpt"')
    .add("if (file.exists(dm_rda)) {")
    .add("  dm_env <- new.env(parent = emptyenv())")
    .add("  load(dm_rda, envir = dm_env)")
    .add("  dm_sdtm <- dm_env[[ls(dm_env)[1]]]")
    .add("  names(dm_sdtm) <- tolower(names(dm_sdtm))")
    .add("} else if (file.exists(dm_xpt)) {")
    .add("  dm_sdtm <- haven::read_xpt(dm_xpt)")
    .add("  names(dm_sdtm) <- tolower(names(dm_sdtm))")
    .add("} else {")
    .add(glue::glue("  dm_sdtm <- {dm_var}"))
    .add("}")
    .blk()
    .add("dm_slim <- dm_sdtm %>%")
    .add(glue::glue('  dplyr::select(dplyr::any_of(c("subjectid", "subjid", "usubjid", "{ref_col}"))) %>%'))
    .add("  dplyr::distinct()")
    .blk()
    # Determine join key: SubjectId→SUBJID or USUBJID
    prim_clean <- gsub("_raw$", "", primary)
    .add(glue::glue("# Join RFSTDTC: try subjectid=subjid, fall back to usubjid"))
    .add(glue::glue('if ("subjectid" %in% names({prim_clean}) && "subjid" %in% names(dm_slim)) {{'))
    .add(glue::glue("  {prim_clean} <- {prim_clean} %>%"))
    .add(glue::glue('    dplyr::left_join(dm_slim %>% dplyr::select(dplyr::any_of(c("subjid", "{ref_col}"))) %>% dplyr::rename(subjectid = subjid), by = "subjectid")'))
    .add(glue::glue("}} else if (\"usubjid\" %in% names({prim_clean}) && \"usubjid\" %in% names(dm_slim)) {{"))
    .add(glue::glue("  {prim_clean} <- {prim_clean} %>%"))
    .add(glue::glue('    dplyr::left_join(dm_slim %>% dplyr::select(dplyr::any_of(c("usubjid", "{ref_col}"))), by = "usubjid")'))
    .add("}")
    # Ensure RFSTDTC is uppercase for derive_dy
    ref_col_uc <- toupper(ref_col)
    if (ref_col != ref_col_uc) {
      .add(glue::glue('if ("{ref_col}" %in% names({prim_clean}) && !"{ref_col_uc}" %in% names({prim_clean})) {prim_clean}${ref_col_uc} <- {prim_clean}${ref_col}'))
    }
    .blk()
  }

  # ---- 7. Derivation chain ---------------------------------------------------
  .add(glue::glue("# {domain} derivations ----"))
  prim_clean <- gsub("_raw$", "", primary)
  .add(glue::glue("{dom_lc}2 <- {prim_clean} %>%"))

  chain <- .build_derivation_chain(dep$order, dom_rules, domain, config,
                                   include_comments)

  # If timepoint map exists, append TPT derivation
  if (has_tpt) {
    tpt_code <- .gen_tpt_code(config, domain, tpt_source_var)
    if (length(tpt_code) > 0L) {
      if (length(chain) > 0L) {
        chain[length(chain)] <- paste0(chain[length(chain)], " %>%")
      }
      chain <- c(chain, tpt_code)
    }
  }

  .add(chain)
  .blk()

  # ---- 8. Export --------------------------------------------------------------
  .add("# Finalize ----")
  keys_str <- paste0('"', keys, '"', collapse = ", ")
  .add(glue::glue("{dom_lc}_final <- export_domain("))
  .add(glue::glue("  data        = {dom_lc}2,"))
  .add("  domain      = sdtm_domain,")
  .add(glue::glue('  output_dir  = "{output_dir}",'))
  fmts_str <- paste0('"', export_formats, '"', collapse = ', ')
  .add(glue::glue('  formats     = c({fmts_str}),'))
  .add("  xpt_version = 8L,")
  .add("  target_meta = target_meta,")
  .add("  domain_meta = domain_meta,")
  .add(glue::glue("  keys        = c({keys_str}),"))
  .add("  drop_empty_perm = TRUE")
  .add(")")
  .blk()

  .add(glue::glue(
    'cli::cli_alert_success("{domain} domain created: {{nrow({dom_lc}2)}} rows")'
  ))

  script <- paste(lines, collapse = "\n")

  if (!is.null(output_path)) {
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    writeLines(script, output_path)
    cli::cli_alert_success("Script written to {output_path}")
    return(invisible(script))
  }

  script
}


#' Render R code for a single rule (legacy helper)
#'
#' Kept for backward compatibility. New code should use the piped code
#' generation via [gen_domain_script()].
#'
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
    sourceid = {
      fid <- p$form_id
      glue::glue('data${var} <- paste("CRF:", sources$formname[match("{fid}", toupper(sources$formid))])')
    },
    direct_map = {
      xform <- if (!is.null(p$transform) && !is.na(p$transform)) paste0(p$transform, "(") else ""
      xform_close <- if (!is.null(p$transform) && !is.na(p$transform)) ")" else ""
      # Auto-infer type cast from metadata target_type
      target_t <- rule$target_type
      if (nchar(xform) == 0 && !is.null(target_t)) {
        if (target_t == "num") {
          xform <- "as.numeric("
          xform_close <- ")"
        } else if (target_t == "char") {
          xform <- "as.character("
          xform_close <- ")"
        }
      }
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
    usubjid = {
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
