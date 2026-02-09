# ==============================================================================
# Module C: Rule Compilation
# ==============================================================================

#' Compile derivation rules from metadata
#'
#' @param target_meta Tibble (validated, normalized).
#' @param source_meta Tibble or `NULL`. Optional; auto-inferred if needed.
#' @param ct_lib Tibble or `NULL`.
#' @param dsl Character. Default `"json"`.
#' @param strict Logical. Default `TRUE`.
#' @return `rule_set` object.
#' @export
compile_rules <- function(target_meta, source_meta = NULL, ct_lib = NULL,
                          dsl = "json", strict = TRUE) {
  domains <- unique(target_meta$domain)
  all_rules  <- list()
  dep_info   <- list()
  type_accum <- character()
  compile_log <- character()

  # Build extensibility lookup from ct_lib (codelist_id → is_extensible)
  ext_lookup <- list()
  if (!is.null(ct_lib) && "is_extensible" %in% names(ct_lib)) {
    ext_df <- dplyr::distinct(ct_lib[, c("codelist_id", "is_extensible"), drop = FALSE])
    ext_lookup <- stats::setNames(ext_df$is_extensible, ext_df$codelist_id)
  }

  for (dom in domains) {
    dom_meta <- dplyr::filter(target_meta, .data$domain == dom)
    dom_rules <- list()

    # Identify variables with value-level metadata (multiple rows per var)
    # These have a non-NA "condition" column from VLM expansion
    cond_col <- if ("condition" %in% names(dom_meta)) dom_meta[["condition"]] else rep(NA, nrow(dom_meta))
    has_condition <- !all(is.na(cond_col))
    if (has_condition) {
      vlm_vars <- unique(dom_meta$var[!is.na(dom_meta$condition)])
    } else {
      vlm_vars <- character()
    }

    # Process VLM variables first: merge multiple rows into case_when rules
    for (vlm_var in vlm_vars) {
      vlm_rows <- dplyr::filter(dom_meta, .data$var == vlm_var,
                                 !is.na(.data$condition))
      if (nrow(vlm_rows) == 0L) next

      # Build case_when conditions from VLM branches
      conditions <- list()
      for (j in seq_len(nrow(vlm_rows))) {
        row <- vlm_rows[j, ]
        cond_expr <- row$condition
        # For CT-mapped variables, the value is the codelist mapping;
        # for direct values, use the source column.
        # The condition determines WHICH branch applies, not the value itself.
        # We store the full VLM branch info for derive_variable to interpret.
        conditions[[j]] <- list(
          condition       = cond_expr,
          codelist_id     = if (!is.na(row$codelist_id)) row$codelist_id else NULL,
          significant_digits = if ("significant_digits" %in% names(row) && !is.na(row$significant_digits)) row$significant_digits else NULL,
          length          = if ("length" %in% names(row) && !is.na(row$length)) row$length else NULL,
          type            = row$type
        )
      }

      # Also get the non-VLM row for this var (the base row without condition)
      base_rows <- dplyr::filter(dom_meta, .data$var == vlm_var,
                                  is.na(.data$condition))
      base_row <- if (nrow(base_rows) > 0L) base_rows[1L, ] else vlm_rows[1L, ]

      rule_type <- base_row$rule_type
      if (is.na(rule_type) || rule_type == "") rule_type <- "direct_map"

      # Parse base rule params
      params <- list()
      if (!is.na(base_row$rule_params) && nchar(base_row$rule_params) > 0) {
        params <- tryCatch(
          jsonlite::fromJSON(base_row$rule_params, simplifyVector = FALSE),
          error = function(e) list()
        )
      }
      params$vlm_branches <- conditions

      deps <- character()
      if (!is.na(base_row$depends_on) && nchar(base_row$depends_on) > 0) {
        deps <- trimws(strsplit(base_row$depends_on, ";")[[1]])
      }

      # Resolve extensibility for the base codelist
      cl_id <- if (!is.na(base_row$codelist_id)) base_row$codelist_id else NULL
      is_ext <- if (!is.null(cl_id) && cl_id %in% names(ext_lookup)) {
        ext_lookup[[cl_id]]
      } else NULL

      rule_obj <- list(
        domain     = dom,
        var        = vlm_var,
        type       = rule_type,
        params     = params,
        depends_on = deps,
        codelist_id = cl_id,
        label      = base_row$label,
        target_type = base_row$type,
        core       = base_row$core,
        order      = base_row$order,
        significant_digits = if ("significant_digits" %in% names(base_row)) base_row$significant_digits else NA,
        length     = if ("length" %in% names(base_row)) base_row$length else NA,
        is_extensible = is_ext,
        has_vlm    = TRUE
      )

      dom_rules[[vlm_var]] <- rule_obj
      type_accum <- c(type_accum, rule_type)
      compile_log <- c(compile_log,
                       glue::glue("{dom}.{vlm_var}: VLM rule with {length(conditions)} condition branch(es)"))
    }

    # Process non-VLM rows (regular single-row variables)
    non_vlm <- if (has_condition) {
      dplyr::filter(dom_meta, is.na(.data$condition) & !(.data$var %in% vlm_vars))
    } else {
      dom_meta
    }

    for (i in seq_len(nrow(non_vlm))) {
      row <- non_vlm[i, ]
      var_name  <- row$var
      rule_type <- row$rule_type

      # --- Auto-assign rule_type when METHOD is NA/empty --------------------
      if (is.na(rule_type) || rule_type == "") {
        # Convention-based defaults for common SDTM variables
        if (var_name == "STUDYID") {
          rule_type <- "constant"
        } else if (var_name == "DOMAIN") {
          rule_type <- "constant"
        } else {
          rule_type <- "direct_map"
        }
        compile_log <- c(compile_log,
                         glue::glue("{dom}.{var_name}: auto-assigned rule_type = '{rule_type}'"))
      }

      # Parse rule params
      params <- list()
      if (!is.na(row$rule_params) && nchar(row$rule_params) > 0) {
        params <- tryCatch(
          jsonlite::fromJSON(row$rule_params, simplifyVector = FALSE),
          error = function(e) {
            compile_log <<- c(compile_log,
                              glue::glue("{dom}.{var_name}: JSON parse error: {e$message}"))
            list()
          }
        )
      }

      # --- Auto-generate params when empty -----------------------------------
      if (length(params) == 0L) {
        if (rule_type == "constant" && var_name == "STUDYID") {
          params <- list(value = "auto")
        } else if (rule_type == "constant" && var_name == "DOMAIN") {
          params <- list(value = dom)
        } else if (rule_type == "direct_map") {
          # Convention: source dataset = {domain_lower}_raw, column = varname_lower
          params <- list(dataset = paste0(tolower(dom), "_raw"),
                         column  = tolower(var_name))
        }
      }

      # Parse depends_on
      deps <- character()
      if (!is.na(row$depends_on) && nchar(row$depends_on) > 0) {
        deps <- trimws(strsplit(row$depends_on, ";")[[1]])
      }

      # Resolve extensibility for this codelist
      cl_id <- if (!is.na(row$codelist_id)) row$codelist_id else NULL
      is_ext <- if (!is.null(cl_id) && cl_id %in% names(ext_lookup)) {
        ext_lookup[[cl_id]]
      } else NULL

      rule_obj <- list(
        domain     = dom,
        var        = var_name,
        type       = rule_type,
        params     = params,
        depends_on = deps,
        codelist_id = if (!is.na(row$codelist_id)) row$codelist_id else NULL,
        label      = row$label,
        target_type = row$type,
        core       = row$core,
        order      = row$order,
        significant_digits = if ("significant_digits" %in% names(row)) row$significant_digits else NA,
        length     = if ("length" %in% names(row)) row$length else NA,
        is_extensible = is_ext,
        has_vlm    = FALSE
      )

      dom_rules[[var_name]] <- rule_obj
      type_accum <- c(type_accum, rule_type)
    }

    all_rules[[dom]] <- dom_rules

    # Build dependency edges
    edges <- tibble::tibble(from_var = character(), to_var = character(),
                            domain = character())
    for (var_name in names(dom_rules)) {
      rule <- dom_rules[[var_name]]
      for (dep in rule$depends_on) {
        edges <- dplyr::bind_rows(edges, tibble::tibble(
          from_var = dep, to_var = var_name, domain = dom))
      }
    }
    dep_info[[dom]] <- edges
  }

  # Enrich CT rules
  if (!is.null(ct_lib)) {
    for (dom in names(all_rules)) {
      for (var_name in names(all_rules[[dom]])) {
        rule <- all_rules[[dom]][[var_name]]
        if (rule$type == "ct_assign" && !is.null(rule$codelist_id)) {
          cl <- dplyr::filter(ct_lib, .data$codelist_id == rule$codelist_id)
          if (nrow(cl) > 0) {
            rule$params$ct_map <- stats::setNames(cl$coded_value,
                                                   tolower(cl$input_value))
            rule$params$ct_resolved <- TRUE
          } else {
            compile_log <- c(compile_log,
                             glue::glue("{dom}.{var_name}: codelist {rule$codelist_id} not found in CT library"))
            rule$params$ct_resolved <- FALSE
          }
          all_rules[[dom]][[var_name]] <- rule
        }
      }
    }
  }

  rs <- new_rule_set(
    rules          = all_rules,
    dependency_info = dep_info,
    rule_types     = unique(type_accum),
    compile_log    = compile_log
  )
  rs$ct_lib <- ct_lib
  rs
}

#' Parse a rule JSON specification
#' @param rule_json Character or list.
#' @param var Character. Target variable name.
#' @return Named list.
#' @export
parse_rule_json <- function(rule_json, var = NA_character_) {
  if (is.character(rule_json)) {
    parsed <- tryCatch(
      jsonlite::fromJSON(rule_json, simplifyVector = FALSE),
      error = function(e) abort(glue::glue("JSON parse error for {var}: {e$message}"))
    )
  } else {
    parsed <- rule_json
  }
  parsed
}

#' Parse a rule DSL specification
#' @param rule_text Character.
#' @param var Character. Target variable name.
#' @return Named list.
#' @export
parse_rule_dsl <- function(rule_text, var = NA_character_) {
  # Minimal DSL parser for future expansion
  parts <- strsplit(rule_text, ":\\s*", perl = TRUE)[[1]]
  if (length(parts) < 2) abort(glue::glue("Malformed DSL rule for {var}: {rule_text}"))
  list(type = trimws(parts[1]), params = list(raw = trimws(parts[2])),
       source_refs = character())
}

#' Validate compiled rules
#' @param rule_set `rule_set` object.
#' @param source_meta Tibble or `NULL`. If `NULL`, dataset checks are skipped.
#' @param ct_lib Tibble or `NULL`.
#' @param raw_data Named list or `NULL`. If provided and `source_meta` is
#'   `NULL`, dataset references are checked against `names(raw_data)`.
#' @return Updated `rule_set`.
#' @export
validate_rules <- function(rule_set, source_meta = NULL, ct_lib = NULL,
                           raw_data = NULL) {
  log <- rule_set$compile_log
  # Build dataset lookup from source_meta or raw_data
  known_ds <- character()
  if (!is.null(source_meta)) {
    known_ds <- tolower(source_meta$dataset)
  } else if (!is.null(raw_data)) {
    known_ds <- tolower(names(raw_data))
  }
  if (length(known_ds) > 0L) {
    for (dom in names(rule_set$rules)) {
      for (var_name in names(rule_set$rules[[dom]])) {
        rule <- rule_set$rules[[dom]][[var_name]]
        if (!is.null(rule$params$dataset)) {
          ds <- tolower(rule$params$dataset)
          if (!ds %in% known_ds) {
            log <- c(log, glue::glue("WARN: {dom}.{var_name} references unknown dataset '{ds}'"))
          }
        }
      }
    }
  }
  rule_set$compile_log <- log
  rule_set
}

#' Enrich rules with CT details
#' @param rule_set `rule_set` object.
#' @param ct_lib Tibble.
#' @return Updated `rule_set`.
#' @export
enrich_rules_with_ct <- function(rule_set, ct_lib) {
  for (dom in names(rule_set$rules)) {
    for (var_name in names(rule_set$rules[[dom]])) {
      rule <- rule_set$rules[[dom]][[var_name]]
      if (rule$type %in% c("ct_assign", "ct_decode") && !is.null(rule$codelist_id)) {
        cl <- dplyr::filter(ct_lib, .data$codelist_id == rule$codelist_id)
        if (nrow(cl) > 0) {
          rule$params$ct_map <- stats::setNames(cl$coded_value,
                                                 tolower(cl$input_value))
          rule$params$ct_resolved <- TRUE
        } else {
          rule$params$ct_resolved <- FALSE
        }
        rule_set$rules[[dom]][[var_name]] <- rule
      }
    }
  }
  rule_set
}

#' Infer inter-variable dependencies from rules
#' @param rule_set `rule_set` object.
#' @return Updated `rule_set`.
#' @export
infer_rule_dependencies <- function(rule_set) {
  # Already done during compile_rules; this is a passthrough for re-inference
  for (dom in names(rule_set$rules)) {
    edges <- tibble::tibble(from_var = character(), to_var = character(),
                            domain = character())
    for (var_name in names(rule_set$rules[[dom]])) {
      rule <- rule_set$rules[[dom]][[var_name]]
      for (dep in rule$depends_on) {
        edges <- dplyr::bind_rows(edges, tibble::tibble(
          from_var = dep, to_var = var_name, domain = dom))
      }
    }
    rule_set$dependency_info[[dom]] <- edges
  }
  rule_set
}

#' Canonicalize rules to standard form
#' @param rule_set `rule_set` object.
#' @return Canonicalized `rule_set`.
#' @export
canonicalize_rules <- function(rule_set) {
  # Idempotent normalization
  for (dom in names(rule_set$rules)) {
    for (var_name in names(rule_set$rules[[dom]])) {
      rule <- rule_set$rules[[dom]][[var_name]]
      rule$type <- tolower(rule$type)
      rule_set$rules[[dom]][[var_name]] <- rule
    }
  }
  rule_set$rule_types <- unique(tolower(rule_set$rule_types))
  rule_set
}
