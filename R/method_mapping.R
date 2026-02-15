# ==============================================================================
# Method-to-Rule-Type Mapping
# ==============================================================================
# The METHOD column in Study_Metadata.xlsx Variables sheet uses explicit
# function-call syntax:
#
#   derive_seq(by = "USUBJID")
#   derive_dy(dtc_var = "AESTDTC", ref_var = "RFSTDTC")
#   map_direct(column = "aeterm")
#   assign_ct(column = "aesev_raw")
#   format_iso_dtc(date_col = "aestdat", time_col = "aesttm")
#
# This allows full user control: the Excel is the single source of truth.
#
# Legacy keyword methods (SEQ, DY, etc.) are still supported and auto-expanded
# into the new syntax with convention-based defaults.
# ==============================================================================

# ---------------------------------------------------------------------------
# Function Registry: maps function names -> rule_type + parameter specs
# ---------------------------------------------------------------------------
#' @noRd
FUNCTION_REGISTRY <- list(
  # -- Mapping / Constants --
  map_direct        = list(rule_type = "direct_map",     required = c("column"),
                           optional  = list(transform = NULL, type = NULL, dataset = NULL)),
  derive_constant   = list(rule_type = "constant",       required = c("value"),
                           optional  = list()),
  assign_ct         = list(rule_type = "ct_assign",      required = c("column"),
                           optional  = list(codelist_id = NULL,
                                            unknown_policy = NULL)),

  # -- Identifiers --
  derive_usubjid    = list(rule_type = "usubjid",        required = character(),
                           optional  = list(subjid_col = "subjid", sep = "-",
                                            studyid_col = "STUDYID")),
  derive_seq        = list(rule_type = "seq",            required = character(),
                           optional  = list(by = "USUBJID", order_by = NULL,
                                            ties = "dense")),

  # -- Date/Time --
  format_iso_dtc    = list(rule_type = "iso_dtc",        required = c("date_col"),
                           optional  = list(time_col = NULL)),
  derive_dy         = list(rule_type = "dy",             required = c("dtc_var"),
                           optional  = list(ref_var = "RFSTDTC")),
  derive_duration   = list(rule_type = "duration",       required = c("start_dtc", "end_dtc"),
                           optional  = list(units = "days")),

  # -- Visit/Epoch --
  derive_epoch      = list(rule_type = "epoch",          required = c("dtc_var"),
                           optional  = list(ref_var = "RFSTDTC")),
  derive_visitnum   = list(rule_type = "visitnum",       required = character(),
                           optional  = list(visit_var = "VISIT")),
  derive_visit      = list(rule_type = "visit",          required = character(),
                           optional  = list(dy_var = NULL)),
  derive_visitdy    = list(rule_type = "visitdy",        required = character(),
                           optional  = list(visit_var = "VISIT", dy_var = NULL)),
  derive_tpt        = list(rule_type = "tpt",            required = character(),
                           optional  = list(source_var = NULL, tpt_map = NULL)),

  # -- Numeric --
  derive_numeric_round = list(rule_type = "numeric_round", required = c("column"),
                              optional  = list(digits = NULL)),

  # -- Flags --
  derive_baseline_flag = list(rule_type = "baseline_flag", required = character(),
                              optional  = list(visit_var = "VISIT",
                                               baseline_visit = "BASELINE",
                                               by = "USUBJID")),
  derive_lastobs_flag  = list(rule_type = "lastobs_flag",  required = character(),
                              optional  = list(by = "USUBJID", order_var = NULL)),

  # -- Controlled Terminology --
  decode_ct         = list(rule_type = "ct_decode",      required = c("column"),
                           optional  = list(codelist_id = NULL)),

  # -- Multi-source / Conditional --
  derive_coalesce   = list(rule_type = "coalesce",       required = c("columns"),
                           optional  = list()),
  derive_concat     = list(rule_type = "concat",         required = c("columns"),
                           optional  = list(separator = "")),
  derive_if_else    = list(rule_type = "if_else",        required = c("condition",
                                                                       "true_value",
                                                                       "false_value"),
                           optional  = list(missing_value = NA)),
  derive_case_when  = list(rule_type = "case_when",      required = c("conditions"),
                           optional  = list(default = NA)),

  # -- Text manipulation --
  derive_regex_extract = list(rule_type = "regex_extract", required = c("column", "pattern"),
                              optional  = list(group = 1L)),
  derive_regex_replace = list(rule_type = "regex_replace", required = c("column", "pattern",
                                                                         "replacement"),
                              optional  = list(all = TRUE)),
  derive_trim_pad   = list(rule_type = "trim_pad",       required = c("column"),
                           optional  = list(side = "both", width = NULL,
                                            pad = " ")),

  # -- Domain-specific --
  derive_occurrence = list(rule_type = "occurrence",     required = c("source_var"),
                           optional  = list(present_value = "Y",
                                            absent_value = NA_character_)),
  derive_status     = list(rule_type = "status",         required = c("result_var"),
                           optional  = list(done_value = NA_character_,
                                            not_done_value = "NOT DONE")),
  derive_seriousness = list(rule_type = "seriousness",   required = c("flag_vars"),
                            optional  = list(present_value = "Y",
                                             absent_value = "N")),
  derive_dict_version = list(rule_type = "dict_version", required = c("dataset"),
                             optional  = list(prefix = "MedDRA", dictvar = "DictInstance")),
  get_dict_version    = list(rule_type = "dict_version", required = c("dataset"),
                             optional  = list(prefix = "MedDRA", dictvar = "DictInstance")),
  derive_ref_time_point = list(rule_type = "ref_time_point", required = c("source_var", "tpt_label"),
                               optional  = list(tpt_var = NULL,
                                                mode = "pattern",
                                                mapping = NULL)),

  # -- Source traceability --
  derive_sourceid = list(rule_type = "sourceid", required = c("form_id"),
                         optional  = list(dataset = "review_status",
                                          id_col = "formid",
                                          name_col = "formname"))
)

# ---------------------------------------------------------------------------
# Legacy keyword -> explicit method expansion
# ---------------------------------------------------------------------------
#' @noRd
LEGACY_KEYWORD_MAP <- c(
  "SEQ"      = "seq",
  "STRESN"   = "numeric_round",
  "USUBJID"  = "usubjid",
  "DY"       = "dy",
  "DUR"      = "duration",
  "EPOCH"    = "epoch",
  "VISITNUM" = "visitnum",
  "VISIT"    = "visit",
  "VISITDY"  = "visitdy",
  "TPT"      = "tpt",
  "BASELINE" = "baseline_flag",
  "LASTOBS"  = "lastobs_flag"
)

# Reverse lookup: rule_type -> function name
#' @noRd
RULE_TYPE_TO_FUNCTION <- stats::setNames(
 names(FUNCTION_REGISTRY),
 vapply(FUNCTION_REGISTRY, `[[`, character(1), "rule_type")
)


# ==============================================================================
# parse_method_call(): Parse "fn(param = val, ...)" from METHOD column
# ==============================================================================

#' Parse a METHOD column value into function name + parameters
#'
#' Supports three formats:
#' 1. **Explicit call**: `derive_seq(by = "USUBJID", order_by = "AESTDTC")`
#' 2. **Legacy keyword**: `SEQ`, `DY`, `STRESN` (auto-expanded with warning)
#' 3. **Empty/NA**: returns `NULL` (auto-assigned by compile_rules)
#'
#' @param method_string Character. The METHOD value from Excel.
#' @param domain Character. Domain code (for convention-based defaults).
#' @param var Character. Variable name (for convention-based defaults).
#' @param meta_row Named list or tibble row with metadata columns.
#' @return Named list: `fn`, `rule_type`, `params` or `NULL` if empty.
#' @export
parse_method_call <- function(method_string, domain = NA_character_,
                              var = NA_character_, meta_row = NULL) {
  if (is.na(method_string) || nchar(trimws(method_string)) == 0L) {
    return(NULL)
  }

  method_string <- trimws(method_string)

  # ------ Try explicit function-call syntax: fn(param = val, ...) ------
  fn_match <- regmatches(method_string,
                         regexec("^([a-z_][a-z0-9_]*)\\((.*)\\)\\s*$",
                                 method_string, perl = TRUE))[[1]]

  if (length(fn_match) == 3L) {
    fn_name   <- fn_match[2]
    args_text <- fn_match[3]

    # Parse arguments
    params <- .parse_call_args(args_text)

    # Validate fn_name is registered
    if (!fn_name %in% names(FUNCTION_REGISTRY)) {
      warn(glue::glue("Unknown function '{fn_name}' in METHOD for {domain}.{var}. ",
                       "Will be treated as custom rule."))
      return(list(fn = fn_name, rule_type = fn_name, params = params))
    }

    reg <- FUNCTION_REGISTRY[[fn_name]]
    return(list(fn = fn_name, rule_type = reg$rule_type, params = params))
  }

  # ------ Try explicit fn name without parens: fn_name ------
  if (grepl("^[a-z_][a-z0-9_]*$", method_string) &&
      method_string %in% names(FUNCTION_REGISTRY)) {
    reg <- FUNCTION_REGISTRY[[method_string]]
    return(list(fn = method_string, rule_type = reg$rule_type, params = list()))
  }

  # ------ Try legacy keyword: SEQ, DY, etc. ------
  keyword <- toupper(method_string)
  if (keyword %in% names(LEGACY_KEYWORD_MAP)) {
    rule_type <- LEGACY_KEYWORD_MAP[[keyword]]
    fn_name   <- RULE_TYPE_TO_FUNCTION[[rule_type]]
    if (is.null(fn_name) || is.na(fn_name)) fn_name <- method_string

    # Auto-generate default params for legacy keywords
    params <- .auto_params_for_legacy(rule_type, domain, var, meta_row)

    return(list(fn = fn_name, rule_type = rule_type, params = params))
  }

  # ------ Unknown method ------
  warn(glue::glue("Unknown METHOD '{method_string}' for {domain}.{var}. ",
                   "Will be treated as direct_map."))
  return(list(fn = "map_direct", rule_type = "direct_map",
              params = list(column = tolower(var))))
}


# ==============================================================================
# .parse_call_args(): Parse "param = val, param2 = val2" from inside parens
# ==============================================================================
#' @noRd
.parse_call_args <- function(args_text) {
  args_text <- trimws(args_text)
  if (nchar(args_text) == 0L) return(list())

  params <- list()

  # Split by comma, but respect quoted strings and nested parens
  parts <- .split_args(args_text)

  for (part in parts) {
    part <- trimws(part)
    if (nchar(part) == 0L) next

    # Split on first "="
    eq_pos <- regexpr("=", part, fixed = TRUE)
    if (eq_pos > 0L) {
      key <- trimws(substr(part, 1, eq_pos - 1))
      val <- trimws(substr(part, eq_pos + 1, nchar(part)))
      params[[key]] <- .parse_value(val)
    } else {
      # Positional arg: store with numeric key
      params[[as.character(length(params) + 1L)]] <- .parse_value(part)
    }
  }

  params
}

#' Split comma-separated arguments respecting quotes and brackets
#' @noRd
.split_args <- function(text) {
  chars     <- strsplit(text, "")[[1]]
  parts     <- list()
  current   <- ""
  depth     <- 0L
  in_quote  <- FALSE
  quote_ch  <- ""

  for (ch in chars) {
    if (in_quote) {
      current <- paste0(current, ch)
      if (ch == quote_ch) in_quote <- FALSE
      next
    }
    if (ch == '"' || ch == "'") {
      in_quote <- TRUE
      quote_ch <- ch
      current  <- paste0(current, ch)
      next
    }
    if (ch %in% c("(", "[", "{")) {
      depth   <- depth + 1L
      current <- paste0(current, ch)
      next
    }
    if (ch %in% c(")", "]", "}")) {
      depth   <- depth - 1L
      current <- paste0(current, ch)
      next
    }
    if (ch == "," && depth == 0L) {
      parts   <- c(parts, list(current))
      current <- ""
      next
    }
    current <- paste0(current, ch)
  }
  if (nchar(trimws(current)) > 0L) parts <- c(parts, list(current))

  unlist(parts)
}

#' Parse a single value from method call args
#' @noRd
.parse_value <- function(val) {
  val <- trimws(val)
  if (nchar(val) == 0L) return(NULL)

  # Quoted string
  if ((startsWith(val, '"') && endsWith(val, '"')) ||
      (startsWith(val, "'") && endsWith(val, "'"))) {
    return(substr(val, 2, nchar(val) - 1))
  }

  # c(...) vector
  if (startsWith(val, "c(") && endsWith(val, ")")) {
    inner <- substr(val, 3, nchar(val) - 1)
    elems <- trimws(strsplit(inner, ",")[[1]])
    return(vapply(elems, .parse_value, character(1), USE.NAMES = FALSE))
  }

  # Logical
  if (val %in% c("TRUE", "FALSE")) return(as.logical(val))

  # NA variants
  if (val == "NA") return(NA)
  if (val == "NA_character_") return(NA_character_)
  if (val == "NA_real_") return(NA_real_)
  if (val == "NULL") return(NULL)

  # Numeric
  num <- suppressWarnings(as.numeric(val))
  if (!is.na(num)) return(num)

  # Bare word: treat as string
  val
}


# ==============================================================================
# Auto-parameter generation for legacy keyword methods
# ==============================================================================
#' @noRd
.auto_params_for_legacy <- function(rule_type, domain, var, meta_row) {
  dom_lower <- tolower(domain)
  var_lower <- tolower(var)

  params <- switch(rule_type,
    seq = list(by = "USUBJID"),
    dy = {
      # Convention: AEDY -> dtc_var = AESTDTC, CMDY -> CMSTDTC, etc.
      prefix <- sub("DY$", "", var)
      dtc_var <- paste0(prefix, "STDTC")
      list(dtc_var = dtc_var, ref_var = "RFSTDTC")
    },
    duration = {
      prefix <- sub("DUR$", "", var)
      list(start_dtc = paste0(prefix, "STDTC"),
           end_dtc   = paste0(prefix, "ENDTC"))
    },
    epoch = {
      prefix <- sub("EPOCH$", "", var)
      dtc_var <- if (nchar(prefix) > 0) paste0(prefix, "STDTC") else paste0(domain, "STDTC")
      list(dtc_var = dtc_var, ref_var = "RFSTDTC")
    },
    visitnum = list(visit_var = "VISIT"),
    visit    = list(),
    visitdy  = list(visit_var = "VISIT"),
    tpt      = list(),
    baseline_flag = list(visit_var = "VISIT", baseline_visit = "BASELINE",
                         by = "USUBJID"),
    lastobs_flag  = list(by = "USUBJID"),
    numeric_round = {
      # Convention: LBSTRESN -> column = lborres, VSSTRESN -> column = vsorres
      src <- sub("stresn$", "orres", var_lower)
      list(column = src)
    },
    usubjid = list(subjid_col = "subjid", sep = "-"),
    list()
  )

  params
}


# ==============================================================================
# Format params for display
# ==============================================================================
#' @noRd
.format_params <- function(params) {
  if (length(params) == 0L) return("")
  keys <- names(params)
  if (is.null(keys)) keys <- as.character(seq_along(params))
  parts <- vapply(seq_along(params), function(i) {
    k <- keys[i]
    v <- params[[i]]
    if (is.character(v) && length(v) == 1L) {
      paste0(k, ' = "', v, '"')
    } else if (is.character(v) && length(v) > 1L) {
      paste0(k, " = c(", paste0('"', v, '"', collapse = ", "), ")")
    } else if (is.numeric(v) && length(v) == 1L) {
      paste0(k, " = ", v)
    } else if (is.logical(v) && length(v) == 1L) {
      paste0(k, " = ", toupper(as.character(v)))
    } else if (is.null(v)) {
      paste0(k, " = NULL")
    } else {
      paste0(k, " = ", deparse(v))
    }
  }, character(1))
  paste(parts, collapse = ", ")
}


# ==============================================================================
# Enrichment: merge metadata columns into parsed params
# ==============================================================================

#' Enrich parsed method params with metadata columns
#'
#' Merges codelist_id, significant_digits, type, length from the metadata row
#' into the parsed params. Does NOT overwrite params already set by the user
#' in the METHOD call.
#'
#' @param parsed Named list from [parse_method_call()].
#' @param meta_row Tibble row with metadata columns.
#' @return Updated parsed list with enriched params.
#' @export
enrich_params_from_metadata <- function(parsed, meta_row) {
  if (is.null(parsed)) return(parsed)

  p <- parsed$params

  # Codelist ID: for ct_assign and ct_decode
  if (parsed$rule_type %in% c("ct_assign", "ct_decode")) {
    cl <- meta_row[["codelist_id"]]
    if (is.null(p$codelist_id) && !is.null(cl) && !is.na(cl) && nchar(cl) > 0) {
      p$codelist_id <- cl
    }
  }

  # Significant digits: for numeric_round
  if (parsed$rule_type == "numeric_round") {
    sd <- meta_row[["significant_digits"]]
    if (is.null(p$digits) && !is.null(sd) && !is.na(sd)) {
      p$digits <- as.integer(sd)
    }
  }

  # Length: stored for type enforcement in finalize
  len <- meta_row[["length"]]
  if (!is.null(len) && !is.na(len)) {
    p$.length <- as.integer(len)
  }

  # Data type: stored for type coercion
  dtype <- meta_row[["type"]]
  if (!is.null(dtype) && !is.na(dtype)) {
    p$.target_type <- dtype
  }

  parsed$params <- p
  parsed
}


# ==============================================================================
# Backwards compatibility: map_method_to_rule_type()
# ==============================================================================

#' Map METHOD values to internal rule types
#'
#' Translates a vector of METHOD strings (from the Variables sheet) into
#' the corresponding `rule_type` values used by [compile_rules()] and
#' [derive_variable()].
#'
#' Supports both explicit function-call syntax (new) and legacy keywords.
#'
#' @param method_vector Character vector. METHOD values from the Variables sheet.
#' @return Character vector of the same length with mapped rule_type values.
#'   `NA` for empty/unknown methods.
#'
#' @examples
#' map_method_to_rule_type(c("SEQ", NA, 'derive_dy(dtc_var = "AEDTC")'))
#'
#' @export
map_method_to_rule_type <- function(method_vector) {
  result <- rep(NA_character_, length(method_vector))

  for (i in seq_along(method_vector)) {
    m <- method_vector[i]
    if (is.na(m) || nchar(trimws(m)) == 0L) next

    parsed <- suppressWarnings(parse_method_call(m))
    if (!is.null(parsed)) {
      result[i] <- parsed$rule_type
    }
  }

  result
}


# ==============================================================================
# Reconstruct METHOD string from rule_type + params (for display / Excel export)
# ==============================================================================

#' Reconstruct an explicit METHOD string from a rule type and parameters
#'
#' Useful for generating the METHOD column value when creating or exporting
#' metadata.
#'
#' @param rule_type Character. Internal rule type.
#' @param params Named list of parameters.
#' @return Character. Method call string like `derive_seq(by = "USUBJID")`.
#' @export
reconstruct_method_string <- function(rule_type, params = list()) {
  fn_name <- RULE_TYPE_TO_FUNCTION[[rule_type]]
  if (is.null(fn_name) || is.na(fn_name)) fn_name <- rule_type

  # Filter out internal params (starting with ".")
  display_params <- params[!grepl("^\\.", names(params))]

  # Filter out params that are NULL or single NA
  display_params <- display_params[!vapply(display_params, function(x) {
    is.null(x) || (length(x) == 1 && is.na(x))
  }, logical(1))]

  if (length(display_params) == 0L) {
    return(paste0(fn_name, "()"))
  }

  paste0(fn_name, "(", .format_params(display_params), ")")
}
