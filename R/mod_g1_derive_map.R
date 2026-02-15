# ==============================================================================
# Module G1: Mapping & Transform Derivations
# ==============================================================================

#' Direct mapping from source column
#' @param data Tibble.
#' @param target_var Character.
#' @param source_var Character.
#' @param transform Function or `NULL`.
#' @param type Character or `NULL`.
#' @param blank_to_na Logical. Default `TRUE`.
#' @return Tibble.
#' @export
map_direct <- function(data, target_var, source_var, transform = NULL,
                       type = NULL, blank_to_na = TRUE) {
  if (!source_var %in% names(data)) {
    abort(glue::glue("map_direct: source column '{source_var}' not found in data"))
  }
  vals <- data[[source_var]]
  if (blank_to_na && is.character(vals)) {
    vals[!is.na(vals) & trimws(vals) == ""] <- NA_character_
  }
  if (!is.null(transform)) {
    if (is.character(transform)) {
      transform <- match.fun(transform)
    }
    vals <- transform(vals)
  }
  if (!is.null(type)) {
    vals <- switch(type,
      char = , character = as.character(vals),
      num  = , numeric   = suppressWarnings(as.numeric(vals)),
      int  = , integer   = suppressWarnings(as.integer(vals)),
      vals
    )
  }
  data[[target_var]] <- vals
  data
}

#' Derive a constant value
#' @param data Tibble.
#' @param target_var Character.
#' @param value Scalar.
#' @param when Expression or `NULL`.
#' @param type Character or `NULL`.
#' @return Tibble.
#' @export
derive_constant <- function(data, target_var, value, when = NULL, type = NULL) {
  if (is.null(when)) {
    data[[target_var]] <- rep(value, nrow(data))
  } else {
    data[[target_var]] <- dplyr::if_else(when, value, NA)
  }
  if (!is.null(type)) {
    data[[target_var]] <- switch(type,
      char = as.character(data[[target_var]]),
      num  = as.numeric(data[[target_var]]),
      data[[target_var]]
    )
  }
  data
}

#' Derive SOURCEID from review_status lookup
#'
#' Looks up the CRF form name from a review_status-like dataset and sets
#' SOURCEID = "CRF: <FormName>".
#'
#' @param data Tibble.
#' @param target_var Character — target column name.
#' @param form_id Character — the form code to look up (e.g. "AE").
#' @param raw_data Named list of raw datasets.
#' @param dataset Character — name of the review_status dataset in raw_data.
#' @param id_col Character — column containing form IDs.
#' @param name_col Character — column containing form names.
#' @return Tibble.
#' @export
derive_sourceid <- function(data, target_var, form_id,
                            raw_data = NULL,
                            dataset = "review_status",
                            id_col = "formid",
                            name_col = "formname") {
  form_name <- NA_character_
  if (!is.null(raw_data) && dataset %in% names(raw_data)) {
    rs <- raw_data[[dataset]]
    nms <- tolower(names(rs))
    names(rs) <- nms
    id_col_lc   <- tolower(id_col)
    name_col_lc <- tolower(name_col)
    if (id_col_lc %in% nms && name_col_lc %in% nms) {
      match_idx <- which(tolower(rs[[id_col_lc]]) == tolower(form_id))
      if (length(match_idx) > 0L) {
        form_name <- trimws(rs[[name_col_lc]][match_idx[1]])
      }
    }
  }
  val <- if (!is.na(form_name) && nchar(form_name) > 0L) {
    paste("CRF:", form_name)
  } else {
    paste("CRF:", form_id)
  }
  data[[target_var]] <- val
  data
}

#' Derive variable as first non-missing across sources
#' @param data Tibble.
#' @param target_var Character.
#' @param sources Character vector.
#' @param blank_to_na Logical. Default `TRUE`.
#' @return Tibble.
#' @export
derive_coalesce <- function(data, target_var, sources, blank_to_na = TRUE) {
  cols <- lapply(sources, function(s) {
    v <- data[[s]]
    if (blank_to_na && is.character(v)) v[!is.na(v) & trimws(v) == ""] <- NA_character_
    as.character(v)
  })
  data[[target_var]] <- do.call(dplyr::coalesce, cols)
  data
}

#' Derive variable using binary conditional logic
#' @param data Tibble.
#' @param target_var Character.
#' @param condition Expression or character.
#' @param true_value Value.
#' @param false_value Value.
#' @param missing_value Value. Default `NA`.
#' @return Tibble.
#' @export
derive_if_else <- function(data, target_var, condition, true_value,
                           false_value, missing_value = NA) {
  if (is.character(condition)) {
    mask <- rlang::eval_tidy(rlang::parse_expr(condition), data = data)
  } else {
    mask <- condition
  }
  data[[target_var]] <- dplyr::if_else(mask, true_value, false_value, missing = missing_value)
  data
}

#' Derive variable using multi-branch conditional logic
#' @param data Tibble.
#' @param target_var Character.
#' @param conditions Named list.
#' @param default Value. Default `NA`.
#' @return Tibble.
#' @export
derive_case_when <- function(data, target_var, conditions, default = NA) {
  n <- nrow(data)
  result  <- rep(as.character(default), n)
  matched <- rep(FALSE, n)
  for (cond_expr in names(conditions)) {
    mask <- rlang::eval_tidy(rlang::parse_expr(cond_expr), data = data)
    mask[is.na(mask)] <- FALSE
    to_set <- mask & !matched

    val_expr <- conditions[[cond_expr]]
    # If the value looks like an R expression (references a column or function),
    # evaluate it against the data; else treat as literal string.
    val <- tryCatch({
      parsed <- rlang::parse_expr(val_expr)
      evaluated <- rlang::eval_tidy(parsed, data = data)
      # length-n vector  = row-wise expression result
      # length-1 scalar  = constant expression (broadcast)
      # other length     = mismatch, fall back to literal
      if (length(evaluated) == n) {
        as.character(evaluated)
      } else if (length(evaluated) == 1L) {
        rep(as.character(evaluated), n)
      } else {
        rep(as.character(val_expr), n)
      }
    }, error = function(e) {
      rep(as.character(val_expr), n)
    })
    result[to_set] <- val[to_set]
    matched <- matched | mask
  }
  data[[target_var]] <- result
  data
}

#' Extract values using regex
#' @param data Tibble.
#' @param target_var Character.
#' @param source_var Character.
#' @param pattern Character.
#' @param group Integer. Default `1L`.
#' @param transform Function or `NULL`.
#' @return Tibble.
#' @export
derive_regex_extract <- function(data, target_var, source_var, pattern,
                                 group = 1L, transform = NULL) {
  vals <- stringr::str_match(data[[source_var]], pattern)[, group + 1L]
  if (!is.null(transform)) vals <- transform(vals)
  data[[target_var]] <- vals
  data
}

#' Replace text patterns using regex
#' @param data Tibble.
#' @param target_var Character.
#' @param source_var Character.
#' @param pattern Character.
#' @param replacement Character.
#' @param all Logical. Default `TRUE`.
#' @return Tibble.
#' @export
derive_regex_replace <- function(data, target_var, source_var, pattern,
                                 replacement, all = TRUE) {
  fn <- if (all) stringr::str_replace_all else stringr::str_replace
  data[[target_var]] <- fn(data[[source_var]], pattern, replacement)
  data
}

#' Concatenate multiple source columns
#' @param data Tibble.
#' @param target_var Character.
#' @param sources Character vector.
#' @param sep Character. Default `""`.
#' @param na_rm Logical. Default `TRUE`.
#' @param trim Logical. Default `TRUE`.
#' @return Tibble.
#' @export
derive_concat <- function(data, target_var, sources, sep = "",
                          na_rm = TRUE, trim = TRUE) {
  vals <- purrr::pmap_chr(data[sources], function(...) {
    parts <- c(...)
    if (na_rm) parts <- parts[!is.na(parts)]
    paste(parts, collapse = sep)
  })
  if (trim) vals <- trimws(vals)
  data[[target_var]] <- vals
  data
}

#' Trim or pad a string variable
#' @param data Tibble.
#' @param target_var Character.
#' @param source_var Character.
#' @param width Integer or `NULL`.
#' @param side Character. Default `"both"`.
#' @param pad Character. Default `" "`.
#' @return Tibble.
#' @export
derive_trim_pad <- function(data, target_var, source_var, width = NULL,
                            side = "both", pad = " ") {
  vals <- trimws(data[[source_var]], which = side)
  if (!is.null(width)) vals <- stringr::str_pad(vals, width = width, side = "right", pad = pad)
  data[[target_var]] <- vals
  data
}

#' Round numeric values
#' @param data Tibble.
#' @param target_var Character.
#' @param source_var Character.
#' @param digits Integer. Default `0L`.
#' @return Tibble.
#' @export
derive_numeric_round <- function(data, target_var, source_var, digits = 0L) {
  data[[target_var]] <- round(as.numeric(data[[source_var]]), digits = digits)
  data
}

#' Standardize units
#' @param data Tibble.
#' @param target_var Character.
#' @param source_var Character.
#' @param unit_map Named list.
#' @return Tibble.
#' @export
derive_unit_standardize <- function(data, target_var, source_var, unit_map = list()) {
  vals <- data[[source_var]]
  for (from in names(unit_map)) {
    vals[tolower(vals) == tolower(from)] <- unit_map[[from]]
  }
  data[[target_var]] <- vals
  data
}
