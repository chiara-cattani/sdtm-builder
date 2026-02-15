# ===========================================================================
# Module H0: Metadata-driven domain preprocessing
# ===========================================================================
#
# Replaces manual hooks with a standardized, metadata-driven approach.
# Reads "Sources" + "Source Columns" sheets from Study_Metadata.xlsx.
# Provides generic preprocessing functions (filter, derive, stack, merge)
# called per domain based on metadata declarations.
#
# Architecture mirrors the variable-level derivation system:
#   Variable level:  Variables sheet -> METHOD -> FUNCTION_REGISTRY -> derive_variable()
#   Domain level:    Sources sheet   -> METHOD -> source methods    -> preprocess_domain()
#
# @keywords internal
# ===========================================================================

#' @importFrom stats setNames
NULL

# ---- Parse & Evaluate Source Methods ----------------------------------------

#' Parse a source column method string into function name + arguments
#'
#' Supports syntax: `fn_name(arg1, arg2, "quoted", .default = val)`
#'
#' @param method Character. Method string like `hardcode("Y")`.
#' @return List with `fn` and `args`.
#' @noRd
.parse_source_method <- function(method) {
  if (is.na(method) || !nzchar(trimws(method))) {
    return(list(fn = "hardcode", args = list(NA_character_)))
  }
  method <- trimws(method)

  fn_match <- regmatches(
    method,
    regexec("^([a-z_][a-z0-9_]*)\\((.*)\\)\\s*$", method, perl = TRUE)
  )[[1]]

	if (length(fn_match) == 3L) {
    fn_name   <- fn_match[2]
    args_text <- fn_match[3]
    args <- .split_source_args(args_text)
    return(list(fn = fn_name, args = args))
  }

  # Unquoted token: treat as column reference via assign()
  return(list(fn = "assign", args = list(method)))
}

#' Split comma-separated arguments respecting quotes and parens
#' @noRd
.split_source_args <- function(text) {
  if (is.na(text) || !nzchar(trimws(text))) return(list())
  args <- list()
  current <- ""
  depth  <- 0L
  in_q1  <- FALSE
  in_q2  <- FALSE
  chars  <- strsplit(text, "")[[1]]

  for (ch in chars) {
    if (ch == '"' && !in_q1)  { in_q2 <- !in_q2; current <- paste0(current, ch); next }
    if (ch == "'" && !in_q2)  { in_q1 <- !in_q1; current <- paste0(current, ch); next }
    if (!in_q1 && !in_q2) {
      if (ch == "(") depth <- depth + 1L
      if (ch == ")") depth <- depth - 1L
      if (ch == "," && depth == 0L) {
        args <- c(args, list(trimws(current)))
        current <- ""
        next
      }
    }
    current <- paste0(current, ch)
  }
  if (nzchar(trimws(current))) args <- c(args, list(trimws(current)))

  # Unquote string literals, parse numerics/NA
  lapply(args, .parse_source_val)
}

#' Parse a single argument value
#' @noRd
.parse_source_val <- function(x) {
  if (grepl('^".*"$', x))   return(gsub('^"|"$', "", x))
  if (grepl("^'.*'$", x))   return(gsub("^'|'$", "", x))
  if (tolower(x) == "na")   return(NA_character_)
  if (tolower(x) == "true")  return(TRUE)
  if (tolower(x) == "false") return(FALSE)
  if (grepl("^-?\\d+\\.?\\d*$", x)) return(as.numeric(x))
  # Named argument: .default = "val" (already handled by split)
  x
}


#' Evaluate a single source-column method against the data
#'
#' @param method Character. Method string (e.g., `hardcode("Y")`).
#' @param data Data frame. Current block data.
#' @param raw_data Named list of raw datasets.
#' @param config Config object.
#' @param built_domains Named list of built domain results.
#' @return Vector of derived values (length == nrow(data)).
#' @noRd
eval_source_method <- function(method, data, raw_data = list(),
                               config = list(), built_domains = list()) {
  parsed <- .parse_source_method(method)
  fn     <- parsed$fn
  args   <- parsed$args
  n      <- nrow(data)

  switch(fn,
    # -- Constants & column copy --
    hardcode = {
      val <- if (length(args) >= 1L) args[[1]] else NA_character_
      rep(val, n)
    },
    assign = {
      col <- tolower(as.character(args[[1]]))
      if (col %in% names(data)) as.character(data[[col]]) else rep(NA_character_, n)
    },

    # -- Date formatting --
    format_dtc = {
      col <- tolower(as.character(args[[1]]))
      raw_vals <- if (col %in% names(data)) as.character(data[[col]]) else rep(NA_character_, n)
      .strip_midnight(raw_vals)
    },

    # -- Missingness check --
    if_missing = {
      col         <- tolower(as.character(args[[1]]))
      when_miss   <- if (length(args) >= 2L) args[[2]] else NA_character_
      when_pres   <- if (length(args) >= 3L) args[[3]] else NA_character_
      vals <- if (col %in% names(data)) as.character(data[[col]]) else rep(NA_character_, n)
      ifelse(is.na(vals) | vals == "", when_miss, when_pres)
    },

    # -- Simple value mapping: case_value(col, from1, to1, from2, to2, ...) --
    case_value = {
      col <- tolower(as.character(args[[1]]))
      vals <- if (col %in% names(data)) as.character(data[[col]]) else rep(NA_character_, n)
      result <- rep(NA_character_, n)
      pairs <- args[-1]  # remaining args are from/to pairs
      # Check for .default
      default_val <- NA_character_
      named <- which(grepl("^\\.default=", pairs))
      if (length(named) > 0L) {
        default_val <- sub("^\\.default=", "", pairs[[named[1]]])
        default_val <- .parse_source_val(default_val)
        pairs <- pairs[-named]
      }
      result <- rep(as.character(default_val), n)
      # Apply pairs in reverse order so first match wins
      if (length(pairs) >= 2L) {
        for (k in seq(length(pairs), 2, by = -2)) {
          from_val <- as.character(pairs[[k - 1]])
          to_val   <- as.character(pairs[[k]])
          result[!is.na(vals) & vals == from_val] <- to_val
        }
      }
      result
    },

    # -- Multi-column priority: first_match(col1, val1, res1, col2, val2, res2, ..., default) --
    first_match = {
      result <- rep(NA_character_, n)
      # Parse triples: col, match_val, result_val, ...
      rest   <- args
      # Last item may be the default if count mod 3 != 0
      if (length(rest) %% 3 != 0) {
        default_val <- as.character(rest[[length(rest)]])
        rest <- rest[-length(rest)]
        result <- rep(default_val, n)
      }
      # Apply triples in reverse so first wins
      if (length(rest) >= 3L) {
        triples <- split(seq_along(rest), ceiling(seq_along(rest) / 3))
        for (tri in rev(triples)) {
          col_name  <- tolower(as.character(rest[[tri[1]]]))
          match_val <- as.character(rest[[tri[2]]])
          res_val   <- as.character(rest[[tri[3]]])
          col_vals  <- if (col_name %in% names(data)) as.character(data[[col_name]]) else rep(NA_character_, n)
          matched   <- !is.na(col_vals) & col_vals == match_val
          result[matched] <- res_val
        }
      }
      result
    },

    # -- Conditional copy: conditional_assign(cond_col, cond_val, src_col, default) --
    conditional_assign = {
      cond_col <- tolower(as.character(args[[1]]))
      cond_val <- as.character(args[[2]])
      src_col  <- tolower(as.character(args[[3]]))
      default_val <- if (length(args) >= 4) args[[4]] else NA_character_
      cond_vals <- if (cond_col %in% names(data)) as.character(data[[cond_col]]) else rep(NA_character_, n)
      src_vals  <- if (src_col %in% names(data))  as.character(data[[src_col]])  else rep(NA_character_, n)
      result <- rep(as.character(default_val), n)
      hit <- !is.na(cond_vals) & cond_vals == cond_val
      result[hit] <- src_vals[hit]
      result
    },

    # -- Concatenation: concat(sep, part1, part2, ...) --
    concat = {
      sep  <- as.character(args[[1]])
      parts <- args[-1]
      # Each part: quoted = literal, unquoted = column reference
      resolved <- lapply(parts, function(p) {
        p_str <- as.character(p)
        col <- tolower(p_str)
        if (col %in% names(data)) as.character(data[[col]]) else rep(p_str, n)
      })
      result <- resolved[[1]]
      if (length(resolved) > 1L) {
        for (k in seq(2, length(resolved))) {
          result <- paste0(result, sep, resolved[[k]])
        }
      }
      result
    },

    # -- Paste with optional part: paste_if_present(base_col, sep, opt_col) --
    paste_if_present = {
      base_col <- tolower(as.character(args[[1]]))
      sep      <- as.character(args[[2]])
      opt_col  <- tolower(as.character(args[[3]]))
      base_vals <- if (base_col %in% names(data)) as.character(data[[base_col]]) else rep(NA_character_, n)
      opt_vals  <- if (opt_col %in% names(data))  data[[opt_col]]                else rep(NA, n)
      ifelse(
        !is.na(opt_vals),
        paste0(base_vals, sep, as.character(as.integer(opt_vals))),
        base_vals
      )
    },

    # -- Arithmetic: calc(expr) --
    calc = {
      expr_str <- as.character(args[[1]])
      tryCatch(
        eval(parse(text = expr_str), envir = data),
        error = function(e) rep(NA_real_, n)
      )
    },

    # -- Aggregate min: agg_min(col) --
    agg_min = {
      # Returns the min value of col — typically combined with GROUP_BY
      col <- tolower(as.character(args[[1]]))
      if (col %in% names(data)) as.character(data[[col]]) else rep(NA_character_, n)
      # Actual aggregation is handled at block level when merge_type = LEFT_JOIN
    },

    # -- Rank: rank_by(order_col, by_col) --
    rank_by = {
      order_col <- tolower(as.character(args[[1]]))
      by_col    <- tolower(as.character(args[[2]]))
      if (order_col %in% names(data) && by_col %in% names(data)) {
        o <- order(data[[order_col]], data[[by_col]], na.last = TRUE)
        result <- rep(NA_character_, n)
        result[o] <- as.character(seq_len(n))
        result
      } else {
        rep(NA_character_, n)
      }
    },

    # -- Cross-dataset existence check: exists_in(source_name, col) --
    exists_in = {
      src_name <- tolower(as.character(args[[1]]))
      col      <- tolower(as.character(args[[2]]))
      col_vals <- if (col %in% names(data)) as.character(data[[col]]) else rep(NA_character_, n)
      if (src_name %in% names(raw_data)) {
        other <- raw_data[[src_name]]
        names(other) <- tolower(names(other))
        other_ids <- as.character(other[[col]])
        col_vals %in% other_ids
      } else {
        rep(FALSE, n)
      }
    },

    # -- Cross-domain lookup: lookup(domain_or_source, key_col, return_col) --
    lookup = {
      domain_name <- toupper(as.character(args[[1]]))
      key_col     <- toupper(as.character(args[[2]]))
      return_col  <- toupper(as.character(args[[3]]))
      # Try built_domains first
      lk_data <- NULL
      if (domain_name %in% names(built_domains)) {
        lk_data <- built_domains[[domain_name]]$data
      }
      # Fallback: try loading from disk
      if (is.null(lk_data) && !is.null(config$output_dir)) {
        rda_path <- file.path(config$output_dir, "RDA", paste0(tolower(domain_name), ".rda"))
        if (file.exists(rda_path)) {
          env <- new.env(parent = emptyenv())
          load(rda_path, envir = env)
          lk_data <- env[[ls(env)[1]]]
        }
      }
      if (!is.null(lk_data)) {
        names(lk_data) <- toupper(names(lk_data))
        # The local key column (in current data)
        local_key <- tolower(as.character(args[[2]]))
        # Find the matching derived column in current data
        # Convention: key_col = IETESTCD, local column = ietestcd_derived
        local_col <- paste0(local_key, "_derived")
        if (!local_col %in% names(data)) local_col <- tolower(local_key)
        if (!local_col %in% names(data)) return(rep(NA_character_, n))

        lk_ref <- unique(lk_data[, c(key_col, return_col), drop = FALSE])
        match_idx <- match(data[[local_col]], lk_ref[[key_col]])
        result <- rep(NA_character_, n)
        matched <- !is.na(match_idx)
        result[matched] <- as.character(lk_ref[[return_col]][match_idx[matched]])
        result
      } else {
        rep(NA_character_, n)
      }
    },

    # -- Pivot index: pivot_index(format) --
    pivot_index = {
      fmt <- as.character(args[[1]])
      # .pivot_idx is injected by expand_pivot()
      idx <- data$.pivot_idx
      if (is.null(idx)) rep(NA_character_, n)
      else sprintf(fmt, idx)
    },

    # -- General R expression: expression(r_code) --
    expression = {
      expr_str <- as.character(args[[1]])
      tryCatch(
        as.character(eval(parse(text = expr_str), envir = data)),
        error = function(e) rep(NA_character_, n)
      )
    },

    # -- case_when: case_when(cond1, result1, cond2, result2, ..., .default=val) --
    case_when = {
      rest <- args
      default_val <- NA_character_
      # Extract .default
      named_idx <- which(vapply(rest, function(a) {
        is.character(a) && grepl("^\\.default=", a)
      }, logical(1)))
      if (length(named_idx) > 0L) {
        default_val <- sub("^\\.default=", "", rest[[named_idx[1]]])
        default_val <- .parse_source_val(default_val)
        rest <- rest[-named_idx]
      }
      # Resolve .default: column reference or literal
      default_str <- as.character(default_val)
      default_lc  <- tolower(default_str)
      if (!is.na(default_str) && default_lc %in% names(data)) {
        result <- as.character(data[[default_lc]])
      } else {
        result <- rep(default_str, n)
      }
      # Process condition/result pairs in reverse so first match wins
      if (length(rest) >= 2L) {
        for (k in seq(length(rest), 2, by = -2)) {
          cond_str <- as.character(rest[[k - 1]])
          res_val  <- rest[[k]]
          # Evaluate condition
          cond <- tryCatch(
            eval(parse(text = cond_str), envir = data),
            error = function(e) rep(FALSE, n)
          )
          cond[is.na(cond)] <- FALSE
          # Resolve result: expression(), column reference, or literal
          res_str <- as.character(res_val)
          if (grepl("^expression\\(", res_str)) {
            expr_text <- sub("^expression\\((.*)\\)$", "\\1", res_str)
            full_res <- tryCatch(
              as.character(eval(parse(text = expr_text), envir = data)),
              error = function(e) rep(NA_character_, n)
            )
            result[cond] <- full_res[cond]
          } else {
            res_col <- tolower(res_str)
            if (res_col %in% names(data)) {
              result[cond] <- as.character(data[[res_col]][cond])
            } else {
              result[cond] <- res_str
            }
          }
        }
      }
      result
    },

    # -- Paste columns/literals together: paste_cols(part1, part2, ...) --
    paste_cols = {
      resolved <- vapply(args, function(p) {
        p_str <- as.character(p)
        col   <- tolower(p_str)
        if (col %in% names(data)) as.character(data[[col]]) else rep(p_str, n)
      }, character(n))
      if (is.matrix(resolved)) {
        apply(resolved, 1, paste0, collapse = "")
      } else {
        paste0(resolved, collapse = "")
      }
    },

    # -- Sum of (col_i - col_j) pairs: sum_pairs(a1, b1, a2, b2, ...) --
    sum_pairs = {
      result <- rep(0, n)
      if (length(args) >= 2L) {
        for (k in seq(1, length(args), by = 2)) {
          a_col <- tolower(as.character(args[[k]]))
          b_col <- if (k + 1 <= length(args)) tolower(as.character(args[[k + 1]])) else NULL
          a_vals <- if (a_col %in% names(data)) suppressWarnings(as.numeric(data[[a_col]])) else rep(0, n)
          a_vals[is.na(a_vals)] <- 0
          if (!is.null(b_col)) {
            b_vals <- if (b_col %in% names(data)) suppressWarnings(as.numeric(data[[b_col]])) else rep(0, n)
            b_vals[is.na(b_vals)] <- 0
            result <- result + (a_vals - b_vals)
          } else {
            result <- result + a_vals
          }
        }
      }
      as.character(result)
    },

    # Default: unknown method
    {
      warning("Unknown source method: ", fn, call. = FALSE)
      rep(NA_character_, n)
    }
  )
}


# ---- Helpers ----------------------------------------------------------------

#' Strip midnight timestamps from DTC strings
#' @noRd
.strip_midnight <- function(dtc) {
  dtc <- as.character(dtc)
  vapply(dtc, function(d) {
    if (is.na(d) || d == "") return(NA_character_)
    if (grepl("T00:00", d)) return(substr(d, 1, 10))
    return(d)
  }, character(1), USE.NAMES = FALSE)
}

#' Convert all Date/POSIXt columns to character
#' @noRd
.char_dates <- function(d) {
  for (col in names(d)) {
    if (inherits(d[[col]], c("Date", "POSIXt")))
      d[[col]] <- as.character(d[[col]])
  }
  d
}


# ---- Visit & Timepoint Mapping ---------------------------------------------

#' Apply visit mapping from config to source data
#'
#' Adds visit_derived, visitnum_derived, visitdy_derived from EventId.
#'
#' @param data Data frame with an `eventid` column.
#' @param config Config with `cfg_yaml$visit_map`.
#' @param filter_unmatched If TRUE, drop rows without a visit_map match.
#' @return Modified data frame.
#' @noRd
.apply_visit_mapping <- function(data, config, filter_unmatched = FALSE) {
  vm <- config$cfg_yaml$visit_map
  if (is.null(vm) || !"eventid" %in% names(data)) return(data)

  visit_lookup <- do.call(rbind, lapply(vm, function(v) {
    data.frame(
      code             = toupper(as.character(v$code)),
      visit_derived    = as.character(v$visit),
      visitnum_derived = as.numeric(v$visitnum),
      visitdy_derived  = if (!is.null(v$visitdy)) as.integer(v$visitdy) else NA_integer_,
      stringsAsFactors = FALSE
    )
  }))

  eid <- toupper(trimws(as.character(data$eventid)))
  idx <- match(eid, visit_lookup$code)

  data$visit_derived    <- visit_lookup$visit_derived[idx]
  data$visitnum_derived <- visit_lookup$visitnum_derived[idx]
  data$visitdy_derived  <- visit_lookup$visitdy_derived[idx]

  if (filter_unmatched) {
    data <- data[!is.na(idx), , drop = FALSE]
  }
  data
}

#' Apply timepoint mapping from config to source data
#'
#' Adds tpt_derived, tptnum_derived, tptref_derived, eltm_derived from ActivityID.
#'
#' @param data Data frame with an `activityid` column.
#' @param config Config with `cfg_yaml$timepoint_map`.
#' @param normalize_pattern Optional pattern like `GI_W{w}D{d}` to normalize before lookup.
#' @param filter_unmatched If TRUE, drop rows without a match.
#' @return Modified data frame.
#' @noRd
.apply_tpt_mapping <- function(data, config, normalize_pattern = NULL,
                               filter_unmatched = FALSE,
                               col_prefix = "") {
  tm <- config$cfg_yaml$timepoint_map
  if (is.null(tm) || !"activityid" %in% names(data)) return(data)

  tpt_lookup <- do.call(rbind, lapply(tm, function(t) {
    data.frame(
      code         = toupper(as.character(t$code)),
      tpt_derived  = as.character(t$tpt),
      tptnum_derived = as.numeric(t$tptnum),
      tptref_derived = as.character(t$tptref),
      eltm_derived   = as.character(t$eltm),
      stringsAsFactors = FALSE
    )
  }))

  aid_raw <- toupper(trimws(as.character(data$activityid)))

  # Normalise ActivityID if a pattern is provided
  if (!is.null(normalize_pattern) && nzchar(normalize_pattern)) {
    if (normalize_pattern == "GI_W{w}D{d}") {
      aid_raw <- vapply(aid_raw, function(a) {
        m <- regmatches(a, regexec("^GI_W(\\d+)D(\\d+)$", a, ignore.case = TRUE))[[1]]
        if (length(m) == 3L) paste0("D", m[3], "W", m[2], "_SPI") else a
      }, character(1), USE.NAMES = FALSE)
    }
  }

  idx <- match(aid_raw, tpt_lookup$code)

  data[[paste0(col_prefix, "tpt_derived")]]    <- tpt_lookup$tpt_derived[idx]
  data[[paste0(col_prefix, "tptnum_derived")]] <- tpt_lookup$tptnum_derived[idx]
  data[[paste0(col_prefix, "tptref_derived")]] <- tpt_lookup$tptref_derived[idx]
  data[[paste0(col_prefix, "eltm_derived")]]   <- tpt_lookup$eltm_derived[idx]

  if (filter_unmatched) {
    data <- data[!is.na(idx), , drop = FALSE]
  }
  data
}


# ---- Pivot / Unpivot -------------------------------------------------------

#' Expand rows for UNPIVOT blocks
#'
#' For each row in `data`, find columns matching `pattern` that are non-missing,
#' and create one output row per match. Adds `.pivot_idx` column.
#'
#' @param data Data frame.
#' @param pattern Regex pattern matching column names (e.g., `"^ieintestcd"`).
#' @return Expanded data frame with `.pivot_idx` column.
#' @noRd
.expand_pivot <- function(data, pattern) {
  pivot_cols <- grep(pattern, names(data), value = TRUE, ignore.case = TRUE)
  # Sort by numeric suffix
  nums <- as.integer(gsub("\\D", "", pivot_cols))
  pivot_cols <- pivot_cols[order(nums)]

  if (length(pivot_cols) == 0L) return(data)

  rows <- list()
  for (i in seq_len(nrow(data))) {
    for (j in seq_along(pivot_cols)) {
      val <- data[[pivot_cols[j]]][i]
      if (!is.na(val) && nzchar(trimws(as.character(val)))) {
        row <- data[i, , drop = FALSE]
        row$.pivot_idx <- j
        rows[[length(rows) + 1L]] <- row
      }
    }
  }
  if (length(rows) == 0L) return(data[0, , drop = FALSE])
  dplyr::bind_rows(rows)
}


# ---- Block & Domain Processing ----------------------------------------------

#' Process a single source block
#'
#' @param block_info One-row data frame from the Sources sheet.
#' @param source_cols Data frame of Source Columns for this block.
#' @param raw_data Named list of raw datasets.
#' @param config Config object.
#' @param built_domains Named list of built domain results.
#' @return Processed data frame, or NULL if empty.
#' @noRd
.process_source_block <- function(block_info, source_cols, raw_data, config,
                                  built_domains = list()) {
  source_name <- tolower(trimws(as.character(block_info$source)))
  if (!source_name %in% names(raw_data)) return(NULL)

  src <- raw_data[[source_name]]
  names(src) <- tolower(names(src))
  src <- .char_dates(src)

  # Apply filter
  filter_expr <- as.character(block_info$filter)
  if (!is.na(filter_expr) && nzchar(trimws(filter_expr))) {
    keep <- tryCatch(
      eval(parse(text = filter_expr), envir = src),
      error = function(e) { warning("Filter error: ", e$message); rep(TRUE, nrow(src)) }
    )
    keep[is.na(keep)] <- FALSE
    src <- src[keep, , drop = FALSE]
  }
  if (nrow(src) == 0L) return(NULL)

  # Handle PIVOT_PATTERN
  pivot_pattern <- as.character(block_info$pivot_pattern)
  if (!is.na(pivot_pattern) && nzchar(trimws(pivot_pattern))) {
    src <- .expand_pivot(src, pivot_pattern)
    if (nrow(src) == 0L) return(NULL)
  }

  # Track original columns to identify added ones later
  orig_cols <- names(src)

  # Apply column derivations in order
  if (nrow(source_cols) > 0L) {
    source_cols <- source_cols[order(source_cols$col_order), , drop = FALSE]
    for (j in seq_len(nrow(source_cols))) {
      target <- as.character(source_cols$target_column[j])
      method <- as.character(source_cols$method[j])
      if (is.na(target) || !nzchar(trimws(target))) next
      src[[target]] <- eval_source_method(method, src, raw_data, config, built_domains)
    }
  }

  # Apply visit mapping
  map_visit <- toupper(trimws(as.character(block_info$map_visit)))
  if (!is.na(map_visit) && map_visit %in% c("Y", "FILTER")) {
    src <- .apply_visit_mapping(src, config, filter_unmatched = (map_visit == "FILTER"))
  }
  if (nrow(src) == 0L) return(NULL)

  # Apply timepoint mapping (domain-prefixed column names)
  map_tpt <- toupper(trimws(as.character(block_info$map_timepoint)))
  if (!is.na(map_tpt) && map_tpt %in% c("Y", "FILTER")) {
    tpt_norm <- as.character(block_info$tpt_normalize)
    domain_prefix <- tolower(as.character(block_info$domain))
    src <- .apply_tpt_mapping(
      src, config,
      normalize_pattern = if (!is.na(tpt_norm)) tpt_norm else NULL,
      filter_unmatched  = (map_tpt == "FILTER"),
      col_prefix = domain_prefix
    )
  }
  if (nrow(src) == 0L) return(NULL)

  # Add SOURCEID
  sourceid <- as.character(block_info$sourceid)
  if (!is.na(sourceid) && nzchar(trimws(sourceid))) {
    src$sourceid_derived <- sourceid
  }

  # Select output columns: subjectid + all newly added columns
  added_cols <- setdiff(names(src), orig_cols)
  keep_cols <- unique(c("subjectid", added_cols))
  keep_cols <- intersect(keep_cols, names(src))
  src <- src[, keep_cols, drop = FALSE]

  # Remove working columns (prefixed with _ or .)
  working <- grep("^[_.]", names(src), value = TRUE)
  if (length(working)) src <- src[, setdiff(names(src), working), drop = FALSE]

  # Convert all non-subjectid columns to character for consistent bind_rows
  for (col in setdiff(names(src), "subjectid")) {
    src[[col]] <- as.character(src[[col]])
  }

  src
}


#' Preprocess one domain using metadata-driven sources
#'
#' Reads the Sources sheet entries for the given domain, processes
#' each source block, and combines them (STACK or LEFT_JOIN).
#'
#' @param domain Character. Domain name (e.g., "CE").
#' @param raw_data Named list of raw datasets.
#' @param config Config object.
#' @param sources_meta Tibble from the "Sources" sheet.
#' @param source_cols_meta Tibble from the "Source Columns" sheet.
#' @param built_domains Named list of already-built domain results.
#' @param verbose Logical.
#' @return Modified `raw_data` (with preprocessed domain data added).
#' @export
preprocess_domain <- function(domain, raw_data, config,
                              sources_meta, source_cols_meta,
                              built_domains = list(),
                              verbose = FALSE) {
  dom_sources <- sources_meta[toupper(sources_meta$domain) == toupper(domain), , drop = FALSE]
  if (nrow(dom_sources) == 0L) return(raw_data)

  dom_sources <- dom_sources[order(dom_sources$step_order), , drop = FALSE]

  # Separate POST blocks from regular blocks
  is_post <- toupper(trimws(as.character(dom_sources$merge_type))) == "POST"
  post_blocks    <- dom_sources[is_post, , drop = FALSE]
  regular_blocks <- dom_sources[!is_post, , drop = FALSE]

  # Process regular blocks
  stack_results <- list()
  join_blocks   <- list()

  for (bid in unique(regular_blocks$block_id)) {
    block_meta <- regular_blocks[regular_blocks$block_id == bid, , drop = FALSE]
    block_info <- block_meta[1L, ]
    block_cols <- source_cols_meta[source_cols_meta$block_id == bid, , drop = FALSE]

    if (verbose) {
      cli::cli_alert_info("    Block: {bid} (source={block_info$source}, type={block_info$merge_type})")
    }

    result <- .process_source_block(
      block_info    = block_info,
      source_cols   = block_cols,
      raw_data      = raw_data,
      config        = config,
      built_domains = built_domains
    )

    mtype <- toupper(trimws(as.character(block_info$merge_type)))

    if (is.null(result) || nrow(result) == 0L) {
      # For LEFT_JOIN blocks with no rows, still register so the columns are
      # added (as NA) to the combined dataset during the merge step.
      if (mtype == "LEFT_JOIN") {
        join_by <- trimws(strsplit(as.character(block_info$join_by), ",")[[1]])
        target_cols <- block_cols$target_column
        target_cols <- target_cols[!grepl("^[_.]", target_cols)]
        cols <- c(join_by, target_cols)
        empty_df <- as.data.frame(
          setNames(replicate(length(cols), character(0), simplify = FALSE), cols)
        )
        join_blocks[[bid]] <- list(data = empty_df, join_by = join_by)
      }
      next
    }

    if (mtype == "LEFT_JOIN") {
      join_by <- trimws(strsplit(as.character(block_info$join_by), ",")[[1]])

      # Check for aggregation methods (agg_min, etc.)
      has_agg <- any(grepl("^agg_", block_cols$method))
      if (has_agg) {
        # Aggregate: group by join_by, apply aggregation function
        target_cols <- setdiff(names(result), c("subjectid", join_by))
        if (length(target_cols) > 0L && all(join_by %in% names(result))) {
          agg_result <- stats::aggregate(
            result[, target_cols, drop = FALSE],
            by = result[, join_by, drop = FALSE],
            FUN = function(x) {
              x <- x[!is.na(x) & x != "" & x != "NA"]
              if (length(x) == 0L) return(NA_character_)
              as.character(min(x))
            }
          )
          result <- agg_result
        }
      } else {
        # Deduplicate by join_by
        result <- result[!duplicated(result[, join_by, drop = FALSE]), , drop = FALSE]
      }
      join_blocks[[bid]] <- list(data = result, join_by = join_by)
    } else {
      stack_results[[bid]] <- result
    }
  }

  # Combine STACK blocks
  combined <- NULL
  if (length(stack_results) > 0L) {
    combined <- dplyr::bind_rows(stack_results)
  }

  # Apply LEFT_JOIN blocks
  if (length(join_blocks) > 0L) {
    if (is.null(combined)) {
      # Start with existing raw_data for this domain
      dom_lc <- tolower(domain)
      if (dom_lc %in% names(raw_data)) {
        combined <- raw_data[[dom_lc]]
        names(combined) <- tolower(names(combined))
        combined <- .char_dates(combined)
      }
    }
    if (!is.null(combined)) {
      for (bid in names(join_blocks)) {
        jb <- join_blocks[[bid]]
        combined <- merge(combined, jb$data, by = jb$join_by,
                          all.x = TRUE, sort = FALSE)
      }
    }
  }

  if (is.null(combined) || nrow(combined) == 0L) return(raw_data)

  # Apply POST blocks
  if (nrow(post_blocks) > 0L) {
    for (pid in unique(post_blocks$block_id)) {
      post_cols <- source_cols_meta[source_cols_meta$block_id == pid, , drop = FALSE]
      post_cols <- post_cols[order(post_cols$col_order), , drop = FALSE]
      for (j in seq_len(nrow(post_cols))) {
        target <- as.character(post_cols$target_column[j])
        method <- as.character(post_cols$method[j])
        if (is.na(target) || !nzchar(trimws(target))) next
        combined[[target]] <- eval_source_method(method, combined, raw_data, config, built_domains)
      }
    }
  }

  raw_data[[tolower(domain)]] <- combined
  raw_data
}
