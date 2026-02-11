# ==============================================================================
# Module G3: Dates, Times, ISO 8601
# ==============================================================================

#' Parse partial or complete dates
#' @param x Character vector.
#' @param formats Character vector.
#' @param partial Character vector.
#' @param unknown_tokens Character vector.
#' @param day_first Logical. Default `FALSE`.
#' @return Tibble.
#' @export
parse_partial_date <- function(x,
                               formats = c("Y-m-d","d/m/Y","m/d/Y","Y","Y-m"),
                               partial = c("Y","YM","YMD"),
                               unknown_tokens = c("UNK","UN","UNKN","XX","99"),
                               day_first = FALSE) {
  # Handle Date / POSIXt objects
  if (inherits(x, c("Date", "POSIXt"))) {
    x <- as.character(x)
  }
  n <- length(x)
  result <- tibble::tibble(
    year      = rep(NA_integer_, n),
    month     = rep(NA_integer_, n),
    day       = rep(NA_integer_, n),
    precision = rep(NA_character_, n),
    original  = x,
    valid     = rep(NA, n)
  )

  for (i in seq_len(n)) {
    val <- x[i]
    if (is.na(val) || trimws(val) == "") {
      result$valid[i] <- NA
      next
    }

    val <- trimws(val)

    # Try YYYY-MM-DD (most common in our data)
    if (grepl("^\\d{4}-\\d{2}-\\d{2}$", val)) {
      parts <- as.integer(strsplit(val, "-")[[1]])
      if (parts[2] >= 1 && parts[2] <= 12 && parts[3] >= 1 && parts[3] <= 31) {
        result$year[i]  <- parts[1]
        result$month[i] <- parts[2]
        result$day[i]   <- parts[3]
        result$precision[i] <- "YMD"
        result$valid[i] <- TRUE
        next
      }
    }

    # Try YYYY-MM (partial)
    if (grepl("^\\d{4}-\\d{2}$", val)) {
      parts <- as.integer(strsplit(val, "-")[[1]])
      if (parts[2] >= 1 && parts[2] <= 12) {
        result$year[i]  <- parts[1]
        result$month[i] <- parts[2]
        result$precision[i] <- "YM"
        result$valid[i] <- TRUE
        next
      }
    }

    # Try YYYY (year only)
    if (grepl("^\\d{4}$", val)) {
      result$year[i]  <- as.integer(val)
      result$precision[i] <- "Y"
      result$valid[i] <- TRUE
      next
    }

    # Could not parse
    result$valid[i] <- FALSE
  }

  result
}

#' Combine date and time columns
#' @param date Parsed date tibble or character vector.
#' @param time Character vector.
#' @param seconds Logical. Default `TRUE`.
#' @param tz Character. Default `"UTC"`.
#' @return Tibble.
#' @export
combine_date_time <- function(date, time, seconds = TRUE, tz = "UTC") {
  if (is.character(date)) date <- parse_partial_date(date)

  n <- nrow(date)
  result <- date
  result$hour   <- rep(NA_integer_, n)
  result$minute <- rep(NA_integer_, n)
  result$second <- rep(NA_integer_, n)

  for (i in seq_len(n)) {
    t <- if (i <= length(time)) time[i] else NA_character_
    if (!is.na(t) && nchar(trimws(t)) > 0) {
      parts <- strsplit(trimws(t), ":")[[1]]
      if (length(parts) >= 2) {
        result$hour[i]   <- as.integer(parts[1])
        result$minute[i] <- as.integer(parts[2])
        if (length(parts) >= 3 && seconds) result$second[i] <- as.integer(parts[3])
        if (result$precision[i] == "YMD") result$precision[i] <- "YMDHM"
      }
    }
  }

  result
}

#' Format parsed date/time as ISO 8601 --DTC string
#' @param parsed Tibble from parse_partial_date or combine_date_time,
#'   OR a character vector of raw date strings.
#' @param time Character vector or `NULL`.
#' @param keep_partial Logical. Default `TRUE`.
#' @param impute Function or `NULL`.
#' @return Character vector of ISO 8601 strings.
#' @export
format_iso_dtc <- function(parsed, time = NULL, keep_partial = TRUE,
                           impute = NULL) {
  # If parsed is a Date/POSIXt object, convert to character first
  if (inherits(parsed, c("Date", "POSIXt"))) {
    parsed <- as.character(parsed)
  }
  # If parsed is a character vector, parse it first
  if (is.character(parsed)) {
    parsed <- parse_partial_date(parsed)
  }
  if (!is.null(time)) parsed <- combine_date_time(parsed, time)
  if (!is.null(impute)) parsed <- impute(parsed)

  n <- nrow(parsed)
  result <- rep(NA_character_, n)

  for (i in seq_len(n)) {
    if (is.na(parsed$valid[i]) || !isTRUE(parsed$valid[i])) next
    prec <- parsed$precision[i]
    if (is.na(prec)) next

    if (!keep_partial && prec != "YMD" && !startsWith(prec, "YMDH")) next

    iso <- as.character(parsed$year[i])
    if (!is.na(parsed$month[i])) iso <- sprintf("%s-%02d", iso, parsed$month[i])
    if (!is.na(parsed$day[i]))   iso <- sprintf("%s-%02d", iso, parsed$day[i])

    # Add time if available
    if ("hour" %in% names(parsed) && !is.na(parsed$hour[i])) {
      iso <- sprintf("%sT%02d:%02d", iso, parsed$hour[i], parsed$minute[i])
      if ("second" %in% names(parsed) && !is.na(parsed$second[i])) {
        iso <- sprintf("%s:%02d", iso, parsed$second[i])
      }
    }

    result[i] <- iso
  }

  result
}

#' Derive study day (--DY)
#' @param data Tibble.
#' @param target_var Character.
#' @param dtc_var Character.
#' @param ref_var Character. Default `"RFSTDTC"`.
#' @param impute_policy List or function or `NULL`.
#' @return Tibble.
#' @export
derive_dy <- function(data, target_var, dtc_var, ref_var = "RFSTDTC",
                      impute_policy = NULL) {
  if (!dtc_var %in% names(data)) {
    abort(glue::glue("derive_dy: dtc_var '{dtc_var}' not found"))
  }
  if (!ref_var %in% names(data)) {
    # DY can't be computed without reference date
    data[[target_var]] <- NA_real_
    return(data)
  }

  dtc_vals <- data[[dtc_var]]
  ref_vals <- data[[ref_var]]

  dy <- rep(NA_real_, nrow(data))

  for (i in seq_len(nrow(data))) {
    dtc <- dtc_vals[i]
    ref <- ref_vals[i]

    if (is.na(dtc) || is.na(ref)) next
    # Both must be at least YYYY-MM-DD (10 chars)
    if (nchar(dtc) < 10 || nchar(ref) < 10) next

    dtc_date <- tryCatch(as.Date(substr(dtc, 1, 10)), error = function(e) NA)
    ref_date <- tryCatch(as.Date(substr(ref, 1, 10)), error = function(e) NA)

    if (is.na(dtc_date) || is.na(ref_date)) next

    diff_days <- as.integer(dtc_date - ref_date)
    # No Day 0 convention: >= 0 means +1, < 0 stays as-is
    if (diff_days >= 0) {
      dy[i] <- diff_days + 1L
    } else {
      dy[i] <- diff_days
    }
  }

  data[[target_var]] <- dy
  data
}

#' Derive EPOCH
#'
#' Assigns EPOCH values by comparing each observation's date with study-day
#' windows defined in `epoch_map`.
#'
#' @param data Tibble.
#' @param target_var Character. Default `"EPOCH"`.
#' @param dtc_var Character. The DTC variable whose date determines the epoch.
#' @param epoch_map A list of lists (or data-frame), each with `epoch`,
#'   `start_day` (integer or `NULL`), and `end_day` (integer or `NULL`).
#' @param ref_var Character. Reference-date column in `data`. Default
#'   `"RFSTDTC"`.
#' @param by Character vector. Grouping columns (unused for now, reserved for
#'   multi-arm studies).
#' @return Tibble with `target_var` populated.
#' @export
derive_epoch <- function(data, target_var = "EPOCH", dtc_var,
                         epoch_map, ref_var = "RFSTDTC",
                         by = "USUBJID") {
  n <- nrow(data)
  result <- rep(NA_character_, n)

 # If epoch_map is empty / NULL, return early
  if (is.null(epoch_map) || length(epoch_map) == 0L) {
    data[[target_var]] <- result
    return(data)
  }

  # Normalise epoch_map to a list-of-lists
  if (is.data.frame(epoch_map)) {
    emap <- lapply(seq_len(nrow(epoch_map)), function(i) as.list(epoch_map[i, ]))
  } else {
    emap <- epoch_map
  }

  has_dtc <- dtc_var %in% names(data)
  has_ref <- ref_var %in% names(data)

  if (!has_dtc || !has_ref) {
    data[[target_var]] <- result
    return(data)
  }

  for (i in seq_len(n)) {
    dtc <- data[[dtc_var]][i]
    ref <- data[[ref_var]][i]
    if (is.na(dtc) || is.na(ref) || nchar(dtc) < 10 || nchar(ref) < 10) next

    dtc_date <- tryCatch(as.Date(substr(dtc, 1, 10)), error = function(e) NA)
    ref_date <- tryCatch(as.Date(substr(ref, 1, 10)), error = function(e) NA)
    if (is.na(dtc_date) || is.na(ref_date)) next

    diff_days <- as.integer(dtc_date - ref_date)
    # Convert to study-day convention (no day 0)
    sdy <- if (diff_days >= 0) diff_days + 1L else diff_days

    for (ep in emap) {
      lo <- ep$start_day  # NULL means -Inf
      hi <- ep$end_day    # NULL means +Inf
      if (is.null(lo)) lo <- -.Machine$integer.max
      if (is.null(hi)) hi <- .Machine$integer.max
      if (sdy >= lo && sdy <= hi) {
        result[i] <- ep$epoch
        break
      }
    }
  }

  data[[target_var]] <- result
  data
}

#' Derive ISO 8601 duration
#' @param data Tibble.
#' @param target_var Character.
#' @param start_dtc Character.
#' @param end_dtc Character.
#' @param units Character. Default `"auto"`.
#' @return Tibble.
#' @export
derive_duration <- function(data, target_var, start_dtc, end_dtc,
                            units = "auto") {
  dur <- rep(NA_character_, nrow(data))
  for (i in seq_len(nrow(data))) {
    s <- data[[start_dtc]][i]
    e <- data[[end_dtc]][i]
    if (is.na(s) || is.na(e) || nchar(s) < 10 || nchar(e) < 10) next
    sd <- tryCatch(as.Date(substr(s, 1, 10)), error = function(e) NA)
    ed <- tryCatch(as.Date(substr(e, 1, 10)), error = function(e) NA)
    if (is.na(sd) || is.na(ed)) next
    d <- as.integer(ed - sd)
    dur[i] <- paste0("P", d, "D")
  }
  data[[target_var]] <- dur
  data
}

#' Apply imputation policy
#' @param parsed Tibble from parse_partial_date.
#' @param policy List.
#' @return Tibble.
#' @export
apply_imputation_policy <- function(parsed, policy = list()) {
  if (!is.null(policy$day) && policy$day == "first") {
    na_day <- is.na(parsed$day) & !is.na(parsed$month)
    parsed$day[na_day] <- 1L
    parsed$precision[na_day & parsed$precision == "YM"] <- "YMD"
  }
  parsed
}
