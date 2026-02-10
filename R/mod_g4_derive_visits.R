# ==============================================================================
# Module G4: Visits & Timing
# ==============================================================================

#' Derive VISIT from visit map
#' @param data Tibble.
#' @param target_var Character. Default `"VISIT"`.
#' @param visit_map Tibble.
#' @param dy_var Character. Study day variable.
#' @param by Character vector. Default `"USUBJID"`.
#' @return Tibble.
#' @export
derive_visit <- function(data, target_var = "VISIT", visit_map,
                         dy_var = NULL, by = "USUBJID") {
  if (is.null(visit_map) || nrow(visit_map) == 0L) {
    data[[target_var]] <- NA_character_
    return(data)
  }
  data[[target_var]] <- NA_character_
  if (!is.null(dy_var) && dy_var %in% names(data) &&
      "START_DAY" %in% names(visit_map) && "END_DAY" %in% names(visit_map)) {
    for (i in seq_len(nrow(data))) {
      dy <- data[[dy_var]][i]
      if (is.na(dy)) next
      for (v in seq_len(nrow(visit_map))) {
        if (dy >= visit_map$START_DAY[v] && dy <= visit_map$END_DAY[v]) {
          data[[target_var]][i] <- visit_map$VISIT[v]
          break
        }
      }
    }
  }
  data
}

#' Derive VISITNUM
#' @param data Tibble.
#' @param target_var Character. Default `"VISITNUM"`.
#' @param visit_map Tibble.
#' @param visit_var Character. Default `"VISIT"`.
#' @return Tibble.
#' @export
derive_visitnum <- function(data, target_var = "VISITNUM", visit_map,
                            visit_var = "VISIT") {
  if (is.null(visit_map) || nrow(visit_map) == 0L || !visit_var %in% names(data)) {
    data[[target_var]] <- NA_real_
    return(data)
  }
  lookup <- stats::setNames(visit_map$VISITNUM, visit_map$VISIT)
  data[[target_var]] <- unname(lookup[data[[visit_var]]])
  data
}

#' Derive VISITDY
#' @param data Tibble.
#' @param target_var Character. Default `"VISITDY"`.
#' @param visit_var Character. Default `"VISIT"`.
#' @param dy_var Character. Study day variable.
#' @return Tibble.
#' @export
derive_visitdy <- function(data, target_var = "VISITDY",
                           visit_var = "VISIT", dy_var = NULL,
                           visit_map = NULL) {
  data[[target_var]] <- NA_real_
  # If we have a visit_map with START_DAY, use it to map VISIT -> planned day
  if (!is.null(visit_map) && nrow(visit_map) > 0L &&
      visit_var %in% names(data) && "START_DAY" %in% names(visit_map)) {
    lookup <- stats::setNames(visit_map$START_DAY, visit_map$VISIT)
    data[[target_var]] <- unname(lookup[data[[visit_var]]])
  }
  data
}

#' Derive timepoint
#' @param data Tibble.
#' @param target_var Character. Default `"--TPT"`.
#' @param source_var Character.
#' @param tpt_map Named character vector.
#' @return Tibble.
#' @export
derive_tpt <- function(data, target_var, source_var = NULL,
                       tpt_map = NULL) {
  data[[target_var]] <- NA_character_
  # If source_var is provided and present, attempt to derive timepoint
  if (!is.null(source_var) && source_var %in% names(data)) {
    if (!is.null(tpt_map) && length(tpt_map) > 0L) {
      # Named vector: time_value -> timepoint label
      data[[target_var]] <- unname(tpt_map[data[[source_var]]])
    } else {
      # Default: classify time into named time-points
      times <- data[[source_var]]
      tpt <- dplyr::case_when(
        is.na(times) ~ NA_character_,
        TRUE         ~ paste0("PT", times)
      )
      data[[target_var]] <- tpt
    }
  }
  data
}

#' Derive elapsed time
#' @param data Tibble.
#' @param target_var Character.
#' @param start_var Character.
#' @param end_var Character.
#' @param units Character. Default `"hours"`.
#' @return Tibble.
#' @export
derive_elapsed_time <- function(data, target_var, start_var, end_var,
                                units = "hours") {
  data[[target_var]] <- NA_real_
  data
}

#' Derive relative time
#' @param data Tibble.
#' @param target_var Character.
#' @param dtc_var Character.
#' @param ref_dtc_var Character.
#' @return Tibble.
#' @export
derive_rel_time <- function(data, target_var, dtc_var, ref_dtc_var) {
  data[[target_var]] <- NA_real_
  data
}
