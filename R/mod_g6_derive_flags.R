# ==============================================================================
# Module G6: Flags & Status Variables
# ==============================================================================

#' Derive baseline flag
#' @param data Tibble.
#' @param target_var Character. Default `"--BLFL"`.
#' @param by Character vector. Default `c("USUBJID","--TESTCD")`.
#' @param baseline_visit Character or numeric. Default `"BASELINE"`.
#' @param visit_var Character. Default `"VISIT"`.
#' @return Tibble.
#' @export
derive_baseline_flag <- function(data, target_var = "BLFL",
                                 by = c("USUBJID"),
                                 baseline_visit = "BASELINE",
                                 visit_var = "VISIT") {
  data[[target_var]] <- NA_character_
  if (visit_var %in% names(data)) {
    data[[target_var]] <- dplyr::if_else(
      toupper(data[[visit_var]]) == toupper(baseline_visit), "Y", NA_character_
    )
  }
  data
}

#' Derive last observation flag
#' @param data Tibble.
#' @param target_var Character.
#' @param by Character vector. Default `"USUBJID"`.
#' @param order_var Character.
#' @return Tibble.
#' @export
derive_lastobs_flag <- function(data, target_var, by = "USUBJID",
                                order_var) {
  data[[target_var]] <- NA_character_
  if (order_var %in% names(data)) {
    data <- data %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(by))) %>%
      dplyr::mutate(!!target_var := dplyr::if_else(
        dplyr::row_number() == dplyr::n(), "Y", NA_character_
      )) %>%
      dplyr::ungroup()
  }
  data
}

#' Derive occurrence variable
#' @param data Tibble.
#' @param target_var Character.
#' @param source_var Character.
#' @param present_value Character. Default `"Y"`.
#' @param absent_value Character. Default `NA_character_`.
#' @return Tibble.
#' @export
derive_occurrence <- function(data, target_var, source_var = NULL,
                              present_value = "Y",
                              absent_value = NA_character_) {
  if (is.null(source_var)) {
    data[[target_var]] <- present_value
  } else {
    data[[target_var]] <- dplyr::if_else(
      !is.na(data[[source_var]]), present_value, absent_value
    )
  }
  data
}

#' Derive status variable
#' @param data Tibble.
#' @param target_var Character.
#' @param result_var Character.
#' @param done_value Character. Default `NA_character_`.
#' @param not_done_value Character. Default `"NOT DONE"`.
#' @return Tibble.
#' @export
derive_status <- function(data, target_var, result_var,
                          done_value = NA_character_,
                          not_done_value = "NOT DONE") {
  data[[target_var]] <- dplyr::if_else(
    is.na(data[[result_var]]), not_done_value, done_value
  )
  data
}

#' Derive reason variable
#' @param data Tibble.
#' @param target_var Character.
#' @param source_var Character.
#' @param stat_var Character.
#' @return Tibble.
#' @export
derive_reason <- function(data, target_var, source_var = NULL,
                          stat_var = NULL) {
  data[[target_var]] <- NA_character_
  if (!is.null(source_var) && source_var %in% names(data)) {
    data[[target_var]] <- data[[source_var]]
  }
  data
}
