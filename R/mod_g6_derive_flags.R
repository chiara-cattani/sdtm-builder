# ==============================================================================
# Module G6: Flags & Status Variables
# ==============================================================================
# Functions: derive_baseline_flag, derive_lastobs_flag, derive_lobxfl,
#            derive_occurrence, derive_status, derive_reason,
#            derive_ref_time_point
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



#' Derive reference time point variables (--STRTPT/--STTPT or --ENRTPT/--ENTPT)
#'
#' Handles the common SDTM pattern where a reference time point (e.g.,
#' "BEFORE", "AFTER", "ONGOING") is derived from a raw CRF variable,
#' and a companion label variable is set when the time point is non-missing.
#'
#' Two modes are supported via the `mode` parameter:
#' \describe{
#'   \item{`"pattern"`}{Text pattern matching on `source_var` (e.g.,
#'     `AeStPer` → check for "BEFORE" / "AFTER" substrings).}
#'   \item{`"value"`}{Direct value mapping on `source_var` (e.g.,
#'     `AeOnGo` "Y" → "ONGOING", "N" → "BEFORE").}
#' }
#'
#' @param data Tibble.
#' @param rtpt_var Character. The --STRTPT or --ENRTPT target variable.
#' @param tpt_var Character. The --STTPT or --ENTPT target variable.
#' @param source_var Character. Raw source column to evaluate.
#' @param tpt_label Character. Label for the reference time point (e.g.,
#'   `"FIRST PRODUCT INTAKE"` or `"END OF STUDY"`).
#' @param mode Character. `"pattern"` for substring matching, `"value"`
#'   for direct value mapping. Default `"pattern"`.
#' @param mapping Named list. For `"pattern"` mode: names are SDTM values
#'   (e.g., `"BEFORE"`, `"AFTER"`), values are regex patterns to search for.
#'   For `"value"` mode: names are SDTM values, values are raw values to
#'   match (case-insensitive).
#'   Default handles the AESTRTPT pattern: `list("BEFORE" = "before", "AFTER" = "after")`.
#' @return Tibble with `rtpt_var` and `tpt_var` populated.
#' @export
derive_ref_time_point <- function(data, rtpt_var, tpt_var,
                                  source_var, tpt_label,
                                  mode = c("pattern", "value"),
                                  mapping = list("BEFORE" = "before",
                                                 "AFTER"  = "after")) {
  mode <- match.arg(mode)
  data[[rtpt_var]] <- NA_character_
  data[[tpt_var]]  <- NA_character_

  if (!source_var %in% names(data)) {
    if (tolower(source_var) %in% names(data)) {
      source_var <- tolower(source_var)
    } else {
      return(data)
    }
  }

  idx <- !is.na(data[[source_var]])

  for (sdtm_val in names(mapping)) {
    raw_match <- mapping[[sdtm_val]]
    if (mode == "pattern") {
      mask <- idx & grepl(raw_match, data[[source_var]], ignore.case = TRUE)
    } else {
      # Direct value comparison (case-insensitive)
      mask <- idx & toupper(trimws(data[[source_var]])) == toupper(raw_match)
    }
    data[[rtpt_var]][mask] <- sdtm_val
  }

  # Set the reference time point label where RTPT is non-missing
  data[[tpt_var]][!is.na(data[[rtpt_var]])] <- tpt_label
  data
}


#' Derive --LOBXFL flag for last pre-exposure observation
#'
#' Flags the last valid observation before the reference exposure start
#' date (`RFXSTDTC`) for each subject and test code, using the SDTM
#' convention for `--LOBXFL`.
#'
#' @param domain_data A tibble containing the SDTM domain data.
#' @param dm_data A tibble containing the DM dataset with `USUBJID`
#'   and `RFXSTDTC`.
#' @param sdtm_domain Character. Two-letter SDTM domain code (e.g.,
#'   `"LB"`, `"VS"`, `"QS"`).
#' @param add_val_rule Optional quoted expression (e.g., `rlang::expr(...)`)
#'   adding extra conditions to the default `!is.na(--ORRES)` rule.
#' @return A tibble with the derived `--LOBXFL` flag.
#'
#' @details
#' Processing steps:
#' 1. Parse `--DTC` and `RFXSTDTC` into datetime.
#' 2. Define *valid* = `!is.na(--ORRES)` AND (optionally) `add_val_rule`.
#' 3. Flag the latest pre-exposure record per `USUBJID` / `--TESTCD`.
#'
#' @export
derive_lobxfl <- function(domain_data, dm_data, sdtm_domain,
                          add_val_rule = NULL) {
  date_var   <- paste0(sdtm_domain, "DTC")
  testcd_var <- paste0(sdtm_domain, "TESTCD")
  orres_var  <- paste0(sdtm_domain, "ORRES")
  lobxfl_var <- paste0(sdtm_domain, "LOBXFL")

  if (nrow(domain_data) == 0L) {
    domain_data[[lobxfl_var]] <- character(0)
    return(domain_data)
  }
  if (nrow(dm_data) == 0L) {
    domain_data[[lobxfl_var]] <- ""
    return(domain_data)
  }

  domain_data <- domain_data %>%
    dplyr::mutate(USUBJID = as.character(.data$USUBJID))
  dm_data <- dm_data %>%
    dplyr::mutate(USUBJID = as.character(.data$USUBJID))

  merged <- domain_data %>%
    dplyr::left_join(
      dm_data %>% dplyr::select(dplyr::all_of(c("USUBJID", "RFXSTDTC"))),
      by = "USUBJID"
    ) %>%
    dplyr::mutate(
      DTC_PARSED = lubridate::parse_date_time(
        .data[[date_var]],
        orders = c("ymd HMS", "ymd HM", "ymd H", "ymd")
      ),
      RFXSTDTM = lubridate::parse_date_time(
        .data[["RFXSTDTC"]],
        orders = c("ymd HMS", "ymd HM", "ymd H", "ymd")
      ),
      DTC_PARSED = dplyr::if_else(
        is.na(.data$DTC_PARSED),
        lubridate::parse_date_time(
          gsub("T", " ", .data[[date_var]], fixed = TRUE),
          orders = c("ymd HMS", "ymd")
        ),
        .data$DTC_PARSED
      ),
      RFXSTDTM = dplyr::if_else(
        is.na(.data$RFXSTDTM),
        lubridate::parse_date_time(
          gsub("T", " ", .data[["RFXSTDTC"]], fixed = TRUE),
          orders = c("ymd HMS", "ymd")
        ),
        .data$RFXSTDTM
      )
    )

  base_valid <- rlang::expr(!is.na(!!rlang::sym(orres_var)))
  full_valid <- if (!is.null(add_val_rule)) {
    rlang::expr(!!base_valid & !!add_val_rule)
  } else {
    base_valid
  }

  merged <- merged %>%
    dplyr::mutate(
      VALID   = !!full_valid,
      PRE_EXP = .data$VALID &
        !is.na(.data$DTC_PARSED) &
        !is.na(.data$RFXSTDTM) &
        .data$DTC_PARSED < .data$RFXSTDTM
    ) %>%
    dplyr::group_by(.data$USUBJID, !!rlang::sym(testcd_var)) %>%
    dplyr::mutate(
      !!lobxfl_var := dplyr::if_else(
        any(.data$PRE_EXP) &
          .data$DTC_PARSED == max(.data$DTC_PARSED[.data$PRE_EXP], na.rm = TRUE),
        "Y", ""
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      -dplyr::all_of(c("DTC_PARSED", "RFXSTDTM", "VALID", "PRE_EXP", "RFXSTDTC"))
    )

  merged
}
