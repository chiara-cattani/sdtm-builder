# ******************************************************************************
# Danone Nutricia Research
# ******************************************************************************
# STUDY/PROJECT : SONA / 23REX0061265
# PROGRAM PATH  : SAS/SONA/Files/SDTM Conversion/01 Macros
# PROGRAM NAME  : add_planned_actual_arm.R
# PURPOSE       : Add planned and actual arms (ieyn taken from raw IE dataset).
# ------------------------------------------------------------------------------
# NOTES :
#   - ieyn is derived from `ie1` (not from `inset`) via assign_no_ct() and
#     one-record-per-subject selection.
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2025-09-29 - cattanch - Initial program
# ******************************************************************************

#' @title Add planned and actual arms (ieyn from raw IE)
#'
#' @description
#' Derives ieyn from `ie1` (raw), merges to `inset` by `patient_number`,
#' then derives planned arm (`ARM`,`ARMCD`,`ARMNRS`) and actual arm
#' (`ACTARM`,`ACTARMCD`,`ACTARMUD`) using `RFXSTDTC`/`RFXENDTC` and enrollment.
#'
#' @param inset   data.frame with at least `USUBJID` (or `usubjid`),
#'                `RFXSTDTC`, `RFXENDTC`, and preferably `patient_number`.
#' @param vstat1  data.frame with `usubjid`,`vstatyn` (enrolled Y/N).
#' @param ie1     raw IE dataset used to derive ieyn.
#' @param armcd_const character, planned/actual ARMCD when applicable (default "Total").
#' @param arm_const   character, planned/actual ARM   when applicable (default "Total").
#'
#' @return A tibble with added/overwritten variables:
#'   `ARM`, `ARMCD`, `ACTARM`, `ACTARMCD`, `ARMNRS`, `ACTARMUD`.
#'
#' @import dplyr

add_planned_actual_arm <- function(inset, vstat1, ie1,
                                   armcd_const = "Total",
                                   arm_const   = "Total") {
  stopifnot(is.data.frame(inset), is.data.frame(vstat1))
  if (!is.data.frame(ie1)) {
    warning("add_planned_actual_arm: `ie1` not provided as data.frame; ieyn will be treated as missing.")
    ie1 <- tibble::tibble(patient_number = character(), ieyn = character())
  }
  # -- Ensure USUBJID exists in inset
  if (!"USUBJID" %in% names(inset)) {
    if ("usubjid" %in% names(inset)) {
      inset <- dplyr::rename(inset, USUBJID = usubjid)
    } else {
      stop("`inset` must contain `USUBJID` or `usubjid`.")
    }
  }
  # -- Derive ieyn from raw IE (independent of any ieyn in `inset`)
  ie2 <- assign_no_ct(
    raw_dat = ie1,
    raw_var = "ieyn",
    tgt_var = "ieyn",
    id_vars = oak_id_vars()
  ) %>%
    dplyr::group_by(patient_number) %>%
    dplyr::arrange(dplyr::desc(!is.na(ieyn)), .by_group = TRUE) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(ieyn = toupper(trimws(as.character(ieyn)))) %>%
    dplyr::select(patient_number, ieyn)
  # -- Enrolled subjects VSTATYN == "Y" (vector of USUBJID)
  inv_yes_usubjid <- vstat1 %>%
    dplyr::rename_with(tolower) %>%
    dplyr::mutate(vstatyn = toupper(trimws(as.character(vstatyn)))) %>%
    dplyr::filter(vstatyn == "Y") %>%
    dplyr::pull(usubjid) %>%
    unique()
  # -- Start output; initialize fields
  out <- inset %>%
    dplyr::mutate(
      ARM      = NA_character_,
      ARMCD    = NA_character_,
      ACTARM   = NA_character_,
      ACTARMCD = NA_character_,
      ARMNRS   = NA_character_,
      ACTARMUD = NA_character_
    )
  # -- Bring in ieyn (from ie2) by patient_number when possible
  if (!"patient_number" %in% names(out)) {
    # fallback: if missing patient_number, try to carry ieyn by USUBJID using oak ids (best effort)
    warning("add_planned_actual_arm: `patient_number` not found in inset; ieyn merge skipped.")
    ieyn_UP <- rep(NA_character_, nrow(out))
  } else {
    out <- dplyr::left_join(out, ie2, by = "patient_number", keep = FALSE)
    ieyn_UP <- toupper(trimws(as.character(out$ieyn)))
  }
  # -- Useful predicates
  INVSTAT_YES <- out$USUBJID %in% inv_yes_usubjid
  is_blank    <- function(x) is.na(x) | (is.character(x) & trimws(x) == "")
  CMISS_EQ_2  <- (is_blank(out$RFXSTDTC) + is_blank(out$RFXENDTC)) == 2   # both missing
  CMISS_NE_2  <- !CMISS_EQ_2                                              # product intake present
  # =========================
  # PLANNED ARM - ARM/CD/NRS
  # =========================
  idx_planned <- (ieyn_UP == "Y") | INVSTAT_YES
  out$ARM[idx_planned]   <- arm_const
  out$ARMCD[idx_planned] <- armcd_const
  # Assigned but not treated
  idx_assigned_not_treated <- idx_planned & CMISS_EQ_2
  out$ARMNRS[idx_assigned_not_treated] <- "ASSIGNED, NOT TREATED"  # C142238
  # Screen failure
  idx_sf <- ieyn_UP == "N"
  out$ARMNRS[idx_sf] <- "SCREEN FAILURE"  # C49628
  # ieyn missing => Not assigned (if not already set)
  idx_not_assigned <- is_blank(ieyn_UP)
  out$ARMNRS[idx_not_assigned] <- ifelse(
    is.na(out$ARMNRS[idx_not_assigned]) | out$ARMNRS[idx_not_assigned] == "",
    "NOT ASSIGNED",  # C142239
    out$ARMNRS[idx_not_assigned]
  )
  # Unplanned treatment: product intake but no planned ARMCD
  idx_unplanned <- CMISS_NE_2 & is_blank(out$ARMCD)
  if (any(idx_unplanned, na.rm = TRUE)) {
    set_armnrs <- is.na(out$ARMNRS[idx_unplanned]) | out$ARMNRS[idx_unplanned] == ""
    out$ARMNRS[idx_unplanned][set_armnrs] <- "UNPLANNED TREATMENT"  # C142240
  }
  # =======================
  # ACTUAL ARM - ACTARM/CD/UD
  # =======================
  idx_actual <- CMISS_NE_2
  fill_act   <- idx_actual & (is_blank(out$ACTARM) & is_blank(out$ACTARMCD))
  # If planned exists, copy to actual; else set ACTARMUD
  idx_actual_planned_exists <- fill_act & !is_blank(out$ARMCD)
  out$ACTARM[idx_actual_planned_exists]   <- ifelse(is_blank(out$ARM[idx_actual_planned_exists]), arm_const, out$ARM[idx_actual_planned_exists])
  out$ACTARMCD[idx_actual_planned_exists] <- ifelse(is_blank(out$ARMCD[idx_actual_planned_exists]), armcd_const, out$ARMCD[idx_actual_planned_exists])
  idx_actual_unplanned <- fill_act & is_blank(out$ARMCD)
  if (any(idx_actual_unplanned, na.rm = TRUE)) {
    out$ACTARMUD[idx_actual_unplanned] <- "Treatment unknown"
    warning("add_planned_actual_arm: UNPLANNED TREATMENT; ACTARMUD set for ",
            sum(idx_actual_unplanned, na.rm = TRUE), " record(s).")
  }
  # Remove helper ieyn if present (optional; comment this out if you want to keep it)
  if ("ieyn" %in% names(out)) out$ieyn <- NULL
  out
}
