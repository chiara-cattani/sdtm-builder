# ******************************************************************************
# Danone Nutricia Research
# ******************************************************************************
# STUDY/PROJECT : SONA
# PROGRAM PATH  : R/SONA/Files/SDTM Conversion/01 Macros
# PROGRAM NAME  : derive_visit_vars.R
# PURPOSE       : Derive VISIT and VISITNUM
# ------------------------------------------------------------------------------
# NOTES : To be used inside mutate() during SDTM domain creation
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2025-10-08 - cattanch - Initial translation from SAS
# ******************************************************************************

derive_visit_vars <- function(df, eventid_var = "EVENTID") {
  df %>%
    mutate(
      VISIT = case_when(
        !!sym(eventid_var) == "SCR" ~ "Screening",
        !!sym(eventid_var) == "V1"  ~ "Visit 1",
        !!sym(eventid_var) == "V2"  ~ "Visit 2",
        !!sym(eventid_var) == "V3"  ~ "Visit 3",
        !!sym(eventid_var) == "V4"  ~ "Visit 4",
        str_starts(!!sym(eventid_var), "ED") ~ NA_character_,
        TRUE ~ NA_character_
      ),
      VISITNUM = case_when(
        !!sym(eventid_var) == "SCR" ~ 1,
        !!sym(eventid_var) == "V1"  ~ 2,
        !!sym(eventid_var) == "V2"  ~ 3,
        !!sym(eventid_var) == "V3"  ~ 4,
        !!sym(eventid_var) == "V4"  ~ 5,
        str_starts(!!sym(eventid_var), "ED") ~ NA_real_,
        TRUE ~ NA_real_
      )
    )
}
