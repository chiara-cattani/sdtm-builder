# ******************************************************************************
# Danone Nutricia Research
# ******************************************************************************
# STUDY/PROJECT : SONA / 23REX0061265
# PROGRAM PATH  : R/SONA/Files/SDTM Conversion/40 SDTM Programs
# PROGRAM NAME  : derive_tpt_vars.R
# PURPOSE       : Derive SDTM timing variables --TPT, --TPTNUM, --TPTREF, --ELTM
# ------------------------------------------------------------------------------
# NOTES : 
# - This function is designed to be used inside a dplyr::mutate() pipeline.
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2025-10-10 - cattanch - Initial program
# ******************************************************************************

derive_tpt_vars <- function(df, eventid_var = "eventid", sdtm_prefix = sdtm_domain) {
  eventid_sym <- rlang::sym(eventid_var)
  
  df %>%
    mutate(
      !!paste0(sdtm_prefix, "TPT") := case_when(
        !!eventid_sym == "BL1" ~ "Day -3",
        !!eventid_sym == "BL2" ~ "Day -2",
        !!eventid_sym == "BL3" ~ "Day -1",
        !!eventid_sym == "A1A" ~ "Day 7",
        !!eventid_sym == "A1B" ~ "Day 8",
        !!eventid_sym == "A1C" ~ "Day 9",
        !!eventid_sym == "A2A" ~ "Day 14",
        !!eventid_sym == "A2B" ~ "Day 15",
        !!eventid_sym == "A2C" ~ "Day 16",
        !!eventid_sym == "A3A" ~ "Day 26",
        !!eventid_sym == "A3B" ~ "Day 27",
        !!eventid_sym == "A3C" ~ "Day 28",
        !!eventid_sym == "A4A" ~ "Day 39",
        !!eventid_sym == "A4B" ~ "Day 40",
        !!eventid_sym == "A4C" ~ "Day 41",
        TRUE ~ NA_character_
      ),
      !!paste0(sdtm_prefix, "TPTNUM") := case_when(
        !!eventid_sym == "BL1" ~ -3,
        !!eventid_sym == "BL2" ~ -2,
        !!eventid_sym == "BL3" ~ -1,
        !!eventid_sym == "A1A" ~ 7,
        !!eventid_sym == "A1B" ~ 8,
        !!eventid_sym == "A1C" ~ 9,
        !!eventid_sym == "A2A" ~ 14,
        !!eventid_sym == "A2B" ~ 15,
        !!eventid_sym == "A2C" ~ 16,
        !!eventid_sym == "A3A" ~ 26,
        !!eventid_sym == "A3B" ~ 27,
        !!eventid_sym == "A3C" ~ 28,
        !!eventid_sym == "A4A" ~ 39,
        !!eventid_sym == "A4B" ~ 40,
        !!eventid_sym == "A4C" ~ 41,
        TRUE ~ NA_real_
      ),
      !!paste0(sdtm_prefix, "TPTREF") := case_when(
        !!eventid_sym %in% c("BL1", "BL2", "BL3",
                             "A1A", "A1B", "A1C",
                             "A2A", "A2B", "A2C",
                             "A3A", "A3B", "A3C",
                             "A4A", "A4B", "A4C") ~ "Visit 1",
        TRUE ~ NA_character_
      ),
      !!paste0(sdtm_prefix, "ELTM") := case_when(
        !!eventid_sym == "BL1" ~ "-P3D",
        !!eventid_sym == "BL2" ~ "-P2D",
        !!eventid_sym == "BL3" ~ "-P1D",
        !!eventid_sym == "A1A" ~ "P7D",
        !!eventid_sym == "A1B" ~ "P8D",
        !!eventid_sym == "A1C" ~ "P9D",
        !!eventid_sym == "A2A" ~ "P14D",
        !!eventid_sym == "A2B" ~ "P15D",
        !!eventid_sym == "A2C" ~ "P16D",
        !!eventid_sym == "A3A" ~ "P26D",
        !!eventid_sym == "A3B" ~ "P27D",
        !!eventid_sym == "A3C" ~ "P28D",
        !!eventid_sym == "A4A" ~ "P39D",
        !!eventid_sym == "A4B" ~ "P40D",
        !!eventid_sym == "A4C" ~ "P41D",
        TRUE ~ NA_character_
      )
    )
}