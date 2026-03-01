# ******************************************************************************
# Danone Nutricia Research
# ******************************************************************************
# STUDY/PROJECT : SONA / 23REX0061265
# PROGRAM PATH  : SAS/SONA/Files/SDTM Conversion/40 SDTM Programs
# PROGRAM NAME  : QC_BE.R 
# PURPOSE       : QC SDTM data for Biospecimen Events
# ------------------------------------------------------------------------------
# NOTES :
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2025-10-03 - cattanch - Initial program
# 2025-12-08 - cattanch - Add BEPRESP and BEENDY.
# 2026-01-04 - cattanch - Add BELNKID.
# ******************************************************************************

# Configuration ----
library(nutriciaconfig)
nutricia_config()

library(dplyr)
library(haven)
library(purrr)
library(readxl)
library(rlang)
library(sdtm.oak)
library(stringr)
library(tibble)
library(tidyr)

# Set working directory ----
if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  # Running in RStudio
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
} else {
  # Running in LSAF
  setwd(progdir)
}

# Set parameters
study       <- "SONA"
sdtm_domain <- "BE"

# Import metadata
sdtm_ct  <- read_excel("../31 Metadata Data/SONA_CT.xlsx", sheet = "CT")
sdtm_var <- read_sas("../31 Metadata Data/sdtm_3_4_variables.sas7bdat")
sdtm_dom <- read_sas("../31 Metadata Data/sdtm_3_4_domains.sas7bdat")

# Import data ----
dm1  <- read_sas("../41 SDTM Data/dm.sas7bdat")
lbs1 <- copy_import(inset = "lbs")
giq1 <- copy_import(inset = "giq")
rs1  <- copy_import(inset = "review_status")

# Sources ----
sources <- rs1 %>%
  filter(trimws(formname) != "") %>%
  distinct(formid, formname)

# BE ----
be1 <-
  assign_ct(
    raw_dat = lbs1,
    raw_var = "stool_perf",
    tgt_var = "BEOCCUR",
    ct_spec = sdtm_ct,
    ct_clst = "C66742",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_no_ct(
    raw_dat = lbs1,
    raw_var = "stool_perf",
    tgt_var = "BEPRESP",
    tgt_val = "Y",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = lbs1,
    raw_var = "stool_reasnd",
    tgt_var = "BEREASOC",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = lbs1,
    raw_var = "stool_tube",
    tgt_var = "BETUBCNT",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = lbs1,
    raw_var = "usubjid",
    tgt_var = "USUBJID",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = lbs1,
    raw_var = "subjectid",
    tgt_var = "SUBJID",
    id_vars = oak_id_vars()
  ) %>%
  assign_datetime(
    raw_dat = lbs1,
    raw_var = "stool_dat",
    tgt_var = "BEDTC",
    raw_fmt = c("y-m-d"),
    raw_unk = c("NK", "UNK", "UN"),
    id_vars = oak_id_vars()
  ) %>%
  assign_datetime(
    raw_dat = lbs1,
    raw_var = "stool_dat",
    tgt_var = "BESTDTC",
    raw_fmt = c("y-m-d"),
    raw_unk = c("NK", "UNK", "UN"),
    id_vars = oak_id_vars()
  ) %>%
  mutate(BEDTC_TMP = BEDTC) %>%
  derive_study_day(
    sdtm_in       = .,
    dm_domain     = dm1,
    tgdt          = "BEDTC",
    refdt         = "RFSTDTC",
    study_day_var = "BEDY",
    merge_key     = "USUBJID"
  ) %>%
  mutate(BEDTC = BEDTC_TMP) %>%
  mutate(BESTDTC_TMP = BESTDTC) %>%
  derive_study_day(
    sdtm_in       = .,
    dm_domain     = dm1,
    tgdt          = "BESTDTC",
    refdt         = "RFSTDTC",
    study_day_var = "BESTDY",
    merge_key     = "USUBJID"
  ) %>%
  mutate(BESTDTC = BESTDTC_TMP) %>%
  mutate(
    STUDYID  = study,
    DOMAIN   = sdtm_domain,
    BECAT    = "SAMPLE COLLECTION",
    BEDECOD  = "COLLECTING",
    BETERM   = "COLLECTED",
    BESPEC   = "STOOL",
    SOURCEID = paste("CRF:", sources$formname[match("LBS", sources$formid)]),
    BEREFID  = NA,
    BEENDTC  = NA
  ) %>%
  mutate(BEENDTC_TMP = BEENDTC) %>%
  derive_study_day(
    sdtm_in       = .,
    dm_domain     = dm1,
    tgdt          = "BEENDTC",
    refdt         = "RFSTDTC",
    study_day_var = "BEENDY",
    merge_key     = "USUBJID"
  ) %>%
  mutate(BEENDTC = BEENDTC_TMP) %>%
  left_join(
    lbs1 %>% select(all_of(c(oak_id_vars(), "eventid"))),
    by = oak_id_vars()
  ) %>%
  derive_visit_vars(., eventid_var = "eventid") %>%
  derive_seq(
    tgt_var  = "BESEQ",
    rec_vars = c("STUDYID", "USUBJID", "BETERM", "BESTDTC")
  )

# BELNKID ----

dt2dtc <- function(x) {
  if (inherits(x, "Date")) {
    return(format(x, "%Y-%m-%d"))
  }
  if (inherits(x, "POSIXt")) {
    return(format(as.Date(x), "%Y-%m-%d"))
  }
  if (is.numeric(x)) {

    return(format(as.Date(x, origin = "1960-01-01"), "%Y-%m-%d"))
  }
  if (is.character(x)) {
    x10 <- substr(trimws(x), 1, 10)
    suppressWarnings({
      d <- as.Date(x10)
      ifelse(!is.na(d), format(d, "%Y-%m-%d"), x10)
    })
  } else {
    as.character(x)
  }
}

giq2 <- giq1 %>%
  select(
    any_of(c("usubjid", "eventid", "gidat")),
    matches("^sampyn[1-8]$"),
    matches("^st7p[1-8]$")
  ) %>%
  mutate(
    eventid = trimws(as.character(eventid)),
    VISIT = case_when(
      toupper(eventid) == "BL" ~ "Visit 1",
      toupper(eventid) == "A1" ~ "Visit 2",
      toupper(eventid) == "A2" ~ "Visit 3",
      toupper(eventid) == "A3" ~ "Visit 3",
      toupper(eventid) == "A4" ~ "Visit 4",
      TRUE ~ ""
    ),
    FADTC = dt2dtc(gidat)
  ) %>%
  pivot_longer(
    cols = any_of(c(paste0("sampyn", 1:8), paste0("st7p", 1:8))),
    names_to = c(".value", "slot"),
    names_pattern = "^(sampyn|st7p)([1-8])$"
  ) %>%
  mutate(
    sampyn_val = as.character(sampyn),
    st7p_val = as.character(st7p)
  ) %>%
  filter(
    !(is.na(st7p_val) | trimws(st7p_val) == "") |
      !(is.na(sampyn_val) | trimws(sampyn_val) == "")
  ) %>%
  select(usubjid, VISIT, FADTC, sampyn_val)

giq3 <- giq2 %>%
  group_by(usubjid, VISIT, FADTC) %>%
  summarise(
    n_sampyn_y = sum(toupper(trimws(sampyn_val)) == "Y", na.rm = TRUE),
    has_selected_stool = n_sampyn_y > 0,
    .groups = "drop"
  )

be2 <- be1 %>%
  mutate(
    BELNKID = dplyr::if_else(
      toupper(trimws(as.character(BEOCCUR))) == "Y",
      paste0(trimws(as.character(VISIT)), " - ", trimws(as.character(BEDTC))),
      NA_character_
    ),
    BEDTC_JOIN = as.character(BEDTC),
    VISIT_JOIN = as.character(VISIT),
    USUBJID_JOIN = as.character(USUBJID)
  )

giq3_join <- giq3 %>%
  transmute(
    USUBJID_JOIN = as.character(usubjid),
    VISIT_JOIN = as.character(VISIT),
    BEDTC_JOIN = as.character(FADTC),
    n_sampyn_y,
    has_selected_stool
  )

be3 <- be2 %>%
  left_join(
    giq3_join,
    by = c("USUBJID_JOIN", "VISIT_JOIN", "BEDTC_JOIN")
  ) %>%
  mutate(
    needs_blank_belnkid =
      toupper(trimws(as.character(BEOCCUR))) == "Y" &
      (is.na(has_selected_stool) | has_selected_stool == FALSE),
    BELNKID = if_else(needs_blank_belnkid, "", BELNKID)
  )

# warn_rows <- be3 %>%
#   filter(.data[["needs_blank_belnkid"]] %in% TRUE) %>%
#   transmute(
#    usubjid = as.character(USUBJID),
#     visit = as.character(VISIT),
#    bedtc = as.character(BEDTC_JOIN)
#  )

# if (nrow(warn_rows) > 0) {
#   apply(warn_rows, 1, function(r) {
#     warning(
#       paste0(
#        "No selected stool in GIQ for usubjid=", r[["usubjid"]],
#         " at visit=", r[["visit"]],
#         " bedtc=", r[["bedtc"]],
#         " — BELNKID left empty for this date."
#       ),
#       call. = FALSE
#     )
#     invisible(NULL)
#   })
# }

# Finalize ----
be <- sdtm_create_domain(be3, delperm = FALSE)
