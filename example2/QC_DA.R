# ******************************************************************************
# Danone Nutricia Research
# ******************************************************************************
# STUDY/PROJECT : SONA / 23REX0061265
# PROGRAM PATH  : SAS/SONA/Files/SDTM Conversion/40 SDTM Programs
# PROGRAM NAME  : QC_DA.R
# PURPOSE       : QC SDTM data for Product Accountability
# ------------------------------------------------------------------------------
# NOTES : 
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2025-10-08 - cattanch - Initial program
# 2025-12-08 - cattanch - Updated DAREFID, DADTC, and DADY mapping.
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
sdtm_domain <- "DA"

# Import metadata
sdtm_ct  <- read_excel("../31 Metadata Data/SONA_CT.xlsx", sheet = "CT")
sdtm_var <- read_sas("../31 Metadata Data/sdtm_3_4_variables.sas7bdat")
sdtm_dom <- read_sas("../31 Metadata Data/sdtm_3_4_domains.sas7bdat")

# Import data ----
dm1  <- read_sas("../41 SDTM Data/dm.sas7bdat")
da1  <- copy_import(inset = "da")
chk1 <- copy_import(inset = "chk")
rs1  <- copy_import(inset = "review_status")

# Sources ----
sources <- rs1 %>%
  filter(trimws(formname) != "") %>%
  distinct(formid, formname)

# Dispensed amount ----
da_dispamt <-
  assign_no_ct(
    raw_dat = da1,
    raw_var = "dispamt",
    tgt_var = "DAORRES",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_ct(
    raw_dat = da1,
    raw_var = "dispamt",
    tgt_var = "DATESTCD",
    tgt_val = "DISPAMT",
    ct_spec = sdtm_ct,
    ct_clst = "C78732"
  ) %>%
  hardcode_ct(
    raw_dat = da1,
    raw_var = "dispamt",
    tgt_var = "DATEST",
    tgt_val = "Dispensed Amount",
    ct_spec = sdtm_ct,
    ct_clst = "C78731",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = da1,
    raw_var = "dispamt",
    tgt_var = "DASTRESC",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = da1,
    raw_var = "dispamt",
    tgt_var = "DASTRESN",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_ct(
    raw_dat = da1,
    raw_var = "dispamt",
    tgt_var = "DAORRESU",
    tgt_val = "SACHET",
    ct_spec = sdtm_ct,
    ct_clst = "C71620",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = da1,
    raw_var = "usubjid",
    tgt_var = "USUBJID",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = da1,
    raw_var = "subjectid",
    tgt_var = "SUBJID",
    id_vars = oak_id_vars()
  ) %>%
  assign_datetime(
    raw_dat = da1,
    raw_var = "eventdate",
    tgt_var = "DADTC",
    raw_fmt = c("y-m-d"),
    raw_unk = c("NK", "UNK", "UN"),
    id_vars = oak_id_vars()
  ) %>%
  mutate(DADTC_TMP = DADTC) %>%
  derive_study_day(
    sdtm_in       = .,
    dm_domain     = dm1,
    tgdt          = "DADTC",
    refdt         = "RFSTDTC",
    study_day_var = "DADY",
    merge_key     = "USUBJID"
  ) %>%
  mutate(DADTC = DADTC_TMP) %>%
  left_join(
    da1 %>% select(all_of(c(oak_id_vars(), "eventid"))),
    by = oak_id_vars()
  ) %>%
  derive_visit_vars(., eventid_var = "eventid") %>%
  left_join(
    chk1 %>%
      select(all_of(c("patient_number", "extrtnum"))) %>%
      filter(!is.na(extrtnum)) %>%
      rename(DAREFID = extrtnum),
    by = "patient_number"
  )

# Returned amount ----
da_retamt <-
  hardcode_ct(
    raw_dat = da1,
    raw_var = "retamt",
    tgt_var = "DATESTCD",
    tgt_val = "RETAMT",
    ct_spec = sdtm_ct,
    ct_clst = "C78732"
  ) %>%
  hardcode_ct(
    raw_dat = da1,
    raw_var = "retamt",
    tgt_var = "DATEST",
    tgt_val = "Returned Amount",
    ct_spec = sdtm_ct,
    ct_clst = "C78731",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = da1,
    raw_var = "retamt",
    tgt_var = "DAORRES",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = da1,
    raw_var = "retamt",
    tgt_var = "DASTRESC",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = da1,
    raw_var = "retamt",
    tgt_var = "DASTRESN",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_ct(
    raw_dat = da1,
    raw_var = "retamt",
    tgt_var = "DAORRESU",
    tgt_val = "SACHET",
    ct_spec = sdtm_ct,
    ct_clst = "C71620",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = da1,
    raw_var = "usubjid",
    tgt_var = "USUBJID",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = da1,
    raw_var = "subjectid",
    tgt_var = "SUBJID",
    id_vars = oak_id_vars()
  ) %>%
  assign_datetime(
    raw_dat = da1,
    raw_var = "eventdate",
    tgt_var = "DADTC",
    raw_fmt = c("y-m-d"),
    raw_unk = c("NK", "UNK", "UN"),
    id_vars = oak_id_vars()
  ) %>%
  mutate(DADTC_TMP = DADTC) %>%
  derive_study_day(
    sdtm_in       = .,
    dm_domain     = dm1,
    tgdt          = "DADTC",
    refdt         = "RFSTDTC",
    study_day_var = "DADY",
    merge_key     = "USUBJID"
  ) %>%
  mutate(DADTC = DADTC_TMP) %>%
  left_join(
    da1 %>% select(all_of(c(oak_id_vars(), "eventid", "eventdate"))),
    by = oak_id_vars()
  ) %>%
  derive_visit_vars(., eventid_var = "eventid")

# DA ----
da2 <- 
  bind_rows(
    da_dispamt,
    da_retamt
  ) %>%
  mutate(
    STUDYID  = study,
    DOMAIN   = sdtm_domain,
    SOURCEID = paste("CRF:", sources$formname[match(sdtm_domain, sources$formid)])
  ) %>%
  derive_seq(
    tgt_var = "DASEQ", 
    rec_vars = c("STUDYID", "USUBJID", "DATESTCD", "DADTC")
  )

# Finalize ----
da <- sdtm_create_domain(da2, delperm = FALSE)
