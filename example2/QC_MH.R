# ******************************************************************************
# Danone Nutricia Research
# ******************************************************************************
# STUDY/PROJECT : SONA / 23REX0061265
# PROGRAM PATH  : SAS/SONA/Files/SDTM Conversion/40 SDTM Programs
# PROGRAM NAME  : QC_MH.R 
# PURPOSE       : QC SDTM data for Medical History
# ------------------------------------------------------------------------------
# NOTES : 
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2025-10-07 - cattanch - Initial program
# 2025-12-08 - cattanch - Use function for MHDICT and updated keys.
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
sdtm_domain <- "MH"

# Import metadata
sdtm_ct  <- read_excel("../31 Metadata Data/SONA_CT.xlsx", sheet = "CT")
sdtm_var <- read_sas("../31 Metadata Data/sdtm_3_4_variables.sas7bdat")
sdtm_dom <- read_sas("../31 Metadata Data/sdtm_3_4_domains.sas7bdat")

# Import data ----
dm1 <- read_sas("../41 SDTM Data/dm.sas7bdat")
mh1 <- copy_import(inset = "mh_meddra")
rs1 <- copy_import(inset = "review_status")

# Sources ----
sources <- rs1 %>%
  filter(trimws(formname) != "") %>%
  distinct(formid, formname)

# MH ----
mh2 <-
  assign_no_ct(
    raw_dat = mh1,
    raw_var = "mhspid",
    tgt_var = "MHSPID",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = mh1,
    raw_var = "mhterm",
    tgt_var = "MHTERM",
    id_vars = oak_id_vars()
  ) %>%
  assign_datetime(
    raw_dat = mh1,
    raw_var = "mhstdat",
    tgt_var = "MHSTDTC",
    raw_fmt = c("y-m-d"),
    raw_unk = c("NK", "UNK", "UN"),
    id_vars = oak_id_vars()
  ) %>%
  assign_datetime(
    raw_dat = mh1,
    raw_var = "mhendat",
    tgt_var = "MHENDTC",
    raw_fmt = c("y-m-d"),
    raw_unk = c("NK", "UNK", "UN"),
    id_vars = oak_id_vars()
  ) %>%
  hardcode_ct(
    raw_dat = condition_add(mh1, mhongo == "Y"),
    raw_var = "mhongo",
    tgt_var = "MHENRTPT",
    ct_spec = sdtm_ct,
    ct_clst = "C66728",
    tgt_val = "ONGOING",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_ct(
    raw_dat = condition_add(mh1, mhongo == "N"),
    raw_var = "mhongo",
    tgt_var = "MHENRTPT",
    ct_spec = sdtm_ct,
    ct_clst = "C66728",
    tgt_val = "BEFORE",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_no_ct(
    raw_dat = condition_add(mh1, mhongo != ""),
    raw_var = "mhongo",
    tgt_var = "MHENTPT",
    tgt_val = "INFORMED CONSENT",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = mh1,
    raw_var = "mhcurmed",
    tgt_var = "MHCURMED",
    ct_spec = sdtm_ct,
    ct_clst = "C66742",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = mh1,
    raw_var = "mhpr",
    tgt_var = "MHMEDINT",
    ct_spec = sdtm_ct,
    ct_clst = "C66742",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = mh1,
    raw_var = "usubjid",
    tgt_var = "USUBJID",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = mh1,
    raw_var = "subjectid",
    tgt_var = "SUBJID",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = mh1,
    raw_var = "soc_code",
    tgt_var = "MHBDSYCD",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = mh1,
    raw_var = "soc_name",
    tgt_var = "MHBODSYS",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = mh1,
    raw_var = "pt_name",
    tgt_var = "MHDECOD",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = mh1,
    raw_var = "hlgt_name",
    tgt_var = "MHHLGT",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = mh1,
    raw_var = "hlgt_code",
    tgt_var = "MHHLGTCD",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = mh1,
    raw_var = "hlt_name",
    tgt_var = "MHHLT",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = mh1,
    raw_var = "hlt_code",
    tgt_var = "MHHLTCD",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = mh1,
    raw_var = "llt_name",
    tgt_var = "MHLLT",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = mh1,
    raw_var = "llt_code",
    tgt_var = "MHLLTCD",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = mh1,
    raw_var = "pt_code",
    tgt_var = "MHPTCD",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = mh1,
    raw_var = "pt_soc_name",
    tgt_var = "MHSOC",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = mh1,
    raw_var = "pt_soc_code",
    tgt_var = "MHSOCCD",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = mh1,
    raw_var = "soc_list",
    tgt_var = "MHSOCLST",
    id_vars = oak_id_vars()
  ) %>%
  mutate(MHSTDTC_TMP = MHSTDTC) %>%
  derive_study_day(
    sdtm_in       = .,
    dm_domain     = dm1,
    tgdt          = "MHSTDTC",
    refdt         = "RFSTDTC",
    study_day_var = "MHSTDY",
    merge_key     = "USUBJID"
  ) %>%
  mutate(MHSTDTC = MHSTDTC_TMP) %>%
  mutate(MHENDTC_TMP = MHENDTC) %>%
  derive_study_day(
    sdtm_in       = .,
    dm_domain     = dm1,
    tgdt          = "MHENDTC",
    refdt         = "RFSTDTC",
    study_day_var = "MHENDY",
    merge_key     = "USUBJID"
  ) %>%
  mutate(MHENDTC = MHENDTC_TMP) %>%
  mutate(
    STUDYID  = study,
    DOMAIN   = sdtm_domain,
    MHCAT    = "RELEVANT MEDICAL HISTORY AND PRE-EXISTING CONDITIONS",
    SOURCEID = paste("CRF:", sources$formname[match(sdtm_domain, sources$formid)]),
    MHDICT   = if_else(!is.na(MHDECOD), paste("MedDRA", get_meddra_version(indata = mh1)), NA_character_)
  ) %>%
  derive_seq(
    tgt_var  = "MHSEQ",
    rec_vars = c("STUDYID", "USUBJID", "MHTERM", "MHDECOD", "MHSTDTC", "MHENDTC")
  )

# Finalize ----
mh <- sdtm_create_domain(mh2, delperm = FALSE, keys = "STUDYID, USUBJID, MHTERM, MHDECOD, MHSTDTC, MHENDTC")
