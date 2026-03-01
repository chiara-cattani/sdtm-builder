# ******************************************************************************
# Danone Nutricia Research
# ******************************************************************************
# STUDY/PROJECT : SONA / 23REX0061265
# PROGRAM PATH  : SAS/SONA/Files/SDTM Conversion/40 SDTM Programs
# PROGRAM NAME  : QC_CM.R 
# PURPOSE       : QC SDTM data for Concomitant Medications
# ------------------------------------------------------------------------------
# NOTES : 
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2025-10-01 - cattanch - Initial program
# 2025-12-08 - cattanch - Use function for CMDICT.
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
sdtm_domain <- "CM"

# Import metadata
sdtm_ct  <- read_excel("../31 Metadata Data/SONA_CT.xlsx", sheet = "CT")
sdtm_var <- read_sas("../31 Metadata Data/sdtm_3_4_variables.sas7bdat")
sdtm_dom <- read_sas("../31 Metadata Data/sdtm_3_4_domains.sas7bdat")

# Import data ----
dm1 <- read_sas("../41 SDTM Data/dm.sas7bdat")
cm1 <- copy_import(inset = "cm_whodrug")
rs1 <- copy_import(inset = "review_status")

# Sources ----
sources <- rs1 %>%
  filter(trimws(formname) != "") %>%
  distinct(formid, formname)

# CM ----
cm2 <-
  assign_no_ct(
    raw_dat = cm1,
    raw_var = "cmspid",
    tgt_var = "CMSPID",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = cm1,
    raw_var = "cmtrt",
    tgt_var = "CMTRT",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = cm1 %>%
      mutate(cmdostxt_clean = if_else(
        !is.na(cmdostxt),
        gsub(",", ".", as.character(cmdostxt)),
        NA_character_
      )) %>%
      mutate(cmdostxt_num = suppressWarnings(as.numeric(cmdostxt_clean))) %>%
      condition_add(!is.na(cmdostxt_num)),
    raw_var = "cmdostxt_num",
    tgt_var = "CMDOSE",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = cm1 %>%
      mutate(cmdostxt_clean = if_else(
        !is.na(cmdostxt),
        gsub(",", ".", as.character(cmdostxt)),
        NA_character_
      )) %>%
      mutate(cmdostxt_num = suppressWarnings(as.numeric(cmdostxt_clean))) %>%
      condition_add(is.na(cmdostxt_num)),
    raw_var = "cmdostxt",
    tgt_var = "CMDOSTXT",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = cm1,
    raw_var = "cmdosu",
    tgt_var = "CMDOSU",
    ct_spec = sdtm_ct,
    ct_clst = "C71620",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = cm1,
    raw_var = "cmfrq",
    tgt_var = "CMDOSFRQ",
    ct_spec = sdtm_ct,
    ct_clst = "C71113",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = cm1,
    raw_var = "cmfrqsp",
    tgt_var = "CMDSFRQX",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = cm1,
    raw_var = "cmroute",
    tgt_var = "CMROUTE",
    ct_spec = sdtm_ct,
    ct_clst = "C66729",
    id_vars = oak_id_vars()
  ) %>%
  assign_datetime(
    raw_dat = cm1,
    raw_var = "cmstdat",
    tgt_var = "CMSTDTC",
    raw_fmt = c("y-m-d"),
    raw_unk = c("NK", "UNK", "UN"),
    id_vars = oak_id_vars()
  ) %>%
  assign_datetime(
    raw_dat = cm1,
    raw_var = "cmendat",
    tgt_var = "CMENDTC",
    raw_fmt = c("y-m-d"),
    raw_unk = c("NK", "UNK", "UN"),
    id_vars = oak_id_vars()
  ) %>%
  hardcode_ct(
    raw_dat = condition_add(cm1, cmongo == "Y"),
    raw_var = "cmongo",
    tgt_var = "CMENRTPT",
    ct_spec = sdtm_ct,
    ct_clst = "C66728",
    tgt_val = "ONGOING",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_ct(
    raw_dat = condition_add(cm1, cmongo == "N"),
    raw_var = "cmongo",
    tgt_var = "CMENRTPT",
    ct_spec = sdtm_ct,
    ct_clst = "C66728",
    tgt_val = "BEFORE",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_no_ct(
    raw_dat = condition_add(cm1, cmongo != ""),
    raw_var = "cmongo",
    tgt_var = "CMENTPT",
    tgt_val = "END OF STUDY",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = cm1,
    raw_var = "indicat",
    tgt_var = "CMINDC",
    ct_spec = sdtm_ct,
    ct_clst = "D0024",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = cm1,
    raw_var = "cmindsp",
    tgt_var = "CMINDCX",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = cm1,
    raw_var = "usubjid",
    tgt_var = "USUBJID",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = cm1,
    raw_var = "subjectid",
    tgt_var = "SUBJID",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = cm1,
    raw_var = "cmdecod",
    tgt_var = "CMDECOD",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = cm1,
    raw_var = "cmclas",
    tgt_var = "CMCLAS",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = cm1,
    raw_var = "cmclascd",
    tgt_var = "CMCLASCD",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = cm1,
    raw_var = "cmatc1",
    tgt_var = "CMATC1",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = cm1,
    raw_var = "cmatc2",
    tgt_var = "CMATC2",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = cm1,
    raw_var = "cmatc3",
    tgt_var = "CMATC3",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = cm1,
    raw_var = "cmatc4",
    tgt_var = "CMATC4",
    id_vars = oak_id_vars()
  ) %>%
  mutate(CMSTDTC_TMP = CMSTDTC) %>%
  derive_study_day(
    sdtm_in       = .,
    dm_domain     = dm1,
    tgdt          = "CMSTDTC",
    refdt         = "RFSTDTC",
    study_day_var = "CMSTDY",
    merge_key     = "USUBJID"
  ) %>%
  mutate(CMSTDTC = CMSTDTC_TMP) %>%
  mutate(CMENDTC_TMP = CMENDTC) %>%
  derive_study_day(
    sdtm_in       = .,
    dm_domain     = dm1,
    tgdt          = "CMENDTC",
    refdt         = "RFSTDTC",
    study_day_var = "CMENDY",
    merge_key     = "USUBJID"
  ) %>%
  mutate(CMENDTC = CMENDTC_TMP) %>%
  mutate(
    STUDYID  = study,
    DOMAIN   = sdtm_domain,
    CMCAT    = "CONCOMITANT MEDICATIONS AND NUTRITIONAL SUPPLEMENTS",
    SOURCEID = paste("CRF:", sources$formname[match(sdtm_domain, sources$formid)]),
    CMDICT   = if_else(!is.na(CMDECOD), paste("WHODrug", get_whodrug_version(indata = cm1)), NA_character_)
  ) %>%
  derive_seq(
    tgt_var  = "CMSEQ",
    rec_vars = c("STUDYID", "USUBJID", "CMTRT", "CMSTDTC")
  )

# Finalize ----
cm <- sdtm_create_domain(cm2, delperm = FALSE)

