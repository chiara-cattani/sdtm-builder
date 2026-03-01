# ******************************************************************************
# Danone Nutricia Research
# ******************************************************************************
# STUDY/PROJECT : SONA / 23REX0061265
# PROGRAM PATH  : SAS/SONA/Files/SDTM Conversion/40 SDTM Programs
# PROGRAM NAME  : QC_SU.R 
# PURPOSE       : QC SDTM data for Substance Use
# ------------------------------------------------------------------------------
# NOTES : 
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2025-10-14 - cattanch - Initial program
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
sdtm_domain <- "SU"

# Import metadata
sdtm_ct  <- read_excel("../31 Metadata Data/SONA_CT.xlsx", sheet = "CT")
sdtm_var <- read_sas("../31 Metadata Data/sdtm_3_4_variables.sas7bdat")
sdtm_dom <- read_sas("../31 Metadata Data/sdtm_3_4_domains.sas7bdat")

# Import data ----
dm1 <- read_sas("../41 SDTM Data/dm.sas7bdat")
su1 <- copy_import(inset = "su")
rs1 <- copy_import(inset = "review_status")

# Sources ----
sources <- rs1 %>%
  filter(trimws(formname) != "") %>%
  distinct(formid, formname)

# Alcohol ----
su_alc <-
  assign_ct(
    raw_dat = condition_add(su1, !is.na(alcyn)),
    raw_var = "alcyn",
    tgt_var = "SUOCCUR",
    ct_spec = sdtm_ct,
    ct_clst = "C66742",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_no_ct(
    raw_dat = condition_add(su1, !is.na(alcsp)),
    raw_var = "alcsp",
    tgt_var = "SUDOSU",
    tgt_val = "/wk",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = su1,
    raw_var = "subjectid",
    tgt_var = "SUBJID",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = su1,
    raw_var = "usubjid",
    tgt_var = "USUBJID",
    id_vars = oak_id_vars()
  ) %>%
  mutate(
    SUDOSTXT = str_trim(tolower(str_replace(su1$alcsp, regex("per week", ignore_case = TRUE), ""))),    SUEVINTX = "DURING THE LAST 6 MONTHS",
    SUTRT    = "ALCOHOL",
    SUCAT    = "ALCOHOL",
    SUPRESP  = "Y"
  )

# Smoking ----

## Cigarettes ----
su_cig <-
  assign_ct(
    raw_dat = condition_add(su1, !is.na(smokciyn)),
    raw_var = "smokciyn",
    tgt_var = "SUOCCUR",
    ct_spec = sdtm_ct,
    ct_clst = "C66742",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = su1,
    raw_var = "smokcig",
    tgt_var = "SUDOSE",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = su1,
    raw_var = "cigfreq",
    tgt_var = "SUDOSU",
    ct_spec = sdtm_ct,
    ct_clst = "C71620",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = su1,
    raw_var = "subjectid",
    tgt_var = "SUBJID",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = su1,
    raw_var = "usubjid",
    tgt_var = "USUBJID",
    id_vars = oak_id_vars()
  ) %>%
  mutate(
    SUTRT    = "CIGARETTES",
    SUCAT    = "TOBACCO AND/OR E-CIGARETTES",
    SUPRESP  = "Y"
  )

## Cigars ----
su_cgr <-
  assign_ct(
    raw_dat = condition_add(su1, !is.na(smokcgyn)),
    raw_var = "smokcgyn",
    tgt_var = "SUOCCUR",
    ct_spec = sdtm_ct,
    ct_clst = "C66742",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = su1,
    raw_var = "smokcgr",
    tgt_var = "SUDOSE",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = su1,
    raw_var = "cgrfreq",
    tgt_var = "SUDOSU",
    ct_spec = sdtm_ct,
    ct_clst = "C71620",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = su1,
    raw_var = "subjectid",
    tgt_var = "SUBJID",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = su1,
    raw_var = "usubjid",
    tgt_var = "USUBJID",
    id_vars = oak_id_vars()
  ) %>%
  mutate(
    SUTRT    = "CIGARS",
    SUCAT    = "TOBACCO AND/OR E-CIGARETTES",
    SUPRESP  = "Y"
  )

## Pipe ----
su_pip <-
  assign_ct(
    raw_dat = condition_add(su1, !is.na(smokpiyn)),
    raw_var = "smokpiyn",
    tgt_var = "SUOCCUR",
    ct_spec = sdtm_ct,
    ct_clst = "C66742",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = su1,
    raw_var = "smokpip",
    tgt_var = "SUDOSE",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = su1,
    raw_var = "pipfreq",
    tgt_var = "SUDOSU",
    ct_spec = sdtm_ct,
    ct_clst = "C71620",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = su1,
    raw_var = "subjectid",
    tgt_var = "SUBJID",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = su1,
    raw_var = "usubjid",
    tgt_var = "USUBJID",
    id_vars = oak_id_vars()
  ) %>%
  mutate(
    SUTRT    = "PIPE",
    SUCAT    = "TOBACCO AND/OR E-CIGARETTES",
    SUPRESP  = "Y"
  )

## E-cigarettes with nicotine ----
su_ecn <-
  assign_ct(
    raw_dat = condition_add(su1, !is.na(smokenyn)),
    raw_var = "smokenyn",
    tgt_var = "SUOCCUR",
    ct_spec = sdtm_ct,
    ct_clst = "C66742",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = su1,
    raw_var = "smokecn",
    tgt_var = "SUDOSE",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = su1,
    raw_var = "ecnfreq",
    tgt_var = "SUDOSU",
    ct_spec = sdtm_ct,
    ct_clst = "C71620",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = su1,
    raw_var = "subjectid",
    tgt_var = "SUBJID",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = su1,
    raw_var = "usubjid",
    tgt_var = "USUBJID",
    id_vars = oak_id_vars()
  ) %>%
  mutate(
    SUTRT    = "E-CIGARETTES WITH NICOTINE",
    SUCAT    = "TOBACCO AND/OR E-CIGARETTES",
    SUPRESP  = "Y"
  )

## E-cigarettes without nicotine ----
su_ecw <-
  assign_ct(
    raw_dat = condition_add(su1, !is.na(smokewyn)),
    raw_var = "smokewyn",
    tgt_var = "SUOCCUR",
    ct_spec = sdtm_ct,
    ct_clst = "C66742",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = su1,
    raw_var = "smokecwn",
    tgt_var = "SUDOSE",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = su1,
    raw_var = "ecwnfreq",
    tgt_var = "SUDOSU",
    ct_spec = sdtm_ct,
    ct_clst = "C71620",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = su1,
    raw_var = "subjectid",
    tgt_var = "SUBJID",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = su1,
    raw_var = "usubjid",
    tgt_var = "USUBJID",
    id_vars = oak_id_vars()
  ) %>%
  mutate(
    SUTRT    = "E-CIGARETTES WITHOUT NICOTINE",
    SUCAT    = "TOBACCO AND/OR E-CIGARETTES",
    SUPRESP  = "Y"
  )

# SU ----
su2 <- 
  bind_rows(
    su_alc,
    su_cig,
    su_cgr,
    su_pip,
    su_ecn,
    su_ecw
  ) %>%
  assign_datetime(
    raw_dat = su1,
    raw_var = "eventdate",
    tgt_var = "SUDTC",
    raw_fmt = c("y-m-d"),
    raw_unk = c("NK", "UNK", "UN"),
    id_vars = oak_id_vars()
  ) %>%
  mutate(SUDTC_TMP = SUDTC) %>%
  derive_study_day(
    sdtm_in       = .,
    dm_domain     = dm1,
    tgdt          = "SUDTC",
    refdt         = "RFSTDTC",
    study_day_var = "SUDY",
    merge_key     = "USUBJID"
  ) %>%
  mutate(SUDTC = SUDTC_TMP) %>%
  mutate(
    STUDYID  = study,
    DOMAIN   = sdtm_domain,
    SOURCEID = paste("CRF:", sources$formname[match(sdtm_domain, sources$formid)])
  ) %>%
  left_join(
    su1 %>% select(all_of(c(oak_id_vars(), "eventid"))),
    by = oak_id_vars()
  ) %>%
  derive_visit_vars(., eventid_var = "eventid") %>%
  derive_seq(
    tgt_var = "SUSEQ", 
    rec_vars = c("STUDYID", "USUBJID", "SUTRT")
  ) 

# Finalize ----
su <- sdtm_create_domain(su2, keys = "STUDYID, USUBJID, SUTRT")
