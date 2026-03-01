# ******************************************************************************
# Danone Nutricia Research
# ******************************************************************************
# STUDY/PROJECT : SONA / 23REX0061265
# PROGRAM PATH  : SAS/SONA/Files/SDTM Conversion/40 SDTM Programs
# PROGRAM NAME  : QC_PR.R 
# PURPOSE       : QC SDTM data for Procedures
# ------------------------------------------------------------------------------
# NOTES : 
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2025-10-17 - cattanch - Initial program
# 2025-10-29 - Vikas Shokeen - Extended Checks
# 2025-12-08 - cattanch - Minor: display PRINDCX with addsuppvars
#                         Minor: Use get_meddra_version for PRDICT.
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
library(lubridate)

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
sdtm_domain <- "PR"

# Import metadata
sdtm_ct  <- read_excel("../31 Metadata Data/SONA_CT.xlsx", sheet = "CT")
sdtm_var <- read_sas("../31 Metadata Data/sdtm_3_4_variables.sas7bdat")
sdtm_dom <- read_sas("../31 Metadata Data/sdtm_3_4_domains.sas7bdat")

# Import data ----
dm1 <- read_sas("../41 SDTM Data/dm.sas7bdat")
pr1 <- copy_import(inset = "pr_meddra")
rs1 <- copy_import(inset = "review_status")

# Sources ----
sources <- rs1 %>%
  filter(trimws(formname) != "") %>%
  distinct(formid, formname)

# PR ----
pr2 <-
  assign_no_ct(
    raw_dat = pr1,
    raw_var = "prspid",
    tgt_var = "PRSPID",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = pr1,
    raw_var = "prtrt",
    tgt_var = "PRTRT",
    id_vars = oak_id_vars()
  ) %>%
  assign_datetime(
    raw_dat = pr1,
    raw_var = "prstdat",
    tgt_var = "PRSTDTC",
    raw_fmt = c("y-m-d"),
    raw_unk = c("NK", "UNK", "UN"),
    id_vars = oak_id_vars()
  ) %>%
  assign_datetime(
    raw_dat = pr1,
    raw_var = "prendat",
    tgt_var = "PRENDTC",
    raw_fmt = c("y-m-d"),
    raw_unk = c("NK", "UNK", "UN"),
    id_vars = oak_id_vars()
  ) %>%
  hardcode_ct(
    raw_dat = condition_add(pr1, prongo == "Y"),
    raw_var = "prongo",
    tgt_var = "PRENRTPT",
    ct_spec = sdtm_ct,
    ct_clst = "C66728",
    tgt_val = "ONGOING",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_ct(
    raw_dat = condition_add(pr1, prongo == "N"),
    raw_var = "prongo",
    tgt_var = "PRENRTPT",
    ct_spec = sdtm_ct,
    ct_clst = "C66728",
    tgt_val = "BEFORE",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_no_ct(
    raw_dat = pr1,
    raw_var = "prongo",
    tgt_var = "PRENTPT",
    tgt_val = "END OF STUDY",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = pr1,
    raw_var = "prindicat",
    tgt_var = "PRINDC",
    ct_spec = sdtm_ct,
    ct_clst = "D0017",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = condition_add(pr1, prindicat == "PREVENTIVE / FOR SCREENING PURPOSES"),
    raw_var = "prindicat",
    tgt_var = "PRINDCX",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = pr1,
    raw_var = "usubjid",
    tgt_var = "USUBJID",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = pr1,
    raw_var = "subjectid",
    tgt_var = "SUBJID",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = pr1,
    raw_var = "soc_code",
    tgt_var = "PRBDSYCD",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = pr1,
    raw_var = "soc_name",
    tgt_var = "PRBODSYS",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = pr1,
    raw_var = "pt_name",
    tgt_var = "PRDECOD",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = pr1,
    raw_var = "hlgt_name",
    tgt_var = "PRHLGT",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = pr1,
    raw_var = "hlgt_code",
    tgt_var = "PRHLGTCD",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = pr1,
    raw_var = "hlt_name",
    tgt_var = "PRHLT",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = pr1,
    raw_var = "hlt_code",
    tgt_var = "PRHLTCD",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = pr1,
    raw_var = "llt_name",
    tgt_var = "PRLLT",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = pr1,
    raw_var = "llt_code",
    tgt_var = "PRLLTCD",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = pr1,
    raw_var = "pt_code",
    tgt_var = "PRPTCD",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = pr1,
    raw_var = "pt_soc_name",
    tgt_var = "PRSOC",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = pr1,
    raw_var = "pt_soc_code",
    tgt_var = "PRSOCCD",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = pr1,
    raw_var = "soc_list",
    tgt_var = "PRSOCLST",
    id_vars = oak_id_vars()
  ) %>%
  mutate(
    STUDYID  = study,
    DOMAIN   = sdtm_domain,
    SOURCEID = paste("CRF:", sources$formname[match(sdtm_domain, sources$formid)]),
    PRCAT    = "MEDICAL PROCEDURE",
    PRDICT   = if_else(!is.na(PRDECOD), paste("MedDRA", get_meddra_version(indata = pr1)), NA_character_)
  ) %>%
  mutate(PRSTDTC_TMP = PRSTDTC) %>%
  derive_study_day(
    sdtm_in       = .,
    dm_domain     = dm1,
    tgdt          = "PRSTDTC",
    refdt         = "RFSTDTC",
    study_day_var = "PRSTDY",
    merge_key     = "USUBJID"
  ) %>%
  mutate(PRSTDTC = PRSTDTC_TMP) %>%
  mutate(PRENDTC_TMP = PRENDTC) %>%
  derive_study_day(
    sdtm_in       = .,
    dm_domain     = dm1,
    tgdt          = "PRENDTC",
    refdt         = "RFSTDTC",
    study_day_var = "PRENDY",
    merge_key     = "USUBJID"
  ) %>%
  mutate(PRENDTC = PRENDTC_TMP) %>%
  derive_seq(
    tgt_var  = "PRSEQ",
    rec_vars = c("STUDYID", "USUBJID", "PRTRT", "PRSTDTC")
  )

attr(pr2$PRINDCX, "label") <- "Indication Specification"

# Finalize ----
pr <- sdtm_create_domain(pr2, delperm = FALSE, keys = "STUDYID, USUBJID, PRTRT, PRSTDTC", addsuppvars = c("PRINDCX"))
