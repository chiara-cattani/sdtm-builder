# ******************************************************************************
# Danone Nutricia Research
# ******************************************************************************
# STUDY/PROJECT : SONA / 23REX0061265
# PROGRAM PATH  : SAS/SONA/Files/SDTM Conversion/40 SDTM Programs
# PROGRAM NAME  : QC_VS.R 
# PURPOSE       : QC SDTM data for Vital Signs
# ------------------------------------------------------------------------------
# NOTES : 
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2025-10-02 - cattanch - Initial program
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
sdtm_domain <- "VS"

# Import metadata
sdtm_ct  <- read_excel("../31 Metadata Data/SONA_CT.xlsx", sheet = "CT")
sdtm_var <- read_sas("../31 Metadata Data/sdtm_3_4_variables.sas7bdat")
sdtm_dom <- read_sas("../31 Metadata Data/sdtm_3_4_domains.sas7bdat")

# Import data ----
dm1 <- read_sas("../41 SDTM Data/dm.sas7bdat")
an1 <- copy_import(inset = "an")
rs1 <- copy_import(inset = "review_status")

# Sources ----
sources <- rs1 %>%
  filter(trimws(formname) != "") %>%
  distinct(formid, formname)

# VSALL ----
vs_all <-
  hardcode_ct(
    raw_dat = condition_add(an1, !is.na(an_ndnd)),
    raw_var = "an_ndnd",
    tgt_var = "VSTESTCD",
    tgt_val = "VSALL",
    ct_spec = sdtm_ct,
    ct_clst = "C66741"
  ) %>%
  hardcode_ct(
    raw_dat = an1,
    raw_var = "an_ndnd",
    tgt_var = "VSTEST",
    tgt_val = "All Vital Signs Tests",
    ct_spec = sdtm_ct,
    ct_clst = "C67153",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_ct(
    raw_dat = an1,
    raw_var = "an_ndnd",
    tgt_var = "VSSTAT",
    tgt_val = "NOT DONE",
    ct_spec = sdtm_ct,
    ct_clst = "C66789",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = an1,
    raw_var = "anreasnd",
    tgt_var = "VSREASND",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = an1,
    raw_var = "usubjid",
    tgt_var = "USUBJID",
    id_vars = oak_id_vars()
  ) %>%
  filter(!is.na(VSTESTCD))

# Weight ----
vs_weight <-
  hardcode_ct(
    raw_dat = condition_add(an1, !is.na(weight)),
    raw_var = "weight",
    tgt_var = "VSTESTCD",
    tgt_val = "WEIGHT",
    ct_spec = sdtm_ct,
    ct_clst = "C66741"
  ) %>%
  hardcode_ct(
    raw_dat = an1,
    raw_var = "weight",
    tgt_var = "VSTEST",
    tgt_val = "Weight",
    ct_spec = sdtm_ct,
    ct_clst = "C67153",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = an1,
    raw_var = "weight",
    tgt_var = "VSORRES",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = an1,
    raw_var = "weight",
    tgt_var = "VSSTRESC",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = an1,
    raw_var = "weight",
    tgt_var = "VSSTRESN",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_ct(
    raw_dat = an1,
    raw_var = "weight_u",
    tgt_var = "VSORRESU",
    tgt_val = "kg",
    ct_spec = sdtm_ct,
    ct_clst = "C66770",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_ct(
    raw_dat = an1,
    raw_var = "weight_u",
    tgt_var = "VSSTRESU",
    tgt_val = "kg",
    ct_spec = sdtm_ct,
    ct_clst = "C66770",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = an1,
    raw_var = "usubjid",
    tgt_var = "USUBJID",
    id_vars = oak_id_vars()
  ) %>%
  filter(!is.na(VSTESTCD))

# Weight ND ----
vs_weight_nd <-
  hardcode_ct(
    raw_dat = condition_add(an1, !is.na(weight_ndnd)),
    raw_var = "weight_ndnd",
    tgt_var = "VSTESTCD",
    tgt_val = "WEIGHT",
    ct_spec = sdtm_ct,
    ct_clst = "C66741"
  ) %>%
  hardcode_ct(
    raw_dat = an1,
    raw_var = "weight_ndnd",
    tgt_var = "VSTEST",
    tgt_val = "Weight",
    ct_spec = sdtm_ct,
    ct_clst = "C67153",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_ct(
    raw_dat = an1,
    raw_var = "weight_ndnd",
    tgt_var = "VSSTAT",
    tgt_val = "NOT DONE",
    ct_spec = sdtm_ct,
    ct_clst = "C66789",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = an1,
    raw_var = "usubjid",
    tgt_var = "USUBJID",
    id_vars = oak_id_vars()
  ) %>%
  filter(!is.na(VSTESTCD))

# Height ----
vs_height <-
  hardcode_ct(
    raw_dat = condition_add(an1, !is.na(height)),
    raw_var = "height",
    tgt_var = "VSTESTCD",
    tgt_val = "HEIGHT",
    ct_spec = sdtm_ct,
    ct_clst = "C66741"
  ) %>%
  hardcode_ct(
    raw_dat = an1,
    raw_var = "height",
    tgt_var = "VSTEST",
    tgt_val = "Height",
    ct_spec = sdtm_ct,
    ct_clst = "C67153",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = an1,
    raw_var = "height",
    tgt_var = "VSORRES",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = an1,
    raw_var = "height",
    tgt_var = "VSSTRESC",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = an1,
    raw_var = "height",
    tgt_var = "VSSTRESN",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_ct(
    raw_dat = an1,
    raw_var = "height_u",
    tgt_var = "VSORRESU",
    tgt_val = "cm",
    ct_spec = sdtm_ct,
    ct_clst = "C66770",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_ct(
    raw_dat = an1,
    raw_var = "height_u",
    tgt_var = "VSSTRESU",
    tgt_val = "cm",
    ct_spec = sdtm_ct,
    ct_clst = "C66770",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = an1,
    raw_var = "usubjid",
    tgt_var = "USUBJID",
    id_vars = oak_id_vars()
  ) %>%
  filter(!is.na(VSTESTCD))

# Height ND ----
vs_height_nd <-
  hardcode_ct(
    raw_dat = condition_add(an1, !is.na(height_ndnd)),
    raw_var = "height_ndnd",
    tgt_var = "VSTESTCD",
    tgt_val = "HEIGHT",
    ct_spec = sdtm_ct,
    ct_clst = "C66741"
  ) %>%
  hardcode_ct(
    raw_dat = an1,
    raw_var = "height_ndnd",
    tgt_var = "VSTEST",
    tgt_val = "Height",
    ct_spec = sdtm_ct,
    ct_clst = "C67153",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_ct(
    raw_dat = an1,
    raw_var = "height_ndnd",
    tgt_var = "VSSTAT",
    tgt_val = "NOT DONE",
    ct_spec = sdtm_ct,
    ct_clst = "C66789",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = an1,
    raw_var = "usubjid",
    tgt_var = "USUBJID",
    id_vars = oak_id_vars()
  ) %>%
  filter(!is.na(VSTESTCD))

# BMI ----
vs_bmi <-
  hardcode_ct(
    raw_dat = condition_add(an1, !is.na(bmi)),
    raw_var = "bmi",
    tgt_var = "VSTESTCD",
    tgt_val = "BMI",
    ct_spec = sdtm_ct,
    ct_clst = "C66741"
  ) %>%
  hardcode_ct(
    raw_dat = an1,
    raw_var = "bmi",
    tgt_var = "VSTEST",
    tgt_val = "Body Mass Index",
    ct_spec = sdtm_ct,
    ct_clst = "C67153",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = an1,
    raw_var = "bmi",
    tgt_var = "VSORRES",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = an1,
    raw_var = "bmi",
    tgt_var = "VSSTRESC",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = an1,
    raw_var = "bmi",
    tgt_var = "VSSTRESN",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_ct(
    raw_dat = an1,
    raw_var = "bmi_u",
    tgt_var = "VSORRESU",
    tgt_val = "kg/m2",
    ct_spec = sdtm_ct,
    ct_clst = "C66770",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_ct(
    raw_dat = an1,
    raw_var = "bmi_u",
    tgt_var = "VSSTRESU",
    tgt_val = "kg/m2",
    ct_spec = sdtm_ct,
    ct_clst = "C66770",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = an1,
    raw_var = "usubjid",
    tgt_var = "USUBJID",
    id_vars = oak_id_vars()
  ) %>%
  filter(!is.na(VSTESTCD))

# VS ----
vs1 <- 
  bind_rows(
    vs_all,
    vs_weight,
    vs_weight_nd,
    vs_height,
    vs_height_nd,
    vs_bmi
  ) %>%
  assign_datetime(
    raw_dat = an1,
    raw_var = "andat",
    tgt_var = "VSDTC",
    raw_fmt = c("y-m-d"),
    raw_unk = c("NK", "UNK", "UN"),
    id_vars = oak_id_vars()
  ) %>%
  mutate(
    STUDYID  = study,
    DOMAIN   = sdtm_domain,
    VSCAT    = "ANTHROPOMETRICS",
    SOURCEID =  paste("CRF:", sources$formname[match("AN", sources$formid)])
  ) %>%
  assign_no_ct(
    raw_dat = an1,
    raw_var = "subjectid",
    tgt_var = "SUBJID",
    id_vars = oak_id_vars()
  ) %>%
  mutate(VSDTC_TMP = VSDTC) %>%
  derive_study_day(
    sdtm_in       = .,
    dm_domain     = dm1,
    tgdt          = "VSDTC",
    refdt         = "RFSTDTC",
    study_day_var = "VSDY",
    merge_key     = "USUBJID"
  ) %>%
  mutate(VSDTC = VSDTC_TMP) %>%
  derive_lobxfl(., dm1, sdtm_domain,
    add_val_rule = expr(toupper(coalesce(VSSTAT, "")) != "NOT DONE")
  ) %>%
  left_join(
    an1 %>% select(all_of(c(oak_id_vars(), "eventid"))),
    by = oak_id_vars()
  ) %>%
  derive_visit_vars(., eventid_var = "eventid") %>%
  derive_seq(
    tgt_var = "VSSEQ", 
    rec_vars = c("STUDYID", "USUBJID", "VSTESTCD", "VISITNUM")
  ) 

# Finalize ----
vs <- sdtm_create_domain(vs1, keys = "STUDYID, USUBJID, VSTESTCD, VISITNUM", delperm = FALSE)
