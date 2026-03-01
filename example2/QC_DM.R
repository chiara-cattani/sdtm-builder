# ******************************************************************************
# Danone Nutricia Research
# ******************************************************************************
# STUDY/PROJECT : SONA / 23REX0061265
# PROGRAM PATH  : SAS/SONA/Files/SDTM Conversion/40 SDTM Programs
# PROGRAM NAME  : QC_DM.R 
# PURPOSE       : QC SDTM data for Demographics
# ------------------------------------------------------------------------------
# NOTES : 
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2025-09-29 - cattanch - Initial program
# 2025-10-07 - cattanch - Updated RFENDTC
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
sdtm_domain <- "DM"

# Import metadata
sdtm_ct  <- read_excel("../31 Metadata Data/SONA_CT.xlsx", sheet = "CT")
sdtm_var <- read_sas("../31 Metadata Data/sdtm_3_4_variables.sas7bdat")
sdtm_dom <- read_sas("../31 Metadata Data/sdtm_3_4_domains.sas7bdat")

# Import data ----
dm1    <- copy_import(inset = "dm")
ae1    <- copy_import(inset = "ae_meddra")
ie1    <- copy_import(inset = "ie")
eos1   <- copy_import(inset = "eos")
ic1    <- copy_import(inset = "ic")
ex1    <- copy_import(inset = "ex")
sae1   <- copy_import(inset = "sae")
vstat1 <- copy_import(inset = "vstat")
rs1    <- copy_import(inset = "review_status")

# Sources ----
sources <- rs1 %>%
  filter(trimws(formname) != "") %>%
  distinct(formid, formname)

# DM ----
dm2 <- 
  assign_no_ct(
    raw_dat = dm1,
    raw_var = "usubjid",
    tgt_var = "USUBJID",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = dm1,
    raw_var = "subjectid",
    tgt_var = "SUBJID",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = dm1,
    raw_var = "sex",
    tgt_var = "SEX",
    ct_spec = sdtm_ct,
    ct_clst = "C66731",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = dm1,
    raw_var = "country",
    tgt_var = "COUNTRY",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = dm1,
    raw_var = "age",
    tgt_var = "AGE",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = dm1,
    raw_var = "ageu",
    tgt_var = "AGEU",
    ct_spec = sdtm_ct,
    ct_clst = "C66781",
    id_vars = oak_id_vars()
  ) %>%
  mutate(
    STUDYID  = study,
    DOMAIN   = sdtm_domain,
    SITEID   = paste0(study, "-", dm1$sitecode),
    SOURCEID = paste("CRF:", sources$formname[match(sdtm_domain, sources$formid)]),
    RACE     = NA_real_,
  ) 

# AE ----
ae2 <-
  hardcode_no_ct(
    raw_dat = condition_add(ae1, aeout == "FATAL"),
    raw_var = "aeout",
    tgt_var = "DTHFL",
    tgt_val = "Y",
    id_vars = oak_id_vars()
  ) %>%
  # Keep one record per subject
  group_by(patient_number) %>%
  arrange(desc(!is.na(DTHFL))) %>%  
  slice(1) %>%                    
  ungroup()

# EOS ----
eos2 <-
  assign_no_ct(
    raw_dat = eos1,
    raw_var = "lcdat",
    tgt_var = "RFPENDTC",
    id_vars = (oak_id_vars())
  ) %>%
  assign_no_ct(
    raw_dat = eos1,
    raw_var = "rfxendat",
    tgt_var = "RFXENDTC",
    id_vars = (oak_id_vars())
  ) %>%
  assign_no_ct(
    raw_dat = condition_add(eos1, complyn != "Y"),
    raw_var = "etdat",
    tgt_var = "RFENDTC",
    id_vars = (oak_id_vars())
  ) %>%
  assign_no_ct(
    raw_dat = condition_add(eos1, complyn == "Y"),
    raw_var = "eventdate",
    tgt_var = "RFENDTC",
    id_vars = (oak_id_vars())
  )

# IC ----
ic2 <-
  assign_no_ct(
    raw_dat = ic1,
    raw_var = "icdat",
    tgt_var = "RFICDTC",
    id_vars = (oak_id_vars())
  ) %>%
  assign_no_ct(
    raw_dat = ic1,
    raw_var = "icdat",
    tgt_var = "RFSTDTC",
    id_vars = (oak_id_vars())
  )

# EX ----
ex2 <-
  assign_no_ct(
    raw_dat = ex1,
    raw_var = "exstdat",
    tgt_var = "RFXSTDTC",
    id_vars = (oak_id_vars())
  ) %>%
  # Keep one record per subject
  group_by(patient_number) %>%
  arrange(desc(!is.na(RFXSTDTC))) %>%  
  slice(1) %>%                    
  ungroup()

# SAE ----
sae2 <- 
  assign_no_ct(
    raw_dat = sae1,
    raw_var = "aesdtdat",
    tgt_var = "DTHDTC",
    id_vars = oak_id_vars()
  )

# Merge ----
dm3 <- merge_inputs(dm2, ae2, eos2, ic2, ex2, sae2)

# Arms ----
dm4 <- add_planned_actual_arm(
  inset       = dm3,
  ie1         = ie1,
  vstat1      = vstat1,
  armcd_const = "ONS",
  arm_const   = "4-week ONS Consumption (2 servings/day)"
  )

# Finalize ----
dm <- sdtm_create_domain(dm4)
