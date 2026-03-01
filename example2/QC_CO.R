# ******************************************************************************
# Danone Nutricia Research
# ******************************************************************************
# STUDY/PROJECT : SONA
# PROGRAM PATH  : SAS/SONA/Files/SDTM Conversion/40 SDTM Programs
# PROGRAM NAME  : QC_CO.R 
# PURPOSE       : QC SDTM data for Comments
# ------------------------------------------------------------------------------
# NOTES : 
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2025-10-03 - cattanch - Initial program
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
sdtm_domain <- "CO"

# Import metadata
sdtm_ct  <- read_excel("../31 Metadata Data/SONA_CT.xlsx", sheet = "CT")
sdtm_var <- read_sas("../31 Metadata Data/sdtm_3_4_variables.sas7bdat")
sdtm_dom <- read_sas("../31 Metadata Data/sdtm_3_4_domains.sas7bdat")

# Import data ----
dm1 <- read_sas("../41 SDTM Data/dm.sas7bdat")
co1 <- copy_import(inset = "co")
rs1 <- copy_import(inset = "review_status")
fm1 <- copy_import(inset = "formats")

# Sources ----
sources <- rs1 %>%
  filter(trimws(formname) != "") %>%
  distinct(formid, formname)

# Formats ----
formats <- fm1 %>%
  filter(fmtname == "_COVISIT") %>%
  distinct(start, label)

# CO ----
co2 <-
  assign_no_ct(
    raw_dat = co1,
    raw_var = "cospid",
    tgt_var = "COSPID",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = co1,
    raw_var = "coval",
    tgt_var = "COVAL",
    id_vars = oak_id_vars()
  ) %>%
  mutate(
    STUDYID  = study,
    DOMAIN   = sdtm_domain,
    SOURCEID = paste("CRF:", sources$formname[match(sdtm_domain, sources$formid)]),
    COREF    = trimws(ifelse(!is.na(co1$covisit), paste0(formats$label[match(co1$covisit, formats$start)], " - ", co1$coref), co1$coref))
  ) %>%
  assign_no_ct(
    raw_dat = co1,
    raw_var = "usubjid",
    tgt_var = "USUBJID",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = co1,
    raw_var = "subjectid",
    tgt_var = "SUBJID",
    id_vars = oak_id_vars()
  ) %>%
  derive_seq(
    tgt_var = "COSEQ", 
    rec_vars = c("STUDYID", "USUBJID", "COSPID")
  )

# Finalize ----
co <- sdtm_create_domain(co2, keys = "STUDYID, USUBJID, COSPID")
