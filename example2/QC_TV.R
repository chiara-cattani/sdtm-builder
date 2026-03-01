# ******************************************************************************
# Danone Nutricia Research
# ******************************************************************************
# STUDY/PROJECT : SONA / 23REX0061265
# PROGRAM PATH  : SAS/SONA/Files/SDTM Conversion/40 SDTM Programs
# PROGRAM NAME  : QC_TV.R 
# PURPOSE       : QC SDTM data for Trial Visits
# ------------------------------------------------------------------------------
# NOTES : 
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2025-10-01 - cattanch - Initial program
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
sdtm_domain <- "TV"

# Import metadata
sdtm_ct  <- read_excel("../31 Metadata Data/SONA_CT.xlsx", sheet = "CT")
sdtm_var <- read_sas("../31 Metadata Data/sdtm_3_4_variables.sas7bdat")
sdtm_dom <- read_sas("../31 Metadata Data/sdtm_3_4_domains.sas7bdat")

# TV ----
tv1 <- tibble(
  STUDYID  = study,
  DOMAIN   = sdtm_domain,
  VISITNUM = c(1, 2, 3, 4, 5),
  VISIT    = c(
    "Screening",
    "Visit 1",
    "Visit 2",
    "Visit 3",
    "Visit 4"
  ),
  TVSTRL   = c(
    "Between Day -14 and Day -4",
    "Between 4 - 14 days after Screening",
    "Between Day 9 and Day 13",
    "Between Day 28 and Day 32",
    "Between Day 41 and Day 45"
  ),
  ARMCD = "ONS",
  ARM   = "4-week ONS Consumption (2 servings/day)"
)

# Finalize ----
tv <- sdtm_create_domain(tv1)
