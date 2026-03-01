# ******************************************************************************
# Danone Nutricia Research
# ******************************************************************************
# STUDY/PROJECT : SONA / 23REX0061265
# PROGRAM PATH  : SAS/SONA/Files/SDTM Conversion/70 QC
# PROGRAM NAME  : create_all_qc.R
# PURPOSE       : Automatically detect and run all SDTM QC programs in 70 QC folder
# ------------------------------------------------------------------------------
# NOTES :
#   - It calls `create_sdtm_qc()` to source all QC_*.R programs from the "70 QC" folder.
#   - QC programs are executed in the following order: TI, TV, DM, then all others alphabetically.
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2025-10-09 - cattanch - Initial program
# ******************************************************************************

# Configuration ----
library(nutriciaconfig)
nutricia_config()

# Set working directory ----
if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  # Running in RStudio
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
} else {
  # Running in LSAF
  setwd(progdir)
}

# Create all qc datasets ----
create_sdtm_qc()
