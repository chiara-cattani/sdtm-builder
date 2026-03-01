# ******************************************************************************
# Danone Nutricia Research
# ******************************************************************************
# STUDY/PROJECT : SONA / 23REX0061265
# PROGRAM PATH  : SAS/SONA/Files/SDTM Conversion/01 Macros
# PROGRAM NAME  : create_sdtm_qc.r
# PURPOSE       : Run all QC programs in the correct order
# ------------------------------------------------------------------------------
# NOTES :
#   - This function sources all QC_*.R files in the "70 QC" folder.
#   - It runs QC_TI.R, QC_TV.R, QC_DM.R first, then all others alphabetically.
#   - Temporarily sets working directory to "70 QC" so relative paths work.
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2025-10-09 - cattanch - Initial program
# ******************************************************************************

#' @title Run all QC programs in the correct order
#'
#' @description
#' This function sources all QC_*.R files in the "70 QC" folder,
#' starting with TI, TV, DM, then all others alphabetically.
#'
#' @param qc_dir Character. Path to the "70 QC" folder.
#'
#' @return Invisibly returns the list of files that were sourced.
#'
#' @examples
#' create_all_qc("SAS/SONA/Files/15 SDTM Conversion/70 QC")
create_sdtm_qc <- function(qc_dir = "../70 QC") {
  stopifnot(dir.exists(qc_dir))
  
  # List all QC_*.R files
  all_qc_files <- list.files(qc_dir, pattern = "^QC_.*\\.R$", full.names = TRUE)
  
  # Extract domain codes
  domain_order <- c("TI", "TV", "DM")
  file_domains <- basename(all_qc_files) |>
    stringr::str_remove("^QC_") |>
    stringr::str_remove("\\.R$")
  
  # Create ordered list: TI, TV, DM, then rest alphabetically
  ordered_domains <- c(domain_order, sort(setdiff(file_domains, domain_order)))
  ordered_files <- file.path(qc_dir, paste0("QC_", ordered_domains, ".R"))
  ordered_files <- ordered_files[file.exists(ordered_files)]
  
  # Save current working directory
  original_wd <- getwd()
  setwd(qc_dir)
  
  # Source each file
  for (f in ordered_files) {
    message("Running: ", basename(f))
    tryCatch(
      source(f, local = FALSE),
      error = function(e) {
        message("Error in ", basename(f), ": ", conditionMessage(e))
      }
    )
  }
  
  # Restore original working directory
  setwd(original_wd)
  
  invisible(ordered_files)
}