# ******************************************************************************
# Danone Nutricia Research
# ******************************************************************************
# STUDY/PROJECT : SONA / 23REX0061265
# PROGRAM PATH  : SAS/SONA/Files/SDTM Conversion/01 Macros
# PROGRAM NAME  : get_meddra_version.R 
# PURPOSE       : Obtain MedDRA version from dataset
# ------------------------------------------------------------------------------
# NOTES : 
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2025-12-08 - cattanch - Initial program
# ******************************************************************************

library(dplyr)
library(stringr)
library(glue)

get_meddra_version <- function(indata,
                               dictvar = "DictInstance",
                               compare = NULL) {
  
  # Validate input ----
  if (missing(indata) || is.null(indata)) {
    message("WARNING: No dataset provided.")
    return("")
  }
  
  if (!is.data.frame(indata)) {
    message("WARNING: Input is not a data frame.")
    return("")
  }
  
  # Case-insensitive column selection ----
  dictvar_match <- names(indata)[tolower(names(indata)) == tolower(dictvar)]
  
  if (length(dictvar_match) == 0) {
    message(glue("WARNING: Neither '{dictvar}' nor its lowercase variant found in dataset."))
    return("")
  }
  
  dictvar <- dictvar_match[1]  # Use the matched column name
  
  # Extract distinct versions ----
  versions <- indata %>%
    filter(!is.na(.data[[dictvar]]), trimws(.data[[dictvar]]) != "") %>%
    mutate(
      tmp_version = str_replace_all(.data[[dictvar]],
                                 regex("^([^\\d]*)(\\d{2}\\.\\d)([^\\d]*)$", ignore_case = TRUE),
                                 "\\2")
    ) %>%
    distinct(.data[[dictvar]], tmp_version) %>%
    mutate(tmp_meddra = if_else(str_trim(tmp_version) == str_trim(.data[[dictvar]]), "", tmp_version)) %>%
    arrange(tmp_meddra) %>%
    pull(tmp_meddra)
  
  # Collapse versions with "#" delimiter
  result <- paste(versions[versions != ""], collapse = "#")
  
  # QC messages ----
  if (result == "") {
    message("-DQC: MedDRA version could not be determined.")
  } else if (str_detect(result, "#")) {
    message(glue("-DQC: More than one distinct MedDRA version present: {str_replace_all(result, '#', ', ')}"))
  } else if (!is.null(compare) && result != compare) {
    message(glue("-DQC: MedDRA version in data ({result}) does not match expected version ({compare})."))
  }
  
  return(result)
}

# Example usage:
# meddra_version <- get_meddra_version(indata = mydata, compare = "27.1")
# print(meddra_version)
