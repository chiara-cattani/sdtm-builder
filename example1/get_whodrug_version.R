# ******************************************************************************
# Danone Nutricia Research
# ******************************************************************************
# STUDY/PROJECT : SONA / 23REX0061265
# PROGRAM PATH  : SAS/SONA/Files/SDTM Conversion/01 Macros
# PROGRAM NAME  : get_whodrug_version.R 
# PURPOSE       : Obtain WHODrug  version from dataset
# ------------------------------------------------------------------------------
# NOTES : 
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2025-12-08 - cattanch - Initial program
# ******************************************************************************

library(dplyr)
library(stringr)
library(glue)

get_whodrug_version <- function(indata,
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
  
  dictvar <- dictvar_match[1] # use the matched column name
  
  # Extract WHODrug version candidates ----
  versions_df <- indata %>%
    filter(!is.na(.data[[dictvar]]),
           trimws(.data[[dictvar]]) != "") %>%
    mutate(
      dict_raw = as.character(.data[[dictvar]]),
      # SAS: strip(prxchange('s/^(.*)(20\d\d)(.*)$/$2/io', -1, &dictvar.))
      # If regex doesn't match, stringr leaves it unchanged -> same behaviour.
      version_year = str_replace(
        dict_raw,
        regex("^(.*)(20\\d\\d)(.*)$", ignore_case = TRUE),
        "\\2"
      ),
      dict_trim = str_trim(dict_raw),
      year_trim = str_trim(version_year),
      dict_upper = toupper(dict_raw),
      whodrug = case_when(
        # If regex didn't change the string => no usable pattern -> blank
        year_trim == dict_trim ~ "",
        str_detect(dict_upper, "MAR") | str_detect(dict_upper, "Q1") ~
          paste0(year_trim, "Q1"),
        str_detect(dict_upper, "SEP") | str_detect(dict_upper, "Q3") ~
          paste0(year_trim, "Q3"),
        TRUE ~ ""
      )
    ) %>%
    distinct(whodrug) %>%
    filter(!is.na(whodrug), whodrug != "") %>%
    arrange(whodrug)
  
  # Collapse versions with "#" delimiter (like SAS separated by '#') ----
  versions <- versions_df$whodrug
  result <- paste(versions, collapse = "#")
  
  # QC messages ----
  if (result == "") {
    message("-DQC: WHODrug version could not be determined.")
  } else if (str_detect(result, "#")) {
    message(glue(
      "-DQC: More than one distinct WHODrug version present in dataset ",
      "({str_replace_all(result, '#', ', ')})."
    ))
  } else if (!is.null(compare) && nzchar(compare) && result != compare) {
    message(glue(
      "-DQC: WHODrug version in data ({result}) does not match expected WHODrug version ({compare})."
    ))
  }
  
  return(result)
}

# Example usage:
# whodrug_version <- get_whodrug_version(indata = ae_whodrug, compare = "2023Q1")
# print(whodrug_version)