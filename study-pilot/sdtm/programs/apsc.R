# ******************************************************************************
# STUDY-PILOT
# ******************************************************************************
# PROGRAM NAME  : APSC.R
# PURPOSE       : SDTM APSC Domain - Subject Characteristics Associated Persons
# ------------------------------------------------------------------------------
# NOTES :
#   Raw datasets : apsc
#   Dependencies : sdtmbuilder package
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2026-02-15 - sdtmbuilder - Auto-generated program
# ******************************************************************************

# Configuration ----
library(sdtmbuilder)
library(dplyr)

study       <- "STUDY-PILOT"
sdtm_domain <- "APSC"

# Set working directory to study root ----
if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  prog_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
  setwd(file.path(prog_dir, "..", ".."))
} else if (exists("progdir")) {
  setwd(file.path(progdir, "..", ".."))
}

# Import metadata ----
study_meta <- read_study_metadata_excel("metadata/Study_Metadata.xlsx")
ct_spec    <- read_study_ct_excel("metadata/Study_CT.xlsx")

target_meta      <- study_meta$target_meta
domain_meta      <- study_meta$domain_meta
value_level_meta <- study_meta$value_level_meta

# Import data ----
all_raw <- load_raw_datasets("raw")
for (nm in names(all_raw)) {
  all_raw[[nm]] <- all_raw[[nm]] %>% standardize_names() %>% convert_blanks_to_na()
}

apsc <- all_raw[["apsc"]]

# Sources lookup (for SOURCEID) ----
review_status <- all_raw[["review_status"]]
sources <- review_status %>%
  dplyr::filter(trimws(formname) != "") %>%
  dplyr::distinct(formid, formname)

# APSC derivations ----
apsc2 <- apsc %>%
  # --- Identifiers ---
  mutate(
    STUDYID = "STUDY-PILOT",
    DOMAIN = "APSC"
  ) %>%
  # --- Derived Variables ---
  mutate(
    APID = as.character(apid_derived),
    SREL = as.character(srel_derived),
    SCTESTCD = as.character(sctestcd_derived),
    SCTEST = as.character(sctest_derived),
    SCORRES = as.character(scorres_derived),
    SCSTRESC = as.character(scstresc_derived)
  ) %>%
  # --- Visit Variables ---
  mutate(
    VISITNUM = as.numeric(visitnum_derived),
    VISIT = as.character(visit_derived)
  ) %>%
  # --- Identifiers ---
  mutate(
    SOURCEID = paste("CRF:", sources$formname[match("APSC", toupper(sources$formid))]),
    SUBJID = as.character(subjectid)
  ) %>%
  # --- Derived Variables ---
  derive_usubjid(study, subjid_col = "subjectid") %>%
  # --- Sequence ---
  derive_seq("SCSEQ", by = c("APID"))

# Finalize ----
apsc_final <- export_domain(
  data        = apsc2,
  domain      = sdtm_domain,
  output_dir  = "sdtm/datasets",
  formats     = c("xpt", "rda"),
  xpt_version = 8L,
  target_meta = target_meta,
  domain_meta = domain_meta,
  keys        = c("STUDYID", "APID", "SCTESTCD"),
  drop_empty_perm = TRUE
)

cli::cli_alert_success("APSC domain created: {nrow(apsc2)} rows")
