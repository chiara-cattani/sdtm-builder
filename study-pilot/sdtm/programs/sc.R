# ******************************************************************************
# STUDY-PILOT
# ******************************************************************************
# PROGRAM NAME  : SC.R
# PURPOSE       : SDTM SC Domain - Subject Characteristics
# ------------------------------------------------------------------------------
# NOTES :
#   Raw datasets : sc
#   Dependencies : sdtmbuilder package
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2026-03-01 - sdtmbuilder - Auto-generated program
# ******************************************************************************

# Configuration ----
library(sdtmbuilder)
library(dplyr)

study       <- "STUDY-PILOT"
sdtm_domain <- "SC"

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

sc <- all_raw[["sc"]]

# SC derivations ----
sc2 <- sc %>%
  # --- Identifiers ---
  mutate(
    STUDYID = study,
    DOMAIN = "SC"
  ) %>%
  derive_usubjid(study, subjid_col = "subjectid") %>%
  # --- Sequence ---
  derive_seq("SCSEQ", by = c("USUBJID")) %>%
  # --- Derived Variables ---
  mutate(
    SCTESTCD = as.character(sctestcd_derived),
    SCTEST = as.character(sctest_derived),
    SCORRESU = as.character(scorresu_derived),
    SCSTRESC = as.character(scstresc_derived),
    SCSTRESN = as.numeric(scstresn_derived),
    SCSTRESU = as.character(scstresu_derived)
  ) %>%
  # --- Visit Variables ---
  mutate(
    VISITNUM = as.numeric(visitnum_derived),
    VISIT = as.character(visit_derived),
    VISITDY = as.numeric(visitdy_derived)
  ) %>%
  # --- Identifiers ---
  mutate(
    SOURCEID = as.character(sourceid_derived),
    SUBJID = as.character(subjectid)
  ) %>%
  # --- Derived Variables ---
  mutate(SCORRES = as.character(scorres_derived))

# Finalize ----
sc_final <- export_domain(
  data        = sc2,
  domain      = sdtm_domain,
  output_dir  = "sdtm/datasets",
  formats     = c("xpt", "rda"),
  xpt_version = 8L,
  target_meta = target_meta,
  domain_meta = domain_meta,
  keys        = c("STUDYID", "USUBJID", "SCTESTCD"),
  drop_empty_perm = TRUE
)

cli::cli_alert_success("SC domain created: {nrow(sc2)} rows")
