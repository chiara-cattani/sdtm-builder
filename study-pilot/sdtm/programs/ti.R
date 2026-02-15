# ******************************************************************************
# STUDY-PILOT
# ******************************************************************************
# PROGRAM NAME  : TI.R
# PURPOSE       : SDTM TI Domain - Trial Inclusion/Exclusion Criteria
# ------------------------------------------------------------------------------
# NOTES :
#   Raw datasets : ti
#   Dependencies : sdtmbuilder package
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2026-02-15 - sdtmbuilder - Auto-generated program
# ******************************************************************************

# Configuration ----
library(sdtmbuilder)
library(dplyr)

study       <- "STUDY-PILOT"
sdtm_domain <- "TI"

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

ti <- all_raw[["ti"]]

# TI derivations ----
ti2 <- ti %>%
  # --- Identifiers ---
  mutate(
    STUDYID = study,
    DOMAIN = "TI"
  ) %>%
  # --- Derived Variables ---
  mutate(
    IETESTCD = as.character(ietestcd),
    IETEST = as.character(ietest)
  ) %>%
  # --- Categories ---
  mutate(IECAT = as.character(iecat)) %>%
  # --- Derived Variables ---
  mutate(TIVERS = as.character(tivers))

# Finalize ----
ti_final <- export_domain(
  data        = ti2,
  domain      = sdtm_domain,
  output_dir  = "sdtm/datasets",
  formats     = c("xpt", "rda"),
  xpt_version = 8L,
  target_meta = target_meta,
  domain_meta = domain_meta,
  keys        = c("STUDYID", "IETESTCD", "TIVERS"),
  drop_empty_perm = TRUE
)

cli::cli_alert_success("TI domain created: {nrow(ti2)} rows")
