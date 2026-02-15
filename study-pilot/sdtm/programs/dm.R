# ******************************************************************************
# STUDY-PILOT
# ******************************************************************************
# PROGRAM NAME  : DM.R
# PURPOSE       : SDTM DM Domain - Demographics
# ------------------------------------------------------------------------------
# NOTES :
#   Raw datasets : dm, ic, eos, sae, rand
#   Dependencies : sdtmbuilder package
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2026-02-15 - sdtmbuilder - Auto-generated program
# ******************************************************************************

# Configuration ----
library(sdtmbuilder)
library(dplyr)

study       <- "STUDY-PILOT"
sdtm_domain <- "DM"

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

dm <- all_raw[["dm"]]
ic <- all_raw[["ic"]]
eos <- all_raw[["eos"]]
sae <- all_raw[["sae"]]
rand <- all_raw[["rand"]]

# Sources lookup (for SOURCEID) ----
review_status <- all_raw[["review_status"]]
sources <- review_status %>%
  dplyr::filter(trimws(formname) != "") %>%
  dplyr::distinct(formid, formname)

# Merge secondary source datasets ----
ic <- dplyr::rename(ic, dplyr::any_of(c(dmspid = "icspid")))
ic_new <- setdiff(names(ic), names(dm))
ic_slim <- ic[, c("subjectid", "dmspid", ic_new), drop = FALSE]
dm <- dm %>%
  dplyr::left_join(ic_slim, by = c("subjectid", "dmspid"))
eos <- dplyr::rename(eos, dplyr::any_of(c(dmspid = "eosspid")))
eos_new <- setdiff(names(eos), names(dm))
eos_slim <- eos[, c("subjectid", "dmspid", eos_new), drop = FALSE]
dm <- dm %>%
  dplyr::left_join(eos_slim, by = c("subjectid", "dmspid"))
sae <- dplyr::rename(sae, dplyr::any_of(c(dmspid = "saespid")))
sae_new <- setdiff(names(sae), names(dm))
sae_slim <- sae[, c("subjectid", "dmspid", sae_new), drop = FALSE]
dm <- dm %>%
  dplyr::left_join(sae_slim, by = c("subjectid", "dmspid"))
rand <- dplyr::rename(rand, dplyr::any_of(c(dmspid = "randspid")))
rand_new <- setdiff(names(rand), names(dm))
rand_slim <- rand[, c("subjectid", "dmspid", rand_new), drop = FALSE]
dm <- dm %>%
  dplyr::left_join(rand_slim, by = c("subjectid", "dmspid"))

# DM derivations ----
dm2 <- dm %>%
  # --- Identifiers ---
  mutate(
    STUDYID = study,
    DOMAIN = "DM"
  ) %>%
  derive_usubjid(study, subjid_col = "subjectid") %>%
  mutate(SUBJID = as.character(subjectid)) %>%
  # --- Derived Variables ---
  mutate(
    RFSTDTC = as.character(icdat),
    RFENDTC = NA_character_,  # TODO: case_when derivation
    RFXSTDTC = as.character(first_exstdat),
    RFXENDTC = as.character(rfxendat),
    RFICDTC = as.character(icdat),
    RFPENDTC = as.character(lcdat),
    DTHDTC = as.character(aesdtdat),
    DTHFL = NA_character_,  # TODO: case_when derivation
    SITEID = as.character(sitecode),
    BRTHDTC = as.character(brthdat),
    AGE = as.numeric(age),
    AGEU = as.character(ageu),
    SEX = toupper(sex),
    CETHNIC = as.character(ethnicc),
    ARMCD = as.character(trtcd),
    ARM = as.character(trt),
    ACTARMCD = as.character(trtcd),
    ACTARM = as.character(trt),
    ARMNRS = NA_character_,  # TODO: case_when derivation
    ACTARMUD = NA_character_,  # TODO: case_when derivation
    COUNTRY = stringr::str_extract(subjectid, "^([^-]+)"),
    RANDNUM = as.character(randnum_derived)
  ) %>%
  # --- Identifiers ---
  mutate(SOURCEID = paste("CRF:", sources$formname[match("DM", toupper(sources$formid))]))

# Finalize ----
dm_final <- export_domain(
  data        = dm2,
  domain      = sdtm_domain,
  output_dir  = "sdtm/datasets",
  formats     = c("xpt", "rda"),
  xpt_version = 8L,
  target_meta = target_meta,
  domain_meta = domain_meta,
  keys        = c("STUDYID", "USUBJID"),
  drop_empty_perm = TRUE
)

cli::cli_alert_success("DM domain created: {nrow(dm2)} rows")
