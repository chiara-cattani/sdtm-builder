# ******************************************************************************
# STUDY-PILOT
# ******************************************************************************
# PROGRAM NAME  : SV.R
# PURPOSE       : SDTM SV Domain - Subject Visits
# ------------------------------------------------------------------------------
# NOTES :
#   Raw datasets : sv
#   Dependencies : sdtmbuilder package
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2026-03-01 - sdtmbuilder - Auto-generated program
# ******************************************************************************

# Configuration ----
library(sdtmbuilder)
library(dplyr)

study       <- "STUDY-PILOT"
sdtm_domain <- "SV"

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

sv <- all_raw[["sv"]]
dm1 <- all_raw[["dm"]]

# Prepare DM for RFSTDTC ----
# Load DM SDTM dataset (has RFSTDTC); fall back to raw DM
dm_rda <- "sdtm/datasets/RDA/dm.rda"
dm_xpt <- "sdtm/datasets/XPT/dm.xpt"
if (file.exists(dm_rda)) {
  dm_env <- new.env(parent = emptyenv())
  load(dm_rda, envir = dm_env)
  dm_sdtm <- dm_env[[ls(dm_env)[1]]]
  names(dm_sdtm) <- tolower(names(dm_sdtm))
} else if (file.exists(dm_xpt)) {
  dm_sdtm <- haven::read_xpt(dm_xpt)
  names(dm_sdtm) <- tolower(names(dm_sdtm))
} else {
  dm_sdtm <- dm1
}

dm_slim <- dm_sdtm %>%
  dplyr::select(dplyr::any_of(c("subjectid", "subjid", "usubjid", "rfstdtc"))) %>%
  dplyr::distinct()

# Join RFSTDTC: try subjectid=subjid, fall back to usubjid
if ("subjectid" %in% names(sv) && "subjid" %in% names(dm_slim)) {
  sv <- sv %>%
    dplyr::left_join(dm_slim %>% dplyr::select(dplyr::any_of(c("subjid", "rfstdtc"))) %>% dplyr::rename(subjectid = subjid), by = "subjectid")
} else if ("usubjid" %in% names(sv) && "usubjid" %in% names(dm_slim)) {
  sv <- sv %>%
    dplyr::left_join(dm_slim %>% dplyr::select(dplyr::any_of(c("usubjid", "rfstdtc"))), by = "usubjid")
}
if ("rfstdtc" %in% names(sv) && !"RFSTDTC" %in% names(sv)) sv$RFSTDTC <- sv$rfstdtc

# SV derivations ----
sv2 <- sv %>%
  # --- Identifiers ---
  mutate(
    STUDYID = study,
    DOMAIN = "SV"
  ) %>%
  derive_usubjid(study, subjid_col = "subjectid") %>%
  # --- Visit Variables ---
  mutate(
    VISITNUM = as.numeric(visitnum_derived),
    VISIT = as.character(visit_derived)
  ) %>%
  # --- Derived Variables ---
  mutate(
    SVPRESP = "Y",
    SVCNTMOD = as.character(svcntmod_derived)
  ) %>%
  # --- Visit Variables ---
  mutate(VISITDY = as.numeric(visitdy_derived)) %>%
  # --- Derived Variables ---
  mutate(
    SVSTDTC = as.character(svstdtc_derived),
    SVENDTC = as.character(svstdtc_derived)
  ) %>%
  # --- Identifiers ---
  mutate(
    SOURCEID = "Viedoc Event",
    SUBJID = as.character(subjectid)
  ) %>%
  # --- Derived Variables ---
  mutate(SVOCCUR = NA_character_  # TODO: case_when derivation) %>%
  # --- Study Day ---
  derive_dy("SVSTDY", "SVSTDTC", "RFSTDTC") %>%
  derive_dy("SVENDY", "SVENDTC", "RFSTDTC")

# Finalize ----
sv_final <- export_domain(
  data        = sv2,
  domain      = sdtm_domain,
  output_dir  = "sdtm/datasets",
  formats     = c("xpt", "rda"),
  xpt_version = 8L,
  target_meta = target_meta,
  domain_meta = domain_meta,
  keys        = c("STUDYID", "USUBJID", "VISITNUM"),
  drop_empty_perm = TRUE
)

cli::cli_alert_success("SV domain created: {nrow(sv2)} rows")
