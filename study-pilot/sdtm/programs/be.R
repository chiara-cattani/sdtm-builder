# ******************************************************************************
# STUDY-PILOT
# ******************************************************************************
# PROGRAM NAME  : BE.R
# PURPOSE       : SDTM BE Domain - Biospecimen Events
# ------------------------------------------------------------------------------
# NOTES :
#   Raw datasets : be
#   Dependencies : sdtmbuilder package
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2026-02-15 - sdtmbuilder - Auto-generated program
# ******************************************************************************

# Configuration ----
library(sdtmbuilder)
library(dplyr)

study       <- "STUDY-PILOT"
sdtm_domain <- "BE"

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

be <- all_raw[["be"]]
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
if ("subjectid" %in% names(be) && "subjid" %in% names(dm_slim)) {
  be <- be %>%
    dplyr::left_join(dm_slim %>% dplyr::select(dplyr::any_of(c("subjid", "rfstdtc"))) %>% dplyr::rename(subjectid = subjid), by = "subjectid")
} else if ("usubjid" %in% names(be) && "usubjid" %in% names(dm_slim)) {
  be <- be %>%
    dplyr::left_join(dm_slim %>% dplyr::select(dplyr::any_of(c("usubjid", "rfstdtc"))), by = "usubjid")
}
if ("rfstdtc" %in% names(be) && !"RFSTDTC" %in% names(be)) be$RFSTDTC <- be$rfstdtc

# BE derivations ----
be2 <- be %>%
  # --- Identifiers ---
  mutate(
    STUDYID = study,
    DOMAIN = "BE"
  ) %>%
  derive_usubjid(study, subjid_col = "subjectid") %>%
  # --- Sequence ---
  derive_seq("BESEQ", by = c("USUBJID")) %>%
  # --- Derived Variables ---
  mutate(BEREFID = as.character(berefid_derived)) %>%
  # --- Topic / Term ---
  mutate(BETERM = as.character(beterm_derived)) %>%
  # --- Coded Fields ---
  mutate(BEDECOD = as.character(bedecod_derived)) %>%
  # --- Categories ---
  mutate(BECAT = as.character(becat_derived)) %>%
  # --- Derived Variables ---
  mutate(
    BEPRESP = as.character(bepresp_derived),
    BEOCCUR = as.character(beoccur_derived)
  ) %>%
  # --- Coded Fields ---
  mutate(BEREASOC = as.character(bereasoc_derived)) %>%
  # --- Visit Variables ---
  mutate(
    VISITNUM = as.numeric(visitnum_derived),
    VISIT = as.character(visit_derived)
  ) %>%
  # --- Derived Variables ---
  mutate(
    BEDTC = as.character(bedtc_derived),
    BESTDTC = as.character(bestdtc_derived),
    BEENDTC = as.character(beendtc_derived)
  ) %>%
  # --- Coded Fields ---
  mutate(
    BEENRTPT = as.character(beenrtpt_derived),
    BEENTPT = as.character(beentpt_derived)
  ) %>%
  # --- Derived Variables ---
  mutate(
    BECLMETH = as.character(beclmeth_derived),
    BEGEO = as.character(begeo_derived),
    BESPEC = as.character(bespec_derived)
  ) %>%
  # --- Identifiers ---
  mutate(
    SOURCEID = as.character(sourceid_derived),
    SUBJID = as.character(subjectid)
  ) %>%
  # --- Study Day ---
  derive_dy("BEDY", "BEDTC", "RFSTDTC") %>%
  derive_dy("BESTDY", "BESTDTC", "RFSTDTC") %>%
  derive_dy("BEENDY", "BEENDTC", "RFSTDTC")

# Finalize ----
be_final <- export_domain(
  data        = be2,
  domain      = sdtm_domain,
  output_dir  = "sdtm/datasets",
  formats     = c("xpt", "rda"),
  xpt_version = 8L,
  target_meta = target_meta,
  domain_meta = domain_meta,
  keys        = c("STUDYID", "USUBJID", "BESPEC", "BETERM", "VISIT", "BESTDTC"),
  drop_empty_perm = TRUE
)

cli::cli_alert_success("BE domain created: {nrow(be2)} rows")
