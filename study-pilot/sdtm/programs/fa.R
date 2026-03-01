# ******************************************************************************
# STUDY-PILOT
# ******************************************************************************
# PROGRAM NAME  : FA.R
# PURPOSE       : SDTM FA Domain - Findings About Events or Interventions
# ------------------------------------------------------------------------------
# NOTES :
#   Raw datasets : fa
#   Dependencies : sdtmbuilder package
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2026-03-01 - sdtmbuilder - Auto-generated program
# ******************************************************************************

# Configuration ----
library(sdtmbuilder)
library(dplyr)

study       <- "STUDY-PILOT"
sdtm_domain <- "FA"

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

fa <- all_raw[["fa"]]
dm1 <- all_raw[["dm"]]

# Sources lookup (for SOURCEID) ----
review_status <- all_raw[["review_status"]]
sources <- review_status %>%
  dplyr::filter(trimws(formname) != "") %>%
  dplyr::distinct(formid, formname)

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
if ("subjectid" %in% names(fa) && "subjid" %in% names(dm_slim)) {
  fa <- fa %>%
    dplyr::left_join(dm_slim %>% dplyr::select(dplyr::any_of(c("subjid", "rfstdtc"))) %>% dplyr::rename(subjectid = subjid), by = "subjectid")
} else if ("usubjid" %in% names(fa) && "usubjid" %in% names(dm_slim)) {
  fa <- fa %>%
    dplyr::left_join(dm_slim %>% dplyr::select(dplyr::any_of(c("usubjid", "rfstdtc"))), by = "usubjid")
}
if ("rfstdtc" %in% names(fa) && !"RFSTDTC" %in% names(fa)) fa$RFSTDTC <- fa$rfstdtc

# FA derivations ----
fa2 <- fa %>%
  # --- Identifiers ---
  mutate(
    STUDYID = "STUDY-PILOT",
    DOMAIN = "FA"
  ) %>%
  derive_usubjid(study, subjid_col = "subjectid") %>%
  # --- Sequence ---
  derive_seq("FASEQ", by = c("USUBJID")) %>%
  # --- Identifiers ---
  mutate(FASPID = as.character(faspid_derived)) %>%
  # --- Derived Variables ---
  mutate(
    FALNKID = as.character(falnkid),
    FATESTCD = as.character(fatestcd_derived),
    FATEST = as.character(fatest_derived),
    FAOBJ = as.character(faobj_derived)
  ) %>%
  # --- Categories ---
  mutate(
    FACAT = as.character(facat_derived),
    FASCAT = as.character(fascat_derived)
  ) %>%
  # --- Derived Variables ---
  mutate(
    FAORRESU = as.character(faorresu),
    FASTRESC = as.character(faorres_derived),
    FASTRESN = signif(as.numeric(faorres), 3),
    FASTRESU = as.character(fastresu),
    FASTAT = as.character(fastat_derived),
    FAREASND = as.character(fareasnd_derived),
    FAEVAL = as.character(faeval_derived)
  ) %>%
  # --- Visit Variables ---
  mutate(
    VISITNUM = as.numeric(visitnum_derived),
    VISIT = as.character(visit_derived)
  ) %>%
  # --- Derived Variables ---
  mutate(FADTC = as.character(fadtc_derived)) %>%
  # --- Coded Fields ---
  mutate(FATPT = as.character(fatpt)) %>%
  # --- Time Points ---
  mutate(
    FATPTNUM = as.numeric(fatptnum),
    FAELTM = as.character(faeltm),
    FATPTREF = as.character(fatptref)
  ) %>%
  # --- Derived Variables ---
  mutate(
    FAEVINTX = as.character(faevintx_derived),
    FAORIG = as.character(faorig_derived)
  ) %>%
  # --- Identifiers ---
  mutate(
    SOURCEID = paste("CRF:", sources$formname[match("FA", toupper(sources$formid))]),
    SUBJID = as.character(subjectid)
  ) %>%
  # --- Derived Variables ---
  mutate(FAORRES = as.numeric()) %>%
  # --- Study Day ---
  derive_dy("FADY", "FADTC", "RFSTDTC")

# Finalize ----
fa_final <- export_domain(
  data        = fa2,
  domain      = sdtm_domain,
  output_dir  = "sdtm/datasets",
  formats     = c("xpt", "rda"),
  xpt_version = 8L,
  target_meta = target_meta,
  domain_meta = domain_meta,
  keys        = c("STUDYID", "USUBJID", "FATESTCD", "FAOBJ", "FASPID", "VISITNUM", "FATPTNUM"),
  drop_empty_perm = TRUE
)

cli::cli_alert_success("FA domain created: {nrow(fa2)} rows")
