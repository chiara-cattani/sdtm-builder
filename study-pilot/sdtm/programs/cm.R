# ******************************************************************************
# STUDY-PILOT
# ******************************************************************************
# PROGRAM NAME  : CM.R
# PURPOSE       : SDTM CM Domain - Concomitant/Prior Medications
# ------------------------------------------------------------------------------
# NOTES :
#   Raw datasets : cm_whodrug
#   Dependencies : sdtmbuilder package
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2026-02-15 - sdtmbuilder - Auto-generated program
# ******************************************************************************

# Configuration ----
library(sdtmbuilder)
library(dplyr)

study       <- "STUDY-PILOT"
sdtm_domain <- "CM"

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

cm_whodrug <- all_raw[["cm_whodrug"]]
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
if ("subjectid" %in% names(cm_whodrug) && "subjid" %in% names(dm_slim)) {
  cm_whodrug <- cm_whodrug %>%
    dplyr::left_join(dm_slim %>% dplyr::select(dplyr::any_of(c("subjid", "rfstdtc"))) %>% dplyr::rename(subjectid = subjid), by = "subjectid")
} else if ("usubjid" %in% names(cm_whodrug) && "usubjid" %in% names(dm_slim)) {
  cm_whodrug <- cm_whodrug %>%
    dplyr::left_join(dm_slim %>% dplyr::select(dplyr::any_of(c("usubjid", "rfstdtc"))), by = "usubjid")
}
if ("rfstdtc" %in% names(cm_whodrug) && !"RFSTDTC" %in% names(cm_whodrug)) cm_whodrug$RFSTDTC <- cm_whodrug$rfstdtc

# CM derivations ----
cm2 <- cm_whodrug %>%
  # --- Identifiers ---
  mutate(
    STUDYID = study,
    DOMAIN = "CM",
    CMSPID = as.character(cmspid)
  ) %>%
  # --- Topic / Term ---
  mutate(CMTRT = as.character(cmtrt)) %>%
  # --- Coded Fields ---
  mutate(CMDECOD = as.character(cmdecod)) %>%
  # --- Categories ---
  mutate(CMCAT = "CONCOMITANT MEDICATIONS AND NUTRITIONAL SUPPLEMENTS") %>%
  # --- Derived Variables ---
  mutate(
    CMINDC = as.character(indicat),
    CMCLAS = as.character(cmclas),
    CMCLASCD = as.character(cmclascd),
    CMDOSE = as.numeric(cmdose),
    CMDOSTXT = as.character(cmdostxt),
    CMDOSU = as.character(cmdosu),
    CMDOSFRQ = as.character(cmfrq),
    CMROUTE = as.character(cmroute)
  ) %>%
  # --- Dates ---
  mutate(
    CMSTDTC = format_iso_dtc(cmstdat),
    CMENDTC = format_iso_dtc(cmendat)
  ) %>%
  # --- Coded Fields ---
  mutate(
    CMENRTPT = NA_character_,  # TODO: case_when derivation
    CMENTPT = dplyr::if_else(!is.na(cmongo), "END OF STUDY", NA_character_)
  ) %>%
  # --- Derived Variables ---
  mutate(
    CMATC1 = as.character(cmatc1),
    CMATC2 = as.character(cmatc2),
    CMATC3 = as.character(cmatc3),
    CMATC4 = as.character(cmatc4)
  ) %>%
  # --- Dates ---
  mutate(CMCODED = format_iso_dtc(codedondate)) %>%
  # --- Derived Variables ---
  mutate(
    CMDSFRQX = as.character(cmfrqsp),
    CMINDCX = as.character(cmindsp)
  ) %>%
  # --- Dates ---
  mutate(
    CMINITD = format_iso_dtc(initiateddate),
    CMLTEDID = format_iso_dtc(lastediteddate)
  ) %>%
  # --- Identifiers ---
  mutate(
    SOURCEID = paste("CRF:", sources$formname[match("CM", toupper(sources$formid))]),
    SUBJID = as.character(subjectid)
  ) %>%
  derive_usubjid(study, subjid_col = "subjectid") %>%
  # --- Dictionary Version ---
  mutate(CMDICT = dplyr::if_else(!is.na(CMDECOD), paste("WHODrug", get_whodrug_version(cm_whodrug, dictvar = "DictInstance")), "")) %>%
  # --- Study Day ---
  derive_dy("CMSTDY", "CMSTDTC", "RFSTDTC") %>%
  derive_dy("CMENDY", "CMENDTC", "RFSTDTC") %>%
  # --- Sequence ---
  derive_seq("CMSEQ", by = c("USUBJID"))

# Finalize ----
cm_final <- export_domain(
  data        = cm2,
  domain      = sdtm_domain,
  output_dir  = "sdtm/datasets",
  formats     = c("xpt", "rda"),
  xpt_version = 8L,
  target_meta = target_meta,
  domain_meta = domain_meta,
  keys        = c("STUDYID", "USUBJID", "CMTRT", "CMSTDTC"),
  drop_empty_perm = TRUE
)

cli::cli_alert_success("CM domain created: {nrow(cm2)} rows")
