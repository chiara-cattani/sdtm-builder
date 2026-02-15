# ******************************************************************************
# STUDY-PILOT
# ******************************************************************************
# PROGRAM NAME  : MH.R
# PURPOSE       : SDTM MH Domain - Medical History
# ------------------------------------------------------------------------------
# NOTES :
#   Raw datasets : mh, mh_meddra
#   Dependencies : sdtmbuilder package
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2026-02-15 - sdtmbuilder - Auto-generated program
# ******************************************************************************

# Configuration ----
library(sdtmbuilder)
library(dplyr)

study       <- "STUDY-PILOT"
sdtm_domain <- "MH"

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

mh <- all_raw[["mh"]]
mh_meddra <- all_raw[["mh_meddra"]]
dm1 <- all_raw[["dm"]]

# Sources lookup (for SOURCEID) ----
review_status <- all_raw[["review_status"]]
sources <- review_status %>%
  dplyr::filter(trimws(formname) != "") %>%
  dplyr::distinct(formid, formname)

# Merge secondary source datasets ----
mh_meddra <- dplyr::rename(mh_meddra, dplyr::any_of(c(mhspid = "mh_meddraspid")))
mh_meddra_new <- setdiff(names(mh_meddra), names(mh))
mh_meddra_slim <- mh_meddra[, c("subjectid", "mhspid", mh_meddra_new), drop = FALSE]
mh <- mh %>%
  dplyr::left_join(mh_meddra_slim, by = c("subjectid", "mhspid"))

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
if ("subjectid" %in% names(mh) && "subjid" %in% names(dm_slim)) {
  mh <- mh %>%
    dplyr::left_join(dm_slim %>% dplyr::select(dplyr::any_of(c("subjid", "rfstdtc"))) %>% dplyr::rename(subjectid = subjid), by = "subjectid")
} else if ("usubjid" %in% names(mh) && "usubjid" %in% names(dm_slim)) {
  mh <- mh %>%
    dplyr::left_join(dm_slim %>% dplyr::select(dplyr::any_of(c("usubjid", "rfstdtc"))), by = "usubjid")
}
if ("rfstdtc" %in% names(mh) && !"RFSTDTC" %in% names(mh)) mh$RFSTDTC <- mh$rfstdtc

# MH derivations ----
mh2 <- mh %>%
  # --- Identifiers ---
  mutate(
    STUDYID = study,
    DOMAIN = "MH",
    MHSPID = as.character(mhspid)
  ) %>%
  # --- Topic / Term ---
  mutate(MHTERM = as.character(mhterm)) %>%
  # --- Coded Fields ---
  mutate(
    MHLLT = as.character(llt_name),
    MHLLTCD = as.numeric(llt_code),
    MHDECOD = as.character(pt_name),
    MHPTCD = as.numeric(pt_code),
    MHHLT = as.character(hlt_name),
    MHHLTCD = as.numeric(hlt_code),
    MHHLGT = as.character(hlgt_name),
    MHHLGTCD = as.numeric(hlgt_code)
  ) %>%
  # --- Categories ---
  mutate(MHCAT = "RELEVANT MEDICAL HISTORY AND PRE-EXISTING CONDITIONS") %>%
  # --- Coded Fields ---
  mutate(
    MHBODSYS = as.character(soc_name),
    MHBDSYCD = as.numeric(soc_code),
    MHSOC = as.character(pt_soc_name),
    MHSOCCD = as.numeric(pt_soc_code)
  ) %>%
  # --- Dates ---
  mutate(
    MHSTDTC = format_iso_dtc(mhstdat),
    MHENDTC = format_iso_dtc(mhendat)
  ) %>%
  # --- Coded Fields ---
  mutate(
    MHENRTPT = NA_character_,  # TODO: case_when derivation
    MHENTPT = dplyr::if_else(!is.na(mhongo), "INFORMED CONSENT", NA_character_)
  ) %>%
  # --- Dates ---
  mutate(MHCODED = format_iso_dtc(codedondate)) %>%
  # --- Derived Variables ---
  mutate(MHCURMED = as.character(mhcurmed)) %>%
  # --- Dates ---
  mutate(
    MHINITD = format_iso_dtc(initiateddate),
    MHLTEDID = format_iso_dtc(lastediteddate)
  ) %>%
  # --- Derived Variables ---
  mutate(MHMEDINT = as.character(mhpr)) %>%
  # --- Coded Fields ---
  mutate(MHSOCLST = as.character(soc_list)) %>%
  # --- Identifiers ---
  mutate(
    SOURCEID = paste("CRF:", sources$formname[match("MH", toupper(sources$formid))]),
    SUBJID = as.character(subjectid)
  ) %>%
  derive_usubjid(study, subjid_col = "subjectid") %>%
  # --- Dictionary Version ---
  mutate(MHDICT = dplyr::if_else(!is.na(MHDECOD), paste("MedDRA", get_meddra_version(mh_meddra, dictvar = "DictInstance")), "")) %>%
  # --- Study Day ---
  derive_dy("MHSTDY", "MHSTDTC", "RFSTDTC") %>%
  derive_dy("MHENDY", "MHENDTC", "RFSTDTC") %>%
  # --- Sequence ---
  derive_seq("MHSEQ", by = c("USUBJID"))

# Finalize ----
mh_final <- export_domain(
  data        = mh2,
  domain      = sdtm_domain,
  output_dir  = "sdtm/datasets",
  formats     = c("xpt", "rda"),
  xpt_version = 8L,
  target_meta = target_meta,
  domain_meta = domain_meta,
  keys        = c("STUDYID", "USUBJID", "MHTERM", "MHSTDTC"),
  drop_empty_perm = TRUE
)

cli::cli_alert_success("MH domain created: {nrow(mh2)} rows")
