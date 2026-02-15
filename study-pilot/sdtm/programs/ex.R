# ******************************************************************************
# STUDY-PILOT
# ******************************************************************************
# PROGRAM NAME  : EX.R
# PURPOSE       : SDTM EX Domain - Exposure
# ------------------------------------------------------------------------------
# NOTES :
#   Raw datasets : ex
#   Dependencies : sdtmbuilder package
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2026-02-15 - sdtmbuilder - Auto-generated program
# ******************************************************************************

# Configuration ----
library(sdtmbuilder)
library(dplyr)

study       <- "STUDY-PILOT"
sdtm_domain <- "EX"

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

ex <- all_raw[["ex"]]
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
if ("subjectid" %in% names(ex) && "subjid" %in% names(dm_slim)) {
  ex <- ex %>%
    dplyr::left_join(dm_slim %>% dplyr::select(dplyr::any_of(c("subjid", "rfstdtc"))) %>% dplyr::rename(subjectid = subjid), by = "subjectid")
} else if ("usubjid" %in% names(ex) && "usubjid" %in% names(dm_slim)) {
  ex <- ex %>%
    dplyr::left_join(dm_slim %>% dplyr::select(dplyr::any_of(c("usubjid", "rfstdtc"))), by = "usubjid")
}
if ("rfstdtc" %in% names(ex) && !"RFSTDTC" %in% names(ex)) ex$RFSTDTC <- ex$rfstdtc

# EX derivations ----
ex2 <- ex %>%
  # --- Identifiers ---
  mutate(
    STUDYID = "STUDY-PILOT",
    DOMAIN = "EX"
  ) %>%
  derive_usubjid(study, subjid_col = "subjectid") %>%
  # --- Topic / Term ---
  mutate(EXTRT = "STUDY PRODUCT") %>%
  # --- Categories ---
  mutate(EXCAT = "STUDY PRODUCT") %>%
  # --- Derived Variables ---
  mutate(
    EXDOSE = as.numeric(exdose_derived),
    EXDOSU = as.character(exdosu_derived),
    EXDOSFRM = "POWDER, FOR SUSPENSION",
    EXDOSFRQ = "QD",
    EXROUTE = "ORAL",
    EXSTDTC = as.character(exstdtc_derived),
    EXENDTC = as.character(exendtc_derived)
  ) %>%
  # --- Coded Fields ---
  mutate(EXTPT = as.character(extpt)) %>%
  # --- Time Points ---
  mutate(
    EXTPTNUM = as.numeric(extptnum),
    EXELTM = as.character(exeltm),
    EXTPTREF = as.character(extptref)
  ) %>%
  # --- Derived Variables ---
  mutate(
    EXEVAL = as.character(exeval_derived),
    EXORIG = as.character(exorig_derived)
  ) %>%
  # --- Identifiers ---
  mutate(
    SOURCEID = paste("CRF:", sources$formname[match("EX", toupper(sources$formid))]),
    SUBJID = as.character(subjectid)
  ) %>%
  # --- Sequence ---
  derive_seq("EXSEQ", by = c("USUBJID")) %>%
  # --- Study Day ---
  derive_dy("EXSTDY", "EXSTDTC", "RFSTDTC") %>%
  derive_dy("EXENDY", "EXENDTC", "RFSTDTC")

# Finalize ----
ex_final <- export_domain(
  data        = ex2,
  domain      = sdtm_domain,
  output_dir  = "sdtm/datasets",
  formats     = c("xpt", "rda"),
  xpt_version = 8L,
  target_meta = target_meta,
  domain_meta = domain_meta,
  keys        = c("STUDYID", "USUBJID", "EXTRT", "EXSTDTC", "EXDOSU"),
  drop_empty_perm = TRUE
)

cli::cli_alert_success("EX domain created: {nrow(ex2)} rows")
