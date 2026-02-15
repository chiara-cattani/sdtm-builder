# ******************************************************************************
# STUDY-PILOT
# ******************************************************************************
# PROGRAM NAME  : QS.R
# PURPOSE       : SDTM QS Domain - Questionnaires
# ------------------------------------------------------------------------------
# NOTES :
#   Raw datasets : qs
#   Dependencies : sdtmbuilder package
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2026-02-15 - sdtmbuilder - Auto-generated program
# ******************************************************************************

# Configuration ----
library(sdtmbuilder)
library(dplyr)

study       <- "STUDY-PILOT"
sdtm_domain <- "QS"

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

qs <- all_raw[["qs"]]
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
if ("subjectid" %in% names(qs) && "subjid" %in% names(dm_slim)) {
  qs <- qs %>%
    dplyr::left_join(dm_slim %>% dplyr::select(dplyr::any_of(c("subjid", "rfstdtc"))) %>% dplyr::rename(subjectid = subjid), by = "subjectid")
} else if ("usubjid" %in% names(qs) && "usubjid" %in% names(dm_slim)) {
  qs <- qs %>%
    dplyr::left_join(dm_slim %>% dplyr::select(dplyr::any_of(c("usubjid", "rfstdtc"))), by = "usubjid")
}
if ("rfstdtc" %in% names(qs) && !"RFSTDTC" %in% names(qs)) qs$RFSTDTC <- qs$rfstdtc

# QS derivations ----
qs2 <- qs %>%
  # --- Identifiers ---
  mutate(
    STUDYID = "STUDY-PILOT",
    DOMAIN = "QS"
  ) %>%
  derive_usubjid(study, subjid_col = "subjectid") %>%
  # --- Sequence ---
  derive_seq("QSSEQ", by = c("USUBJID")) %>%
  # --- Categories ---
  mutate(
    QSCAT = as.character(qscat_derived),
    QSSCAT = as.character(qsscat_derived)
  ) %>%
  # --- Derived Variables ---
  mutate(
    QSSTRESN = signif(as.numeric(qsorres), 3),
    QSSTAT = as.character(qsstat),
    QSREASND = as.character(qsreasnd),
    QSLOBXFL = as.character(qslobxfl),
    QSEVAL = as.character(qseval_derived)
  ) %>%
  # --- Visit Variables ---
  mutate(
    VISITNUM = as.numeric(visitnum_derived),
    VISIT = as.character(visit_derived),
    VISITDY = as.numeric(visitdy_derived)
  ) %>%
  # --- Derived Variables ---
  mutate(
    QSDTC = as.character(qsdtc_derived),
    QSORIG = as.character(qsorig_derived),
    QSOTHX = as.character(qsothx)
  ) %>%
  # --- Identifiers ---
  mutate(
    SOURCEID = paste("CRF:", sources$formname[match("QS", toupper(sources$formid))]),
    SUBJID = as.character(subjectid)
  ) %>%
  # --- Derived Variables ---
  mutate(
    QSTESTCD = as.character(),
    QSTEST = as.character()
  ) %>%
  # --- Study Day ---
  derive_dy("QSDY", "QSDTC", "RFSTDTC") %>%
  # --- Derived Variables ---
  mutate(
    QSORRES = as.numeric(),
    QSSTRESC = as.numeric()
  )

# Finalize ----
qs_final <- export_domain(
  data        = qs2,
  domain      = sdtm_domain,
  output_dir  = "sdtm/datasets",
  formats     = c("xpt", "rda"),
  xpt_version = 8L,
  target_meta = target_meta,
  domain_meta = domain_meta,
  keys        = c("STUDYID", "USUBJID", "QSCAT", "QSTESTCD", "VISITNUM"),
  drop_empty_perm = TRUE
)

cli::cli_alert_success("QS domain created: {nrow(qs2)} rows")
