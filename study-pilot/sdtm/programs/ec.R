# ******************************************************************************
# STUDY-PILOT
# ******************************************************************************
# PROGRAM NAME  : EC.R
# PURPOSE       : SDTM EC Domain - Exposure as Collected
# ------------------------------------------------------------------------------
# NOTES :
#   Raw datasets : ec
#   Dependencies : sdtmbuilder package
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2026-02-15 - sdtmbuilder - Auto-generated program
# ******************************************************************************

# Configuration ----
library(sdtmbuilder)
library(dplyr)

study       <- "STUDY-PILOT"
sdtm_domain <- "EC"

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

ec <- all_raw[["ec"]]
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
if ("subjectid" %in% names(ec) && "subjid" %in% names(dm_slim)) {
  ec <- ec %>%
    dplyr::left_join(dm_slim %>% dplyr::select(dplyr::any_of(c("subjid", "rfstdtc"))) %>% dplyr::rename(subjectid = subjid), by = "subjectid")
} else if ("usubjid" %in% names(ec) && "usubjid" %in% names(dm_slim)) {
  ec <- ec %>%
    dplyr::left_join(dm_slim %>% dplyr::select(dplyr::any_of(c("usubjid", "rfstdtc"))), by = "usubjid")
}
if ("rfstdtc" %in% names(ec) && !"RFSTDTC" %in% names(ec)) ec$RFSTDTC <- ec$rfstdtc

# EC derivations ----
ec2 <- ec %>%
  # --- Identifiers ---
  mutate(
    STUDYID = "STUDY-PILOT",
    DOMAIN = "EC"
  ) %>%
  derive_usubjid(study, subjid_col = "subjectid") %>%
  mutate(ECSPID = as.character(ecspid_derived)) %>%
  # --- Topic / Term ---
  mutate(ECTRT = as.character(ectrt_derived)) %>%
  # --- Categories ---
  mutate(ECCAT = as.character(eccat_derived)) %>%
  # --- Derived Variables ---
  mutate(
    ECPRESP = as.character(ecpresp_derived),
    ECOCCUR = as.character(ecoccur_derived)
  ) %>%
  # --- Coded Fields ---
  mutate(ECREASOC = as.character(ecreasoc_derived)) %>%
  # --- Derived Variables ---
  mutate(
    ECSTAT = as.character(ecstat_derived),
    ECREASND = as.character(ecreasnd_derived),
    ECDOSE = as.numeric(ecdose_derived),
    ECDOSU = as.character(ecdosu_derived),
    ECDOSFRM = as.character(ecdosfrm_derived),
    ECROUTE = as.character(ecroute_derived),
    ECSTDTC = as.character(ecstdtc_derived),
    ECENDTC = as.character(ecendtc_derived)
  ) %>%
  # --- Coded Fields ---
  mutate(ECTPT = as.character(ectpt)) %>%
  # --- Time Points ---
  mutate(
    ECTPTNUM = as.numeric(ectptnum),
    ECELTM = as.character(eceltm),
    ECTPTREF = as.character(ectptref)
  ) %>%
  # --- Derived Variables ---
  mutate(
    ECEVINTX = as.character(ecevintx_derived),
    ECENREAS = as.character(ecenreas),
    ECEVAL = as.character(eceval_derived),
    ECORIG = as.character(ecorig_derived),
    ECVPREP = as.numeric(ecvprep_derived),
    ECVPREPU = as.character(ecvprepu_derived),
    ECVLFT = as.numeric(ecvlft_derived),
    ECVLFTU = as.character(ecvlftu_derived)
  ) %>%
  # --- Identifiers ---
  mutate(
    SOURCEID = paste("CRF:", sources$formname[match("EC", toupper(sources$formid))]),
    SUBJID = as.character(subjectid)
  ) %>%
  # --- Sequence ---
  derive_seq("ECSEQ", by = c("USUBJID")) %>%
  # --- Study Day ---
  derive_dy("ECSTDY", "ECSTDTC", "RFSTDTC") %>%
  derive_dy("ECENDY", "ECENDTC", "RFSTDTC")

# Finalize ----
ec_final <- export_domain(
  data        = ec2,
  domain      = sdtm_domain,
  output_dir  = "sdtm/datasets",
  formats     = c("xpt", "rda"),
  xpt_version = 8L,
  target_meta = target_meta,
  domain_meta = domain_meta,
  keys        = c("STUDYID", "USUBJID", "ECTRT", "ECSTDTC", "ECSPID"),
  drop_empty_perm = TRUE
)

cli::cli_alert_success("EC domain created: {nrow(ec2)} rows")
