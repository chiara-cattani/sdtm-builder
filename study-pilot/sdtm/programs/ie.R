# ******************************************************************************
# STUDY-PILOT
# ******************************************************************************
# PROGRAM NAME  : IE.R
# PURPOSE       : SDTM IE Domain - Inclusion/Exclusion Criteria Not Met
# ------------------------------------------------------------------------------
# NOTES :
#   Raw datasets : ie
#   Dependencies : sdtmbuilder package
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2026-03-01 - sdtmbuilder - Auto-generated program
# ******************************************************************************

# Configuration ----
library(sdtmbuilder)
library(dplyr)

study       <- "STUDY-PILOT"
sdtm_domain <- "IE"

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

ie <- all_raw[["ie"]]
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
if ("subjectid" %in% names(ie) && "subjid" %in% names(dm_slim)) {
  ie <- ie %>%
    dplyr::left_join(dm_slim %>% dplyr::select(dplyr::any_of(c("subjid", "rfstdtc"))) %>% dplyr::rename(subjectid = subjid), by = "subjectid")
} else if ("usubjid" %in% names(ie) && "usubjid" %in% names(dm_slim)) {
  ie <- ie %>%
    dplyr::left_join(dm_slim %>% dplyr::select(dplyr::any_of(c("usubjid", "rfstdtc"))), by = "usubjid")
}
if ("rfstdtc" %in% names(ie) && !"RFSTDTC" %in% names(ie)) ie$RFSTDTC <- ie$rfstdtc

# IE derivations ----
ie2 <- ie %>%
  # --- Identifiers ---
  mutate(
    STUDYID = study,
    DOMAIN = "IE"
  ) %>%
  derive_usubjid(study, subjid_col = "subjectid") %>%
  # --- Sequence ---
  derive_seq("IESEQ", by = c("USUBJID")) %>%
  # --- Derived Variables ---
  mutate(
    IETESTCD = as.character(ietestcd_derived),
    IETEST = as.character(ietest_derived)
  ) %>%
  # --- Categories ---
  mutate(IECAT = as.character(iecat_derived)) %>%
  # --- Derived Variables ---
  mutate(
    IEORRES = as.character(ieorres_derived),
    IESTRESC = as.character(iestresc_derived)
  ) %>%
  # --- Visit Variables ---
  mutate(
    VISITNUM = as.numeric(visitnum_derived),
    VISIT = as.character(visit_derived)
  ) %>%
  # --- Derived Variables ---
  mutate(IEDTC = as.character(iedtc_derived)) %>%
  # --- Identifiers ---
  mutate(
    SOURCEID = as.character(sourceid_derived),
    SUBJID = as.character(subjectid)
  ) %>%
  # --- Study Day ---
  derive_dy("IEDY", "IEDTC", "RFSTDTC")

# Finalize ----
ie_final <- export_domain(
  data        = ie2,
  domain      = sdtm_domain,
  output_dir  = "sdtm/datasets",
  formats     = c("xpt", "rda"),
  xpt_version = 8L,
  target_meta = target_meta,
  domain_meta = domain_meta,
  keys        = c("STUDYID", "USUBJID", "IETESTCD"),
  drop_empty_perm = TRUE
)

cli::cli_alert_success("IE domain created: {nrow(ie2)} rows")
