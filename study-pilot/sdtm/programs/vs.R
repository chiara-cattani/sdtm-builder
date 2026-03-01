# ******************************************************************************
# STUDY-PILOT
# ******************************************************************************
# PROGRAM NAME  : VS.R
# PURPOSE       : SDTM VS Domain - Vital Signs
# ------------------------------------------------------------------------------
# NOTES :
#   Raw datasets : vs
#   Dependencies : sdtmbuilder package
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2026-03-01 - sdtmbuilder - Auto-generated program
# ******************************************************************************

# Configuration ----
library(sdtmbuilder)
library(dplyr)

study       <- "STUDY-PILOT"
sdtm_domain <- "VS"

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

vs <- all_raw[["vs"]]
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
if ("subjectid" %in% names(vs) && "subjid" %in% names(dm_slim)) {
  vs <- vs %>%
    dplyr::left_join(dm_slim %>% dplyr::select(dplyr::any_of(c("subjid", "rfstdtc"))) %>% dplyr::rename(subjectid = subjid), by = "subjectid")
} else if ("usubjid" %in% names(vs) && "usubjid" %in% names(dm_slim)) {
  vs <- vs %>%
    dplyr::left_join(dm_slim %>% dplyr::select(dplyr::any_of(c("usubjid", "rfstdtc"))), by = "usubjid")
}
if ("rfstdtc" %in% names(vs) && !"RFSTDTC" %in% names(vs)) vs$RFSTDTC <- vs$rfstdtc

# VS derivations ----
vs2 <- vs %>%
  # --- Identifiers ---
  mutate(
    STUDYID = "STUDY-PILOT",
    DOMAIN = "VS"
  ) %>%
  derive_usubjid(study, subjid_col = "subjectid") %>%
  # --- Sequence ---
  derive_seq("VSSEQ", by = c("USUBJID")) %>%
  # --- Derived Variables ---
  mutate(
    VSTESTCD = as.character(vstestcd_derived),
    VSTEST = as.character(vstest_derived)
  ) %>%
  # --- Categories ---
  mutate(VSCAT = as.character(vscat_derived)) %>%
  # --- Derived Variables ---
  mutate(
    VSPOS = as.character(vspos_derived),
    VSORRESU = as.character(vsorresu_derived),
    VSSTRESN = signif(as.numeric(vsorres), 3),
    VSSTRESU = as.character(vsorresu_derived),
    VSSTAT = as.character(vsstat_derived),
    VSREASND = as.character(vsreasnd_derived),
    VSLOBXFL = as.character(vslobxfl),
    VSEVAL = as.character(vseval_derived),
    VSEVALID = as.character(vsevalid_derived)
  ) %>%
  # --- Visit Variables ---
  mutate(
    VISITNUM = as.numeric(visitnum_derived),
    VISIT = as.character(visit_derived),
    VISITDY = as.numeric(visitdy_derived)
  ) %>%
  # --- Derived Variables ---
  mutate(
    VSDTC = as.character(vsdtc_derived),
    VSEVINTX = as.character(vsevintx)
  ) %>%
  # --- Identifiers ---
  mutate(
    SOURCEID = paste("CRF:", sources$formname[match("VS", toupper(sources$formid))]),
    SUBJID = as.character(subjectid)
  ) %>%
  # --- Derived Variables ---
  mutate(
    VSSTRESC = as.character(vsorres_derived),
    VSORRES = as.numeric()
  ) %>%
  # --- Study Day ---
  derive_dy("VSDY", "VSDTC", "RFSTDTC")

# Finalize ----
vs_final <- export_domain(
  data        = vs2,
  domain      = sdtm_domain,
  output_dir  = "sdtm/datasets",
  formats     = c("xpt", "rda"),
  xpt_version = 8L,
  target_meta = target_meta,
  domain_meta = domain_meta,
  keys        = c("STUDYID", "USUBJID", "VSTESTCD", "VISITNUM", "VSEVINTX"),
  drop_empty_perm = TRUE
)

cli::cli_alert_success("VS domain created: {nrow(vs2)} rows")
