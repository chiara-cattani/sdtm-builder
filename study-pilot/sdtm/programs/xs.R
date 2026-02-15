# ******************************************************************************
# STUDY-PILOT
# ******************************************************************************
# PROGRAM NAME  : XS.R
# PURPOSE       : SDTM XS Domain - Serious Adverse Events
# ------------------------------------------------------------------------------
# NOTES :
#   Raw datasets : sae
#   Dependencies : sdtmbuilder package
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2026-02-15 - sdtmbuilder - Auto-generated program
# ******************************************************************************

# Configuration ----
library(sdtmbuilder)
library(dplyr)

study       <- "STUDY-PILOT"
sdtm_domain <- "XS"

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

sae <- all_raw[["sae"]]
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
if ("subjectid" %in% names(sae) && "subjid" %in% names(dm_slim)) {
  sae <- sae %>%
    dplyr::left_join(dm_slim %>% dplyr::select(dplyr::any_of(c("subjid", "rfstdtc"))) %>% dplyr::rename(subjectid = subjid), by = "subjectid")
} else if ("usubjid" %in% names(sae) && "usubjid" %in% names(dm_slim)) {
  sae <- sae %>%
    dplyr::left_join(dm_slim %>% dplyr::select(dplyr::any_of(c("usubjid", "rfstdtc"))), by = "usubjid")
}
if ("rfstdtc" %in% names(sae) && !"RFSTDTC" %in% names(sae)) sae$RFSTDTC <- sae$rfstdtc

# XS derivations ----
xs2 <- sae %>%
  # --- Identifiers ---
  mutate(
    STUDYID = study,
    DOMAIN = "XS"
  ) %>%
  derive_usubjid(study, subjid_col = "subjectid") %>%
  # --- Sequence ---
  derive_seq("XSSEQ", by = c("USUBJID")) %>%
  # --- Identifiers ---
  mutate(XSSPID = as.character(saespid)) %>%
  # --- Topic / Term ---
  mutate(XSTERM = as.character(saeterm)) %>%
  # --- Derived Variables ---
  mutate(
    XSACN = as.character(saeacnp),
    XSSCONG = toupper(aescong),
    XSSDISAB = toupper(aesdisab),
    XSSDTH = toupper(aesdth),
    XSSHOSP = toupper(aeshosp),
    XSSMIE = toupper(aesmie)
  ) %>%
  # --- Dates ---
  mutate(
    XSSTDTC = format_iso_dtc(aesstdat),
    XSENDTC = format_iso_dtc(aesendat)
  ) %>%
  # --- Coded Fields ---
  mutate(
    XSENRTPT = NA_character_,  # TODO: case_when derivation
    XSENTPT = NA_character_  # TODO: case_when derivation
  ) %>%
  # --- Identifiers ---
  mutate(
    SOURCEID = paste("CRF:", sources$formname[match("SAE", toupper(sources$formid))]),
    SUBJID = as.character(subjectid)
  ) %>%
  # --- Dates ---
  mutate(XSADMDTC = format_iso_dtc(aesaddat)) %>%
  # --- Derived Variables ---
  mutate(
    XSAGE = as.numeric(aesage),
    XSAGEU = NA_character_,  # TODO: case_when derivation
    XSAUTOP = toupper(aesautop),
    XSBROKEN = toupper(aescb)
  ) %>%
  # --- Dates ---
  mutate(XSDISDTC = format_iso_dtc(aesdidat)) %>%
  # --- Derived Variables ---
  mutate(
    XSTRTDOS = as.character(aesdose),
    XSDTHCT = toupper(aesdthct)
  ) %>%
  # --- Dates ---
  mutate(XSDTHDTC = format_iso_dtc(aesdtdat)) %>%
  # --- Derived Variables ---
  mutate(
    XSDTHRSN = as.character(aesdtres),
    XSEABATE = toupper(aesabate),
    XSEREAPP = toupper(aesrecur)
  ) %>%
  # --- Dates ---
  mutate(XSFPIDTC = format_iso_dtc(aesfpi)) %>%
  # --- Derived Variables ---
  mutate(
    XSINDOSF = as.character(aesindof),
    XSINDOST = as.character(aesindot)
  ) %>%
  # --- Dates ---
  mutate(
    XSINDTC = format_iso_dtc(aesindat),
    XSIT1DTC = format_iso_dtc(aesitdat),
    XSIT2DTC = format_iso_dtc(aesitdat2),
    XSIT3DTC = format_iso_dtc(aesitdat3)
  ) %>%
  # --- Derived Variables ---
  mutate(
    XSRDDOSF = as.character(aesrdosf),
    XSRDDOST = as.character(aesrddot)
  ) %>%
  # --- Dates ---
  mutate(XSRDDTC = format_iso_dtc(aesrddat)) %>%
  # --- Derived Variables ---
  mutate(
    XSRIT1 = toupper(aerein),
    XSRIT2 = toupper(aerein2)
  ) %>%
  # --- Dates ---
  mutate(
    XSRS1DTC = format_iso_dtc(aesrsdat),
    XSRS2DTC = format_iso_dtc(aesrsdat2),
    XSRS3DTC = format_iso_dtc(aesrsdat3)
  ) %>%
  # --- Derived Variables ---
  mutate(
    XSSEX = toupper(aessex),
    XSSLIFE = toupper(aeslife)
  ) %>%
  # --- Study Day ---
  derive_dy("XSSTDY", "XSSTDTC", "RFSTDTC") %>%
  derive_dy("XSENDY", "XSENDTC", "RFSTDTC")

# Finalize ----
xs_final <- export_domain(
  data        = xs2,
  domain      = sdtm_domain,
  output_dir  = "sdtm/datasets",
  formats     = c("xpt", "rda"),
  xpt_version = 8L,
  target_meta = target_meta,
  domain_meta = domain_meta,
  keys        = c("STUDYID", "USUBJID", "XSTERM", "XSSTDTC"),
  drop_empty_perm = TRUE
)

cli::cli_alert_success("XS domain created: {nrow(xs2)} rows")
