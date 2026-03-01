# ******************************************************************************
# STUDY-PILOT
# ******************************************************************************
# PROGRAM NAME  : AE.R
# PURPOSE       : SDTM AE Domain - Adverse Events
# ------------------------------------------------------------------------------
# NOTES :
#   Raw datasets : ae, ae_meddra, sae
#   Dependencies : sdtmbuilder package
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2026-03-01 - sdtmbuilder - Auto-generated program
# ******************************************************************************

# Configuration ----
library(sdtmbuilder)
library(dplyr)

study       <- "STUDY-PILOT"
sdtm_domain <- "AE"

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

ae <- all_raw[["ae"]]
ae_meddra <- all_raw[["ae_meddra"]]
sae <- all_raw[["sae"]]
dm1 <- all_raw[["dm"]]

# Sources lookup (for SOURCEID) ----
review_status <- all_raw[["review_status"]]
sources <- review_status %>%
  dplyr::filter(trimws(formname) != "") %>%
  dplyr::distinct(formid, formname)

# Merge secondary source datasets ----
ae_meddra <- dplyr::rename(ae_meddra, dplyr::any_of(c(aespid = "ae_meddraspid")))
ae_meddra_new <- setdiff(names(ae_meddra), names(ae))
ae_meddra_slim <- ae_meddra[, c("subjectid", "aespid", ae_meddra_new), drop = FALSE]
ae <- ae %>%
  dplyr::left_join(ae_meddra_slim, by = c("subjectid", "aespid"))
sae <- dplyr::rename(sae, dplyr::any_of(c(aespid = "saespid")))
sae_new <- setdiff(names(sae), names(ae))
sae_slim <- sae[, c("subjectid", "aespid", sae_new), drop = FALSE]
ae <- ae %>%
  dplyr::left_join(sae_slim, by = c("subjectid", "aespid"))

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
if ("subjectid" %in% names(ae) && "subjid" %in% names(dm_slim)) {
  ae <- ae %>%
    dplyr::left_join(dm_slim %>% dplyr::select(dplyr::any_of(c("subjid", "rfstdtc"))) %>% dplyr::rename(subjectid = subjid), by = "subjectid")
} else if ("usubjid" %in% names(ae) && "usubjid" %in% names(dm_slim)) {
  ae <- ae %>%
    dplyr::left_join(dm_slim %>% dplyr::select(dplyr::any_of(c("usubjid", "rfstdtc"))), by = "usubjid")
}
if ("rfstdtc" %in% names(ae) && !"RFSTDTC" %in% names(ae)) ae$RFSTDTC <- ae$rfstdtc

# AE derivations ----
ae2 <- ae %>%
  # --- Identifiers ---
  mutate(
    STUDYID = study,
    DOMAIN = "AE",
    AESPID = as.character(aespid)
  ) %>%
  # --- Topic / Term ---
  mutate(AETERM = as.character(aeterm)) %>%
  # --- Coded Fields ---
  mutate(
    AELLT = as.character(llt_name),
    AELLTCD = as.numeric(llt_code),
    AEDECOD = as.character(pt_name),
    AEPTCD = as.numeric(pt_code),
    AEHLT = as.character(hlt_name),
    AEHLTCD = as.numeric(hlt_code),
    AEHLGT = as.character(hlgt_name),
    AEHLGTCD = as.numeric(hlgt_code),
    AEBODSYS = as.character(soc_name),
    AEBDSYCD = as.numeric(soc_code),
    AESOC = as.character(pt_soc_name),
    AESOCCD = as.numeric(pt_soc_code)
  ) %>%
  # --- Derived Variables ---
  mutate(AESEV = as.character(aesev)) %>%
  mutate(AESER = as.character(aeser)) %>%
  mutate(
    AEACN = as.character(aeacnp),
    AEACNOTH = purrr::pmap_chr(list(aeacns0, aeacns1, aeacns2), ~ paste(na.omit(c(...)), collapse = "; ")),
    AEREL = as.character(aerel1),
    AEOUT = as.character(aeout),
    AESCONG = as.character(aescong),
    AESDISAB = as.character(aesdisab),
    AESDTH = as.character(aesdth),
    AESHOSP = as.character(aeshosp),
    AESLIFE = as.character(aeslife),
    AESMIE = as.character(aesmie)
  ) %>%
  # --- Dates ---
  mutate(
    AESTDTC = format_iso_dtc(aestdat),
    AEENDTC = format_iso_dtc(aeendat)
  ) %>%
  # --- Coded Fields ---
  mutate(
    AESTRTPT = NA_character_,  # TODO: case_when derivation
    AESTTPT = dplyr::if_else(!is.na(aestper), "FIRST PRODUCT INTAKE", NA_character_),
    AEENRTPT = NA_character_,  # TODO: case_when derivation
    AEENTPT = dplyr::if_else(!is.na(aeongo), "END OF STUDY", NA_character_)
  ) %>%
  # --- Derived Variables ---
  mutate(
    AEACNEVL = NA,
    AEACNOTX = as.character(aeacnssp),
    AEACNX = as.character(aeacnpsp)
  ) %>%
  # --- Dates ---
  mutate(AECODED = format_iso_dtc(codedondate)) %>%
  # --- Derived Variables ---
  mutate(AEDESC = as.character(aecoval)) %>%
  # --- Dates ---
  mutate(
    AEINITD = format_iso_dtc(initiateddate),
    AELTEDID = format_iso_dtc(lastediteddate)
  ) %>%
  # --- Derived Variables ---
  mutate(
    AEOUTX = as.character(aesequel1),
    AERELP = as.character(aerel2),
    AERELPX = as.character(aerel2co),
    AERELX = as.character(aerel1co)
  ) %>%
  # --- Identifiers ---
  mutate(
    SOURCEID = paste("CRF:", sources$formname[match("AE", toupper(sources$formid))]),
    SUBJID = as.character(subjectid)
  ) %>%
  # --- Derived Variables ---
  mutate(AEFUNEED = as.character(aefu)) %>%
  # --- Coded Fields ---
  mutate(AESOCLST = as.character(soc_list)) %>%
  # --- Identifiers ---
  derive_usubjid(study, subjid_col = "subjectid") %>%
  # --- Dictionary Version ---
  mutate(AEDICT = dplyr::if_else(!is.na(AEDECOD), paste("MedDRA", get_meddra_version(ae_meddra, dictvar = "DictInstance")), "")) %>%
  # --- Study Day ---
  derive_dy("AESTDY", "AESTDTC", "RFSTDTC") %>%
  derive_dy("AEENDY", "AEENDTC", "RFSTDTC") %>%
  # --- Sequence ---
  derive_seq("AESEQ", by = c("USUBJID"))

# Finalize ----
ae_final <- export_domain(
  data        = ae2,
  domain      = sdtm_domain,
  output_dir  = "sdtm/datasets",
  formats     = c("xpt", "rda"),
  xpt_version = 8L,
  target_meta = target_meta,
  domain_meta = domain_meta,
  keys        = c("STUDYID", "USUBJID", "AETERM", "AESTDTC"),
  drop_empty_perm = TRUE
)

cli::cli_alert_success("AE domain created: {nrow(ae2)} rows")
