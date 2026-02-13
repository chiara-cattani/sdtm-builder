# ******************************************************************************
# STUDY-PILOT
# ******************************************************************************
# PROGRAM NAME  : cm.R
# PURPOSE       : SDTM CM Domain - Concomitant Medications
# ------------------------------------------------------------------------------
# NOTES :
#   Raw datasets : cm_whodrug
#   Dependencies : sdtmbuilder package
#   Reference    : SAS template
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2026-02-12 - sdtmbuilder - Auto-generated program
# ******************************************************************************

# Configuration ----
library(sdtmbuilder)
library(dplyr)
library(stringr)

study       <- "STUDY-PILOT"
sdtm_domain <- "CM"

# Set working directory ----
if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
} else if (exists("progdir")) {
  setwd(progdir)
}

# Import metadata ----
study_meta <- read_study_metadata_excel("../../metadata/Study_Metadata.xlsx")
ct_spec    <- read_study_ct_excel("../../metadata/Study_CT.xlsx")

target_meta      <- study_meta$target_meta
domain_meta      <- study_meta$domain_meta
value_level_meta <- study_meta$value_level_meta

# Import data ----
all_raw <- load_raw_datasets("../../raw")
for (nm in names(all_raw)) {
  all_raw[[nm]] <- all_raw[[nm]] %>% standardize_names() %>% convert_blanks_to_na()
}

cm1 <- all_raw[["cm_whodrug"]]
dm1 <- all_raw[["dm"]]
ic1 <- all_raw[["ic"]]

# Get WHODrug version ----
whodrug_ver <- get_whodrug_version(indata = cm1)

# Prepare DM for RFSTDTC ----
dm_slim <- dm1 %>%
  left_join(ic1 %>% select(subjectid, icdat), by = "subjectid") %>%
  mutate(
    subjid  = subjectid,
    rfstdtc = format_iso_dtc(parse_partial_date(icdat))
  ) %>%
  select(subjid, rfstdtc) %>%
  distinct()

# Filter: keep only records with non-missing CMTRT ----
cm1 <- cm1 %>%
  filter(!is.na(cmtrt) & trimws(cmtrt) != "")

# CM derivations ----
cm2 <- cm1 %>%
  left_join(dm_slim, by = c("subjectid" = "subjid")) %>%
  mutate(
    # --- Identifiers ---
    STUDYID  = study,
    DOMAIN   = sdtm_domain,
    SUBJID   = subjectid,
    USUBJID  = paste(study, subjectid, sep = "-"),
    CMSPID   = as.character(cmspid),
    SOURCEID = paste("CRF:", sdtm_domain),

    # --- Topic / Term ---
    CMTRT   = cmtrt,
    CMDECOD = cmdecod,

    # --- Categories ---
    CMCAT = "CONCOMITANT MEDICATIONS AND NUTRITIONAL SUPPLEMENTS",

    # --- Indication ---
    CMINDC  = indicat,
    CMINDCX = cmindsp,

    # --- Drug class (WHODrug ATC) ---
    CMCLAS   = cmclas,
    CMCLASCD = cmclascd,
    CMATC1   = cmatc1,
    CMATC2   = cmatc2,
    CMATC3   = cmatc3,
    CMATC4   = cmatc4,

    # --- Dose ---
    # If cmdostxt is purely numeric, derive CMDOSE; otherwise keep CMDOSTXT
    CMDOSE   = if_else(
      str_detect(cmdostxt, "^[0-9,\\.]+$"),
      as.numeric(str_replace(cmdostxt, ",", ".")),
      NA_real_
    ),
    CMDOSTXT = if_else(
      !str_detect(cmdostxt, "^[0-9,\\.]+$"),
      cmdostxt,
      NA_character_
    ),
    CMDOSU   = cmdosu,
    CMDOSFRQ = cmfrq,
    CMDSFRQX = cmfrqsp,
    CMROUTE  = cmroute,

    # --- Dictionary Version ---
    CMDICT = if_else(!is.na(CMDECOD) & CMDECOD != "",
                     paste("WHODrug", whodrug_ver),
                     NA_character_),

    # --- Dates ---
    CMSTDTC = format_iso_dtc(parse_partial_date(cmstdat)),
    CMENDTC = format_iso_dtc(parse_partial_date(cmendat)),

    # --- CMENRTPT + CMENTPT ---
    CMENRTPT = case_when(
      cmongo == "Y" ~ "ONGOING",
      cmongo == "N" ~ "BEFORE",
      TRUE ~ NA_character_
    ),
    CMENTPT = if_else(!is.na(cmongo) & cmongo != "",
                      "END OF STUDY", NA_character_),

    # --- Audit variables ---
    CMCODED  = format_iso_dtc(parse_partial_date(codedondate)),
    CMINITD  = format_iso_dtc(parse_partial_date(initiateddate)),
    CMLTEDID = format_iso_dtc(parse_partial_date(lastediteddate))
  ) %>%

  # --- Study Day ---
  derive_dy("CMSTDY", "CMSTDTC", "rfstdtc") %>%
  derive_dy("CMENDY", "CMENDTC", "rfstdtc") %>%

  # --- Sequence ---
  derive_seq("CMSEQ", by = "USUBJID",
             order_by = c("CMTRT", "CMSTDTC", "CMENDTC"))

# Finalize ----
cm_final <- export_domain(
  data        = cm2,
  domain      = sdtm_domain,
  output_dir  = "../../sdtm/datasets",
  formats     = c("xpt", "rds", "csv"),
  xpt_version = 8L,
  target_meta = target_meta,
  domain_meta = domain_meta,
  keys        = c("STUDYID", "USUBJID", "CMTRT", "CMSTDTC", "CMENDTC"),
  drop_empty_perm = TRUE
)

cli::cli_alert_success("CM domain created: {nrow(cm2)} rows")
