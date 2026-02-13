# ******************************************************************************
# STUDY-PILOT
# ******************************************************************************
# PROGRAM NAME  : mh.R
# PURPOSE       : SDTM MH Domain - Medical History
# ------------------------------------------------------------------------------
# NOTES :
#   Raw datasets : mh_meddra
#   Dependencies : sdtmbuilder package
#   Reference    : SAS template
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2026-02-12 - sdtmbuilder - Auto-generated program
# ******************************************************************************

# Configuration ----
library(sdtmbuilder)
library(dplyr)

study       <- "STUDY-PILOT"
sdtm_domain <- "MH"

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

mh1 <- all_raw[["mh_meddra"]]
dm1 <- all_raw[["dm"]]
ic1 <- all_raw[["ic"]]

# Get MedDRA version ----
meddra_ver <- get_meddra_version(indata = mh1)

# Prepare DM for RFSTDTC ----
dm_slim <- dm1 %>%
  left_join(ic1 %>% select(subjectid, icdat), by = "subjectid") %>%
  mutate(
    subjid  = subjectid,
    rfstdtc = format_iso_dtc(parse_partial_date(icdat))
  ) %>%
  select(subjid, rfstdtc) %>%
  distinct()

# Filter: keep only records with non-missing MHTERM ----
mh1 <- mh1 %>%
  filter(!is.na(mhterm) & trimws(mhterm) != "")

# MH derivations ----
mh2 <- mh1 %>%
  left_join(dm_slim, by = c("subjectid" = "subjid")) %>%
  mutate(
    # --- Identifiers ---
    STUDYID  = study,
    DOMAIN   = sdtm_domain,
    SUBJID   = subjectid,
    USUBJID  = paste(study, subjectid, sep = "-"),
    MHSPID   = as.character(mhspid),
    SOURCEID = paste("CRF:", sdtm_domain),

    # --- Topic / Term ---
    MHTERM = trimws(mhterm),

    # --- Categories ---
    MHCAT = "RELEVANT MEDICAL HISTORY AND PRE-EXISTING CONDITIONS",

    # --- Coded Fields (MedDRA) ---
    MHLLT    = trimws(llt_name),
    MHLLTCD  = as.numeric(llt_code),
    MHDECOD  = trimws(pt_name),
    MHPTCD   = as.numeric(pt_code),
    MHHLT    = trimws(hlt_name),
    MHHLTCD  = as.numeric(hlt_code),
    MHHLGT   = trimws(hlgt_name),
    MHHLGTCD = as.numeric(hlgt_code),
    MHBODSYS = trimws(soc_name),
    MHBDSYCD = as.numeric(soc_code),
    MHSOC    = trimws(pt_soc_name),
    MHSOCCD  = as.numeric(pt_soc_code),
    MHSOCLST = trimws(soc_list),

    # --- Dictionary Version ---
    MHDICT = if_else(!is.na(MHDECOD),
                     paste("MedDRA", meddra_ver),
                     NA_character_),

    # --- Current medication / Medical intervention ---
    MHCURMED = mhcurmed,
    MHMEDINT = mhpr,

    # --- Dates ---
    MHSTDTC = format_iso_dtc(parse_partial_date(mhstdat)),
    MHENDTC = format_iso_dtc(parse_partial_date(mhendat)),

    # --- MHENRTPT + MHENTPT ---
    MHENRTPT = case_when(
      tolower(mhongo) == "y" ~ "ONGOING",
      tolower(mhongo) == "n" ~ "BEFORE",
      TRUE ~ NA_character_
    ),
    MHENTPT = if_else(!is.na(mhongo), "INFORMED CONSENT", NA_character_),

    # --- Audit variables ---
    MHCODED  = format_iso_dtc(parse_partial_date(codedondate)),
    MHINITD  = format_iso_dtc(parse_partial_date(initiateddate)),
    MHLTEDID = format_iso_dtc(parse_partial_date(lastediteddate))
  ) %>%

  # --- Study Day ---
  derive_dy("MHSTDY", "MHSTDTC", "rfstdtc") %>%
  derive_dy("MHENDY", "MHENDTC", "rfstdtc") %>%

  # --- Sequence ---
  derive_seq("MHSEQ", by = "USUBJID",
             order_by = c("MHDECOD", "MHTERM", "MHSTDTC", "MHENDTC"))

# Finalize ----
mh_final <- export_domain(
  data        = mh2,
  domain      = sdtm_domain,
  output_dir  = "../../sdtm/datasets",
  formats     = c("xpt", "rds", "csv"),
  xpt_version = 8L,
  target_meta = target_meta,
  domain_meta = domain_meta,
  keys        = c("STUDYID", "USUBJID", "MHDECOD", "MHTERM", "MHSTDTC", "MHENDTC"),
  drop_empty_perm = TRUE
)

cli::cli_alert_success("MH domain created: {nrow(mh2)} rows")
