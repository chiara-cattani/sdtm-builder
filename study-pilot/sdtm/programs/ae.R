# ******************************************************************************
# STUDY-PILOT
# ******************************************************************************
# PROGRAM NAME  : ae.R
# PURPOSE       : SDTM AE Domain - Adverse Events
# ------------------------------------------------------------------------------
# NOTES :
#   Raw datasets : ae, sae, ae_meddra
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
sdtm_domain <- "AE"

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

ae1        <- all_raw[["ae"]]
sae1       <- all_raw[["sae"]]
ae_meddra1 <- all_raw[["ae_meddra"]]
dm1        <- all_raw[["dm"]]
ic1        <- all_raw[["ic"]]

# Get MedDRA version ----
meddra_ver <- get_meddra_version(indata = ae_meddra1)

# Prepare DM for RFSTDTC ----
# In this study RFSTDTC comes from icdat (informed consent date)
dm_slim <- dm1 %>%
  left_join(ic1 %>% select(subjectid, icdat), by = "subjectid") %>%
  mutate(
    subjid   = subjectid,
    rfstdtc  = format_iso_dtc(parse_partial_date(icdat))
  ) %>%
  select(subjid, rfstdtc) %>%
  distinct()

# Merge raw AE with MedDRA and SAE ----
ae_all <- ae_meddra1 %>%
  left_join(
    sae1 %>%
      select(subjectid, saespid, aesdth, aeslife, aeshosp, aesdisab, aescong, aesmie) %>%
      mutate(saespid = as.numeric(saespid)),
    by = c("subjectid", "aespid" = "saespid")
  )

# AE derivations ----
ae2 <- ae_all %>%
  left_join(dm_slim, by = c("subjectid" = "subjid")) %>%
  mutate(
    # --- Identifiers ---
    STUDYID  = study,
    DOMAIN   = sdtm_domain,
    SUBJID   = subjectid,
    USUBJID  = paste(study, subjectid, sep = "-"),
    AESPID   = as.character(aespid),
    SOURCEID = paste("CRF:", sdtm_domain),

    # --- Topic / Term ---
    AETERM   = ae1$aeterm[match(paste(subjectid, aespid),
                                paste(ae1$subjectid, ae1$aespid))],

    # --- Coded Fields (MedDRA) ---
    AELLT    = llt_name,
    AELLTCD  = as.numeric(llt_code),
    AEDECOD  = pt_name,
    AEPTCD   = as.numeric(pt_code),
    AEHLT    = hlt_name,
    AEHLTCD  = as.numeric(hlt_code),
    AEHLGT   = hlgt_name,
    AEHLGTCD = as.numeric(hlgt_code),
    AEBODSYS = soc_name,
    AEBDSYCD = as.numeric(soc_code),
    AESOC    = pt_soc_name,
    AESOCCD  = as.numeric(pt_soc_code),
    AESOCLST = soc_list,

    # --- Dictionary Version ---
    AEDICT = if_else(!is.na(AEDECOD),
                     paste("MedDRA", meddra_ver),
                     NA_character_)
  ) %>%

  # --- Severity, Actions, Relationships ---
  # (fields from ae1 joined via subjectid + aespid)
  left_join(
    ae1 %>% select(subjectid, aespid, aeterm, aesev, aeout,
                   aeacnp, aeacnpsp, aeacns0, aeacns1, aeacns2, aeacnssp,
                   aerel1, aerel1co, aerel2, aerel2co,
                   aesequel1, aestper, aeongo, aefu, aecoval,
                   aestdat, aeendat,
                   codedondate, initiateddate, lastediteddate),
    by = c("subjectid", "aespid"),
    suffix = c("", ".ae")
  ) %>%
  mutate(
    # --- Severity ---
    AESEV = aesev,

    # --- Action Taken ---
    AEACN    = str_replace_all(aeacnp, "^NA$", "NOT APPLICABLE"),
    AEACNX   = aeacnpsp,
    AEACNOTH = if_else(
      !is.na(aeacns0),
      aeacns0,
      paste(na.omit(c(aeacns1, aeacns2)), collapse = "; ")
    ),
    AEACNOTX = aeacnssp,

    # --- Relationships ---
    AEREL = {
      r <- str_replace_all(aerel1, "^NA$", "NOT APPLICABLE")
      if_else(
        !r %in% c(NA_character_, " ", "NOT RELATED", "NOT APPLICABLE"),
        paste0(r, " RELATED"),
        r
      )
    },
    AERELX  = aerel1co,
    AERELP  = str_replace_all(aerel2, "^NA$", "NOT APPLICABLE"),
    AERELPX = aerel2co,

    # --- Outcome ---
    AEOUT  = str_replace_all(aeout, " / ", "/"),
    AEOUTX = aesequel1,

    # --- Description ---
    AEDESC   = aecoval,
    AEFUNEED = aefu,

    # --- Seriousness (from SAE) ---
    AESDTH   = aesdth,
    AESLIFE  = aeslife,
    AESHOSP  = aeshosp,
    AESDISAB = aesdisab,
    AESCONG  = aescong,
    AESMIE   = aesmie,

    # --- Dates ---
    AESTDTC = format_iso_dtc(parse_partial_date(aestdat)),
    AEENDTC = format_iso_dtc(parse_partial_date(aeendat)),

    # --- AESTRTPT + AESTTPT ---
    AESTRTPT = case_when(
      str_detect(toupper(aestper), "BEFORE") ~ "BEFORE",
      str_detect(toupper(aestper), "AFTER")  ~ "AFTER",
      TRUE ~ NA_character_
    ),
    AESTTPT = if_else(!is.na(aestper), "FIRST PRODUCT INTAKE", NA_character_),

    # --- AEENRTPT + AEENTPT ---
    AEENRTPT = case_when(
      aeongo == "Y" ~ "ONGOING",
      aeongo == "N" ~ "BEFORE",
      TRUE ~ NA_character_
    ),
    AEENTPT = if_else(!is.na(aeongo), "END OF STUDY", NA_character_),

    # --- Audit variables ---
    AECODED  = format_iso_dtc(parse_partial_date(codedondate)),
    AEINITD  = format_iso_dtc(parse_partial_date(initiateddate)),
    AELTEDID = format_iso_dtc(parse_partial_date(lastediteddate))
  ) %>%

  # --- Study Day ---
  derive_dy("AESTDY", "AESTDTC", "rfstdtc") %>%
  derive_dy("AEENDY", "AEENDTC", "rfstdtc") %>%

  # --- Sequence ---
  derive_seq("AESEQ", by = "USUBJID", order_by = c("AESTDTC", "AETERM"))

# Finalize ----
ae_final <- export_domain(
  data        = ae2,
  domain      = sdtm_domain,
  output_dir  = "../../sdtm/datasets",
  formats     = c("xpt", "rds", "csv"),
  xpt_version = 8L,
  target_meta = target_meta,
  domain_meta = domain_meta,
  keys        = c("STUDYID", "USUBJID", "AESEQ"),
  drop_empty_perm = TRUE
)

cli::cli_alert_success("AE domain created: {nrow(ae2)} rows")
