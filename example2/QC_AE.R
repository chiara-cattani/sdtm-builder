# ******************************************************************************
# Danone Nutricia Research
# ******************************************************************************
# STUDY/PROJECT : SONA / 23REX0061265
# PROGRAM PATH  : SAS/SONA/Files/SDTM Conversion/40 SDTM Programs
# PROGRAM NAME  : QC_AE.R 
# PURPOSE       : QC SDTM data for Adverse Events
# ------------------------------------------------------------------------------
# NOTES : 
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2025-10-03 - cattanch - Initial program
# 2025-11-14 - cattanch - Removed aeacnevl__2.
# 2025-12-08 - cattanch - Removed AESTPER.
#                         Use get_meddra_version for AEDICT.
#                         Add AEFUNEED, AESDTH, AESLIFE, AESHOSP, AESDISAB, AESCONG, AESMIE.
# ******************************************************************************

# Configuration ----
library(nutriciaconfig)
nutricia_config()

library(dplyr)
library(haven)
library(purrr)
library(readxl)
library(rlang)
library(sdtm.oak)
library(stringr)
library(tibble)
library(tidyr)
library(lubridate)

# Set working directory ----
if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  # Running in RStudio
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
} else {
  # Running in LSAF
  setwd(progdir)
}

# Set parameters
study       <- "SONA"
sdtm_domain <- "AE"

# Import metadata
sdtm_ct  <- read_excel("../31 Metadata Data/SONA_CT.xlsx", sheet = "CT")
sdtm_var <- read_sas("../31 Metadata Data/sdtm_3_4_variables.sas7bdat")
sdtm_dom <- read_sas("../31 Metadata Data/sdtm_3_4_domains.sas7bdat")

# Import data ----
dm1 <- read_sas("../41 SDTM Data/dm.sas7bdat")
ae1 <- copy_import(inset = "ae_meddra")
sae1 <- copy_import(inset = "sae")
rs1 <- copy_import(inset = "review_status")

# Sources ----
sources <- rs1 %>%
  filter(trimws(formname) != "") %>%
  distinct(formid, formname)

# AE ----
ae2 <-
  assign_no_ct(
    raw_dat = ae1,
    raw_var = "aespid",
    tgt_var = "AESPID",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ae1,
    raw_var = "aeterm",
    tgt_var = "AETERM",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = ae1,
    raw_var = "aeser",
    tgt_var = "AESER",
    ct_spec = sdtm_ct,
    ct_clst = "C66742",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ae1,
    raw_var = "aecoval",
    tgt_var = "AEDESC",
    id_vars = oak_id_vars()
  ) %>%
  assign_datetime(
    raw_dat = ae1,
    raw_var = "aestdat",
    tgt_var = "AESTDTC",
    raw_fmt = c("y-m-d"),
    raw_unk = c("NK", "UNK", "UN"),
    id_vars = oak_id_vars()
  ) %>%
  assign_datetime(
    raw_dat = ae1,
    raw_var = "aeendat",
    tgt_var = "AEENDTC",
    raw_fmt = c("y-m-d"),
    raw_unk = c("NK", "UNK", "UN"),
    id_vars = oak_id_vars()
  ) %>%
  hardcode_ct(
    raw_dat = condition_add(ae1, aeongo == "Y"),
    raw_var = "aeongo",
    tgt_var = "AEENRTPT",
    ct_spec = sdtm_ct,
    ct_clst = "C66728",
    tgt_val = "ONGOING",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_ct(
    raw_dat = condition_add(ae1, aeongo == "N"),
    raw_var = "aeongo",
    tgt_var = "AEENRTPT",
    ct_spec = sdtm_ct,
    ct_clst = "C66728",
    tgt_val = "BEFORE",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_no_ct(
    raw_dat = ae1,
    raw_var = "aeongo",
    tgt_var = "AEENTPT",
    tgt_val = "END OF STUDY",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_ct(
    raw_dat = condition_add(ae1, aestper == "1"),
    raw_var = "aestper",
    tgt_var = "AESTRTPT",
    ct_spec = sdtm_ct,
    ct_clst = "C66728",
    tgt_val = "BEFORE",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_ct(
    raw_dat = condition_add(ae1, aestper == "2"),
    raw_var = "aestper",
    tgt_var = "AESTRTPT",
    ct_spec = sdtm_ct,
    ct_clst = "C66728",
    tgt_val = "AFTER",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_no_ct(
    raw_dat = condition_add(ae1, !is.na(aestper)),
    raw_var = "aestper",
    tgt_var = "AESTTPT",
    tgt_val = "FIRST PRODUCT INTAKE",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = ae1,
    raw_var = "aesev",
    tgt_var = "AESEV",
    ct_spec = sdtm_ct,
    ct_clst = "C66769",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct_with_na(
    raw_dat = ae1,
    raw_var = "aerel1",
    tgt_var = "AEREL",
    ct_spec = sdtm_ct,
    ct_clst = "D0003",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ae1,
    raw_var = "aerel1co",
    tgt_var = "AERELX",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct_with_na(
    raw_dat = ae1,
    raw_var = "aerel2",
    tgt_var = "AERELP",
    ct_spec = sdtm_ct,
    ct_clst = "D0004",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ae1,
    raw_var = "aerel2co",
    tgt_var = "AERELPX",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct_with_na(
    raw_dat = ae1,
    raw_var = "aeacnp",
    tgt_var = "AEACN",
    ct_spec = sdtm_ct,
    ct_clst = "D0001",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = ae1,
    raw_var = "aeacnpsp",
    tgt_var = "AEACNX",
    ct_spec = sdtm_ct,
    ct_clst = "D0030",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = ae1,
    raw_var = "aeacnevl",
    tgt_var = "AEACNEVL",
    ct_spec = sdtm_ct,
    ct_clst = "C78735",
    id_vars = oak_id_vars()
  ) %>%
  mutate(
    AEACNOTH = pmap_chr(
      ae1[, c("aeacns0", "aeacns1", "aeacns2", "aeacns3", "aeacns4", "aeacnsot")],
      ~ paste(na.omit(c(...)), collapse = "; ")
    )
  ) %>%
  assign_no_ct(
    raw_dat = ae1,
    raw_var = "aeacnssp",
    tgt_var = "AEACNOTX",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = ae1,
    raw_var = "aeout",
    tgt_var = "AEOUT",
    ct_spec = sdtm_ct,
    ct_clst = "C66768",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ae1,
    raw_var = "aesequel1",
    tgt_var = "AEOUTX",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ae1,
    raw_var = "aefu",
    tgt_var = "AEFUNEED",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ae1,
    raw_var = "usubjid",
    tgt_var = "USUBJID",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ae1,
    raw_var = "subjectid",
    tgt_var = "SUBJID",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ae1,
    raw_var = "soc_code",
    tgt_var = "AEBDSYCD",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ae1,
    raw_var = "soc_name",
    tgt_var = "AEBODSYS",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ae1,
    raw_var = "pt_name",
    tgt_var = "AEDECOD",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ae1,
    raw_var = "hlgt_name",
    tgt_var = "AEHLGT",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ae1,
    raw_var = "hlgt_code",
    tgt_var = "AEHLGTCD",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ae1,
    raw_var = "hlt_name",
    tgt_var = "AEHLT",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ae1,
    raw_var = "hlt_code",
    tgt_var = "AEHLTCD",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ae1,
    raw_var = "llt_name",
    tgt_var = "AELLT",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ae1,
    raw_var = "llt_code",
    tgt_var = "AELLTCD",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ae1,
    raw_var = "pt_code",
    tgt_var = "AEPTCD",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ae1,
    raw_var = "pt_soc_name",
    tgt_var = "AESOC",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ae1,
    raw_var = "pt_soc_code",
    tgt_var = "AESOCCD",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ae1,
    raw_var = "soc_list",
    tgt_var = "AESOCLST",
    id_vars = oak_id_vars()
  ) %>%
  mutate(
    STUDYID  = study,
    DOMAIN   = sdtm_domain,
    SOURCEID = paste("CRF:", sources$formname[match(sdtm_domain, sources$formid)]),
    AEDICT   = if_else(!is.na(AEDECOD), paste("MedDRA", get_meddra_version(indata = ae1)), NA_character_)
  ) %>%
  mutate(AESTDTC_TMP = AESTDTC) %>%
  derive_study_day(
    sdtm_in       = .,
    dm_domain     = dm1,
    tgdt          = "AESTDTC",
    refdt         = "RFSTDTC",
    study_day_var = "AESTDY",
    merge_key     = "USUBJID"
  ) %>%
  mutate(AESTDTC = AESTDTC_TMP) %>%
  mutate(AEENDTC_TMP = AEENDTC) %>%
  derive_study_day(
    sdtm_in       = .,
    dm_domain     = dm1,
    tgdt          = "AEENDTC",
    refdt         = "RFSTDTC",
    study_day_var = "AEENDY",
    merge_key     = "USUBJID"
  ) %>%
  mutate(AEENDTC = AEENDTC_TMP) %>%
  derive_seq(
    tgt_var  = "AESEQ",
    rec_vars = c("STUDYID", "USUBJID", "AESPID", "AETERM", "AESTDTC")
  )

attr(ae2$AEFUNEED, "label") <- "Follow-up needed"

# Add seriousness ----
ae3 <- ae2 %>%
  left_join(sae1 %>%
              mutate(usubjid = as.character(usubjid),
                     saespid = as.numeric(saespid)) %>%
              select(usubjid, saespid, aesdth, aeslife, aeshosp, aesdisab, aescong, aesmie), by = c("USUBJID" = "usubjid", "AESPID" = "saespid")) %>%
  mutate(AESDTH   = aesdth,
         AESLIFE  = aeslife, 
         AESHOSP  = aeshosp,
         AESDISAB = aesdisab,
         AESCONG  = aescong,
         AESMIE   = aesmie) %>%
  select(-aesdth, -aeslife, -aeshosp, -aesdisab, -aescong, -aesmie)

# Finalize ----
ae <- sdtm_create_domain(ae3, delperm = FALSE, keys = "STUDYID, USUBJID, AESPID, AETERM, AESTDTC", addsuppvars = c("AEFUNEED"))
