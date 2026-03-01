# ******************************************************************************
# Danone Nutricia Research
# ******************************************************************************
# STUDY/PROJECT : SONA / 23REX0061265
# PROGRAM PATH  : SAS/SONA/Files/SDTM Conversion/40 SDTM Programs
# PROGRAM NAME  : QC_DS.R 
# PURPOSE       : QC SDTM data for Disposition
# ------------------------------------------------------------------------------
# NOTES : 
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2025-10-13 - cattanch - Initial program
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
sdtm_domain <- "DS"

# Import metadata
sdtm_ct  <- read_excel("../31 Metadata Data/SONA_CT.xlsx", sheet = "CT")
sdtm_var <- read_sas("../31 Metadata Data/sdtm_3_4_variables.sas7bdat")
sdtm_dom <- read_sas("../31 Metadata Data/sdtm_3_4_domains.sas7bdat")

# Import data ----
dm1  <- read_sas("../41 SDTM Data/dm.sas7bdat")
ie1  <- copy_import(inset = "ie")
eos1 <- copy_import(inset = "eos")
ic1  <- copy_import(inset = "ic")
ed1  <- copy_import(inset = "event_dates")
rs1  <- copy_import(inset = "review_status")

# Sources ----
sources <- rs1 %>%
  filter(trimws(formname) != "") %>%
  distinct(formid, formname)

# IC ----
ds_ic <- 
  assign_datetime(
    raw_dat = ic1,
    raw_var = "icsdat",
    tgt_var = "DSSTDTC",
    raw_fmt = c("y-m-d"),
    raw_unk = c("NK", "UNK", "UN"),
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ic1,
    raw_var = "subjectid",
    tgt_var = "SUBJID",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ic1,
    raw_var = "usubjid",
    tgt_var = "USUBJID",
    id_vars = oak_id_vars()
  ) %>%
  mutate(
    DSTERM   = "INFORMED CONSENT OBTAINED",
    DSDECOD  = "INFORMED CONSENT OBTAINED",
    DSCAT    = "PROTOCOL MILESTONE",
    SOURCEID = paste("CRF:", sources$formname[match("IC", sources$formid)])
  ) %>%
  filter(!is.na(DSSTDTC))

ds_ic_fu <- 
  hardcode_no_ct(
    raw_dat = condition_add(ic1, iceifu == "Y"),
    raw_var = "iceifu",
    tgt_var = "DSDECOD",
    tgt_val = "INFORMED CONSENT OBTAINED",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_no_ct(
    raw_dat = condition_add(ic1, iceifu == "Y"),
    raw_var = "iceifu",
    tgt_var = "DSTERM",
    tgt_val = "INFORMED CONSENT OBTAINED - ENCODED INFORMATION FOR FUTURE RESEARCH",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_no_ct(
    raw_dat = condition_add(ic1, iceifu == "N"),
    raw_var = "iceifu",
    tgt_var = "DSDECOD",
    tgt_val = "INFORMED CONSENT DECLINED",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_no_ct(
    raw_dat = condition_add(ic1, iceifu == "N"),
    raw_var = "iceifu",
    tgt_var = "DSTERM",
    tgt_val = "INFORMED CONSENT DECLINED - ENCODED INFORMATION FOR FUTURE RESEARCH",
    id_vars = oak_id_vars()
  ) %>%
  assign_datetime(
    raw_dat = condition_add(ic1, !is.na(iceifu)),
    raw_var = "icsdat",
    tgt_var = "DSSTDTC",
    raw_fmt = c("y-m-d"),
    raw_unk = c("NK", "UNK", "UN"),
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ic1,
    raw_var = "subjectid",
    tgt_var = "SUBJID",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ic1,
    raw_var = "usubjid",
    tgt_var = "USUBJID",
    id_vars = oak_id_vars()
  ) %>%
  mutate(
    DSCAT    = "OTHER EVENT",
    SOURCEID = paste("CRF:", sources$formname[match("IC", sources$formid)])
  )

ds_ic_lts <- 
  hardcode_no_ct(
    raw_dat = condition_add(ic1, iclts == "Y"),
    raw_var = "iclts",
    tgt_var = "DSDECOD",
    tgt_val = "INFORMED CONSENT OBTAINED",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_no_ct(
    raw_dat = condition_add(ic1, iclts == "Y"),
    raw_var = "iclts",
    tgt_var = "DSTERM",
    tgt_val = "INFORMED CONSENT OBTAINED - FOR KEEPING BODILY MATERIAL FOR SCIENTIFIC RESEARCH",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_no_ct(
    raw_dat = condition_add(ic1, iclts == "N"),
    raw_var = "iclts",
    tgt_var = "DSDECOD",
    tgt_val = "INFORMED CONSENT DECLINED",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_no_ct(
    raw_dat = condition_add(ic1, iclts == "N"),
    raw_var = "iclts",
    tgt_var = "DSTERM",
    tgt_val = "INFORMED CONSENT DECLINED - FOR KEEPING BODILY MATERIAL FOR SCIENTIFIC RESEARCH",
    id_vars = oak_id_vars()
  ) %>%
  assign_datetime(
    raw_dat = condition_add(ic1, !is.na(iclts)),
    raw_var = "icsdat",
    tgt_var = "DSSTDTC",
    raw_fmt = c("y-m-d"),
    raw_unk = c("NK", "UNK", "UN"),
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ic1,
    raw_var = "subjectid",
    tgt_var = "SUBJID",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ic1,
    raw_var = "usubjid",
    tgt_var = "USUBJID",
    id_vars = oak_id_vars()
  ) %>%
  mutate(
    DSCAT    = "OTHER EVENT",
    SOURCEID = paste("CRF:", sources$formname[match("IC", sources$formid)])
  )

# IE ----
ie2 <- ie1 %>%
  filter(!is.na(ieyn)) %>%
  select(-eventid) %>%
  left_join(
    ed1 %>%
      filter(tolower(eventid) == "bl", tolower(eventstatus) == "initiated") %>%
      group_by(usubjid) %>%
      slice(1) %>%
      ungroup() %>%
      transmute(usubjid, eventid = eventid),
    by = "usubjid"
  )
    
ds_ie <- 
  hardcode_no_ct(
    raw_dat = condition_add(ie2, tolower(ieyn) == "y"),
    raw_var = "ieyn",
    tgt_var = "DSDECOD",
    tgt_val = "ELIGIBILITY CRITERIA MET",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_no_ct(
    raw_dat = condition_add(ie2, tolower(ieyn) == "y"),
    raw_var = "ieyn",
    tgt_var = "DSTERM",
    tgt_val = "ELIGIBILITY CRITERIA MET",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_no_ct(
    raw_dat = condition_add(ie2, tolower(ieyn) == "n" & is.na(eventid)),
    raw_var = "ieyn",
    tgt_var = "DSDECOD",
    tgt_val = "ELIGIBILITY CRITERIA NOT MET",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_no_ct(
    raw_dat = condition_add(ie2, tolower(ieyn) == "n" & !is.na(eventid)),
    raw_var = "ieyn",
    tgt_var = "DSDECOD",
    tgt_val = "ELIGIBILITY CRITERIA NOT MET BUT ENROLLED",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_no_ct(
    raw_dat = condition_add(ie2, tolower(ieyn) == "n"),
    raw_var = "ieyn",
    tgt_var = "DSTERM",
    tgt_val = "ELIGIBILITY CRITERIA NOT MET",
    id_vars = oak_id_vars()
  ) %>%
  assign_datetime(
    raw_dat = condition_add(ie2, !is.na(ieyn)),
    raw_var = "eventdate",
    tgt_var = "DSSTDTC",
    raw_fmt = c("y-m-d"),
    raw_unk = c("NK", "UNK", "UN"),
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ie2,
    raw_var = "subjectid",
    tgt_var = "SUBJID",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ie2,
    raw_var = "usubjid",
    tgt_var = "USUBJID",
    id_vars = oak_id_vars()
  ) %>%
  mutate(
    DSCAT = "PROTOCOL MILESTONE",
    EXCL  = if_else(tolower(ie2$ieyn) == "n" & is.na(ie2$eventid), "Y", NA_character_),
    SOURCEID = paste("CRF:", sources$formname[match("IE", sources$formid)])
  )

# EOS ----
eos2 <- eos1 %>%
  left_join(
    dm1 %>% 
      select(USUBJID, RFENDTC) %>%
      rename(usubjid = USUBJID),
    by = "usubjid"
  )

ds_eos <- 
  hardcode_no_ct(
    raw_dat = condition_add(eos2, tolower(complyn) == "y"),
    raw_var = "complyn",
    tgt_var = "DSDECOD",
    tgt_val = "COMPLETED",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_no_ct(
    raw_dat = condition_add(eos2, tolower(complyn) == "y"),
    raw_var = "complyn",
    tgt_var = "DSTERM",
    tgt_val = "COMPLETED",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = condition_add(eos2, tolower(complyn) == "n"),
    raw_var = "etstat",
    tgt_var = "DSDECOD",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = condition_add(eos2, tolower(complyn) == "n" & !is.na(etsp)),
    raw_var = "etsp",
    tgt_var = "DSTERM",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = condition_add(eos2, tolower(complyn) == "n" & is.na(etsp)),
    raw_var = "etstat",
    tgt_var = "DSTERM",
    id_vars = oak_id_vars()
  ) %>%
  assign_datetime(
    raw_dat = condition_add(eos2, !is.na(etdat)),
    raw_var = "etdat",
    tgt_var = "DSSTDTC",
    raw_fmt = c("y-m-d"),
    raw_unk = c("NK", "UNK", "UN"),
    id_vars = oak_id_vars()
  ) %>%
  assign_datetime(
    raw_dat = condition_add(eos2, is.na(etdat) & tolower(complyn) == "y"),
    raw_var = "RFENDTC",
    tgt_var = "DSSTDTC",
    raw_fmt = c("y-m-d"),
    raw_unk = c("NK", "UNK", "UN"),
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = eos2,
    raw_var = "subjectid",
    tgt_var = "SUBJID",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = eos2,
    raw_var = "usubjid",
    tgt_var = "USUBJID",
    id_vars = oak_id_vars()
  ) %>%
  mutate(
    DSCAT    = "DISPOSITION EVENT",
    SOURCEID = paste("CRF:", sources$formname[match("EOS", sources$formid)])
  )

# Combine ----
ds1 <-
  bind_rows(
    ds_ic,
    ds_ic_fu,
    ds_ic_lts,
    ds_ie,
    ds_eos
  ) %>%
  mutate(
    STUDYID  = study,
    DOMAIN   = sdtm_domain
  ) %>%
  arrange(USUBJID, DSDECOD, EXCL)

# Remove multiple disposition events ----

ds_disp <- ds1 %>%
  filter(DSCAT == "DISPOSITION EVENT") %>%
  group_by(USUBJID) %>%
  mutate(one_ds = if_else(n() > 1, "n", "y")) %>%
  ungroup() %>%
  filter(!(tolower(one_ds) == "n" & tolower(EXCL) == "y")) %>%
  select(-one_ds, -EXCL)

ds_no_disp <- ds1 %>%
  filter(DSCAT != "DISPOSITION EVENT") %>%
  select(-EXCL)

ds2 <- bind_rows(ds_disp, ds_no_disp) %>%
  mutate(DSSTDTC_TMP = DSSTDTC) %>%
  derive_study_day(
    sdtm_in       = .,
    dm_domain     = dm1,
    tgdt          = "DSSTDTC",
    refdt         = "RFSTDTC",
    study_day_var = "DSSTDY",
    merge_key     = "USUBJID"
  ) %>%
  mutate(DSSTDTC = DSSTDTC_TMP) %>%
  derive_seq(
    tgt_var  = "DSSEQ",
    rec_vars = c("STUDYID", "USUBJID", "DSSTDTC", "DSTERM")
  )

# Finalize ----
ds <- sdtm_create_domain(ds2, keys = "STUDYID, USUBJID, DSSTDTC, DSTERM")
