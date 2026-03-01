# ******************************************************************************
# Danone Nutricia Research
# ******************************************************************************
# STUDY/PROJECT : SONA / 23REX0061265
# PROGRAM PATH  : SAS/SONA/Files/SDTM Conversion/40 SDTM Programs
# PROGRAM NAME  : QC_EC.R
# PURPOSE       : QC SDTM data for Exposure as Collected
# ------------------------------------------------------------------------------
# NOTES :
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2025-10-08 - Vikas Shokeen - Initial version
# 2025-10-10 - Vikas Shokeen - Updated the working directory according to both
#							   LSAF and RSTUDIO
# 2025-12-08 - Chiara Cattani - Added EXSTDY, ECENDTC, ECENDY.
#                               Updated ECTPTX to ECEVINTX.
#                               Updated ECADJ mapping.
# 2026-01-13 - Vikas Shokeen  - Implemented ECLNKGRP Variable derivation
# ******************************************************************************

# --- Configuration -------------------------------------------------------------

library(nutriciaconfig)
nutricia_config()

library(readxl)
library(haven)
library(sdtm.oak)
library(dplyr)
library(tidyr)
library(stringr)
library(rlang)

# Set working directory ----
if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  # Running in RStudio
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
} else {
  # Running in LSAF
  setwd(progdir)
}

# Import metadata
sdtm_ct  <- read_excel("../31 Metadata Data/SONA_CT.xlsx", sheet = "CT")
sdtm_var <- read_sas("../31 Metadata Data/sdtm_3_4_variables.sas7bdat")
sdtm_dom <- read_sas("../31 Metadata Data/sdtm_3_4_domains.sas7bdat")

# --- Parameters ---------------------------------------------------------------
study       <- "SONA"
sdtm_domain <- "EC"

# --- Inputs -------------------------------------------------------------------
ex_raw <- copy_import(inset = "ex")
dm1    <- read_sas("../41 SDTM Data/dm.sas7bdat")

# Standardize headers early
names(ex_raw) <- tolower(names(ex_raw))

# --- Keep a keyed source (Oak keys) -------------------------------------------
src_keyed <- generate_oak_id_vars(
  raw_dat = ex_raw,
  pat_var = "subjectid",
  raw_src = "CRF: Product Intake"
)

# ============================================================================
# Step 0 — Build EX-like period map for ECLNKGRP (SAS-equivalent)
# ============================================================================

build_ex_period_map <- function(ex_dat) {
  ex0 <- ex_dat %>% rename_with(tolower)
  
  serving_re <- "^(exyn|exdose)\\d+$"
  id_cols <- intersect(names(ex0), c("usubjid", "exdat"))
  
  ex_serv <- ex0 %>%
    select(all_of(id_cols), matches(serving_re)) %>%
    pivot_longer(
      cols = matches(serving_re),
      names_to = c(".value", "k"),
      names_pattern = "^(exyn|exdose)(\\d+)$"
    ) %>%
    mutate(
      exyn   = str_to_lower(str_trim(as.character(exyn))),
      exdose = str_trim(as.character(exdose))
    ) %>%
    filter(exyn == "y") %>%
    mutate(
      exdate = as.Date(substr(as.character(exdat), 1, 10)),
      exdose_num = case_when(
        exdose == "EVERYTHING" ~ 45.6,
        exdose == "3/4"        ~ 34.2,
        exdose == "1/2"        ~ 22.8,
        exdose == "1/4"        ~ 11.4,
        TRUE                   ~ NA_real_
      )
    ) %>%
    select(usubjid, exdate, exdose_num)
  
  ex_daily <- ex_serv %>%
    group_by(usubjid, exdate) %>%
    summarise(exdose = sum(exdose_num, na.rm = TRUE), .groups = "drop") %>%
    arrange(usubjid, exdate)
  
  ex_period <- ex_daily %>%
    group_by(usubjid) %>%
    arrange(exdate, .by_group = TRUE) %>%
    mutate(
      prev_d  = lag(exdose),
      prev_dt = lag(exdate),
      new_prd = if_else(
        is.na(prev_dt) | exdose != prev_d | exdate != (prev_dt + 1),
        1L, 0L
      ),
      prd      = cumsum(new_prd),
      eclnkgrp = as.character(prd)
    ) %>%
    ungroup() %>%
    select(usubjid, exdate, eclnkgrp)
  
  ex_period
}

add_eclnkgrp_to_ec <- function(ec_df, ex_period_map) {
  ec_df %>%
    mutate(ecdate = as.Date(substr(as.character(ECSTDTC), 1, 10))) %>%
    left_join(
      ex_period_map,
      by = c("USUBJID" = "usubjid", "ecdate" = "exdate")
    ) %>%
    mutate(ECLNKGRP = eclnkgrp) %>%
    select(-ecdate, -eclnkgrp)
}

ex_period_map <- build_ex_period_map(ex_raw)

# ============================================================================
# Step 1 — First Intake (EXYN / EXSTDAT)
# ============================================================================

step1_src <- src_keyed %>% filter(!is.na(exyn) & exyn != "")

# IDs + EXYN into ec1 so we can use it in condition_add()
ec1 <-
  assign_no_ct(tgt_var = "SUBJID",  raw_dat = step1_src, raw_var = "subjectid", id_vars = oak_id_vars()) %>%
  assign_no_ct(tgt_var = "USUBJID", raw_dat = step1_src, raw_var = "usubjid",   id_vars = oak_id_vars()) %>%
  assign_no_ct(tgt_var = "EVENTID", raw_dat = step1_src, raw_var = "eventid",   id_vars = oak_id_vars()) %>%
  assign_no_ct(tgt_var = "EXYN",    raw_dat = step1_src, raw_var = "exyn",      id_vars = oak_id_vars()) %>%
  assign_no_ct(tgt_var = "ECSTDTC", raw_dat = step1_src, raw_var = "exstdat",   id_vars = oak_id_vars()) %>%
  assign_no_ct(tgt_var = "ECENDTC", raw_dat = step1_src, raw_var = "exstdat",   id_vars = oak_id_vars()) %>%
  mutate(
    STUDYID = study,
    DOMAIN  = sdtm_domain,
    ECCAT   = "FIRST INTAKE",
    ECSPID  = "FIRST SERVING",
    ECGRPID = as.character(step1_src$exspid),
    ECTRT   = "STUDY PRODUCT",
    ECORIG  = "CRF",
    ECEVAL  = "INVESTIGATOR",
  )

# ECOCCUR / ECSTAT / ECREASND using in-table EXYN
ec1 <- ec1 %>%
  condition_add(tolower(EXYN) %in% c("y","n")) %>%
  dplyr::mutate(ECOCCUR = toupper(EXYN), ECPRESP = "Y") %>%
  condition_add(tolower(EXYN) == "unknown") %>%
  dplyr::mutate(ECOCCUR = "UK", ECSTAT = "NOT DONE", ECREASND = "UNKNOWN", ECPRESP = "Y") %>%
  # ECSTDTC and ECENDTC as character, strip midnight
  dplyr::mutate(ECSTDTC = sub("T00:00(:00)?$", "", as.character(ECSTDTC))) %>%
  dplyr::mutate(ECENDTC = sub("T00:00(:00)?$", "", as.character(ECENDTC)))

# Visit mapping (case-insensitive)
ec1 <- derive_visit_vars(ec1, eventid_var = "EVENTID")

# ============================================================================
# Step 2 — Servings (EX*1/2… with EXSPID present; no Oak joins after pivot)
# ============================================================================

step2_src <- src_keyed %>% filter(!is.na(exspid) & exspid != "")
names(step2_src) <- tolower(names(step2_src))

serving_re <- "^(exyn|extime|exdose|exreas|exsp)\\d+$"
id_cols <- intersect(names(step2_src), c("subjectid","usubjid","eventid","exdat","exspid"))

step2_clean <- step2_src %>% select(all_of(id_cols), matches(serving_re))

ec2 <- step2_clean %>%
  pivot_longer(
    cols = matches(serving_re),
    names_to = c(".value","k"),
    names_pattern = "^(exyn|extime|exdose|exreas|exsp)(\\d+)$"
  ) %>%
  filter(tolower(exyn) %in% c("y","n")) %>%
  mutate(
    STUDYID  = study,
    SUBJID   = subjectid,
    DOMAIN   = sdtm_domain,
    ECOCCUR  = toupper(exyn),
    ECPRESP  = "Y",
    ECSPID   = case_when(
      k == "1" ~ "FIRST SERVING",
      k == "2" ~ "SECOND SERVING",
      TRUE     ~ NA_character_
    ),
    ECGRPID  = as.character(exspid),
    ECEVINTX = as.character(extime),
    ECDOSTXT = as.character(exdose),
    ECADJ    = case_when(
      exreas == "OT"     ~ "OTHER",
      exreas == "FORGOT" ~ "FORGOTTEN",
      TRUE               ~ exreas
    ),
    ECADJX   = as.character(exsp),
    ECCAT    = "STUDY PRODUCT",
    ECTRT    = "STUDY PRODUCT",
    ECORIG   = "pPRO",
    ECEVAL   = "STUDY SUBJECT",
    ECSTDTC  = sub("T00:00(:00)?$", "", as.character(exdat)),
    ECENDTC  = sub("T00:00(:00)?$", "", as.character(exdat)),
    SOURCEID = "CRF: Product Intake"
  )

# ============================================================================
# Step 3 — Combine & finalize
# ============================================================================

# Unify header case before bind
ec1_u <- ec1 %>% rename_with(toupper)
ec2_u <- ec2 %>% rename_with(toupper)

ec_combined <- bind_rows(ec1_u, ec2_u) %>%
  mutate(SOURCEID = "CRF: Product Intake") %>%
  add_eclnkgrp_to_ec(ex_period_map = ex_period_map) %>%
  mutate(ECENDTC_TMP = ECENDTC) %>%
  derive_study_day(
    sdtm_in       = .,
    dm_domain     = dm1,
    tgdt          = "ECENDTC",
    refdt         = "RFSTDTC",
    study_day_var = "ECENDY",
    merge_key     = "USUBJID"
  ) %>%
  mutate(ECENDTC = ECENDTC_TMP) %>%
  mutate(ECSTDTC_TMP = ECSTDTC) %>%
  derive_study_day(
    sdtm_in       = .,
    dm_domain     = dm1,
    tgdt          = "ECSTDTC",
    refdt         = "RFSTDTC",
    study_day_var = "ECSTDY",
    merge_key     = "USUBJID"
  ) %>%
  mutate(ECSTDTC = ECSTDTC_TMP)

# -- Ensure EC has the REQ/EXP variables without deriving anything -------------
add_if_missing <- function(df, name, prototype) {
  if (!name %in% names(df)) df[[name]] <- prototype
  df
}

# numeric
ec_combined <- add_if_missing(ec_combined, "ECDOSE", as.numeric(NA))
# character
ec_combined <- add_if_missing(ec_combined, "ECDOSU",   as.character(NA))
ec_combined <- add_if_missing(ec_combined, "ECDOSFRM", as.character(NA))

# make sure these columns exist (OK if some are NA)
keys_seq <- c("STUDYID", "USUBJID", "ECCAT", "ECTRT", "ECSTDTC", "ECENDTC", "EXSPID")

ec_combined1 <- derive_seq(
  tgt_dat  = ec_combined,
  tgt_var  = "ECSEQ",
  rec_vars = keys_seq
) %>%
  arrange(STUDYID, SUBJID, ECCAT, ECTRT, ECSTDTC, ECENDTC, EXSPID, K)

# Finalize
EC <- sdtm_create_domain(ec_combined1, keys = "STUDYID, USUBJID, ECCAT", delperm = FALSE)