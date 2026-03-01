# ******************************************************************************
# Danone Nutricia Research
# ******************************************************************************
# STUDY/PROJECT : SONA / 23REX0061265
# PROGRAM PATH  : SAS/SONA/Files/SDTM Conversion/40 SDTM Programs
# PROGRAM NAME  : QC_EX.R
# PURPOSE       : Create SDTM data for Exposure (EX) — period-based (daily totals)
# ------------------------------------------------------------------------------
# NOTES :
#   1) Create one record per serving actually taken (EXYN1/2='Y')
#   2) Sum to daily total per subject-day
#   3) Create periods as consecutive days with same daily dose
#   4) Output one EX record per period
#   5) Derive EXSTDY/EXENDY from DM.RFSTDTC
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2025-11-06 - Vikas Shokeen - Initial version
# 2025-12-17 - Vikas Shokeen - Updated the program as per above notes
# 2026-01-14 - Vikas Shokeen - Added EXLNKID
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

# Import metadata (kept for consistency with other programs; not required below)
sdtm_ct  <- read_excel("../31 Metadata Data/SONA_CT.xlsx", sheet = "CT")
sdtm_var <- read_sas("../31 Metadata Data/sdtm_3_4_variables.sas7bdat")
sdtm_dom <- read_sas("../31 Metadata Data/sdtm_3_4_domains.sas7bdat")

# --- Parameters ---------------------------------------------------------------
study        <- "SONA"
sdtm_domain  <- "EX"

# --- Inputs -------------------------------------------------------------------
ex_raw <- copy_import(inset = "ex")
dm_raw <- copy_import(inset = "dm")
dm1 <- read_sas("../41 SDTM Data/dm.sas7bdat")
rs1  <- copy_import(inset = "review_status")

# Sources ----
sources <- rs1 %>%
  filter(trimws(formname) != "") %>%
  distinct(formid, formname)

# Standardize headers early
names(ex_raw) <- tolower(names(ex_raw))
names(dm_raw) <- tolower(names(dm_raw))

# --- Keep a keyed source (Oak keys) -------------------------------------------
src_keyed <- generate_oak_id_vars(
  raw_dat = ex_raw,
  pat_var = "subjectid",
  raw_src = "CRF: Product Intake"
)

# ============================================================================
# Step 1 — Product intake: one row per serving actually taken
#   (matches SAS ex_intake: where exspid not missing; loop servings 1..2)
# ============================================================================

# Helper: SDTM-style DY (matches common SDTM convention)
derive_dy <- function(dtc_chr, rfstdtc_chr) {
  d  <- suppressWarnings(as.Date(dtc_chr))
  rf <- suppressWarnings(as.Date(rfstdtc_chr))
  out <- as.integer(d - rf)
  # add +1 for on/after RFSTDTC
  out <- ifelse(!is.na(out) & !is.na(d) & !is.na(rf) & d >= rf, out + 1L, out)
  out
}

# Keep only records with EXSPID present (SAS: where=(not missing(exspid)))
step1_src <- src_keyed %>%
  filter(!is.na(exspid) & exspid != "")

names(step1_src) <- tolower(names(step1_src))

serving_re <- "^(exyn|exdose)\\d+$"
id_cols <- intersect(names(step1_src), c("subjectid","usubjid","exdat","exspid"))

step1_clean <- step1_src %>%
  select(all_of(id_cols), matches(serving_re))

# Pivot EXYN1/2 and EXDOSE1/2 to rows (servings)
ex_intake <- step1_clean %>%
  pivot_longer(
    cols = matches(serving_re),
    names_to = c(".value","k"),
    names_pattern = "^(exyn|exdose)(\\d+)$"
  ) %>%
  filter(tolower(exyn) %in% c("y")) %>%
  mutate(
    SUBJID  = subjectid,
    # SAS: %sdtm_dt2dtc(dt=exdat, dtc=_dtc); exdate = input(substr(_dtc,1,10), yymmdd10.)
    EXDATE  = as.Date(sub("T00:00(:00)?$", "", as.character(exdat))),
    EXDOSE  = case_when(
      str_trim(as.character(exdose)) == "EVERYTHING" ~ 45.6,
      str_trim(as.character(exdose)) == "3/4"        ~ 34.2,
      str_trim(as.character(exdose)) == "1/2"        ~ 22.8,
      str_trim(as.character(exdose)) == "1/4"        ~ 11.4,
      str_trim(as.character(exdose)) == "NOTHING"    ~ as.numeric(NA),
      TRUE                                           ~ as.numeric(NA)
    )
  ) %>%
  select(USUBJID = usubjid, SUBJID, EXDATE, EXDOSE)

# ============================================================================
# Step 2 — Daily totals: one row per subject-day (SAS PROC SUMMARY SUM)
# ============================================================================

ex_daily <- ex_intake %>%
  group_by(USUBJID, SUBJID, EXDATE) %>%
  summarise(EXDOSE = sum(EXDOSE, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    # if all doses were NA that day, SAS would return missing not 0; emulate that:
    EXDOSE = ifelse(is.infinite(EXDOSE) | is.nan(EXDOSE), NA_real_, EXDOSE),
    EXDOSE = ifelse(EXDOSE == 0 & all(is.na(ex_intake$EXDOSE[ex_intake$USUBJID == USUBJID[1] &
                                                               ex_intake$SUBJID  == SUBJID[1] &
                                                               ex_intake$EXDATE  == EXDATE[1]])),
                    NA_real_, EXDOSE)
  )

# ============================================================================
# Step 3 — Periods: consecutive days with same daily dose (dose change or gap)
# ============================================================================

ex_period_prep <- ex_daily %>%
  arrange(USUBJID, EXDATE) %>%
  group_by(USUBJID) %>%
  mutate(
    PREV_D  = lag(EXDOSE),
    PREV_DT = lag(EXDATE),
    NEW_PRD = case_when(
      row_number() == 1 ~ 1L,
      # SAS: exdose ne prev_d OR exdate ne prev_dt+1
      (is.na(EXDOSE) & !is.na(PREV_D)) ~ 1L,
      (!is.na(EXDOSE) & is.na(PREV_D)) ~ 1L,
      (!is.na(EXDOSE) & !is.na(PREV_D) & EXDOSE != PREV_D) ~ 1L,
      (!is.na(EXDATE) & !is.na(PREV_DT) & EXDATE != (PREV_DT + days(1))) ~ 1L,
      TRUE ~ 0L
    ),
    PRD = cumsum(NEW_PRD)
  ) %>%
  ungroup() %>%
  select(USUBJID, SUBJID, EXDATE, EXDOSE, PRD)

# ============================================================================
# Step 4 — One record per period per subject (SAS ex_final)
# ============================================================================

ex_final <- ex_period_prep %>%
  group_by(USUBJID, SUBJID, PRD) %>%
  summarise(
    START_D = min(EXDATE, na.rm = TRUE),
    END_D   = max(EXDATE, na.rm = TRUE),
    DOSE    = first(EXDOSE),
    .groups = "drop"
  ) %>%
  mutate(
    STUDYID   = study,
    DOMAIN    = sdtm_domain,
    SOURCEID = paste("CRF:", sources$formname[match(sdtm_domain, sources$formid)]),
    EXTRT     = "SYNBIOTIC-ENRICHED ORAL NUTRITIONAL SUPPLEMENT",
    EXDOSFRM  = "POWDER, FOR SOLUTION",
    EXDOSFRQ  = "QD",
    EXDOSU    = "mg",
    EXSTDTC   = format(START_D, "%Y-%m-%d"),
    EXENDTC   = format(END_D,   "%Y-%m-%d"),
    EXDOSE    = DOSE,
    EXLNKID   = as.character(PRD) 
  ) %>%
  select(
    STUDYID, DOMAIN, USUBJID, SUBJID, EXTRT,
    EXDOSU, EXDOSFRM, EXDOSFRQ, EXSTDTC, EXENDTC, EXDOSE, SOURCEID, EXLNKID
  )

# ============================================================================
# Step 5 — Derive --DY from DM.RFSTDTC 
# ============================================================================

ex_final <- ex_final %>% rename_with(toupper)

dm_1 <- dm1 %>% rename_with(toupper)

ex_final1 <- sdtm.oak::derive_study_day(
  sdtm_in = ex_final ,
  dm_domain = dm_1,
  tgdt = "EXSTDTC",
  refdt = "RFSTDTC", 
  study_day_var = "EXSTDY",
  merge_key = "USUBJID"
)

ex_final2 <- sdtm.oak::derive_study_day(
  sdtm_in = ex_final1 ,
  dm_domain = dm_1,
  tgdt = "EXENDTC",
  refdt = "RFSTDTC", 
  study_day_var = "EXENDY",
  merge_key = "USUBJID"
)

# ============================================================================
# Step 6 — Finalize (derive EXSEQ; create domain)
# ============================================================================

# Unify header case before SDTM create
ex_u <- ex_final2 %>% rename_with(toupper)

# EXSEQ
keys_seq <- c("STUDYID", "USUBJID", "EXTRT", "EXSTDTC")


ex_u1 <- sdtm.oak::derive_seq(
  tgt_dat  = ex_u,
  tgt_var  = "EXSEQ",
  rec_vars = keys_seq
) %>%
  arrange(STUDYID, USUBJID, EXTRT, EXSTDTC)

# Finalize
EX <- sdtm_create_domain(
  ex_u1,
  keys = "STUDYID, USUBJID, EXTRT, EXSTDTC",
  delperm = FALSE
)