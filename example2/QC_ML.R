# ******************************************************************************
# Danone Nutricia Research
# ******************************************************************************
# STUDY/PROJECT : SONA / 23REX0061265
# PROGRAM PATH  : SAS/SONA/Files/SDTM Conversion/40 SDTM Programs
# PROGRAM NAME  : QC_ML.R
# PURPOSE       : Create SDTM data for Meal Data
# ------------------------------------------------------------------------------
# NOTES :
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2025-12-18 - Vikas Shokeen - Initial version
# 2025-12-19 - Vikas Shokeen - Updated the key of MLLNKID, and sorting keys
# 2025-12-23 - Vikas Shokeen - Include MLENDTC, and MLENDY
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
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
} else {
  setwd(progdir)
}

# --- Parameters ---------------------------------------------------------------
study      <- "SONA"
sdtm_domain <- "ML"

# Metadata (if/when needed)
sdtm_ct  <- read_excel("../31 Metadata Data/SONA_CT.xlsx", sheet = "CT")
sdtm_var <- read_sas("../31 Metadata Data/sdtm_3_4_variables.sas7bdat")
sdtm_dom <- read_sas("../31 Metadata Data/sdtm_3_4_domains.sas7bdat")

# --- Inputs -------------------------------------------------------------------
MYFOOD24_RAW <- copy_import_myfood24(inset = "myfood24")
names(MYFOOD24_RAW) <- tolower(names(MYFOOD24_RAW))

DM1 <- read_sas("../41 SDTM Data/dm.sas7bdat")
names(DM1) <- tolower(names(DM1))

# Ensure core columns exist 
NEED <- c(
  "usubjid", "subjid",
  "diary_date", "meal_event", "food_name", "food_ean",
  "total_portion_size", "portion_unit"
)
stopifnot(all(NEED %in% names(MYFOOD24_RAW)))
stopifnot(all(c("usubjid", "rfstdtc") %in% names(DM1)))

# --- Oak key scaffold ----------------------------------------------------------
MYFOOD24_KEYED <- generate_oak_id_vars(
  raw_dat = MYFOOD24_RAW,
  pat_var = "subjid",
  raw_src = "participant-breakdown.csv"
)

# ------------------------------------------------------------------------------
# Helper: parse diary_date like SAS (dd/mm/yy or dd/mm/yyyy after '-' -> '/')
# Return SDTM ISO date string YYYY-MM-DD (character) for --DTC.
# ------------------------------------------------------------------------------
PARSE_DIARY_DATE_TO_DTC <- function(x) {
  x <- str_trim(as.character(x))
  x <- str_replace_all(x, "-", "/")
  x <- str_replace_all(x, "\\s+", "")
  x[x == ""] <- NA_character_
  
  d <- rep(as.Date(NA), length(x))
  is8  <- !is.na(x) & nchar(x) == 8
  is10 <- !is.na(x) & nchar(x) == 10
  
  d[is8]  <- as.Date(x[is8],  format = "%d/%m/%y")
  d[is10] <- as.Date(x[is10], format = "%d/%m/%Y")
  
  ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d"))
}

# ------------------------------------------------------------------------------
# Step 1 — Build ML core variables (SAS mf1)
# ------------------------------------------------------------------------------
ML1 <- MYFOOD24_KEYED %>%
  mutate(
    SOURCEID = "participant-breakdown.csv",
    STUDYID  = study,
    DOMAIN   = sdtm_domain,
    MLCAT    = "MYFOOD24",
    MLGRPID  = str_trim(as.character(meal_event)),
    MLTRT    = str_trim(as.character(food_name)),
    MLDECOD  = str_trim(as.character(food_ean)),
    MLDOSE   = suppressWarnings(as.numeric(total_portion_size)),
    MLDOSU   = if_else(str_to_lower(str_trim(portion_unit)) == "ml","mL", str_trim(portion_unit)),
    MLSTDTC  = PARSE_DIARY_DATE_TO_DTC(diary_date),
    MLENDTC = as.Date(NA),  # blank date
    MLENDY  = as.numeric(NA), # blank numeric
    MLINITD = {
      x <- str_trim(as.character(item_added_at))
      x <- str_replace_all(x, "-", "/")
      d <- as.Date(x, format = "%d/%m/%y %H:%M:%S")
      format(d, "%Y-%m-%d")
    }
  ) %>%
  select(
    usubjid, subjid, sourceid = SOURCEID, studyid = STUDYID, domain = DOMAIN,
    mlstdtc = MLSTDTC, mlgrpid = MLGRPID, mlcat = MLCAT, mldecod = MLDECOD,
    mltrt = MLTRT, mldosu = MLDOSU, mldose = MLDOSE, mlendtc=MLENDTC, mlendy=MLENDY, mlinitd=MLINITD
  )

# ------------------------------------------------------------------------------
# Step 2 — Join DM to bring RFSTDTC 
# NOTE: We'll NOT keep RFSTDTC in SDTM_IN for derive_study_day() to avoid .x/.y.
# ------------------------------------------------------------------------------
ML2 <- ML1 %>%
  mutate(usubjid = as.character(usubjid)) %>%
  left_join(
    DM1 %>% transmute(usubjid = as.character(usubjid), rfstdtc = as.character(rfstdtc)),
    by = "usubjid"
  )

# ------------------------------------------------------------------------------
# Step 3 — Derive MLSTDY using Oak (UPPER CASE, no warnings)
# ------------------------------------------------------------------------------
ML3 <- ML2 %>%
  rename_with(toupper) %>%
  mutate(
    USUBJID = as.character(USUBJID),
    MLSTDTC = as.Date(MLSTDTC)
  ) %>%
  # IMPORTANT: remove RFSTDTC from sdtm_in to avoid join suffixes (.x/.y)
  select(-any_of("RFSTDTC"))

DM_FOR_DY <- DM1 %>%
  rename_with(toupper) %>%
  transmute(
    USUBJID = as.character(USUBJID),
    RFSTDTC = as.Date(RFSTDTC)
  ) %>%
  distinct()

ML4 <- sdtm.oak::derive_study_day(
  sdtm_in       = ML3,
  dm_domain     = DM_FOR_DY,
  tgdt          = "MLSTDTC",
  refdt         = "RFSTDTC",
  study_day_var = "MLSTDY",
  merge_key     = "USUBJID"
)

# Convert MLSTDTC back to SDTM ISO character
ML4 <- ML4 %>%
  mutate(MLSTDTC = format(MLSTDTC, "%Y-%m-%d"))

# ------------------------------------------------------------------------------
# Step 4 — Sort and derive MLLNKID
# ------------------------------------------------------------------------------
ML5 <- ML4 %>%
  arrange(USUBJID, MLTRT, MLSTDTC, MLGRPID, SUBJID, MLDOSE) %>%
  group_by(USUBJID) %>%
  mutate(MLLNKID = as.character(row_number())) %>%
  ungroup()

# Derive MLSEQ (Oak) 
ML6 <- sdtm.oak::derive_seq(
  tgt_dat  = ML5,
  tgt_var  = "MLSEQ",
  rec_vars = c("STUDYID", "USUBJID", "MLTRT", "MLSTDTC", "MLGRPID", "MLLNKID")
)

# Label (like SAS label statement)
attr(ML6$MLDECOD, "label") <- "Standardized Meal Name"
attr(ML6$MLINITD, "label") <- "Initiated Date"


# ------------------------------------------------------------------------------
# Step 5 — Create ML domain (Oak)
# ------------------------------------------------------------------------------
ML <- sdtm_create_domain(
  ML6,
  keys = "STUDYID, USUBJID, MLTRT, MLSTDTC, MLGRPID, MLLNKID",
  delperm = "N" ,
  addsuppvars = c("MLDECOD", "MLINITD")
)
