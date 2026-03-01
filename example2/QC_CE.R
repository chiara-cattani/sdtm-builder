# ******************************************************************************
# Danone Nutricia Research
# ******************************************************************************
# STUDY/PROJECT : SONA / 23REX0061265
# PROGRAM PATH  : SAS/SONA/Files/SDTM Conversion/40 SDTM Programs
# PROGRAM NAME  : QC_CE.R
# PURPOSE       : QC SDTM data for Clinical Events
# ------------------------------------------------------------------------------
# NOTES :
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2025-10-10 - Vikas Shokeen - Initial version
# 2025-12-16 - Vikas Shokeen - Added CEDY
# 2025-01-14 - Vikas Shokeen - Added CELNKID
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

# --- Parameters ---------------------------------------------------------------
study       <- "SONA"
sdtm_domain <- "CE"

# Metadata (if/when needed)
sdtm_ct  <- read_excel("../31 Metadata Data/SONA_CT.xlsx", sheet = "CT")
sdtm_var <- read_sas("../31 Metadata Data/sdtm_3_4_variables.sas7bdat")
sdtm_dom <- read_sas("../31 Metadata Data/sdtm_3_4_domains.sas7bdat")

# --- Inputs -------------------------------------------------------------------
giq_raw <- copy_import(inset = "giq")
names(giq_raw) <- tolower(names(giq_raw))

dm1 <- read_sas("../41 SDTM Data/dm.sas7bdat")

# Optional: drop SAS-specified prefixes
giq <- giq_raw %>%
  select(-starts_with("st7p"), -starts_with("sampyn"))

# Ensure core columns exist
need <- c("usubjid","subjectid","gidat")
stopifnot(all(need %in% names(giq)))

# --- Oak key scaffold (stable joins if you later use any Oak assign_*) --------
giq_keyed <- generate_oak_id_vars(
  raw_dat = giq,
  pat_var = "subjectid",
  raw_src = "CRF: Gastro Intestinal Tolerance & BSFC"
)

# ------------------------------------------------------------------------------
# Step 1 — Symptoms block (array → long, per your SAS arrays)
# ------------------------------------------------------------------------------

# Map raw symptom columns -> CE terms (exactly as in SAS)
sym_map <- c(
  nauseaq   = "NAUSEA",
  vomitq_a  = "VOMITING",
  burpingq  = "BURPING",
  bloatq    = "BLOATING",
  passwq_a  = "FLATULENCE",
  abdpainq  = "ABDOMINAL PAIN",
  regurgq   = "REGURGITATION",
  diarrhq   = "DIARRHOEA",
  constipq  = "CONSTIPATION",
  thirstq   = "THIRST",
  dryq      = "DRY MOUTH"
)

present_sym_cols <- intersect(names(giq_keyed), names(sym_map))

ce_symptoms <- giq_keyed %>%
  pivot_longer(
    cols = all_of(present_sym_cols),
    names_to = "symptom_var",
    values_to = "symptom_val"
  ) %>%
  filter(!is.na(symptom_val) & symptom_val != "") %>%
  mutate(
    CETERM   = unname(sym_map[symptom_var]),
    CEOCCUR  = if_else(toupper(symptom_val) == "ABSENT", "N", "Y"),
    CESEV    = if_else(CEOCCUR == "N", NA_character_, as.character(symptom_val)),
    CEPRESP  = "Y",
    CEEVINTX = "PAST 24 HOURS",
    # CEDTC as character from GIQ date; drop trailing midnight if any
    CEDTC    = sub("T00:00(:00)?$", "", as.character(gidat))
  ) %>%
  select(usubjid, subjectid, gidat, CETERM, CEOCCUR, CESEV, CEEVINTX, CEPRESP, CEDTC, activityid)

# ------------------------------------------------------------------------------
# Step 2 — Stool record from STOOLNO
# ------------------------------------------------------------------------------

has_stool <- "stoolno" %in% names(giq_keyed)

ce_stool <- if (has_stool) {
  giq_keyed %>%
    filter(!is.na(stoolno)) %>%
    transmute(
      usubjid, subjectid, gidat, activityid,
      CETERM   = "STOOL",
      CESCAT   = "DEFECATION",
      CEEVINTX = "TODAY",
      CEPRESP  = "Y",
      CEOCCUR  = if_else(stoolno == 0, "N", "Y"),
      CESEV    = NA_character_,
      CEDTC    = sub("T00:00(:00)?$", "", as.character(gidat))
    )
} else {
  tibble(usubjid = character(), subjectid = character(), gidat = as.character(character()))
}

# ------------------------------------------------------------------------------
# Step 3 — Combine, stamp SDTM vars, derive sequence, finalize
# ------------------------------------------------------------------------------

ce_combined <- bind_rows(ce_symptoms, ce_stool) %>%
  mutate(
    STUDYID = study,
    DOMAIN  = sdtm_domain,
    SUBJID  = subjectid,
    CECAT   = "GASTROINTESTINAL SYMPTOMS",
    CEEVAL  = "STUDY SUBJECT",
    CEORIG  = "pPRO",
    SOURCEID = "CRF: Gastro Intestinal Tolerance & BSFC"
  ) %>%
  # Order prior to sequence derivation for determinism
  arrange(SUBJID, CEDTC, CETERM)

# After you’ve built CE or EC and have ACTIVITYID present:
ce1 <- derive_tpt_vars(ce_combined, eventid_var = "activityid", sdtm = "CE")


ce1 <- ce1 %>%
  mutate(CELNKID = ifelse(toupper(CETERM) == "STOOL" &
                            toupper(CEOCCUR) == "Y",
                          trimws(CETPT),
                          NA))


ce1 <- ce1 %>% rename_with(toupper)

dm_ce <- dm1 %>% rename_with(toupper)

ce1 <- sdtm.oak::derive_study_day(
      sdtm_in = ce1 ,
      dm_domain = dm_ce,
      tgdt = "CEDTC",
      refdt = "RFSTDTC", 
      study_day_var = "CEDY",
      merge_key = "USUBJID"
)
keys_seq   <- c("STUDYID", "USUBJID", "CETERM", "CEDTC", "CETPTNUM")

# CESEQ per subject (Oak-native)
ce_combined1 <- sdtm.oak::derive_seq(
  tgt_dat   = ce1,
  tgt_var   = "CESEQ",
  rec_vars  = keys_seq,
  ) 

# Create CE domain
CE <- sdtm_create_domain(ce_combined1, keys= "STUDYID, USUBJID, CETERM, CEDTC, CETPTNUM")