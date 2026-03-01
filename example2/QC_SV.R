# ******************************************************************************
# Danone Nutricia Research
# ******************************************************************************
# STUDY/PROJECT : SONA / 23REX0061265
# PROGRAM PATH  : SAS/SONA/Files/SDTM Conversion/40 SDTM Programs
# PROGRAM NAME  : QC_SV.R
# PURPOSE       : QC SDTM data for Subject Visits
# ------------------------------------------------------------------------------
# NOTES :
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2025-10-07 - Vikas Shokeen - Initial version
# 2025-10-10 - Vikas Shokeen - Updated the working directory according to both
#							   LSAF and RSTUDIO
# 2025-10-20 - cattanch - Updated to use derive_visit_vars()
# ******************************************************************************

# --- Configuration -------------------------------------------------------------

library(nutriciaconfig)
nutricia_config()

library(dplyr)
library(readxl)
library(haven)
library(sdtm.oak)
library(dplyr)
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

# Parameters
studyid     <- "SONA"
sdtm_domain <- "SV"

# Metadata (if/when needed)
sdtm_ct  <- read_excel("../31 Metadata Data/SONA_CT.xlsx", sheet = "CT")
sdtm_var <- read_sas("../31 Metadata Data/sdtm_3_4_variables.sas7bdat")
sdtm_dom <- read_sas("../31 Metadata Data/sdtm_3_4_domains.sas7bdat")

# --- Inputs -------------------------------------------------------------------

# Raw source via in-house importer + SDTM DM
event_dates_1 <- copy_import(inset = "event_dates")
dm_1          <- read_sas("../41 SDTM Data/dm.sas7bdat")
names(dm_1)   <- toupper(names(dm_1))                   # normalize header case

# Ensure DM reference date is character ISO if not already
if (!inherits(dm_1$RFSTDTC, "character")) {
  dm_1$RFSTDTC <- as.character(dm_1$RFSTDTC)
}

# --- Filter to initiated SCR/V1–V4 events ------------------------------------

.keep_ids <- c("scr", "v1", "v2", "v3", "v4")
src0 <- event_dates_1[
  tolower(event_dates_1$eventstatus) == "initiated" &
    tolower(event_dates_1$eventid) %in% .keep_ids,
]

# --- Create Oak key scaffold (oak_id/raw_source/patient_number) ---------------

sv <- generate_oak_id_vars(
  raw_dat = src0,
  pat_var = "subjectid",
  raw_src = "Viedoc Event"
)

# --- STUDYID & DOMAIN (hardcoded constants via Oak) ---------------------------

sv1 <- hardcode_no_ct(
  tgt_val = studyid,
  raw_dat = sv,
  raw_var = "subjectid",
  tgt_var = "STUDYID"
) %>%
  hardcode_no_ct(
    tgt_val = sdtm_domain,
    raw_dat = sv,
    raw_var = "subjectid",
    tgt_var = "DOMAIN"
  )

# --- Map identifiers & carry source flags -------------------------------------

sv2 <- assign_no_ct(
  tgt_dat = sv1,
  tgt_var = "USUBJID",
  raw_dat = sv,
  raw_var = "usubjid",
  id_vars = oak_id_vars()
) %>%
  assign_no_ct(
    tgt_dat = .,
    tgt_var = "SUBJID",
    raw_dat = sv,
    raw_var = "subjectid",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    tgt_dat = .,
    tgt_var = "EVENTID",
    raw_dat = sv,
    raw_var = "eventid",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    tgt_dat = .,
    tgt_var = "EVENTSTATUS",
    raw_dat = sv,
    raw_var = "eventstatus",
    id_vars = oak_id_vars()
  ) %>%
  mutate(SOURCEID = "Viedoc Event")

# --- SVSTDTC (ISO 8601) from EVENTINITIATEDDATE -------------------------------

sv3 <- assign_datetime(
  tgt_dat = sv2,
  tgt_var = "SVSTDTC",
  raw_dat = sv,
  raw_var = "eventinitiateddate",
  raw_fmt = list(c("y-m-d H:M:S", "y-m-d H:M", "y-m-d", "dd-mmm-yyyy", "d-m-y", "ymd")),
  raw_unk = c("UN", "UNK"),
  id_vars = oak_id_vars()
)

# Strip midnight time (keep date only)
cnd_midnight <- condition_add(
  dat = sv3,
  grepl("T00:00(:00)?$", SVSTDTC)
)
sv4 <- dplyr::mutate(
  cnd_midnight,
  SVSTDTC = sub("T00:00(:00)?$", "", SVSTDTC)
)

# --- SVENDTC = SVSTDTC (preserve iso8601 class) -------------------------------

sv5 <- dplyr::mutate(sv4, SVENDTC = SVSTDTC)

# --- Study day derivations vs DM.RFSTDTC --------------------------------------

dm_ref <- dm_1[, c("USUBJID", "RFSTDTC")]

sv7 <- derive_study_day(
  sdtm_in       = sv5,
  dm_domain     = dm_ref,
  tgdt          = "SVSTDTC",
  refdt         = "RFSTDTC",
  study_day_var = "SVSTDY",
  merge_key     = "USUBJID"
)

sv8 <- derive_study_day(
  sdtm_in       = sv7,
  dm_domain     = dm_ref,
  tgdt          = "SVENDTC",
  refdt         = "RFSTDTC",
  study_day_var = "SVENDY",
  merge_key     = "USUBJID"
)

# --- VISITNUM / VISIT ------------------

sv9 <- derive_visit_vars(sv8, eventid_var = "EVENTID")

# --- SVPRESP / SVOCCUR ----------------------------

sv10 <- sv9 %>%
  mutate(SVPRESP = "Y") %>%        # all pre-specified
  condition_add(!is.na(VISITNUM)) %>%
  dplyr::mutate(SVOCCUR = "Y")     # occurred if VISITNUM present

# --- Finalize SDTM SV ---------------------------------------------------------

sv <- sdtm_create_domain(sv10)

