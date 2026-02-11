# =============================================================================
# Create Dummy Raw Data for MH (Medical History) Domain
# =============================================================================
# Generates dummy datasets matching the EDC export structure:
#   - mh_meddra.csv  : Medical history CRF data already MedDRA-coded
#
# Also reuses the shared dm.csv and ic.csv from create_dummy_raw.R.
# =============================================================================

library(dplyr)
library(tibble)

DEMO_DIR <- tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) getwd()
)
setwd(DEMO_DIR)
dir.create("raw", showWarnings = FALSE)

# Same 8 subjects as AE/PR dummy data
subjects <- c("NL-AMC-01-0001", "NL-AMC-01-0002", "NL-AMC-01-0003",
              "DE-CHA-02-0004", "DE-CHA-02-0005", "DE-CHA-02-0006",
              "IT-BGM-03-0007", "IT-BGM-03-0008")

set.seed(456)

# =============================================================================
# mh_meddra.csv — Medical History CRF + MedDRA coded data
# =============================================================================
# EDC import produces mh_meddra dataset
# Columns from SAS keep statement:
#   subjectid, usubjid, mhterm, mhstdat, mhendat, mhongo, mhcurmed, mhpr,
#   mhspid, initiateddate, lastediteddate, codedondate,
#   soc_code, soc_name, pt_name, pt_code, pt_soc_name, pt_soc_code,
#   hlt_name, hlt_code, hlgt_name, hlgt_code, llt_name, llt_code,
#   soc_list, dictinstance, version

# MedDRA lookup: medical history conditions
mh_meddra_lookup <- tribble(
  ~mhterm,                        ~llt_name,                  ~llt_code, ~pt_name,                    ~pt_code, ~hlt_name,                   ~hlt_code,
  "Asthma",                       "Asthma",                   10003553,  "Asthma",                    10003553, "Asthma NEC",                10003556,

  "Seasonal allergic rhinitis",   "Seasonal allergic rhinitis",10039766, "Seasonal allergy",          10048908, "Allergic conditions NEC",   10001723,
  "Hypertension",                 "Hypertension",             10020772,  "Hypertension",              10020772, "Vascular hypertensive disorders",10047065,
  "Iron deficiency anaemia",      "Iron deficiency anaemia",  10022972,  "Iron deficiency anaemia",   10022972, "Anaemias NEC",              10002038,
  "Eczema",                       "Eczema",                   10014184,  "Eczema",                    10014184, "Dermatitis and eczema",     10012431,
  "Migraine",                     "Migraine",                 10027599,  "Migraine",                  10027599, "Migraine headaches",        10027602,
  "Gastro-oesophageal reflux",    "GORD",                     10017885,  "Gastro-oesophageal reflux disease",10017885,"Oesophageal disorders NEC",10030202,
  "Appendectomy",                 "Appendectomy",             10002853,  "Appendectomy",              10002853, "Gastrointestinal therapeutic procedures",10070897,
  "Diabetes mellitus type 2",     "Type 2 diabetes mellitus", 10067585,  "Type 2 diabetes mellitus",  10067585, "Diabetes mellitus NEC",     10012607,
  "Anxiety disorder",             "Anxiety disorder",         10002855,  "Anxiety disorder",          10002855, "Anxiety disorders and symptoms",10002903
)
mh_meddra_lookup$hlgt_name <- c(
  "Bronchial disorders (excl neoplasms)",
  "Allergic conditions",
  "Vascular disorders NEC",
  "Anaemias nonhaemolytic and marrow depression",
  "Epidermal and dermal conditions",
  "Headaches NEC",
  "Gastrointestinal motility and defaecation conditions",
  "Gastrointestinal therapeutic procedures",
  "Glucose metabolism disorders (incl diabetes mellitus)",
  "Anxiety disorders and symptoms"
)
mh_meddra_lookup$hlgt_code <- c(10006482, 10001710, 10047065, 10002034,
                                 10014185, 10019231, 10017944, 10070897,
                                 10018420, 10002903)
mh_meddra_lookup$soc_name <- c(
  "Respiratory, thoracic and mediastinal disorders",
  "Immune system disorders",
  "Vascular disorders",
  "Blood and lymphatic system disorders",
  "Skin and subcutaneous tissue disorders",
  "Nervous system disorders",
  "Gastrointestinal disorders",
  "Surgical and medical procedures",
  "Metabolism and nutrition disorders",
  "Psychiatric disorders"
)
mh_meddra_lookup$soc_code <- c(10038738, 10021428, 10047065, 10005329,
                                10040785, 10029205, 10017947, 10042613,
                                10027433, 10037175)
mh_meddra_lookup$pt_soc_name <- mh_meddra_lookup$soc_name
mh_meddra_lookup$pt_soc_code <- mh_meddra_lookup$soc_code

# Generate 2-4 conditions per subject
rows <- list()
spid_counter <- 0
for (subj in subjects) {
  n_cond <- sample(2:4, 1)
  cond_idx <- sample(nrow(mh_meddra_lookup), n_cond)

  for (j in cond_idx) {
    spid_counter <- spid_counter + 1
    cond <- mh_meddra_lookup[j, ]

    # Medical history dates: conditions started 1-10 years before consent
    start_offset <- sample(365:3650, 1)
    start_date <- as.Date("2025-06-01") - start_offset
    # Some conditions are ongoing, some ended before consent
    is_ongoing <- sample(c("Y", "N"), 1, prob = c(0.6, 0.4))
    end_date <- if (is_ongoing == "Y") NA else start_date + sample(30:365, 1)

    # Current medication? (only if ongoing)
    curmeds <- c("Y", "N")
    curmed_val <- if (is_ongoing == "Y") sample(curmeds, 1) else NA_character_

    # Was a procedure done? (proxy for medical intervention)
    mhpr_val <- sample(c("Y", "N", NA_character_), 1, prob = c(0.2, 0.5, 0.3))

    rows <- c(rows, list(tibble(
      subjectid       = subj,
      usubjid         = NA_character_,  # filled during data preparation
      mhterm          = cond$mhterm,
      mhspid          = spid_counter,
      mhstdat         = format(start_date, "%Y-%m-%d"),
      mhendat         = if (!is.na(end_date)) format(end_date, "%Y-%m-%d") else NA_character_,
      mhongo          = is_ongoing,
      mhcurmed        = curmed_val,
      mhpr            = mhpr_val,
      llt_name        = cond$llt_name,
      llt_code        = as.character(cond$llt_code),
      pt_name         = cond$pt_name,
      pt_code         = as.character(cond$pt_code),
      hlt_name        = cond$hlt_name,
      hlt_code        = as.character(cond$hlt_code),
      hlgt_name       = cond$hlgt_name,
      hlgt_code       = as.character(cond$hlgt_code),
      soc_name        = cond$soc_name,
      soc_code        = as.character(cond$soc_code),
      pt_soc_name     = cond$pt_soc_name,
      pt_soc_code     = as.character(cond$pt_soc_code),
      soc_list        = "PRIMARY",
      dictinstance    = "MedDRA 27.0",
      version         = "27.0",
      codedondate     = format(as.Date("2025-07-01") + sample(0:30, 1), "%Y-%m-%d"),
      initiateddate   = format(as.Date("2025-06-15") + sample(0:15, 1), "%Y-%m-%d"),
      lastediteddate  = format(as.Date("2025-07-15") + sample(0:30, 1), "%Y-%m-%d")
    )))
  }
}
mh_meddra <- bind_rows(rows)
write.csv(mh_meddra, "raw/mh_meddra.csv", row.names = FALSE, na = "")
cat("Created raw/mh_meddra.csv:", nrow(mh_meddra), "rows x", ncol(mh_meddra), "cols\n")

cat("\nDone! MH dummy raw data created in raw/\n")
cat("Run create_dummy_raw.R first to get dm.csv and ic.csv (shared).\n")
