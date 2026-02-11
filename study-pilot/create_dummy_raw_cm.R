# =============================================================================
# Create Dummy Raw Data for CM (Concomitant Medications) Domain
# =============================================================================
# Generates dummy datasets matching the EDC export structure:
#   - cm_whodrug.csv  : Concomitant medications CRF data with WHODrug coding
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

# Same 8 subjects as other dummy data
subjects <- c("NL-AMC-01-0001", "NL-AMC-01-0002", "NL-AMC-01-0003",
              "DE-CHA-02-0004", "DE-CHA-02-0005", "DE-CHA-02-0006",
              "IT-BGM-03-0007", "IT-BGM-03-0008")

set.seed(789)

# =============================================================================
# cm_whodrug.csv — Concomitant Medications CRF + WHODrug coded data
# =============================================================================
# EDC import produces cm_whodrug dataset
# Key columns from the SAS data step:
#   subjectid, usubjid, cmtrt, cmdecod, cmspid, cmstdat, cmendat, cmongo,
#   indicat, cmclas, cmclascd, cmdostxt, cmdosu, cmfrq, cmfrqsp, cmroute,
#   cmatc1-cmatc4, cmindsp,
#   codedondate, initiateddate, lastediteddate

# Medication lookup: common concomitant medications
med_lookup <- tribble(
  ~cmtrt,              ~cmdecod,            ~cmclas,                         ~cmclascd,     ~cmatc1,                  ~cmatc2,                     ~cmatc3,                 ~cmatc4,
  "PARACETAMOL",       "PARACETAMOL",       "ANALGESICS",                    "N02",         "NERVOUS SYSTEM",         "ANALGESICS",                "OTHER ANALGESICS AND ANTIPYRETICS","ANILIDES",
  "IBUPROFEN",         "IBUPROFEN",         "ANTI-INFLAMMATORY PRODUCTS",    "M01",         "MUSCULO-SKELETAL SYSTEM","ANTI-INFLAMMATORY PRODUCTS","ANTIRHEUMATIC PRODUCTS","PROPIONIC ACID DERIVATIVES",
  "OMEPRAZOLE",        "OMEPRAZOLE",        "DRUGS FOR ACID RELATED DISORDERS","A02",       "ALIMENTARY TRACT AND METABOLISM","DRUGS FOR ACID RELATED DISORDERS","DRUGS FOR PEPTIC ULCER","PROTON PUMP INHIBITORS",
  "IRON SUPPLEMENT",   "FERROUS SULPHATE",  "ANTIANAEMIC PREPARATIONS",      "B03",         "BLOOD AND BLOOD FORMING ORGANS","ANTIANAEMIC PREPARATIONS","IRON PREPARATIONS","IRON, BIVALENT, ORAL PREPARATIONS",
  "VITAMIN D",         "COLECALCIFEROL",    "VITAMINS",                      "A11",         "ALIMENTARY TRACT AND METABOLISM","VITAMINS","VITAMIN A AND D","VITAMIN D AND ANALOGUES",
  "SALBUTAMOL",        "SALBUTAMOL",        "ANTI-ASTHMA AND COPD PRODUCTS", "R03",         "RESPIRATORY SYSTEM",     "ANTI-ASTHMA AND COPD PRODUCTS","ADRENERGICS FOR SYSTEMIC USE","SELECTIVE BETA-2 AGONISTS",
  "METFORMIN",         "METFORMIN",         "DRUGS USED IN DIABETES",        "A10",         "ALIMENTARY TRACT AND METABOLISM","DRUGS USED IN DIABETES","BLOOD GLUCOSE LOWERING DRUGS","BIGUANIDES",
  "CETIRIZINE",        "CETIRIZINE",        "ANTIHISTAMINES FOR SYSTEMIC USE","R06",         "RESPIRATORY SYSTEM",     "ANTIHISTAMINES FOR SYSTEMIC USE","ANTIHISTAMINES FOR SYSTEMIC USE NOS","PIPERAZINE DERIVATIVES",
  "CALCIUM CARBONATE", "CALCIUM CARBONATE", "MINERAL SUPPLEMENTS",           "A12",         "ALIMENTARY TRACT AND METABOLISM","MINERAL SUPPLEMENTS","CALCIUM","CALCIUM",
  "MULTIVITAMIN",      "MULTIVITAMINS",     "VITAMINS",                      "A11",         "ALIMENTARY TRACT AND METABOLISM","VITAMINS","MULTIVITAMINS","MULTIVITAMINS WITH MINERALS"
)

dose_units    <- c("mg", "mcg", "IU", "ml")
dose_freqs    <- c("QD", "BID", "TID", "PRN", "QW")
routes        <- c("ORAL", "ORAL", "ORAL", "ORAL", "INHALATION", "TOPICAL")
indications   <- c("HEADACHE", "PAIN", "REFLUX", "IRON DEFICIENCY",
                    "VITAMIN SUPPLEMENTATION", "ASTHMA", "DIABETES",
                    "ALLERGY", "CALCIUM SUPPLEMENTATION", "NUTRITIONAL SUPPORT")

# Generate 2-5 medications per subject
rows <- list()
spid_counter <- 0
for (subj in subjects) {
  n_meds <- sample(2:5, 1)
  med_idx <- sample(nrow(med_lookup), n_meds)

  for (j in med_idx) {
    spid_counter <- spid_counter + 1
    med <- med_lookup[j, ]

    # Start date: some before consent, some after
    start_offset <- sample(-365:180, 1)
    start_date <- as.Date("2025-06-01") + start_offset

    # Ongoing? (60% ongoing, 40% ended)
    is_ongoing <- sample(c("Y", "N"), 1, prob = c(0.6, 0.4))
    end_date <- if (is_ongoing == "Y") NA else start_date + sample(14:180, 1)

    # Dose info
    dose_val <- sample(c("100", "200", "500", "1000", "10 mg", "20 ml"), 1)
    # If dose_val is purely numeric → CMDOSE = number, CMDOSTXT = ""
    # If dose_val has text → CMDOSTXT = original, CMDOSE = NA
    is_numeric_dose <- grepl("^[0-9,\\.]+$", dose_val)
    cmdostxt_val <- dose_val
    cmdose_val <- if (is_numeric_dose) as.numeric(gsub(",", ".", dose_val)) else NA_real_

    freq_val   <- sample(dose_freqs, 1)
    freq_sp    <- if (freq_val == "PRN") sample(c("as needed", "when required"), 1) else NA_character_
    route_val  <- sample(routes, 1)
    unit_val   <- if (is_numeric_dose) sample(dose_units, 1) else NA_character_
    indic_val  <- indications[j]
    indic_sp   <- if (runif(1) < 0.1) "OTHER INDICATION" else NA_character_

    rows <- c(rows, list(tibble(
      subjectid       = subj,
      usubjid         = NA_character_,
      cmtrt           = med$cmtrt,
      cmdecod         = med$cmdecod,
      cmspid          = spid_counter,
      cmstdat         = format(start_date, "%Y-%m-%d"),
      cmendat         = if (!is.na(end_date)) format(end_date, "%Y-%m-%d") else NA_character_,
      cmongo          = is_ongoing,
      cmdostxt        = cmdostxt_val,
      cmdose          = cmdose_val,
      cmdosu          = unit_val,
      cmfrq           = freq_val,
      cmfrqsp         = freq_sp,
      cmroute         = route_val,
      indicat         = indic_val,
      cmindsp         = indic_sp,
      cmclas          = med$cmclas,
      cmclascd        = med$cmclascd,
      cmatc1          = med$cmatc1,
      cmatc2          = med$cmatc2,
      cmatc3          = med$cmatc3,
      cmatc4          = med$cmatc4,
      codedondate     = format(as.Date("2025-07-01") + sample(0:30, 1), "%Y-%m-%d"),
      initiateddate   = format(as.Date("2025-06-15") + sample(0:15, 1), "%Y-%m-%d"),
      lastediteddate  = format(as.Date("2025-07-15") + sample(0:30, 1), "%Y-%m-%d")
    )))
  }
}
cm_whodrug <- bind_rows(rows)
write.csv(cm_whodrug, "raw/cm_whodrug.csv", row.names = FALSE, na = "")
cat("Created raw/cm_whodrug.csv:", nrow(cm_whodrug), "rows x", ncol(cm_whodrug), "cols\n")

cat("\nDone! CM dummy raw data created in raw/\n")
cat("Run create_dummy_raw.R first to get dm.csv and ic.csv (shared).\n")
