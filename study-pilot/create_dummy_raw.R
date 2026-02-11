# =============================================================================
# Create Dummy Raw Data for STUDY-PILOT AE Domain Demo
# =============================================================================
# This script generates realistic dummy raw datasets that mirror the structure
# of the STUDY-PILOT EDC CRF exports.
#
# Datasets created:
#   - ae.csv        : Adverse Events CRF page
#   - sae.csv       : Serious Adverse Events CRF page
#   - ae_meddra.csv : MedDRA coded adverse events
#   - dm.csv        : Demographics CRF
#   - ic.csv        : Informed Consent (for RFSTDTC via icdat)
#   - rand.csv      : Randomisation
#   - eos.csv       : End of Study
#   - ex.csv        : Exposure (study product)
#
# SubjectID format: CC-SITE-NN-SSSS  (e.g., NL-AMC-01-0001)
# =============================================================================

set.seed(42)

library(tibble)

# --- Configuration -----------------------------------------------------------
n_subjects <- 8
studyid    <- "STUDY-PILOT"
sites      <- c("AMC-01", "LUH-02", "UHB-03")
countries  <- c("NL", "DE", "UK")
date_anchor <- as.Date("2025-06-01")

# Generate SubjectIDs in the real format: CC-SITE-NN-SSSS
subjectids <- character(n_subjects)
for (i in seq_len(n_subjects)) {
  country_idx <- ((i - 1) %% length(countries)) + 1
  subjectids[i] <- sprintf("%s-%s-%04d", countries[country_idx], sites[country_idx], i)
}

# Per-subject reference dates
ic_dates  <- date_anchor + sample(-30:0, n_subjects, replace = TRUE)
rand_dates <- ic_dates + sample(1:7, n_subjects, replace = TRUE)
rfst_dates <- ic_dates  # RFSTDTC comes from icdat in the real study

# =============================================================================
# 1. DM — Demographics
# =============================================================================
# Variables referenced in DM:
#   subjectid, sitecode, sex, age, ageu, ethnicc, brthdat
dm <- tibble(
  SubjectId  = subjectids,
  SiteCode   = rep(sites, length.out = n_subjects),
  Sex        = sample(c("F", "M"), n_subjects, replace = TRUE, prob = c(0.5, 0.5)),
  Age        = sample(25:65, n_subjects, replace = TRUE),
  AgeU       = rep("YEARS", n_subjects),
  Ethnicc    = sample(c("NOT HISPANIC OR LATINO", "HISPANIC OR LATINO"),
                      n_subjects, replace = TRUE, prob = c(0.85, 0.15)),
  BrthDat    = format(date_anchor - sample(9000:23000, n_subjects), "%Y-%m-%d")
)

# =============================================================================
# 2. IC — Informed Consent
# =============================================================================
# Variables referenced in DM:
#   subjectid, icdat
ic <- tibble(
  SubjectId = subjectids,
  IcDat     = format(ic_dates, "%Y-%m-%d")
)

# =============================================================================
# 3. RAND — Randomisation
# =============================================================================
rand <- tibble(
  SubjectId = subjectids,
  RandDat   = format(rand_dates, "%Y-%m-%d"),
  TrtCd     = sample(c("A", "B"), n_subjects, replace = TRUE),
  Trt       = NA_character_
)
rand$Trt <- ifelse(rand$TrtCd == "A", "Iron supplement", "Placebo")

# =============================================================================
# 4. EOS — End of Study
# =============================================================================
eos <- tibble(
  SubjectId       = subjectids,
  ComplYN         = sample(c("Y", "N"), n_subjects, replace = TRUE, prob = c(0.8, 0.2)),
  LcDat           = format(rfst_dates + sample(100:150, n_subjects, replace = TRUE), "%Y-%m-%d"),
  RfxEnDat        = format(rfst_dates + sample(80:120, n_subjects, replace = TRUE), "%Y-%m-%d"),
  EtDat           = NA_character_,
  EventDate       = format(rfst_dates + sample(100:150, n_subjects, replace = TRUE), "%Y-%m-%d"),
  LastEditedDate  = format(Sys.Date(), "%Y-%m-%d")
)
# For non-completers, set early termination date
non_completers <- which(eos$ComplYN == "N")
eos$EtDat[non_completers] <- format(rfst_dates[non_completers] + sample(30:60, length(non_completers), replace = TRUE), "%Y-%m-%d")

# =============================================================================
# 5. EX — Exposure
# =============================================================================
ex_rows <- list()
ex_counter <- 0
for (i in seq_len(n_subjects)) {
  n_ex <- sample(3:6, 1)
  for (j in seq_len(n_ex)) {
    ex_counter <- ex_counter + 1
    ex_start <- rfst_dates[i] + (j - 1) * 14
    ex_rows[[ex_counter]] <- tibble(
      SubjectId = subjectids[i],
      ExStDat   = format(ex_start, "%Y-%m-%d"),
      ExEnDat   = format(ex_start + 13, "%Y-%m-%d"),
      ExDose    = 10,
      ExDoseU   = "mg"
    )
  }
}
ex <- dplyr::bind_rows(ex_rows)

# =============================================================================
# 6. AE — Adverse Events CRF page
# =============================================================================
# Variables referenced in AE CRF dataset:
#   SubjectId, AESPID, AETerm, AESev, AEOut, AEStDat, AeEnDat,
#   AEACNp, AEACNpSp, AEACNs0 (other action 0), AEACNsSp (other action specify)
#   AERel1 (relationship to IP), AeRel1Co (rel comment), AeRel2 (relationship
#   to comparator), AeRel2Co (rel2 comment), AeSequel1.
#   AeStPer (AE period relative to first intake), AeOnGo, AeFu,
#   AeCoval (AE description/comment)
ae_terms_pool <- c("Headache", "Nausea", "Dizziness", "Fatigue",
                   "Abdominal pain", "Constipation", "Diarrhoea",
                   "Vomiting", "Rash", "Insomnia", "Back pain",
                   "Nasopharyngitis")
sev_values   <- c("Mild", "Moderate", "Severe")
out_values   <- c("Recovered / Resolved", "Recovering / Resolving",
                  "Not Recovered / Not Resolved", "Recovered / Resolved with Sequelae",
                  "Fatal")
acn_values   <- c("NONE", "DOSE REDUCED", "DRUG INTERRUPTED",
                  "DRUG WITHDRAWN", "NOT APPLICABLE", "NA")
rel_values   <- c("Not Related", "Possibly", "Probably", "Unlikely", "Definitely", "NA")
periodo_values <- c("Before first product intake",
                    "After first product intake", NA_character_)

ae_rows <- list()
ae_counter <- 0
for (i in seq_len(n_subjects)) {
  n_ae <- sample(1:5, 1)
  for (j in seq_len(n_ae)) {
    ae_counter <- ae_counter + 1
    ae_start_offset <- sample(-5:80, 1)
    ae_start <- rfst_dates[i] + ae_start_offset
    ae_dur   <- sample(3:30, 1)
    ae_end   <- ae_start + ae_dur

    # ~15% ongoing
    ongoing <- runif(1) < 0.15
    # ~10% partial start date
    partial <- runif(1) < 0.10

    ae_rows[[ae_counter]] <- tibble(
      SubjectId  = subjectids[i],
      AESPID     = ae_counter,
      AETerm     = sample(ae_terms_pool, 1),
      AESev      = sample(sev_values, 1),
      AEOut      = if (ongoing) "Not Recovered / Not Resolved" else sample(out_values[-5], 1),
      AEStDat    = if (partial) format(ae_start, "%Y-%m") else format(ae_start, "%Y-%m-%d"),
      AeEnDat    = if (ongoing) NA_character_ else format(ae_end, "%Y-%m-%d"),
      AEACNp     = sample(acn_values, 1),
      AEACNpSp   = NA_character_,
      AEACNS0    = NA_character_,
      AEACNS1    = NA_character_,   # checkbox: Concomitant medication given
      AEACNS2    = NA_character_,   # checkbox: Non-drug therapy
      AEACNsSp   = NA_character_,
      AERel1     = sample(rel_values, 1),
      AeRel1Co   = NA_character_,
      AERel2     = sample(rel_values, 1),
      AeRel2Co   = NA_character_,
      AeSequel1  = NA_character_,
      AeStPer    = sample(periodo_values, 1),
      AeOnGo     = if (ongoing) "Y" else "N",
      AeFu       = sample(c("Y", "N", NA_character_), 1),
      AeCoval    = NA_character_,
      # Audit trail dates
      CodedOnDate    = format(ae_start + sample(1:5, 1), "%Y-%m-%d"),
      InitiatedDate  = format(ae_start, "%Y-%m-%d"),
      LastEditedDate = format(ae_start + sample(1:10, 1), "%Y-%m-%d")
    )
  }
}
ae <- dplyr::bind_rows(ae_rows)

# Add some "other action" values for a few AEs
other_action_idx <- sample(nrow(ae), min(3, nrow(ae)))
ae$AEACNS0[other_action_idx] <- "Other medication given"
ae$AEACNsSp[other_action_idx] <- "Paracetamol prescribed"

# Populate checkbox other-action columns (matching EDC checkbox export)
checkbox_idx <- sample(nrow(ae), min(5, nrow(ae)))
ae$AEACNS1[checkbox_idx[1:min(3, length(checkbox_idx))]] <- "Concomitant medication given"
ae$AEACNS2[checkbox_idx[1:min(2, length(checkbox_idx))]] <- "Non-drug therapy"

# Add sequel for some resolved with sequelae
sequel_idx <- which(ae$AEOut == "Recovered / Resolved with Sequelae")
if (length(sequel_idx) > 0) {
  ae$AeSequel1[sequel_idx] <- "Mild residual symptoms"
}

# Add description for some AEs
desc_idx <- sample(nrow(ae), min(4, nrow(ae)))
ae$AeCoval[desc_idx] <- "Patient reported mild discomfort"

# Add relationship comments for related events
rel_idx <- which(ae$AERel1 %in% c("Definitely", "Possibly", "Probably"))
if (length(rel_idx) > 0) {
  ae$AeRel1Co[rel_idx[1:min(2, length(rel_idx))]] <- "Temporal relationship observed"
}

# =============================================================================
# 7. SAE — Serious Adverse Events
# =============================================================================
# Variables referenced in AE.sas (sae dataset):
#   SubjectId, SAESPID, AeSDth, AeSLife, AeSHosp, AeSDiSab,
#   AeSCong, AeSMie, AeSDtDat (SAE death date)
# Some AEs are serious — assign SAE flags to ~20% of AEs
n_sae <- max(1, round(nrow(ae) * 0.2))
sae_idx <- sort(sample(seq_len(nrow(ae)), n_sae))

sae_rows <- list()
for (k in seq_along(sae_idx)) {
  idx <- sae_idx[k]
  sae_rows[[k]] <- tibble(
    SubjectId = ae$SubjectId[idx],
    SAESPID   = ae$AESPID[idx],
    AeSDth    = sample(c("N", "N", "N", "N", "N"), 1),  # mostly N
    AeSLife   = sample(c("Y", "N", "N", "N"), 1),
    AeSHosp   = sample(c("Y", "Y", "N", "N"), 1),
    AeSDiSab  = sample(c("Y", "N", "N", "N", "N"), 1),
    AeSCong   = "N",
    AeSMie    = sample(c("Y", "N", "N"), 1),
    AeSDtDat  = NA_character_   # death date, almost always NA
  )
}
sae <- dplyr::bind_rows(sae_rows)

# =============================================================================
# 8. AE_MEDDRA — MedDRA Coded Terms
# =============================================================================
# Variables referenced in AE.sas (ae_meddra dataset):
#   SubjectId, AESPID, LLT_Name, LLT_Code, PT_Name, PT_Code,
#   HLT_Name, HLT_Code, HLGT_Name, HLGT_Code,
#   SOC_Name, SOC_Code, PT_SOC_Name, PT_SOC_Code, SOC_List

# MedDRA lookup (simplified, realistic terms + codes)
meddra_lookup <- tibble(
  term        = c("Headache", "Nausea", "Dizziness", "Fatigue",
                  "Abdominal pain", "Constipation", "Diarrhoea",
                  "Vomiting", "Rash", "Insomnia", "Back pain",
                  "Nasopharyngitis"),
  llt_name    = c("Headache", "Nausea", "Dizziness", "Fatigue",
                  "Abdominal pain", "Constipation", "Diarrhoea",
                  "Vomiting", "Rash", "Insomnia", "Back pain",
                  "Nasopharyngitis"),
  llt_code    = c("10019211", "10028813", "10013573", "10016256",
                  "10000081", "10010774", "10012735",
                  "10047700", "10037844", "10022437", "10003988",
                  "10028810"),
  pt_name     = c("Headache", "Nausea", "Dizziness", "Fatigue",
                  "Abdominal pain", "Constipation", "Diarrhoea",
                  "Vomiting", "Rash", "Insomnia", "Back pain",
                  "Nasopharyngitis"),
  pt_code     = c("10019211", "10028813", "10013573", "10016256",
                  "10000081", "10010774", "10012735",
                  "10047700", "10037844", "10022437", "10003988",
                  "10028810"),
  hlt_name    = c("Headaches NEC", "Nausea and vomiting symptoms",
                  "Dizziness (excl vertigo)", "Asthenic conditions",
                  "Gastrointestinal and abdominal pains",
                  "Constipation", "Diarrhoea (excl infective)",
                  "Nausea and vomiting symptoms",
                  "Rashes, eruptions and exanthems NEC",
                  "Disturbances in initiating and maintaining sleep",
                  "Back disorders", "Viral upper respiratory tract infections"),
  hlt_code    = c("10019233", "10028817", "10013578", "10003550",
                  "10017999", "10010776", "10012738",
                  "10028817", "10037868", "10013395",
                  "10003997", "10047461"),
  hlgt_name   = c("Headaches", "Gastrointestinal signs and symptoms",
                  "Neurological signs and symptoms NEC",
                  "General system disorders NEC",
                  "Gastrointestinal signs and symptoms",
                  "Gastrointestinal motility and defaecation conditions",
                  "Gastrointestinal motility and defaecation conditions",
                  "Gastrointestinal signs and symptoms",
                  "Epidermal and dermal conditions",
                  "Sleep disturbances (incl subtypes)",
                  "Spinal disorders",
                  "Upper respiratory tract infections"),
  hlgt_code   = c("10019231", "10017996", "10029282",
                  "10018060", "10017996", "10017957",
                  "10017957", "10017996", "10014947",
                  "10040984", "10041568", "10046300"),
  soc_name    = c("Nervous system disorders", "Gastrointestinal disorders",
                  "Nervous system disorders",
                  "General disorders and administration site conditions",
                  "Gastrointestinal disorders", "Gastrointestinal disorders",
                  "Gastrointestinal disorders", "Gastrointestinal disorders",
                  "Skin and subcutaneous tissue disorders",
                  "Psychiatric disorders",
                  "Musculoskeletal and connective tissue disorders",
                  "Infections and infestations"),
  soc_code    = c("10029205", "10017947", "10029205",
                  "10018065", "10017947", "10017947",
                  "10017947", "10017947", "10040785",
                  "10037175", "10028395", "10021881"),
  pt_soc_name = c("Nervous system disorders", "Gastrointestinal disorders",
                  "Nervous system disorders",
                  "General disorders and administration site conditions",
                  "Gastrointestinal disorders", "Gastrointestinal disorders",
                  "Gastrointestinal disorders", "Gastrointestinal disorders",
                  "Skin and subcutaneous tissue disorders",
                  "Psychiatric disorders",
                  "Musculoskeletal and connective tissue disorders",
                  "Infections and infestations"),
  pt_soc_code = c("10029205", "10017947", "10029205",
                  "10018065", "10017947", "10017947",
                  "10017947", "10017947", "10040785",
                  "10037175", "10028395", "10021881"),
  soc_list    = c("P", "P", "P", "P", "P", "P",
                  "P", "P", "P", "P", "P", "P")
)

# Build ae_meddra by joining ae with meddra_lookup
ae_meddra_rows <- list()
for (r in seq_len(nrow(ae))) {
  term <- ae$AETerm[r]
  m <- meddra_lookup[meddra_lookup$term == term, ]
  if (nrow(m) == 0) {
    m <- meddra_lookup[1, ]  # fallback
  }
  ae_meddra_rows[[r]] <- tibble(
    SubjectId   = ae$SubjectId[r],
    AESPID      = ae$AESPID[r],
    LLT_Name    = m$llt_name[1],
    LLT_Code    = m$llt_code[1],
    PT_Name     = m$pt_name[1],
    PT_Code     = m$pt_code[1],
    HLT_Name    = m$hlt_name[1],
    HLT_Code    = m$hlt_code[1],
    HLGT_Name   = m$hlgt_name[1],
    HLGT_Code   = m$hlgt_code[1],
    SOC_Name    = m$soc_name[1],
    SOC_Code    = m$soc_code[1],
    PT_SOC_Name = m$pt_soc_name[1],
    PT_SOC_Code = m$pt_soc_code[1],
    SOC_List    = m$soc_list[1]
  )
}
ae_meddra <- dplyr::bind_rows(ae_meddra_rows)

# =============================================================================
# Write CSV files to raw/ folder
# =============================================================================
raw_dir <- tryCatch(
  file.path(dirname(rstudioapi::getSourceEditorContext()$path), "raw"),
  error = function(e) file.path(getwd(), "raw")
)

dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)

write.csv(ae,        file.path(raw_dir, "ae.csv"),        row.names = FALSE, na = "")
write.csv(sae,       file.path(raw_dir, "sae.csv"),       row.names = FALSE, na = "")
write.csv(ae_meddra, file.path(raw_dir, "ae_meddra.csv"), row.names = FALSE, na = "")
write.csv(dm,        file.path(raw_dir, "dm.csv"),        row.names = FALSE, na = "")
write.csv(ic,        file.path(raw_dir, "ic.csv"),        row.names = FALSE, na = "")
write.csv(rand,      file.path(raw_dir, "rand.csv"),      row.names = FALSE, na = "")
write.csv(eos,       file.path(raw_dir, "eos.csv"),       row.names = FALSE, na = "")
write.csv(ex,        file.path(raw_dir, "ex.csv"),        row.names = FALSE, na = "")

cat("Wrote dummy raw data to:", raw_dir, "\n")
cat("Datasets:\n")
cat("  ae.csv         :", nrow(ae), "rows x", ncol(ae), "cols\n")
cat("  sae.csv        :", nrow(sae), "rows x", ncol(sae), "cols\n")
cat("  ae_meddra.csv  :", nrow(ae_meddra), "rows x", ncol(ae_meddra), "cols\n")
cat("  dm.csv         :", nrow(dm), "rows x", ncol(dm), "cols\n")
cat("  ic.csv         :", nrow(ic), "rows x", ncol(ic), "cols\n")
cat("  rand.csv       :", nrow(rand), "rows x", ncol(rand), "cols\n")
cat("  eos.csv        :", nrow(eos), "rows x", ncol(eos), "cols\n")
cat("  ex.csv         :", nrow(ex), "rows x", ncol(ex), "cols\n")
