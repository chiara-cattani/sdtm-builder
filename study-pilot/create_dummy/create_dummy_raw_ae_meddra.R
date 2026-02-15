# =============================================================================
# Create Dummy Raw Data: ae_meddra.csv (MedDRA Coded Adverse Events)
# =============================================================================
# Run from study-pilot/ directory
# Output: raw/ae_meddra.csv
# Depends: raw/ae.csv (reads AE terms for MedDRA coding)
# =============================================================================

library(tibble)

ae <- read.csv("raw/ae.csv", stringsAsFactors = FALSE)

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
  soc_list    = rep("PRIMARY", 12)
)

# Build ae_meddra by joining ae with meddra_lookup
ae_meddra_rows <- list()
for (r in seq_len(nrow(ae))) {
  term <- ae$AETerm[r]
  m <- meddra_lookup[meddra_lookup$term == term, ]
  if (nrow(m) == 0) m <- meddra_lookup[1, ]  # fallback

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
    SOC_List    = m$soc_list[1],
    DictInstance = "MedDRA 27.0",
    Version      = "27.0",
    CodedOnDate  = ae$AEStDat[r],
    InitiatedDate = ae$AEStDat[r],
    LastEditedDate = ae$AEStDat[r]
  )
}
ae_meddra <- dplyr::bind_rows(ae_meddra_rows)

dir.create("raw", showWarnings = FALSE, recursive = TRUE)
write.csv(ae_meddra, "raw/ae_meddra.csv", row.names = FALSE, na = "")
cat("Created raw/ae_meddra.csv:", nrow(ae_meddra), "rows x", ncol(ae_meddra), "cols\n")
