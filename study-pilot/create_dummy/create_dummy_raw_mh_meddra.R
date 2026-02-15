# =============================================================================
# Create Dummy Raw Data: mh_meddra.csv (MedDRA Coded Medical History)
# =============================================================================
# Run from study-pilot/ directory
# Output: raw/mh_meddra.csv
# Depends: raw/mh.csv (reads MH terms for MedDRA coding)
# =============================================================================

library(tibble)

mh <- read.csv("raw/mh.csv", stringsAsFactors = FALSE)

# MedDRA lookup for medical history conditions
mh_meddra_lookup <- tribble(
  ~mhterm,                        ~llt_name,                    ~llt_code,   ~pt_name,                       ~pt_code,    ~hlt_name,                           ~hlt_code,
  "Asthma",                       "Asthma",                     "10003553",  "Asthma",                       "10003553",  "Asthma NEC",                        "10003556",
  "Seasonal allergic rhinitis",   "Seasonal allergic rhinitis", "10039766",  "Seasonal allergy",             "10048908",  "Allergic conditions NEC",           "10001723",
  "Hypertension",                 "Hypertension",               "10020772",  "Hypertension",                 "10020772",  "Vascular hypertensive disorders",   "10047065",
  "Iron deficiency anaemia",      "Iron deficiency anaemia",    "10022972",  "Iron deficiency anaemia",      "10022972",  "Anaemias NEC",                      "10002038",
  "Eczema",                       "Eczema",                     "10014184",  "Eczema",                       "10014184",  "Dermatitis and eczema",             "10012431",
  "Migraine",                     "Migraine",                   "10027599",  "Migraine",                     "10027599",  "Migraine headaches",                "10027602",
  "Gastro-oesophageal reflux",    "GORD",                       "10017885",  "Gastro-oesophageal reflux disease","10017885","Oesophageal disorders NEC",        "10030202",
  "Appendectomy",                 "Appendectomy",               "10002853",  "Appendectomy",                 "10002853",  "Gastrointestinal therapeutic procedures","10070897",
  "Diabetes mellitus type 2",     "Type 2 diabetes mellitus",   "10067585",  "Type 2 diabetes mellitus",     "10067585",  "Diabetes mellitus NEC",             "10012607",
  "Anxiety disorder",             "Anxiety disorder",           "10002855",  "Anxiety disorder",             "10002855",  "Anxiety disorders and symptoms",    "10002903"
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
mh_meddra_lookup$hlgt_code <- c("10006482", "10001710", "10047065", "10002034",
                                 "10014185", "10019231", "10017944", "10070897",
                                 "10018420", "10002903")
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
mh_meddra_lookup$soc_code <- c("10038738", "10021428", "10047065", "10005329",
                                "10040785", "10029205", "10017947", "10042613",
                                "10027433", "10037175")
mh_meddra_lookup$pt_soc_name <- mh_meddra_lookup$soc_name
mh_meddra_lookup$pt_soc_code <- mh_meddra_lookup$soc_code

meddra_rows <- list()
for (r in seq_len(nrow(mh))) {
  term <- mh$MHTerm[r]
  m <- mh_meddra_lookup[mh_meddra_lookup$mhterm == term, ]
  if (nrow(m) == 0L) m <- mh_meddra_lookup[1, ]

  meddra_rows[[r]] <- tibble(
    SubjectId   = mh$SubjectId[r],
    MHSPID      = mh$MHSPID[r],
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
    SOC_List    = "PRIMARY",
    DictInstance = "MedDRA 27.0",
    Version      = "27.0",
    CodedOnDate  = format(as.Date("2025-07-01") + sample(0:30, 1), "%Y-%m-%d"),
    InitiatedDate = format(as.Date("2025-06-15") + sample(0:15, 1), "%Y-%m-%d"),
    LastEditedDate = format(as.Date("2025-07-15") + sample(0:30, 1), "%Y-%m-%d")
  )
}
mh_meddra <- dplyr::bind_rows(meddra_rows)

dir.create("raw", showWarnings = FALSE, recursive = TRUE)
write.csv(mh_meddra, "raw/mh_meddra.csv", row.names = FALSE, na = "")
cat("Created raw/mh_meddra.csv:", nrow(mh_meddra), "rows x", ncol(mh_meddra), "cols\n")
