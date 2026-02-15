# =============================================================================
# Create Dummy Raw Data: review_status.csv (CRF Form Name Lookup)
# =============================================================================
# Run from study-pilot/ directory
# Output: raw/review_status.csv
# No dependencies - static lookup table
# =============================================================================

review_status <- tibble::tibble(
  FormId   = c("AE", "MH", "PR", "DM", "CM", "EX", "EC", "LB", "VS",
               "QS", "SV", "DS", "IE", "SC", "CO", "FA", "BE", "CE",
               "ML", "XS", "TI", "TV", "SAE", "LBS_IMG", "LBS_SIMG"),
  FormName = c("Adverse Events", "Medical History", "Procedures",
               "Demographics", "Concomitant Medications", "Exposure",
               "Exposure as Collected", "Lab Tests", "Vital Signs",
               "Questionnaires", "Subject Visits", "Disposition",
               "Inclusion/Exclusion", "Subject Characteristics",
               "Comments", "Findings About", "Biospecimen Events",
               "Clinical Events", "Meals", "Extra Scores",
               "Trial Inclusion/Exclusion", "Trial Visits",
               "Serious Adverse Events", "Stool Images",
               "Stool Image Scoring")
)

dir.create("raw", showWarnings = FALSE, recursive = TRUE)
write.csv(review_status, "raw/review_status.csv", row.names = FALSE, na = "")
cat("Created raw/review_status.csv:", nrow(review_status), "rows x", ncol(review_status), "cols\n")
