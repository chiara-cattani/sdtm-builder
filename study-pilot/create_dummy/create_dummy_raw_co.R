# =============================================================================
# Create Dummy Raw Data: co.csv (Comments)
# =============================================================================
# Run from study-pilot/ directory
# Output: raw/co.csv
# =============================================================================

set.seed(99)

library(tibble)

subjectids <- c(
  "XX-SITEA-01-0001", "XX-SITEB-02-0002", "XX-SITEC-03-0003",
  "XX-SITEA-01-0004", "XX-SITEB-02-0005", "XX-SITEC-03-0006",
  "XX-SITEA-01-0007", "XX-SITEB-02-0008"
)

# Baseline dates (approx study start)
base_dates <- as.Date(c(
  "2025-05-18", "2025-05-06", "2025-05-02", "2025-05-26",
  "2025-05-11", "2025-05-05", "2025-05-19", "2025-05-27"
))

comments <- c(
  "Subject reported mild nausea after first dose",
  "Stool sample collected before study product intake",
  "Subject requested phone call follow-up",
  "Diary returned incomplete - week 5 missing",
  "Study product taste acceptable per parent report",
  "Blood draw unsuccessful on first attempt",
  "Subject missed visit due to travel",
  "Dose adjustment discussed with investigator",
  "Adverse event resolved without intervention",
  "Subject reported improved appetite",
  "Spirometry test not performed due to equipment malfunction",
  "Additional informed consent obtained for sub-study"
)

refs <- c(
  "SCREENING", "VISIT 1", "PHONE CALL", "DIARY",
  "VISIT 2", "VISIT 1", "VISIT 3", "VISIT 2",
  "ADVERSE EVENT", "VISIT 3", "VISIT 1", "SCREENING"
)

# Generate ~12 comments across subjects
n_comments <- 12
co <- tibble(
  SubjectId = subjectids[c(1, 1, 2, 3, 4, 4, 5, 6, 6, 7, 7, 8)],
  CoSpId    = sprintf("CO-%03d", seq_len(n_comments)),
  CoRef     = refs,
  CoDtc     = format(
    base_dates[c(1, 1, 2, 3, 4, 4, 5, 6, 6, 7, 7, 8)] +
      sample(0:60, n_comments, replace = TRUE),
    "%Y-%m-%d"
  ),
  CoVal     = comments
)

dir.create("raw", showWarnings = FALSE, recursive = TRUE)
write.csv(co, "raw/co.csv", row.names = FALSE)
cat("Wrote", nrow(co), "rows to raw/co.csv\n")
