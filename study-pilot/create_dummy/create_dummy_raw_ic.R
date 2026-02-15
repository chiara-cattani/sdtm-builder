# =============================================================================
# Create Dummy Raw Data: ic.csv (Informed Consent)
# =============================================================================
# Run from study-pilot/ directory
# Output: raw/ic.csv
# =============================================================================

set.seed(42)

n_subjects <- 8
sites      <- c("SITEA-01", "SITEB-02", "SITEC-03")
countries  <- c("XX", "XX", "XX")
date_anchor <- as.Date("2025-06-01")

subjectids <- character(n_subjects)
for (i in seq_len(n_subjects)) {
  country_idx <- ((i - 1) %% length(countries)) + 1
  subjectids[i] <- sprintf("%s-%s-%04d", countries[country_idx], sites[country_idx], i)
}

ic_dates <- date_anchor + sample(-30:0, n_subjects, replace = TRUE)

ic <- tibble::tibble(
  SubjectId = subjectids,
  IcDat     = format(ic_dates, "%Y-%m-%d")
)

dir.create("raw", showWarnings = FALSE, recursive = TRUE)
write.csv(ic, "raw/ic.csv", row.names = FALSE, na = "")
cat("Created raw/ic.csv:", nrow(ic), "rows x", ncol(ic), "cols\n")
