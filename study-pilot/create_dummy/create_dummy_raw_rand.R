# =============================================================================
# Create Dummy Raw Data: rand.csv (Randomisation)
# =============================================================================
# Run from study-pilot/ directory
# Output: raw/rand.csv
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

ic_dates   <- date_anchor + sample(-30:0, n_subjects, replace = TRUE)
rand_dates <- ic_dates + sample(1:7, n_subjects, replace = TRUE)

rand <- tibble::tibble(
  SubjectId = subjectids,
  RandDat   = format(rand_dates, "%Y-%m-%d"),
  TrtCd     = sample(c("A", "B"), n_subjects, replace = TRUE),
  Trt       = NA_character_
)
rand$Trt <- ifelse(rand$TrtCd == "A", "Iron supplement", "Placebo")

dir.create("raw", showWarnings = FALSE, recursive = TRUE)
write.csv(rand, "raw/rand.csv", row.names = FALSE, na = "")
cat("Created raw/rand.csv:", nrow(rand), "rows x", ncol(rand), "cols\n")
