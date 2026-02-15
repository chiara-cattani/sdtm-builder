# =============================================================================
# Create Dummy Raw Data: dm.csv (Demographics)
# =============================================================================
# Run from study-pilot/ directory
# Output: raw/dm.csv
# =============================================================================

set.seed(42)

library(tibble)

n_subjects <- 8
sites      <- c("SITEA-01", "SITEB-02", "SITEC-03")
countries  <- c("XX", "XX", "XX")
date_anchor <- as.Date("2025-06-01")

subjectids <- character(n_subjects)
for (i in seq_len(n_subjects)) {
  country_idx <- ((i - 1) %% length(countries)) + 1
  subjectids[i] <- sprintf("%s-%s-%04d", countries[country_idx], sites[country_idx], i)
}

# Consume random state to match original sequence (ic_dates, rand_dates)
ic_dates   <- date_anchor + sample(-30:0, n_subjects, replace = TRUE)
rand_dates <- ic_dates + sample(1:7, n_subjects, replace = TRUE)

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

dir.create("raw", showWarnings = FALSE, recursive = TRUE)
write.csv(dm, "raw/dm.csv", row.names = FALSE, na = "")
cat("Created raw/dm.csv:", nrow(dm), "rows x", ncol(dm), "cols\n")
