# =============================================================================
# Create Dummy Raw Data: ex.csv (Exposure)
# =============================================================================
# Run from study-pilot/ directory
# Output: raw/ex.csv
# Depends: raw/ic.csv (for IC dates / RFSTDTC)
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

ic_dates   <- date_anchor + sample(-30:0, n_subjects, replace = TRUE)
rfst_dates <- ic_dates

# Consume random state to stay in sync with original
invisible(ic_dates + sample(1:7, n_subjects, replace = TRUE))    # rand_dates
invisible(sample(c("F", "M"), n_subjects, replace = TRUE))       # DM Sex
invisible(sample(25:65, n_subjects, replace = TRUE))              # DM Age
invisible(sample(c("NL", "HL"), n_subjects, replace = TRUE))     # DM Ethnicc
invisible(sample(9000:23000, n_subjects))                         # DM BrthDat
# IC: no random beyond ic_dates
# RAND: already consumed
# EOS: consume random for EOS
invisible(sample(c("Y", "N"), n_subjects, replace = TRUE))       # ComplYN
invisible(sample(100:150, n_subjects, replace = TRUE))            # LcDat
invisible(sample(80:120, n_subjects, replace = TRUE))             # RfxEnDat
invisible(sample(100:150, n_subjects, replace = TRUE))            # EventDate

# Now generate EX (matches original random state position)
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

dir.create("raw", showWarnings = FALSE, recursive = TRUE)
write.csv(ex, "raw/ex.csv", row.names = FALSE, na = "")
cat("Created raw/ex.csv:", nrow(ex), "rows x", ncol(ex), "cols\n")
