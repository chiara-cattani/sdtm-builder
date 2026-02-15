# =============================================================================
# Create Dummy Raw Data: lbs_simg.csv (Collection Stool Image CRF)
# =============================================================================
# Run from study-pilot/ directory
# Output: raw/lbs_simg.csv
# =============================================================================

set.seed(299)

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

lbs_simg_rows <- list()
simg_counter <- 0
for (i in seq_len(n_subjects)) {
  n_simg <- sample(2:5, 1)
  for (j in seq_len(n_simg)) {
    simg_counter <- simg_counter + 1
    simg_date <- rfst_dates[i] + sample(1:140, 1)

    lbs_simg_rows[[simg_counter]] <- tibble(
      SubjectId      = subjectids[i],
      lbs_dimg       = format(simg_date, "%Y-%m-%d"),
      InitiatedDate  = format(simg_date, "%Y-%m-%d"),
      LastEditedDate = format(simg_date + sample(0:3, 1), "%Y-%m-%d")
    )
  }
}
lbs_simg <- dplyr::bind_rows(lbs_simg_rows)

dir.create("raw", showWarnings = FALSE, recursive = TRUE)
write.csv(lbs_simg, "raw/lbs_simg.csv", row.names = FALSE, na = "")
cat("Created raw/lbs_simg.csv:", nrow(lbs_simg), "rows x", ncol(lbs_simg), "cols\n")
