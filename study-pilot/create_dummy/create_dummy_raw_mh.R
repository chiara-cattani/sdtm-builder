# =============================================================================
# Create Dummy Raw Data: mh.csv (Medical History CRF)
# =============================================================================
# Run from study-pilot/ directory
# Output: raw/mh.csv
# =============================================================================

set.seed(456)

library(dplyr)
library(tibble)

dir.create("raw", showWarnings = FALSE)

subjects <- c("XX-SITEA-01-0001", "XX-SITEA-01-0002", "XX-SITEA-01-0003",
              "XX-SITEB-02-0004", "XX-SITEB-02-0005", "XX-SITEB-02-0006",
              "XX-SITEC-03-0007", "XX-SITEC-03-0008")

mh_terms <- c("Asthma", "Seasonal allergic rhinitis", "Hypertension",
              "Iron deficiency anaemia", "Eczema", "Migraine",
              "Gastro-oesophageal reflux", "Appendectomy",
              "Diabetes mellitus type 2", "Anxiety disorder")

mh_rows <- list()
spid_counter <- 0

for (subj in subjects) {
  n_cond <- sample(2:4, 1)
  cond_idx <- sample(length(mh_terms), n_cond)

  for (j in cond_idx) {
    spid_counter <- spid_counter + 1

    start_offset <- sample(365:3650, 1)
    start_date <- as.Date("2025-06-01") - start_offset
    is_ongoing <- sample(c("Y", "N"), 1, prob = c(0.6, 0.4))
    end_date <- if (is_ongoing == "Y") NA else start_date + sample(30:365, 1)

    curmed_val <- if (is_ongoing == "Y") sample(c("Y", "N"), 1) else NA_character_
    mhpr_val <- sample(c("Y", "N", NA_character_), 1, prob = c(0.2, 0.5, 0.3))

    mh_rows <- c(mh_rows, list(tibble(
      SubjectId       = subj,
      MHSPID          = spid_counter,
      MHTerm          = mh_terms[j],
      MHStDat         = format(start_date, "%Y-%m-%d"),
      MHEnDat         = if (!is.na(end_date)) format(end_date, "%Y-%m-%d") else NA_character_,
      MHOnGo          = is_ongoing,
      MHCurMed        = curmed_val,
      MHPR            = mhpr_val,
      CodedOnDate     = format(as.Date("2025-07-01") + sample(0:30, 1), "%Y-%m-%d"),
      InitiatedDate   = format(as.Date("2025-06-15") + sample(0:15, 1), "%Y-%m-%d"),
      LastEditedDate  = format(as.Date("2025-07-15") + sample(0:30, 1), "%Y-%m-%d")
    )))
  }
}

mh <- bind_rows(mh_rows)

write.csv(mh, "raw/mh.csv", row.names = FALSE, na = "")
cat("Created raw/mh.csv:", nrow(mh), "rows x", ncol(mh), "cols\n")
