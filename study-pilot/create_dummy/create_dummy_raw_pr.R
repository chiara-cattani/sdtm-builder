# =============================================================================
# Create Dummy Raw Data: pr.csv (Medical Procedures CRF)
# =============================================================================
# Run from study-pilot/ directory
# Output: raw/pr.csv
# =============================================================================

set.seed(99)

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

pr_terms_pool <- c(
  "Blood transfusion", "Appendectomy", "Tonsillectomy",
  "Vaccination", "Dental cleaning", "Physical examination",
  "Circumcision", "Caesarean section", "Colonoscopy",
  "MRI scan", "Ultrasound", "X-ray"
)

indication_pool <- c(
  "DIAGNOSTIC", "THERAPEUTIC", "PROPHYLACTIC",
  "Preventive / for screening purposes"
)

pr_rows <- list()
pr_counter <- 0
for (i in seq_len(n_subjects)) {
  n_pr <- sample(1:4, 1)
  for (j in seq_len(n_pr)) {
    pr_counter <- pr_counter + 1
    pr_start_offset <- sample(-60:80, 1)
    pr_start <- rfst_dates[i] + pr_start_offset
    pr_dur   <- sample(1:30, 1)
    pr_end   <- pr_start + pr_dur
    ongoing <- runif(1) < 0.20

    pr_rows[[pr_counter]] <- tibble(
      SubjectId      = subjectids[i],
      PRSPID         = pr_counter,
      PRTrt          = sample(pr_terms_pool, 1),
      PRStDat        = format(pr_start, "%Y-%m-%d"),
      PREnDat        = if (ongoing) NA_character_ else format(pr_end, "%Y-%m-%d"),
      PROnGo         = if (ongoing) "Y" else "N",
      PRIndicat      = sample(indication_pool, 1),
      InitiatedDate  = format(pr_start - sample(0:3, 1), "%Y-%m-%d"),
      LastEditedDate = format(pr_start + sample(1:10, 1), "%Y-%m-%d")
    )
  }
}
pr <- dplyr::bind_rows(pr_rows)

# =============================================================================
# Add cross-domain link columns for RELREC
# =============================================================================
# prmh1 : MHSPID of a related Medical History condition
# prae1 : AESPID of a related Adverse Event
#
# These link columns mirror the EDC structure seen in real studies where the
# CRF allows the investigator to link a procedure record to an MH that
# motivated the procedure, or to an AE that the procedure addresses.
# -----------------------------------------------------------------------------

ae  <- read.csv("raw/ae.csv",  stringsAsFactors = FALSE)
mh  <- read.csv("raw/mh.csv",  stringsAsFactors = FALSE)

pr$prmh1 <- NA_character_
pr$prae1 <- NA_character_

set.seed(2002)
for (i in seq_len(nrow(pr))) {
  subj <- pr$SubjectId[i]
  subj_mhs <- mh[mh$SubjectId == subj, ]
  subj_aes <- ae[ae$SubjectId == subj, ]

  # Link to MH with 35% probability
  if (nrow(subj_mhs) > 0 && runif(1) < 0.35) {
    pr$prmh1[i] <- as.character(sample(subj_mhs$MHSPID, 1))
  }

  # Link to AE with 20% probability
  if (nrow(subj_aes) > 0 && runif(1) < 0.20) {
    pr$prae1[i] <- as.character(sample(subj_aes$AESPID, 1))
  }
}

dir.create("raw", showWarnings = FALSE, recursive = TRUE)
write.csv(pr, "raw/pr.csv", row.names = FALSE, na = "")
cat("Created raw/pr.csv:", nrow(pr), "rows x", ncol(pr), "cols\n")
