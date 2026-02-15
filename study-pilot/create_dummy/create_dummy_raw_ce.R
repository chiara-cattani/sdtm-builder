# =============================================================================
# Create Dummy Raw Data: ce.csv (Clinical Events)
# =============================================================================
# Run from study-pilot/ directory
# Output: raw/ce.csv
# Based on example1/CE.sas pattern — two source types:
#   - STOOL observations from investigator (per-visit)
#   - GI symptom reports from parent (diary timepoints)
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

# Visit schedule for investigator stool observations
visits_ce <- c("V1", "V2", "V3")
visit_offsets_ce <- c(1, 59, 89)

# Diary timepoints for parent GI symptom reports (subset)
diary_tpts <- c("W1D1", "W1D3", "W3D1", "W5D1")
diary_offsets <- c(7, 9, 21, 35)

rows <- list()

for (i in seq_len(n_subjects)) {
  ic_date <- date_anchor + sample(-30:0, 1)

  # ---- Investigator STOOL observations at clinic visits ----
  for (v in seq_along(visits_ce)) {
    visit_date <- ic_date + visit_offsets_ce[v]
    vid <- visits_ce[v]

    # Did stool occur?
    stool_occurred <- sample(c("Y", "N"), 1, prob = c(0.9, 0.1))

    rows[[length(rows) + 1L]] <- data.frame(
      SubjectId  = subjectids[i],
      EventId    = vid,
      CeSource   = "INVESTIGATOR",
      CeOrig     = "CRF",
      CeTerm     = "STOOL",
      CeStDat    = format(visit_date, "%Y-%m-%d"),
      CePresp    = "Y",
      CeOccur    = stool_occurred,
      CeCat      = "GASTROINTESTINAL SYMPTOMS",
      CeSCat     = "DEFECATION",
      CeEvIntx   = "LAST 24 HOURS",
      CeTpt      = "",
      CeTptRef   = "",
      CeEltm     = "",
      CeTptNum   = NA_real_,
      stringsAsFactors = FALSE
    )
  }

  # ---- Parent-reported GI symptoms at diary timepoints ----
  for (d in seq_along(diary_tpts)) {
    tpt_date <- ic_date + diary_offsets[d]

    stool_occurred <- sample(c("Y", "N"), 1, prob = c(0.85, 0.15))

    rows[[length(rows) + 1L]] <- data.frame(
      SubjectId  = subjectids[i],
      EventId    = diary_tpts[d],
      CeSource   = "PARENT",
      CeOrig     = "pPRO",
      CeTerm     = "STOOL",
      CeStDat    = format(tpt_date, "%Y-%m-%d"),
      CePresp    = "Y",
      CeOccur    = stool_occurred,
      CeCat      = "GASTROINTESTINAL SYMPTOMS",
      CeSCat     = "DEFECATION",
      CeEvIntx   = "TODAY",
      CeTpt      = diary_tpts[d],
      CeTptRef   = "FIRST STUDY PRODUCT INTAKE",
      CeEltm     = paste0("P", diary_offsets[d], "D"),
      CeTptNum   = d,
      stringsAsFactors = FALSE
    )
  }
}

ce <- do.call(rbind, rows)

dir.create("raw", showWarnings = FALSE, recursive = TRUE)
write.csv(ce, "raw/ce.csv", row.names = FALSE, na = "")
cat("Created raw/ce.csv:", nrow(ce), "rows x", ncol(ce), "cols\n")
