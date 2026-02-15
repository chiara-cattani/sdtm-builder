# =============================================================================
# Create Dummy Raw Data: ec.csv (Exposure as Collected)
# =============================================================================
# Run from study-pilot/ directory
# Output: raw/ec.csv
# Depends: raw/ex.csv (for exposure periods)
# =============================================================================

set.seed(77)

library(tibble)

subjectids <- c(
  "XX-SITEA-01-0001", "XX-SITEB-02-0002", "XX-SITEC-03-0003",
  "XX-SITEA-01-0004", "XX-SITEB-02-0005", "XX-SITEC-03-0006",
  "XX-SITEA-01-0007", "XX-SITEB-02-0008"
)

# Read EX data to generate matching EC records
ex <- read.csv("raw/ex.csv", stringsAsFactors = FALSE)

ec_rows <- list()
ec_counter <- 0

for (i in seq_len(nrow(ex))) {
  subj  <- ex$SubjectId[i]
  start <- as.Date(ex$ExStDat[i])
  end   <- as.Date(ex$ExEnDat[i])
  dose  <- ex$ExDose[i]
  dosu  <- ex$ExDoseU[i]

  # Generate one EC record per week within this period
  week_starts <- seq(start, end, by = 7)
  for (j in seq_along(week_starts)) {
    ec_counter <- ec_counter + 1
    ws  <- week_starts[j]
    we  <- min(ws + 6, end)

    # ~10% chance of "NOT DONE"
    occur   <- sample(c("Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "N"), 1)
    reasnd  <- ifelse(occur == "N",
                      sample(c("SUBJECT REFUSED", "SUPPLY ISSUE", "LOST TO FOLLOW-UP"), 1),
                      "")
    stat    <- ifelse(occur == "N", "NOT DONE", "")
    ec_dose <- ifelse(occur == "Y", dose, NA_real_)

    ec_rows[[ec_counter]] <- tibble(
      SubjectId = subj,
      EcSpId    = sprintf("EC-%04d", ec_counter),
      EcTrt     = "STUDY PRODUCT",
      EcCat     = "STUDY PRODUCT",
      EcDose    = ec_dose,
      EcDoseU   = dosu,
      EcDosfrm  = "POWDER, FOR SUSPENSION",
      EcRoute   = "ORAL",
      EcStDat   = format(ws, "%Y-%m-%d"),
      EcEnDat   = format(we, "%Y-%m-%d"),
      EcOccur   = occur,
      EcStat    = stat,
      EcReasnd  = reasnd
    )
  }
}

ec <- dplyr::bind_rows(ec_rows)

dir.create("raw", showWarnings = FALSE, recursive = TRUE)
write.csv(ec, "raw/ec.csv", row.names = FALSE)
cat("Wrote", nrow(ec), "rows to raw/ec.csv\n")
