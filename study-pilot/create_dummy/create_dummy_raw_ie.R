# =============================================================================
# Create Dummy Raw Data: ie.csv (Inclusion / Exclusion Criteria – Wide Format)
# =============================================================================
# Run from study-pilot/ directory
# Output: raw/ie.csv
# Depends: raw/ic.csv (reads screening dates per subject)
# =============================================================================
# Wide format: one row per subject at screening.
# Columns: SubjectId, EventId, EventDate, ieyn,
#           ieintestcd1…ieintestcd8   (inclusion criteria flags),
#           ieextestcd1…ieextestcd10  (exclusion criteria flags).
# Most subjects are eligible (ieyn = "Y", all criteria blank).
# Three subjects are NOT eligible (ieyn = "N") with specific criteria flagged.
# =============================================================================

library(tibble)

ic <- read.csv("raw/ic.csv", stringsAsFactors = FALSE)

subjectids <- ic$SubjectId
event_dates <- ic$IcDat  # screening date = informed consent date

n <- length(subjectids)
n_incl <- 8L
n_excl <- 10L

# Initialise blank matrix for criteria columns
incl_mat <- matrix("", nrow = n, ncol = n_incl)
excl_mat <- matrix("", nrow = n, ncol = n_excl)

# Default: everyone is eligible
ieyn <- rep("Y", n)

# Subject 3 – screen failure: exclusion criterion 5 met
ieyn[3]       <- "N"
excl_mat[3, 5] <- "EXCL05"

# Subject 7 – screen failure: inclusion criterion 3 NOT met + exclusion 4 met
ieyn[7]       <- "N"
incl_mat[7, 3] <- "INCL03"
excl_mat[7, 4] <- "EXCL04"

ie <- data.frame(
  SubjectId = subjectids,
  EventId   = "SCR",
  EventDate = event_dates,
  ieyn      = ieyn,
  incl_mat,
  excl_mat,
  stringsAsFactors = FALSE
)

names(ie) <- c("SubjectId", "EventId", "EventDate", "ieyn",
               paste0("ieintestcd", seq_len(n_incl)),
               paste0("ieextestcd", seq_len(n_excl)))

dir.create("raw", showWarnings = FALSE, recursive = TRUE)
write.csv(ie, "raw/ie.csv", row.names = FALSE, na = "")
cat("Created raw/ie.csv:", nrow(ie), "rows x", ncol(ie), "cols\n")
