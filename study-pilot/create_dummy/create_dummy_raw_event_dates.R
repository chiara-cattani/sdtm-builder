# =============================================================================
# Create Dummy Raw Data for STUDY-PILOT SV Domain (event_dates)
# =============================================================================
# This script generates a dummy event_dates dataset that mirrors the structure
# of Viedoc EDC event-status exports used to build the SV (Subject Visits) domain.
#
# Datasets created:
#   - event_dates.csv : One row per subject × visit when status = "Initiated"
#
# The visit schedule follows the config.yaml visit_map:
#   SCR (Screening), V1, PW1, PW5, V2, PW15, PW20, V3, PFU
#   (V1_IMG and V3_IMG are imaging aliases, not separate events)
#
# SubjectID format: CC-SITE-NN-SSSS  (reuses the same 8 subjects)
# =============================================================================

set.seed(42)

library(tibble)
library(dplyr)

# --- Configuration -----------------------------------------------------------
studyid <- "STUDY-PILOT"

# Same 8 subjects as in create_dummy_raw_ae.R
subjectids <- c(
  "XX-SITEA-01-0001", "XX-SITEB-02-0002", "XX-SITEC-03-0003",
  "XX-SITEA-01-0004", "XX-SITEB-02-0005", "XX-SITEC-03-0006",
  "XX-SITEA-01-0007", "XX-SITEB-02-0008"
)

# IC dates (RFSTDTC) from ic.csv — must match exactly
ic <- read.csv("raw/ic.csv", stringsAsFactors = FALSE)
ic_dates <- setNames(as.Date(ic$IcDat), ic$SubjectId)

# Visit schedule with planned offsets from Day 1 (= IC date)
visit_schedule <- tibble::tribble(
  ~eventid, ~planned_offset,
  "SCR",     0L,    # Screening = Day 1

  "V1",      0L,    # Visit 1 = Day 1
  "PW1",     7L,    # Phone Call 1 = Day 8
  "PW5",    35L,    # Phone Call 2 = Day 36
  "V2",     70L,    # Visit 2 = Day 71
  "PW15",  105L,    # Phone Call 3 = Day 106
  "PW20",  139L,    # Phone Call 4 = Day 140
  "V3",    140L,    # Visit 3 = Day 141
  "PFU",   154L     # Follow-up = Day 155
)

# --- Generate event_dates ----------------------------------------------------
rows <- list()
for (sid in subjectids) {
  base_dt <- ic_dates[sid]
  for (i in seq_len(nrow(visit_schedule))) {
    eid <- visit_schedule$eventid[i]
    offset <- visit_schedule$planned_offset[i]

    # Add ± 1-2 day random jitter for realism (except SCR/V1 which are Day 1)
    jitter <- if (offset == 0L) 0L else sample(-2:2, 1)
    event_dt <- base_dt + offset + jitter

    rows <- c(rows, list(tibble::tibble(
      SubjectId          = sid,
      EventId            = eid,
      EventInitiatedDate = format(event_dt, "%Y-%m-%d"),
      EventStatus        = "Initiated"
    )))
  }
}

event_dates <- dplyr::bind_rows(rows)

cat("event_dates:", nrow(event_dates), "rows x", ncol(event_dates), "cols\n")
cat("  Subjects:", length(unique(event_dates$SubjectId)), "\n")
cat("  Visits:", paste(unique(event_dates$EventId), collapse = ", "), "\n")

# --- Write CSV ----------------------------------------------------------------
write.csv(event_dates, "raw/event_dates.csv", row.names = FALSE)
cat("\nSaved: raw/event_dates.csv\n")
