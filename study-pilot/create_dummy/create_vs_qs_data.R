# =============================================================================
# Create Dummy Raw Data: an.csv and qs_asq.csv
# =============================================================================
# Run from study-pilot/ directory
# Outputs:
#   raw/an.csv      — Anthropometric measurements (source for VS domain)
#   raw/qs_asq.csv  — ASQ-3 questionnaire responses (source for QS domain)
#
# an.csv columns (per VS.sas):
#   SubjectId, EventId, AnDat, AnInit, An_NDND, AnReasnd,
#   Weight, Weight_U, Weight_NDND, Weight_NDR,
#   An_LH, Height, Height_U, LenHei_NDND, LenHei_NDR,
#   Length, Length_U, HdCirc, HdCirc_U, HdCirc_NDND, HdCirc_NDR
#
# qs_asq.csv columns (per QS.sas):
#   SubjectId, EventId, Asq3D, Asq31, Asq3_NDND, Asq3_RND,
#   Asq3C1, Asq3GM1, Asq3FM1, Asq3PS1, Asq3PE1
# =============================================================================

set.seed(2026)

# ---------------------------------------------------------------------------
# Read existing raw files for subject IDs and visit dates
# ---------------------------------------------------------------------------
dm          <- read.csv("raw/dm.csv", stringsAsFactors = FALSE)
event_dates <- read.csv("raw/event_dates.csv", stringsAsFactors = FALSE)

subjects <- dm$SubjectId
n_subj   <- length(subjects)

# Helper: look up date from event_dates
get_date <- function(subj, evt) {
  row <- event_dates[event_dates$SubjectId == subj &
                       event_dates$EventId   == evt, ]
  if (nrow(row) == 0) return(NA_character_)
  row$EventInitiatedDate[1]
}

# ---------------------------------------------------------------------------
# Evaluator initials pool
# ---------------------------------------------------------------------------
initials_pool <- c("CC", "KN", "JD", "MB", "AL", "TP", "SR", "LH")

# ===========================================================================
# 1. AN data  (Anthropometric measurements)
# ===========================================================================
an_visits <- c("SCR", "V1", "V2", "V3")

an_rows <- list()

for (i in seq_along(subjects)) {
  subj <- subjects[i]
  init <- initials_pool[((i - 1) %% length(initials_pool)) + 1]

  for (v in an_visits) {
    visit_date <- get_date(subj, v)

    row <- list(
      SubjectId   = subj,
      EventId     = v,
      AnDat       = visit_date,
      AnInit      = init,
      An_NDND     = "",
      AnReasnd    = "",
      Weight      = round(runif(1, 3.5, 14.0), 1),
      Weight_U    = "kg",
      Weight_NDND = "",
      Weight_NDR  = "",
      An_LH       = sample(c("LYING", "STANDING"), 1, prob = c(0.6, 0.4)),
      Height      = round(runif(1, 52, 88), 1),
      Height_U    = "cm",
      LenHei_NDND = "",
      LenHei_NDR  = "",
      Length       = NA_real_,
      Length_U    = "cm",
      HdCirc      = round(runif(1, 32, 48), 1),
      HdCirc_U    = "cm",
      HdCirc_NDND = "",
      HdCirc_NDR  = ""
    )

    # Length = Height when position is LYING
    if (row$An_LH == "LYING") {
      row$Length <- row$Height
    } else {
      row$Length <- round(row$Height - runif(1, 0.5, 1.5), 1)
    }

    an_rows <- c(an_rows, list(row))
  }
}

an <- do.call(rbind, lapply(an_rows, as.data.frame, stringsAsFactors = FALSE))

# --- Apply NOT DONE flags ---------------------------------------------------

# Full assessment NOT DONE: subject 3 at V2, subject 6 at V3
full_nd <- list(
  list(subj = subjects[3], visit = "V2", reason = "SUBJECT UNWELL"),
  list(subj = subjects[6], visit = "V3", reason = "VISIT MISSED")
)
for (nd in full_nd) {
  idx <- which(an$SubjectId == nd$subj & an$EventId == nd$visit)
  an$An_NDND[idx]     <- "NOT DONE"
  an$AnReasnd[idx]    <- nd$reason
  an$Weight[idx]      <- NA
  an$Weight_U[idx]    <- ""
  an$Weight_NDND[idx] <- ""
  an$Weight_NDR[idx]  <- ""
  an$An_LH[idx]       <- ""
  an$Height[idx]      <- NA
  an$Height_U[idx]    <- ""
  an$LenHei_NDND[idx] <- ""
  an$LenHei_NDR[idx]  <- ""
  an$Length[idx]       <- NA
  an$Length_U[idx]     <- ""
  an$HdCirc[idx]      <- NA
  an$HdCirc_U[idx]    <- ""
  an$HdCirc_NDND[idx] <- ""
  an$HdCirc_NDR[idx]  <- ""
}

# Individual Weight NOT DONE: subject 2 at V1
idx_w <- which(an$SubjectId == subjects[2] & an$EventId == "V1")
an$Weight_NDND[idx_w] <- "NOT DONE"
an$Weight_NDR[idx_w]  <- "SCALE UNAVAILABLE"
an$Weight[idx_w]      <- NA
an$Weight_U[idx_w]    <- ""

# Individual LenHei NOT DONE: subject 5 at V3
idx_lh <- which(an$SubjectId == subjects[5] & an$EventId == "V3")
an$LenHei_NDND[idx_lh] <- "NOT DONE"
an$LenHei_NDR[idx_lh]  <- "SUBJECT REFUSED"
an$Height[idx_lh]       <- NA
an$Height_U[idx_lh]     <- ""
an$Length[idx_lh]        <- NA
an$Length_U[idx_lh]      <- ""
an$An_LH[idx_lh]        <- ""

# Individual HdCirc NOT DONE: subject 7 at V2
idx_hc <- which(an$SubjectId == subjects[7] & an$EventId == "V2")
an$HdCirc_NDND[idx_hc] <- "NOT DONE"
an$HdCirc_NDR[idx_hc]  <- "EQUIPMENT MALFUNCTION"
an$HdCirc[idx_hc]      <- NA
an$HdCirc_U[idx_hc]    <- ""

# Write
dir.create("raw", showWarnings = FALSE, recursive = TRUE)
write.csv(an, "raw/an.csv", row.names = FALSE, na = "")
cat("Created raw/an.csv:", nrow(an), "rows x", ncol(an), "cols\n")
cat("  Columns:", paste(names(an), collapse = ", "), "\n")

# ===========================================================================
# 2. QS_ASQ data  (ASQ-3 questionnaire responses)
# ===========================================================================
qs_visits <- c("V1", "V2", "V3")

# Map visit to ASQ-3 sub-category (age-interval questionnaire)
asq_scat <- c(V1 = "2 MONTH", V2 = "6 MONTH", V3 = "12 MONTH")

qs_rows <- list()

for (i in seq_along(subjects)) {
  subj <- subjects[i]

  for (v in qs_visits) {
    visit_date <- get_date(subj, v)

    row <- list(
      SubjectId = subj,
      EventId   = v,
      Asq3D     = visit_date,
      Asq31     = asq_scat[v],
      Asq3_NDND = "",
      Asq3_RND  = "",
      Asq3C1    = sample(20:55, 1),
      Asq3GM1   = sample(20:55, 1),
      Asq3FM1   = sample(20:55, 1),
      Asq3PS1   = sample(20:55, 1),
      Asq3PE1   = sample(20:55, 1)
    )

    qs_rows <- c(qs_rows, list(row))
  }
}

qs <- do.call(rbind, lapply(qs_rows, as.data.frame, stringsAsFactors = FALSE))

# --- NOT DONE records --------------------------------------------------------
# Subject 4 at V2: full questionnaire NOT DONE
idx_nd1 <- which(qs$SubjectId == subjects[4] & qs$EventId == "V2")
qs$Asq3_NDND[idx_nd1] <- "NOT DONE"
qs$Asq3_RND[idx_nd1]  <- "SUBJECT UNCOOPERATIVE"
qs[idx_nd1, c("Asq3C1", "Asq3GM1", "Asq3FM1", "Asq3PS1", "Asq3PE1")] <- NA

# Subject 8 at V3: full questionnaire NOT DONE
idx_nd2 <- which(qs$SubjectId == subjects[8] & qs$EventId == "V3")
qs$Asq3_NDND[idx_nd2] <- "NOT DONE"
qs$Asq3_RND[idx_nd2]  <- "VISIT WINDOW EXCEEDED"
qs[idx_nd2, c("Asq3C1", "Asq3GM1", "Asq3FM1", "Asq3PS1", "Asq3PE1")] <- NA

# Write
write.csv(qs, "raw/qs_asq.csv", row.names = FALSE, na = "")
cat("Created raw/qs_asq.csv:", nrow(qs), "rows x", ncol(qs), "cols\n")
cat("  Columns:", paste(names(qs), collapse = ", "), "\n")

cat("\nDone.\n")
