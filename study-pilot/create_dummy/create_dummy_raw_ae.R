# =============================================================================
# Create Dummy Raw Data: ae.csv (Adverse Events CRF)
# =============================================================================
# Run from study-pilot/ directory
# Output: raw/ae.csv
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

# Consume random state to match original sequence position
invisible(ic_dates + sample(1:7, n_subjects, replace = TRUE))         # rand_dates
invisible(sample(c("F", "M"), n_subjects, replace = TRUE))            # DM Sex
invisible(sample(25:65, n_subjects, replace = TRUE))                   # DM Age
invisible(sample(c("NL", "HL"), n_subjects, replace = TRUE))          # DM Ethnicc
invisible(sample(9000:23000, n_subjects))                              # DM BrthDat
# EOS
invisible(sample(c("Y", "N"), n_subjects, replace = TRUE))
invisible(sample(100:150, n_subjects, replace = TRUE))
invisible(sample(80:120, n_subjects, replace = TRUE))
invisible(sample(100:150, n_subjects, replace = TRUE))
# EX
for (.i in seq_len(n_subjects)) { invisible(sample(3:6, 1)) }

# AE term pools
ae_terms_pool <- c("Headache", "Nausea", "Dizziness", "Fatigue",
                   "Abdominal pain", "Constipation", "Diarrhoea",
                   "Vomiting", "Rash", "Insomnia", "Back pain",
                   "Nasopharyngitis")
sev_values   <- c("Mild", "Moderate", "Severe")
out_values   <- c("Recovered / Resolved", "Recovering / Resolving",
                  "Not Recovered / Not Resolved", "Recovered / Resolved with Sequelae",
                  "Fatal")
acn_values   <- c("NONE", "DOSE REDUCED", "DRUG INTERRUPTED",
                  "DRUG WITHDRAWN", "NOT APPLICABLE", "NA")
rel_values   <- c("Not Related", "Possibly", "Probably", "Unlikely", "Definitely", "NA")
periodo_values <- c("Before first product intake",
                    "After first product intake", NA_character_)

ae_rows <- list()
ae_counter <- 0
for (i in seq_len(n_subjects)) {
  n_ae <- sample(1:5, 1)
  for (j in seq_len(n_ae)) {
    ae_counter <- ae_counter + 1
    ae_start_offset <- sample(-5:80, 1)
    ae_start <- rfst_dates[i] + ae_start_offset
    ae_dur   <- sample(3:30, 1)
    ae_end   <- ae_start + ae_dur
    ongoing <- runif(1) < 0.15
    partial <- runif(1) < 0.10

    ae_rows[[ae_counter]] <- tibble(
      SubjectId  = subjectids[i],
      AESPID     = ae_counter,
      AETerm     = sample(ae_terms_pool, 1),
      AESev      = sample(sev_values, 1),
      AEOut      = if (ongoing) "Not Recovered / Not Resolved" else sample(out_values[-5], 1),
      AEStDat    = if (partial) format(ae_start, "%Y-%m") else format(ae_start, "%Y-%m-%d"),
      AeEnDat    = if (ongoing) NA_character_ else format(ae_end, "%Y-%m-%d"),
      AEACNp     = sample(acn_values, 1),
      AEACNpSp   = NA_character_,
      AEACNS0    = NA_character_,
      AEACNS1    = NA_character_,
      AEACNS2    = NA_character_,
      AEACNsSp   = NA_character_,
      AERel1     = sample(rel_values, 1),
      AeRel1Co   = NA_character_,
      AERel2     = sample(rel_values, 1),
      AeRel2Co   = NA_character_,
      AeSequel1  = NA_character_,
      AeStPer    = sample(periodo_values, 1),
      AeOnGo     = if (ongoing) "Y" else "N",
      AeFu       = sample(c("Y", "N", NA_character_), 1),
      AeCoval    = NA_character_,
      CodedOnDate    = format(ae_start + sample(1:5, 1), "%Y-%m-%d"),
      InitiatedDate  = format(ae_start, "%Y-%m-%d"),
      LastEditedDate = format(ae_start + sample(1:10, 1), "%Y-%m-%d")
    )
  }
}
ae <- dplyr::bind_rows(ae_rows)

# Add some "other action" values
other_action_idx <- sample(nrow(ae), min(3, nrow(ae)))
ae$AEACNS0[other_action_idx] <- "Other medication given"
ae$AEACNsSp[other_action_idx] <- "Paracetamol prescribed"

checkbox_idx <- sample(nrow(ae), min(5, nrow(ae)))
ae$AEACNS1[checkbox_idx[1:min(3, length(checkbox_idx))]] <- "Concomitant medication given"
ae$AEACNS2[checkbox_idx[1:min(2, length(checkbox_idx))]] <- "Non-drug therapy"

sequel_idx <- which(ae$AEOut == "Recovered / Resolved with Sequelae")
if (length(sequel_idx) > 0) {
  ae$AeSequel1[sequel_idx] <- "Mild residual symptoms"
}

desc_idx <- sample(nrow(ae), min(4, nrow(ae)))
ae$AeCoval[desc_idx] <- "Patient reported mild discomfort"

rel_idx <- which(ae$AERel1 %in% c("Definitely", "Possibly", "Probably"))
if (length(rel_idx) > 0) {
  ae$AeRel1Co[rel_idx[1:min(2, length(rel_idx))]] <- "Temporal relationship observed"
}

dir.create("raw", showWarnings = FALSE, recursive = TRUE)
write.csv(ae, "raw/ae.csv", row.names = FALSE, na = "")
cat("Created raw/ae.csv:", nrow(ae), "rows x", ncol(ae), "cols\n")
