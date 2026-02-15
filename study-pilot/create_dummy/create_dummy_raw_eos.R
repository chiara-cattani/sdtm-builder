# =============================================================================
# Create Dummy Raw Data: eos.csv (End of Study)
# =============================================================================
# Run from study-pilot/ directory
# Output: raw/eos.csv
# Depends: raw/ic.csv (for IC dates / RFSTDTC)
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

ic_dates   <- date_anchor + sample(-30:0, n_subjects, replace = TRUE)
rfst_dates <- ic_dates

# Consume random state for rand_dates to stay in sync
rand_dates <- ic_dates + sample(1:7, n_subjects, replace = TRUE)

# Consume random state for DM fields to stay in sync
invisible(sample(c("F", "M"), n_subjects, replace = TRUE))       # Sex
invisible(sample(25:65, n_subjects, replace = TRUE))              # Age
invisible(sample(c("NL", "HL"), n_subjects, replace = TRUE))     # Ethnicc
invisible(sample(9000:23000, n_subjects))                         # BrthDat

eos <- tibble::tibble(
  SubjectId       = subjectids,
  ComplYN         = sample(c("Y", "N"), n_subjects, replace = TRUE, prob = c(0.8, 0.2)),
  LcDat           = format(rfst_dates + sample(100:150, n_subjects, replace = TRUE), "%Y-%m-%d"),
  RfxEnDat        = format(rfst_dates + sample(80:120, n_subjects, replace = TRUE), "%Y-%m-%d"),
  EtDat           = NA_character_,
  EventDate       = format(rfst_dates + sample(100:150, n_subjects, replace = TRUE), "%Y-%m-%d"),
  LastEditedDate  = format(Sys.Date(), "%Y-%m-%d")
)

# Ensure at least 2 non-completers for realistic DS domain
if (sum(eos$ComplYN == "N") < 2) {
  force_nc <- sample(which(eos$ComplYN == "Y"), 2 - sum(eos$ComplYN == "N"))
  eos$ComplYN[force_nc] <- "N"
}

# Add EtStat (early termination reason) and EtSp (specify) columns
et_reasons <- c("ADVERSE EVENT", "WITHDRAWAL BY SUBJECT", "LOST TO FOLLOW-UP",
                "PHYSICIAN DECISION", "PROTOCOL DEVIATION")
eos$EtStat <- NA_character_
eos$EtSp   <- NA_character_

non_completers <- which(eos$ComplYN == "N")
if (length(non_completers) > 0) {
  eos$EtDat[non_completers] <- format(
    rfst_dates[non_completers] + sample(30:60, length(non_completers), replace = TRUE),
    "%Y-%m-%d"
  )
  eos$EtStat[non_completers] <- sample(et_reasons,
                                        length(non_completers), replace = TRUE)
  # Ensure at least one non-completer has "ADVERSE EVENT" as reason
  if (!any(eos$EtStat == "ADVERSE EVENT", na.rm = TRUE) && length(non_completers) > 0) {
    eos$EtStat[non_completers[1]] <- "ADVERSE EVENT"
  }
}

# =============================================================================
# Add cross-domain link column for RELREC : DS ↔ AE
# =============================================================================
# etae1 : AESPID of the AE that led to early termination
#         Only for subjects with EtStat = "ADVERSE EVENT"
# -----------------------------------------------------------------------------

ae  <- read.csv("raw/ae.csv", stringsAsFactors = FALSE)

eos$etae1 <- NA_character_
ae_disc <- which(eos$EtStat == "ADVERSE EVENT")
if (length(ae_disc) > 0) {
  for (k in ae_disc) {
    subj <- eos$SubjectId[k]
    subj_aes <- ae[ae$SubjectId == subj, ]
    if (nrow(subj_aes) > 0) {
      # Pick the most recent AE (highest AESPID) as the cause
      eos$etae1[k] <- as.character(max(subj_aes$AESPID))
    }
  }
}

dir.create("raw", showWarnings = FALSE, recursive = TRUE)
write.csv(eos, "raw/eos.csv", row.names = FALSE, na = "")
cat("Created raw/eos.csv:", nrow(eos), "rows x", ncol(eos), "cols\n")
