# =============================================================================
# Create Dummy Raw Data: sae.csv (Serious Adverse Events)
# =============================================================================
# Run from study-pilot/ directory
# Output: raw/sae.csv
# Depends: raw/ae.csv (reads AE rows to pick serious events)
#          raw/dm.csv (reads DM for age/sex at SAE)
# =============================================================================

set.seed(142)

library(tibble)

ae <- read.csv("raw/ae.csv", stringsAsFactors = FALSE)
dm <- read.csv("raw/dm.csv", stringsAsFactors = FALSE)

# ~20% of AEs are serious
n_sae <- max(1, round(nrow(ae) * 0.2))
sae_idx <- sort(sample(seq_len(nrow(ae)), n_sae))

sexes   <- c("F", "M")
age_units <- c("YEARS")
actions <- c("DOSE NOT CHANGED", "DRUG WITHDRAWN", "DOSE REDUCED", "NOT APPLICABLE")

sae_rows <- list()
for (k in seq_along(sae_idx)) {
  idx <- sae_idx[k]
  subj   <- ae$SubjectId[idx]
  dm_row <- dm[dm$SubjectId == subj, ]

  # Derive dates from AE parent
  ae_stdat  <- ae$AEStDat[idx]
  ae_endat  <- if (!is.na(ae$AeEnDat[idx]) && ae$AeEnDat[idx] != "") ae$AeEnDat[idx] else NA_character_
  ae_ongo   <- if (!is.na(ae$AeOnGo[idx]) && ae$AeOnGo[idx] != "") ae$AeOnGo[idx] else sample(c("Y", "N"), 1)

  is_fatal  <- sample(c("Y", "N", "N", "N", "N"), 1)
  is_hosp   <- sample(c("Y", "Y", "N", "N"), 1)

  # Hospital admission / discharge dates (only if hospitalised)
  adm_dtc <- NA_character_
  dis_dtc <- NA_character_
  if (is_hosp == "Y" && !is.na(ae_stdat)) {
    adm_dtc <- ae_stdat
    if (!is.na(ae_endat)) dis_dtc <- ae_endat
  }

  # Death date only if fatal
dth_dtc <- NA_character_
  dth_rsn <- NA_character_
  autopsy <- NA_character_
  dth_ct  <- NA_character_
  if (is_fatal == "Y") {
    dth_dtc <- if (!is.na(ae_endat)) ae_endat else ae_stdat
    dth_rsn <- ae$AETerm[idx]
    autopsy <- sample(c("Y", "N"), 1)
    dth_ct  <- sample(c("Y", "N"), 1)
  }

  acn     <- sample(actions, 1)
  # First product intake = ae start date - random days
  fpi_dtc <- if (!is.na(ae_stdat)) as.character(as.Date(ae_stdat) - sample(1:30, 1)) else NA_character_
  dose    <- sample(c("400 mg", "800 mg"), 1)
  cb      <- sample(c("Y", "N", "N", "N"), 1)

  # Dose reduction (rare)
  rd_dtc  <- NA_character_; rd_from <- NA_character_; rd_to <- NA_character_
  if (acn == "DOSE REDUCED" && !is.na(ae_stdat)) {
    rd_dtc  <- as.character(as.Date(ae_stdat) + sample(1:5, 1))
    rd_from <- "800 mg"; rd_to <- "400 mg"
  }

  # Dose increase / interruption / restart (mostly NA for dummy)
  in_dtc  <- NA_character_; in_from <- NA_character_; in_to <- NA_character_
  it1_dtc <- NA_character_; rs1_dtc <- NA_character_; rein1 <- NA_character_
  it2_dtc <- NA_character_; rs2_dtc <- NA_character_; rein2 <- NA_character_
  it3_dtc <- NA_character_; rs3_dtc <- NA_character_

  # Sex & age from DM
  sae_sex  <- if (nrow(dm_row) > 0) dm_row$Sex[1] else sample(sexes, 1)
  sae_age  <- if (nrow(dm_row) > 0) dm_row$Age[1] else sample(25:65, 1)
  sae_ageu <- "YEARS"

  abate   <- sample(c("Y", "N"), 1)
  recur   <- sample(c("Y", "N", "N"), 1)

  sae_rows[[k]] <- tibble(
    SubjectId  = subj,
    SAESPID    = ae$AESPID[idx],
    SAETerm    = ae$AETerm[idx],
    AeSStDat   = ae_stdat,
    AeSEnDat   = ae_endat,
    AeSOnGo    = ae_ongo,
    AeSAge     = sae_age,
    AeSAgeU    = sae_ageu,
    AeSSex     = sae_sex,
    AeSDth     = is_fatal,
    AeSLife    = sample(c("Y", "N", "N", "N"), 1),
    AeSHosp    = is_hosp,
    AeSDiSab   = sample(c("Y", "N", "N", "N", "N"), 1),
    AeSCong    = "N",
    AeSMie     = sample(c("Y", "N", "N"), 1),
    AeSAdDat   = adm_dtc,
    AeSDiDat   = dis_dtc,
    AeSDtDat   = dth_dtc,
    AeSDtRes   = dth_rsn,
    AeSAutop   = autopsy,
    AeSDthCt   = dth_ct,
    SAEAcnP    = acn,
    AeSFPI     = fpi_dtc,
    AeSDose    = dose,
    AeSCB      = cb,
    AeSRdDat   = rd_dtc,
    AeSRdOsF   = rd_from,
    AeSRdDOT   = rd_to,
    AeSInDat   = in_dtc,
    AeSInDOF   = in_from,
    AeSInDOT   = in_to,
    AeSItDat   = it1_dtc,
    AeSRsDat   = rs1_dtc,
    AeReIn     = rein1,
    AeSItDat2  = it2_dtc,
    AeSRsDat2  = rs2_dtc,
    AeReIn2    = rein2,
    AeSItDat3  = it3_dtc,
    AeSRsDat3  = rs3_dtc,
    AeSAbate   = abate,
    AeSRecur   = recur
  )
}
sae <- dplyr::bind_rows(sae_rows)

dir.create("raw", showWarnings = FALSE, recursive = TRUE)
write.csv(sae, "raw/sae.csv", row.names = FALSE, na = "")
cat("Created raw/sae.csv:", nrow(sae), "rows x", ncol(sae), "cols\n")
