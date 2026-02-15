# ==============================================================================
# Create a dummy DM SDTM dataset in sdtm/datasets/
# Uses raw dm.csv, ic.csv, rand.csv, and eos.csv to derive reference dates.
# Run from study-pilot/ directory
# ==============================================================================

library(dplyr)
library(tibble)
library(haven)

studyid <- "STUDY-PILOT"

dm_raw  <- read.csv("raw/dm.csv",   stringsAsFactors = FALSE)
ic      <- read.csv("raw/ic.csv",   stringsAsFactors = FALSE)
rand    <- read.csv("raw/rand.csv", stringsAsFactors = FALSE)
eos     <- read.csv("raw/eos.csv",  stringsAsFactors = FALSE)

# Merge everything by SubjectId
dm <- dm_raw %>%
  left_join(ic,   by = "SubjectId") %>%
  left_join(rand, by = "SubjectId") %>%
  left_join(eos,  by = "SubjectId")

# Build SDTM DM
dm_sdtm <- tibble(
  STUDYID  = studyid,
  DOMAIN   = "DM",
  USUBJID  = paste(studyid, dm$SubjectId, sep = "-"),
  SUBJID   = dm$SubjectId,
  RFSTDTC  = dm$IcDat,                          # Informed consent date
  RFENDTC  = dm$LcDat,                           # Last contact date
  RFXSTDTC = dm$RandDat,                          # First exposure date (randomization)
  RFXENDTC = dm$RfxEnDat,                         # Last exposure date
  RFICDTC  = dm$IcDat,                            # IC date
  RFPENDTC = dm$EventDate,                        # End of participation date
  DTHDTC   = ifelse(dm$ComplYN == "N" & !is.na(dm$EtDat) & nchar(dm$EtDat) > 0,
                    dm$EtDat, NA_character_),      # Death date (if early termination)
  DTHFL    = ifelse(!is.na(dm$EtDat) & nchar(dm$EtDat) > 0 & dm$ComplYN == "N",
                    "Y", NA_character_),
  SITEID   = dm$SiteCode,
  BRTHDTC  = dm$BrthDat,
  AGE      = dm$Age,
  AGEU     = "YEARS",
  SEX      = toupper(dm$Sex),
  RACE     = NA_character_,
  ETHNIC   = dm$Ethnicc,
  ARMCD    = dm$TrtCd,
  ARM      = dm$Trt,
  ACTARMCD = dm$TrtCd,
  ACTARM   = dm$Trt,
  COUNTRY  = "ITA"
)

# Write out
dir.create("sdtm/datasets/RDA", recursive = TRUE, showWarnings = FALSE)
dir.create("sdtm/datasets/XPT", recursive = TRUE, showWarnings = FALSE)

# XPT
dm_xpt <- dm_sdtm
attr(dm_xpt, "label") <- "Demographics"
for (cn in names(dm_xpt)) {
  attr(dm_xpt[[cn]], "label") <- cn
}
write_xpt(dm_xpt, "sdtm/datasets/XPT/dm.xpt", version = 8)

# RDA
dm <- dm_sdtm
save(dm, file = "sdtm/datasets/RDA/dm.rda")

cat("Created DM SDTM dataset:\n")
cat("  Rows:", nrow(dm_sdtm), "\n")
cat("  Cols:", ncol(dm_sdtm), "\n")
cat("  RFSTDTC values:\n")
print(dm_sdtm[, c("USUBJID", "RFSTDTC", "RFENDTC", "RFXSTDTC")])
