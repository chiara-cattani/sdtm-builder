# ******************************************************************************
# Create multi-source raw data for CO, EC, EX domains
#
# CO: co.csv (CO form) + comments from lbs (add lbs_spir column)
#                        + comments from vstat (add fupynr column)
# EC: ec.csv (from ex, investigator) + ex_diary.csv (parent) + ex_int.csv (interruptions)
# EX: same 3 sources as EC
# ******************************************************************************

library(dplyr)
library(readr)
set.seed(42)

raw_dir <- "raw"

subjects <- c("XX-SITEA-01-0001", "XX-SITEB-02-0002", "XX-SITEC-03-0003",
              "XX-SITEA-01-0004", "XX-SITEB-02-0005", "XX-SITEC-03-0006",
              "XX-SITEA-01-0007", "XX-SITEB-02-0008")

# ==============================================================================
# 1. UPDATE vstat.csv — add fupynr (reason if phone call not successful)
#    Only some subjects have unsuccessful calls
# ==============================================================================
vstat <- read_csv("raw/vstat.csv", show_col_types = FALSE)
# Add a fupynr column: NA for most, a reason for 2 subjects
vstat$fupynr <- NA_character_
vstat$fupynr[vstat$SubjectId == "XX-SITEC-03-0003"] <- "Subject not reachable"
vstat$fupynr[vstat$SubjectId == "XX-SITEB-02-0005"] <- "Wrong phone number on file"
write_csv(vstat, "raw/vstat.csv")
cat("vstat.csv updated:", nrow(vstat), "rows, cols:", paste(names(vstat), collapse = ", "), "\n")

# ==============================================================================
# 2. UPDATE lbs.csv — add lbs_spir (spirometry comment, used for CO)
#    Only some records have spirometry comments
# ==============================================================================
lbs <- read_csv("raw/lbs.csv", show_col_types = FALSE)
# Add lbs_spir column: actual comment text for some rows
lbs$lbs_spir <- NA_character_
# Put comments on visits where stool was collected (first visit per subject)
first_rows <- !duplicated(lbs$SubjectId)
lbs$lbs_spir[first_rows] <- paste0(
  "Stool collected ",
  sample(c("before", "after"), sum(first_rows), replace = TRUE),
  " first study product intake"
)
write_csv(lbs, "raw/lbs.csv")
cat("lbs.csv updated:", nrow(lbs), "rows, added lbs_spir\n")

# ==============================================================================
# 3. UPDATE co.csv — keep as is (these are direct CO form records)
#    Already has: SubjectId, CoSpId, CoRef, CoDtc, CoVal
# ==============================================================================
co <- read_csv("raw/co.csv", show_col_types = FALSE)
cat("co.csv unchanged:", nrow(co), "rows\n")

# ==============================================================================
# 4. REPLACE ec.csv — make it ONLY investigator-reported CRF records
#    (one record per subject per period, from ex source)
# ==============================================================================
ex <- read_csv("raw/ex.csv", show_col_types = FALSE)
# EC from investigator: one record per EX period, with ECOCCUR = Y/N
ec_inv <- ex %>%
  mutate(
    EcSpId  = paste0("EX-", sprintf("%03d", row_number())),
    EcTrt   = "STUDY PRODUCT",
    EcCat   = "STUDY PRODUCT",
    EcDose  = NA_real_,  # dose not collected at CRF level
    EcDoseU = NA_character_,
    EcDosfrm = "POWDER, FOR SUSPENSION",
    EcRoute  = NA_character_,
    EcStDat  = ExStDat,
    EcEnDat  = NA_character_,  # end date not always collected
    EcOccur  = "Y",
    EcStat   = "",
    EcReasnd = "",
    ExYn     = "Y",
    ExYnr    = ""
  ) %>%
  select(SubjectId, EcSpId, EcTrt, EcCat, EcDose, EcDoseU, EcDosfrm,
         EcRoute, EcStDat, EcEnDat, EcOccur, EcStat, EcReasnd, ExYn, ExYnr)

# Add a few N records (non-compliance)
set.seed(123)
n_skip <- 4
skip_idx <- sample(nrow(ec_inv), n_skip)
ec_inv$EcOccur[skip_idx] <- "N"
ec_inv$ExYn[skip_idx]    <- "N"
ec_inv$ExYnr[skip_idx]   <- sample(c("Subject forgot", "Vomited product", "Travel"),
                                    n_skip, replace = TRUE)

write_csv(ec_inv, "raw/ec.csv")
cat("ec.csv replaced:", nrow(ec_inv), "rows (investigator CRF)\n")

# ==============================================================================
# 5. CREATE ex_diary.csv — parent-reported diary records
#    One row per subject per day of diary (7-day diary windows)
# ==============================================================================
# Each subject has 2-3 diary windows, each with daily entries
diary_rows <- list()
for (subj in subjects) {
  subj_ex <- ex %>% filter(SubjectId == subj)
  # Use first 2 periods as diary windows
  n_windows <- min(2, nrow(subj_ex))
  for (w in seq_len(n_windows)) {
    start_date <- as.Date(subj_ex$ExStDat[w])
    n_servings <- sample(1:3, 1)
    for (d in 0:6) {
      day_date <- start_date + d
      prep_vols <- sample(100:200, n_servings)
      left_vols <- sample(0:30, n_servings)
      diary_rows[[length(diary_rows) + 1]] <- tibble(
        SubjectId  = subj,
        ActivityID = paste0("EX2_", format(day_date, "%Y%m%d")),
        Ex2StDat   = as.character(day_date),
        Ex2Nx      = paste(n_servings, "servings"),
        Ex2Ndna    = NA_character_,
        Ex2Pr1     = prep_vols[1],
        Ex2Lo1     = left_vols[1],
        Ex2Pr2     = if (n_servings >= 2) prep_vols[2] else NA_integer_,
        Ex2Lo2     = if (n_servings >= 2) left_vols[2] else NA_integer_,
        Ex2Pr3     = if (n_servings >= 3) prep_vols[3] else NA_integer_,
        Ex2Lo3     = if (n_servings >= 3) left_vols[3] else NA_integer_
      )
    }
  }
}
ex_diary <- bind_rows(diary_rows)
# Add a few "NOT APPLICABLE" days (no intake)
na_idx <- sample(nrow(ex_diary), 5)
ex_diary$Ex2Ndna[na_idx] <- "NOT APPLICABLE"
ex_diary$Ex2Nx[na_idx]   <- NA_character_
ex_diary$Ex2Pr1[na_idx]  <- NA_integer_
ex_diary$Ex2Lo1[na_idx]  <- NA_integer_

write_csv(ex_diary, "raw/ex_diary.csv")
cat("ex_diary.csv created:", nrow(ex_diary), "rows\n")

# ==============================================================================
# 6. CREATE ex_int.csv — study product interruptions
#    A few subjects had interruptions; some restarted
# ==============================================================================
int_subjects <- sample(subjects, 3)
int_rows <- list()
for (subj in int_subjects) {
  subj_ex <- ex %>% filter(SubjectId == subj)
  # Interruption happens mid-study
  int_date <- as.Date(subj_ex$ExStDat[2]) + sample(3:10, 1)
  restart <- sample(c("Y", "N"), 1, prob = c(0.7, 0.3))
  restart_date <- if (restart == "Y") as.character(int_date + sample(2:5, 1)) else NA_character_
  int_rows[[length(int_rows) + 1]] <- tibble(
    SubjectId  = subj,
    ExSpId     = paste0("EXI-", sprintf("%03d", length(int_rows) + 1)),
    ExEnDat1   = as.character(int_date - 1),  # last SP date before interruption
    ExStartYn  = restart,
    ExStDat1   = restart_date,
    ExSpSp     = sample(c("Adverse Event", "Travel", "Subject Decision"), 1)
  )
}
ex_int <- bind_rows(int_rows)
write_csv(ex_int, "raw/ex_int.csv")
cat("ex_int.csv created:", nrow(ex_int), "rows\n")

# ==============================================================================
# 7. CREATE eos_update — add RFXENDAT to eos.csv if not present
# ==============================================================================
eos <- read_csv("raw/eos.csv", show_col_types = FALSE)
if (!"RfxEnDat" %in% names(eos)) {
  # Last exposure date per subject (from ex)
  last_ex <- ex %>%
    group_by(SubjectId) %>%
    summarise(RfxEnDat = max(ExEnDat, na.rm = TRUE), .groups = "drop")
  eos <- eos %>% left_join(last_ex, by = "SubjectId")
  write_csv(eos, "raw/eos.csv")
  cat("eos.csv updated: added RfxEnDat\n")
} else {
  cat("eos.csv: RfxEnDat already present\n")
}

cat("\n=== Summary ===\n")
for (f in c("co", "ec", "ex", "ex_diary", "ex_int", "vstat", "lbs", "eos")) {
  d <- read_csv(file.path("study-pilot/raw", paste0(f, ".csv")), show_col_types = FALSE)
  cat(sprintf("  %s: %d rows x %d cols\n", f, nrow(d), ncol(d)))
}
