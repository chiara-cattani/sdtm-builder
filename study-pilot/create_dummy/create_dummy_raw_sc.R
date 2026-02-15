# =============================================================================
# Create Dummy Raw Data: sc.csv (Subject Characteristics)
# =============================================================================
# Run from study-pilot/ directory
# Output: raw/sc.csv
# Source columns for SC domain:
#   - gestagew, gestaged → GSTABRTH (Gestational Age at Birth)
#   - btype             → DLVRMODE (Mode of Delivery)
#   - hcst1             → HLTHSTAT (Health Status)
# =============================================================================

set.seed(42)

n_subjects <- 8
sites      <- c("SITEA-01", "SITEB-02", "SITEC-03")
countries  <- c("XX", "XX", "XX")

subjectids <- character(n_subjects)
for (i in seq_len(n_subjects)) {
  country_idx <- ((i - 1) %% length(countries)) + 1
  subjectids[i] <- sprintf("%s-%s-%04d", countries[country_idx], sites[country_idx], i)
}

# Gestational age at birth: weeks (36-42) and days (0-6)
gestagew <- sample(36:42, n_subjects, replace = TRUE)
gestaged <- sample(0:6, n_subjects, replace = TRUE)

# Mode of delivery
btype <- sample(c("VAGINAL", "CAESAREAN SECTION"), n_subjects, replace = TRUE,
                prob = c(0.7, 0.3))

# Health status at screening
hcst1 <- sample(c("GOOD", "FAIR", "POOR"), n_subjects, replace = TRUE,
                prob = c(0.6, 0.3, 0.1))

# EventId: SC/LBS data collected at screening
sc <- tibble::tibble(
  SubjectId = subjectids,
  EventId   = "SCR",
  gestagew  = gestagew,
  gestaged  = gestaged,
  btype     = btype,
  hcst1     = hcst1
)

dir.create("raw", showWarnings = FALSE, recursive = TRUE)
write.csv(sc, "raw/sc.csv", row.names = FALSE, na = "")
cat("Created raw/sc.csv:", nrow(sc), "rows x", ncol(sc), "cols\n")
