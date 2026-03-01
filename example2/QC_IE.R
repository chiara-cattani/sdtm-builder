# ******************************************************************************
# Danone Nutricia Research
# ******************************************************************************
# STUDY/PROJECT : SONA / 23REX0061265
# PROGRAM PATH  : SAS/SONA/Files/SDTM Conversion/40 SDTM Programs
# PROGRAM NAME  : QC_IE.R
# PURPOSE       : QC SDTM data for Inclusion/Exclusion Criteria Not Met
# ------------------------------------------------------------------------------
# NOTES :
#   - Requires: sdtm_ti with IETESTCD/IETEST mapping, SDTM DM (USUBJID, SUBJID,
#     RFSTDTC), and raw IE source (brought in via copy_import()).
#   - SOURCEID is kept as a custom variable; 
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2025-10-07 - Vikas Shokeen - Initial version
# 2025-10-10 - Vikas Shokeen - Updated the working directory according to both
#							   LSAF and RSTUDIO
# ******************************************************************************

# --- Configuration -------------------------------------------------------------

suppressPackageStartupMessages({
  library(nutriciaconfig)
  library(readxl)
  library(haven)
  library(sdtm.oak)
  library(dplyr)
  library(stringr)
  library(rlang)
})

nutricia_config()

# Set working directory ----
if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  # Running in RStudio
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
} else {
  # Running in LSAF
  setwd(progdir)
}


# Import metadata
sdtm_ct  <- read_excel("../31 Metadata Data/SONA_CT.xlsx", sheet = "CT")
sdtm_var <- read_sas("../31 Metadata Data/sdtm_3_4_variables.sas7bdat")
sdtm_dom <- read_sas("../31 Metadata Data/sdtm_3_4_domains.sas7bdat")

# --- Parameters & Metadata -----------------------------------------------------

study       <- "SONA"
sdtm_domain <- "IE"   # Target SDTM domain

# --- Inputs -------------------------------------------------------------------

# Raw IE-like source via your in-house importer
ie_raw  <- copy_import(inset = "ie")

# SDTM DM and TI (TI used for IETESTCD→IETEST lookup)
sdtm_dm <- read_sas("../41 SDTM Data/dm.sas7bdat")
sdtm_ti <- read_sas("../41 SDTM Data/ti.sas7bdat")

# --- Helpers ------------------------------------------------------------------

check_ie <- function(ie_df) {
  # Lowercase names for robust matching
  names(ie_df) <- tolower(names(ie_df))
  
  incl_cols <- grep("^ieintestcd", names(ie_df), value = TRUE)
  excl_cols <- grep("^ieextestcd", names(ie_df), value = TRUE)
  
  for (i in seq_len(nrow(ie_df))) {
    row     <- ie_df[i, ]
    usubjid <- row$usubjid
    ieyn    <- tolower(trimws(row$ieyn))
    
    if (!is.na(ieyn) && ieyn == "y") {
      # Inclusion checks
      for (nm in incl_cols) {
        val <- row[[nm]]
        if (!is.na(val) && val != "") {
          message(sprintf("###WARNING: Subject [%s] eligible='Y' but has inclusion fail value in [%s]",
                          usubjid, nm))
        }
      }
      # Exclusion checks
      for (nm in excl_cols) {
        val <- row[[nm]]
        if (!is.na(val) && val != "") {
          message(sprintf("###WARNING: Subject [%s] eligible='Y' but has exclusion fail value in [%s]",
                          usubjid, nm))
        }
      }
    }
  }
}

get_max_suffix <- function(prefix, colnames_vec) {
  cols <- grep(paste0("^", prefix), colnames_vec, value = TRUE)
  nums <- suppressWarnings(as.numeric(gsub("[^0-9]", "", cols)))
  max(nums, na.rm = TRUE)
}

# Transpose helper: builds one record per failed criterion
transpose_ie <- function(df, prefix, category, studyid, domain) {
  # prefix: "in" for inclusion; "ex" for exclusion
  # category: "inclusion" | "exclusion"
  cols <- grep(paste0("^ie", prefix), names(df), value = TRUE)
  out  <- vector("list", length = 0L)
  
  for (i in seq_len(nrow(df))) {
    row_i <- df[i, ]
    if (!length(cols)) next
    
    for (j in seq_along(cols)) {
      val <- row_i[[cols[j]]]
      if (!is.na(val) && nzchar(as.character(val))) {
        out[[length(out) + 1L]] <- data.frame(
          studyid   = studyid,
          domain    = domain,
          subjid    = row_i$subjectid,
          sourceid  = "CRF: Eligibility",                                    # <- required
          ietestcd  = toupper(paste0(prefix, "CL", sprintf("%02d", j))),     # INCL## / EXCL##
          iecat     = toupper(category),
          eventdate = row_i$eventdate,
          ieorres   = ifelse(prefix == "in", "N", "Y"),
          iestresc  = ifelse(prefix == "in", "N", "Y"),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (length(out)) bind_rows(out) else
    tibble(studyid = character(0), domain = character(0), subjid = character(0),
           sourceid = character(0), ietestcd = character(0), iecat = character(0),
           eventdate = as.Date(character(0)), ieorres = character(0),
           iestresc = character(0))
}

# --- QC check on raw ----------------------------------------------------------

check_ie(ie_raw)

# --- Normalize names & filter non-eligible ------------------------------------

names(ie_raw) <- tolower(names(ie_raw))
names(sdtm_dm) <- tolower(names(sdtm_dm))
names(sdtm_ti) <- tolower(names(sdtm_ti))

non_eligible <- ie_raw[!tolower(trimws(ie_raw$ieyn)) %in% "y", , drop = FALSE]

not_eligible_subjects      <- unique(non_eligible$subjectid)
not_eligible_subjects_num  <- length(not_eligible_subjects)

cat("Non-eligible subjects: ",
    if (length(not_eligible_subjects)) paste(not_eligible_subjects, collapse = ", ") else "None",
    "\n")
cat("Number of non-eligible subjects: ", not_eligible_subjects_num, "\n")

# Keep only relevant variables for transposition
ie_src <- non_eligible %>%
  select(any_of(c(
    "usubjid", "subjectid", "eventdate", "eventid"
  )),
  matches("^iein"),
  matches("^ieex"))

# Optional diagnostics
max_in <- get_max_suffix("iein", names(ie_src))
max_ex <- get_max_suffix("ieex", names(ie_src))
cat("Max number of inclusion criteria: ", ifelse(is.finite(max_in), max_in, 0L), "\n")
cat("Max number of exclusion criteria: ", ifelse(is.finite(max_ex), max_ex, 0L), "\n")

# --- Transpose to long IE -----------------------------------------------------

ie_in <- transpose_ie(ie_src, prefix = "in", category = "inclusion",
                      studyid = study, domain = sdtm_domain)
ie_ex <- transpose_ie(ie_src, prefix = "ex", category = "exclusion",
                      studyid = study, domain = sdtm_domain)

ie_long <- bind_rows(ie_in, ie_ex)

# --- Lookup IETEST from TI ----------------------------------------------------

# Expect TI to have IETESTCD/IETEST (lowercase enforced above)

ietest_lookup <- setNames(sdtm_ti$ietest, sdtm_ti$ietestcd)

# --- Merge DM: add USUBJID/RFSTDTC, derive IEDTC/IEDY ------------------------

# Ensure EVENTDATE and RFSTDTC are handled as dates safely
# (RFSTDTC is usually ISO8601 char in SDTM; we’ll parse first 10 chars)
safe_as_date <- function(x) {
  x <- as.character(x)
  x <- ifelse(nchar(x) >= 10, substr(x, 1, 10), x)
  suppressWarnings(as.Date(x))
}

ie4 <- ie_long %>%
  left_join(
    sdtm_dm %>%
      select(subjid, usubjid, rfstdtc),
    by = "subjid"
  ) %>%
  mutate(
    # IETEST via lookup
    ietest = unname(ietest_lookup[ietestcd]),
    # Dates
    eventdate_dt = safe_as_date(eventdate),
    rfstdtc_dt   = safe_as_date(rfstdtc),
    iedtc        = format(eventdate_dt, "%Y-%m-%d"),
    iedy         = ifelse(!is.na(eventdate_dt) & !is.na(rfstdtc_dt),
                          as.integer(eventdate_dt - rfstdtc_dt + 1L),
                          NA_integer_)
  )

# Warn on curly apostrophes in IETEST (common text-cleanliness QC)
if (any(grepl("’", ie4$ietest %||% ""))) {
  warning("###WARNING: IETEST contains curved apostrophes.")
}

# --- Sequence (IESEQ) & enforce SOURCEID --------------------------------------

ie_final <- ie4 %>%
  arrange(usubjid, iedtc, ietestcd) %>%
  group_by(usubjid) %>%
  mutate(ieseq = dplyr::row_number()) %>%
  ungroup() %>%
  mutate(sourceid = "CRF: Eligibility")  # ensure it’s present on all records

# --- Finalize (may drop non-standard cols depending on config) --------

ie <- sdtm_create_domain(ie_final)


