# =============================================================================
# Create Dummy Raw Data for STUDY-PILOT PR Domain Demo
# =============================================================================
# This script generates realistic dummy raw datasets that mirror the structure
# of the STUDY-PILOT EDC CRF exports for the PR domain.
#
# Datasets created:
#   - pr.csv         : Medical Procedures CRF page
#   - pr_meddra.csv  : MedDRA coded procedures
#   - lbs_img.csv    : Images and Videos CRF page
#   - lbs_simg.csv   : Collection Stool Image CRF page
#
# Variable names match the EDC export column names.
#
# SubjectID format: CC-SITE-NN-SSSS  (e.g., NL-AMC-01-0001)
# =============================================================================

set.seed(99)

library(tibble)

# --- Configuration -----------------------------------------------------------
n_subjects <- 8
studyid    <- "STUDY-PILOT"
sites      <- c("AMC-01", "LUH-02", "UHB-03")
countries  <- c("NL", "DE", "UK")
date_anchor <- as.Date("2025-06-01")

# Generate SubjectIDs in the real format: CC-SITE-NN-SSSS
subjectids <- character(n_subjects)
for (i in seq_len(n_subjects)) {
  country_idx <- ((i - 1) %% length(countries)) + 1
  subjectids[i] <- sprintf("%s-%s-%04d", countries[country_idx], sites[country_idx], i)
}

# Per-subject reference dates
ic_dates   <- date_anchor + sample(-30:0, n_subjects, replace = TRUE)
rfst_dates <- ic_dates  # RFSTDTC comes from icdat in the real study

# Visit event IDs from derive_visit_vars.sas
visit_eventids <- c("SCR", "V1", "PW1", "PW5", "V2", "PW15", "PW20", "V3", "PFU")
# Only site visits (not phone calls) would have imaging
img_eventids   <- c("V1", "V1_IMG", "V3", "V3_IMG")

# =============================================================================
# 1. PR — Medical Procedures CRF page
# =============================================================================
# Variables referenced in PR.sas from the pr raw dataset:
#   SubjectId, PRSPID, PRTrt (mapped to PRTRT), PRSev, PROccur (not used
#   directly, but SAS uses it), PRStDat, PREnDat, PROnGo, PRIndicat,
#   InitiatedDate, LastEditedDate
#
# The SAS program drops InitiatedDate/LastEditedDate from pr_1 and takes
# them from pr_meddra instead.

pr_terms_pool <- c(
  "Blood transfusion", "Appendectomy", "Tonsillectomy",
  "Vaccination", "Dental cleaning", "Physical examination",
  "Circumcision", "Caesarean section", "Colonoscopy",
  "MRI scan", "Ultrasound", "X-ray"
)

indication_pool <- c(
  "DIAGNOSTIC", "THERAPEUTIC", "PROPHYLACTIC",
  "Preventive / for screening purposes"
)

pr_rows <- list()
pr_counter <- 0
for (i in seq_len(n_subjects)) {
  n_pr <- sample(1:4, 1)
  for (j in seq_len(n_pr)) {
    pr_counter <- pr_counter + 1
    pr_start_offset <- sample(-60:80, 1)
    pr_start <- rfst_dates[i] + pr_start_offset
    pr_dur   <- sample(1:30, 1)
    pr_end   <- pr_start + pr_dur

    # ~20% ongoing
    ongoing <- runif(1) < 0.20

    pr_rows[[pr_counter]] <- tibble(
      SubjectId      = subjectids[i],
      PRSPID         = pr_counter,
      PRTrt          = sample(pr_terms_pool, 1),
      PRStDat        = format(pr_start, "%Y-%m-%d"),
      PREnDat        = if (ongoing) NA_character_ else format(pr_end, "%Y-%m-%d"),
      PROnGo         = if (ongoing) "Y" else "N",
      PRIndicat      = sample(indication_pool, 1),
      InitiatedDate  = format(pr_start - sample(0:3, 1), "%Y-%m-%d"),
      LastEditedDate = format(pr_start + sample(1:10, 1), "%Y-%m-%d")
    )
  }
}
pr <- dplyr::bind_rows(pr_rows)

# For some records where indication is "Preventive ...", add explicit text
prev_idx <- which(grepl("preventive", pr$PRIndicat, ignore.case = TRUE))
# The SAS code transforms: if upcase(prindc) = "PREVENTIVE..." then prindc = "OTHER"

# =============================================================================
# 2. PR_MEDDRA — MedDRA Coded Procedures
# =============================================================================
# Variables referenced in PR.sas from pr_meddra:
#   SubjectId, PRSPID, SOC_Name, SOC_Code, PT_Name, PT_Code,
#   HLGT_Name, HLGT_Code, HLT_Name, HLT_Code, LLT_Name, LLT_Code,
#   PT_SOC_Name, PT_SOC_Code, SOC_List,
#   DictInstance, Version, CodedOnDate, InitiatedDate, LastEditedDate

# MedDRA lookup for procedures (SOC = "Surgical and medical procedures")
meddra_pr_lookup <- tibble(
  term        = c("Blood transfusion", "Appendectomy", "Tonsillectomy",
                  "Vaccination", "Dental cleaning", "Physical examination",
                  "Circumcision", "Caesarean section", "Colonoscopy",
                  "MRI scan", "Ultrasound", "X-ray"),
  llt_name    = c("Blood transfusion", "Appendicectomy", "Tonsillectomy",
                  "Vaccination", "Teeth cleaning", "Physical examination",
                  "Circumcision", "Caesarean section", "Colonoscopy",
                  "Magnetic resonance imaging", "Ultrasound scan", "X-ray"),
  llt_code    = c("10005841", "10003074", "10044104",
                  "10046851", "10043262", "10034898",
                  "10009182", "10007560", "10010029",
                  "10025444", "10045265", "10048380"),
  pt_name     = c("Blood transfusion", "Appendicectomy", "Tonsillectomy",
                  "Vaccination", "Dental prophylaxis", "Physical examination",
                  "Circumcision", "Caesarean section", "Colonoscopy",
                  "Magnetic resonance imaging", "Ultrasonography", "Radiography"),
  pt_code     = c("10005841", "10003074", "10044104",
                  "10046851", "10012832", "10034898",
                  "10009182", "10007560", "10010029",
                  "10025444", "10045265", "10037782"),
  hlt_name    = c("Transfusion procedures", "Appendix therapeutic procedures",
                  "Tonsillar therapeutic procedures",
                  "Prophylactic procedures NEC",
                  "Dental therapeutic procedures NEC",
                  "Physical examination procedures",
                  "Male reproductive tract surgical procedures",
                  "Obstetric procedures",
                  "Lower GI tract endoscopic procedures",
                  "Magnetic resonance imaging",
                  "Ultrasonic and echographic investigations",
                  "Skeletal and cardiac muscle analyses"),
  hlt_code    = c("10044660", "10003076", "10044105",
                  "10037041", "10012197", "10034899",
                  "10026936", "10029906", "10010030",
                  "10025445", "10045266", "10039623"),
  hlgt_name   = c("Transfusions and related procedures",
                  "Gastrointestinal therapeutic procedures",
                  "Tonsillar therapeutic procedures NEC",
                  "Prophylaxis", "Dental therapeutic procedures",
                  "Physical examination topics",
                  "Male reproductive tract procedures",
                  "Obstetric procedures",
                  "Gastrointestinal investigations",
                  "Imaging procedures NEC",
                  "Diagnostic procedures NEC",
                  "Skeletal and cardiac muscle analyses"),
  hlgt_code   = c("10044661", "10017965", "10044106",
                  "10037042", "10012198", "10034900",
                  "10026937", "10029907", "10017965",
                  "10025446", "10045267", "10039624"),
  soc_name    = rep("Surgical and medical procedures", 12),
  soc_code    = rep("10042613", 12),
  pt_soc_name = rep("Surgical and medical procedures", 12),
  pt_soc_code = rep("10042613", 12),
  soc_list    = rep("P", 12)
)

pr_meddra_rows <- list()
for (r in seq_len(nrow(pr))) {
  term <- pr$PRTrt[r]
  m <- meddra_pr_lookup[meddra_pr_lookup$term == term, ]
  if (nrow(m) == 0L) m <- meddra_pr_lookup[1, ]  # fallback

  pr_meddra_rows[[r]] <- tibble(
    SubjectId      = pr$SubjectId[r],
    PRSPID         = pr$PRSPID[r],
    LLT_Name       = m$llt_name[1],
    LLT_Code       = m$llt_code[1],
    PT_Name        = m$pt_name[1],
    PT_Code        = m$pt_code[1],
    HLT_Name       = m$hlt_name[1],
    HLT_Code       = m$hlt_code[1],
    HLGT_Name      = m$hlgt_name[1],
    HLGT_Code      = m$hlgt_code[1],
    SOC_Name       = m$soc_name[1],
    SOC_Code       = m$soc_code[1],
    PT_SOC_Name    = m$pt_soc_name[1],
    PT_SOC_Code    = m$pt_soc_code[1],
    SOC_List       = m$soc_list[1],
    DictInstance   = "MedDRAPR",
    Version        = "27.0",
    CodedOnDate    = format(as.Date(pr$PRStDat[r]) + sample(1:5, 1), "%Y-%m-%d"),
    InitiatedDate  = format(as.Date(pr$PRStDat[r]), "%Y-%m-%d"),
    LastEditedDate = format(as.Date(pr$PRStDat[r]) + sample(1:8, 1), "%Y-%m-%d")
  )
}
pr_meddra <- dplyr::bind_rows(pr_meddra_rows)

# =============================================================================
# 3. LBS_IMG — Images and Videos CRF page
# =============================================================================
# Variables referenced in PR.sas (lbs_img dataset):
#   SubjectId, EventId,
#   lbs_imdhndnd  (Image dorsal hands occur: Y/blank)
#   lbs_imphndnd  (Image palm hands occur)
#   lbs_vllndnd   (Video lower lip occur)
#   lbs_vlelndnd  (Video lower eyelid occur)
#   lbs_vtndnd    (Video tongue occur)
#   lbs_imdhndr   (Image dorsal hands reason for not done)
#   lbs_imphndr   (Image palm hands reason)
#   lbs_vllndr    (Video lower lip reason)
#   lbs_vllelndr  (Video lower eyelid reason — note: SAS uses vllelndr)
#   lbs_vtndr     (Video tongue reason)
#   lbs_dhdate    (Dorsal hands date)
#   lbs_phdate    (Palm hands date)
#   lbs_vlldate   (Lower lip date)
#   lbs_vlyldate  (Lower eyelid date — note: SAS uses vlyldate)
#   lbs_vtdate    (Tongue date)
#   InitiatedDate, LastEditedDate

lbs_img_rows <- list()
img_counter <- 0
for (i in seq_len(n_subjects)) {
  # Each subject may have images at V1 and/or V3 visits
  n_visits <- sample(1:2, 1)
  selected_visits <- sample(img_eventids, n_visits)

  for (v in selected_visits) {
    img_counter <- img_counter + 1
    # Visit date offset from rfstdtc
    visit_date <- rfst_dates[i] + switch(v,
      "V1" = , "V1_IMG" = 0,
      "V3" = , "V3_IMG" = 141,
      0
    )

    # Randomly mark some images as not done
    occur_vals <- sample(c("Y", NA_character_), 5, replace = TRUE, prob = c(0.7, 0.3))
    reason_vals <- ifelse(is.na(occur_vals), sample(c("Equipment failure", "Subject refused"), 1), NA_character_)
    date_vals <- ifelse(!is.na(occur_vals), format(visit_date, "%Y-%m-%d"), NA_character_)

    lbs_img_rows[[img_counter]] <- tibble(
      SubjectId      = subjectids[i],
      EventId        = v,
      lbs_imdhndnd   = occur_vals[1],
      lbs_imphndnd   = occur_vals[2],
      lbs_vllndnd    = occur_vals[3],
      lbs_vlelndnd   = occur_vals[4],
      lbs_vtndnd     = occur_vals[5],
      lbs_imdhndr    = reason_vals[1],
      lbs_imphndr    = reason_vals[2],
      lbs_vllndr     = reason_vals[3],
      lbs_vllelndr   = reason_vals[4],
      lbs_vtndr      = reason_vals[5],
      lbs_dhdate     = date_vals[1],
      lbs_phdate     = date_vals[2],
      lbs_vlldate    = date_vals[3],
      lbs_vlyldate   = date_vals[4],
      lbs_vtdate     = date_vals[5],
      InitiatedDate  = format(visit_date, "%Y-%m-%d"),
      LastEditedDate = format(visit_date + sample(1:5, 1), "%Y-%m-%d")
    )
  }
}
lbs_img <- dplyr::bind_rows(lbs_img_rows)

# =============================================================================
# 4. LBS_SIMG — Collection Stool Image CRF page
# =============================================================================
# Variables referenced in PR.sas (lbs_simg dataset):
#   SubjectId, lbs_dimg (stool image collection date),
#   InitiatedDate, LastEditedDate
#
# In PR.sas: set lbs_simg_all; rename lbs_dimg=prstdat
# Then: prtrt = 'STOOL SAMPLE IMAGE'; preval = 'PARENT'; prorig = 'ePRO'

lbs_simg_rows <- list()
simg_counter <- 0
for (i in seq_len(n_subjects)) {
  # Each subject may have 2-5 stool image records (diary entries)
  n_simg <- sample(2:5, 1)
  for (j in seq_len(n_simg)) {
    simg_counter <- simg_counter + 1
    simg_date <- rfst_dates[i] + sample(1:140, 1)

    lbs_simg_rows[[simg_counter]] <- tibble(
      SubjectId      = subjectids[i],
      lbs_dimg       = format(simg_date, "%Y-%m-%d"),
      InitiatedDate  = format(simg_date, "%Y-%m-%d"),
      LastEditedDate = format(simg_date + sample(0:3, 1), "%Y-%m-%d")
    )
  }
}
lbs_simg <- dplyr::bind_rows(lbs_simg_rows)

# =============================================================================
# Write CSV files to raw/ folder
# =============================================================================
raw_dir <- tryCatch(
  file.path(dirname(rstudioapi::getSourceEditorContext()$path), "raw"),
  error = function(e) file.path(getwd(), "raw")
)

dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)

write.csv(pr,        file.path(raw_dir, "pr.csv"),        row.names = FALSE, na = "")
write.csv(pr_meddra, file.path(raw_dir, "pr_meddra.csv"), row.names = FALSE, na = "")
write.csv(lbs_img,   file.path(raw_dir, "lbs_img.csv"),   row.names = FALSE, na = "")
write.csv(lbs_simg,  file.path(raw_dir, "lbs_simg.csv"),  row.names = FALSE, na = "")

cat("Wrote dummy raw data for PR domain to:", raw_dir, "\n")
cat("Datasets:\n")
cat("  pr.csv         :", nrow(pr), "rows x", ncol(pr), "cols\n")
cat("  pr_meddra.csv  :", nrow(pr_meddra), "rows x", ncol(pr_meddra), "cols\n")
cat("  lbs_img.csv    :", nrow(lbs_img), "rows x", ncol(lbs_img), "cols\n")
cat("  lbs_simg.csv   :", nrow(lbs_simg), "rows x", ncol(lbs_simg), "cols\n")
