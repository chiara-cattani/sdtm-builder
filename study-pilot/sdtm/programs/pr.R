# ******************************************************************************
# STUDY-PILOT
# ******************************************************************************
# PROGRAM NAME  : pr.R
# PURPOSE       : SDTM PR Domain - Procedures
# ------------------------------------------------------------------------------
# NOTES :
#   Raw datasets : pr, pr_meddra, lbs_img, lbs_simg
#   Dependencies : sdtmbuilder package
#   Reference    : SAS template
#
#   Three CRF sources are combined:
#     1. PR       - Medical Procedure page (MedDRA coded)
#     2. LBS_IMG  - Images and Videos page (5 assessments per visit)
#     3. LBS_SIMG - Collection Stool Image page (diary entries)
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2026-02-12 - sdtmbuilder - Auto-generated program
# ******************************************************************************

# Configuration ----
library(sdtmbuilder)
library(dplyr)
library(tidyr)
library(stringr)

study       <- "STUDY-PILOT"
sdtm_domain <- "PR"

# Visit map (from config.yaml / derive_visit_vars.sas) ----
visit_lookup <- tribble(
  ~eventid,   ~VISIT,                       ~VISITNUM, ~VISITDY,
  "SCR",      "Visit 0: Screening",         1,         1,
  "V1",       "Visit 1",                    2,         1,
  "V1_IMG",   "Visit 1",                    2,         1,
  "PW1",      "Phone Call 1",               3,         8,
  "PW5",      "Phone Call 2",               4,         36,
  "V2",       "Visit 2",                    5,         71,
  "PW15",     "Phone Call 3",               6,         106,
  "PW20",     "Phone Call 4",               7,         140,
  "V3",       "Visit 3",                    8,         141,
  "V3_IMG",   "Visit 3",                    8,         141,
  "PFU",      "Phone Call 5: Follow-up",    9,         155
)

# Image/video procedure labels (from SAS $lbs_img_prtrt format) ----
img_prtrt_map <- tribble(
  ~img_var,        ~PRTRT,
  "lbs_imdhndnd",  "IMAGE DORSAL VIEW HANDS",
  "lbs_imphndnd",  "IMAGE PALM VIEW HANDS",
  "lbs_vllndnd",   "VIDEO CHILD'S LOWER LIP",
  "lbs_vlelndnd",  "VIDEO CHILD'S LOWER EYELID",
  "lbs_vtndnd",    "VIDEO CHILD'S TONGUE"
)

# Set working directory ----
if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
} else if (exists("progdir")) {
  setwd(progdir)
}

# Import metadata ----
study_meta <- read_study_metadata_excel("../../metadata/Study_Metadata.xlsx")
ct_spec    <- read_study_ct_excel("../../metadata/Study_CT.xlsx")

target_meta      <- study_meta$target_meta
domain_meta      <- study_meta$domain_meta
value_level_meta <- study_meta$value_level_meta

# Import data ----
all_raw <- load_raw_datasets("../../raw")
for (nm in names(all_raw)) {
  all_raw[[nm]] <- all_raw[[nm]] %>% standardize_names() %>% convert_blanks_to_na()
}

pr1       <- all_raw[["pr"]]
pr_med1   <- all_raw[["pr_meddra"]]
lbs_img1  <- all_raw[["lbs_img"]]
lbs_simg1 <- all_raw[["lbs_simg"]]
dm1       <- all_raw[["dm"]]
ic1       <- all_raw[["ic"]]

# Get MedDRA version ----
meddra_ver <- get_meddra_version(indata = pr_med1)

# Prepare DM for RFSTDTC ----
dm_slim <- dm1 %>%
  left_join(ic1 %>% select(subjectid, icdat), by = "subjectid") %>%
  mutate(
    subjid  = subjectid,
    rfstdtc = format_iso_dtc(parse_partial_date(icdat))
  ) %>%
  select(subjid, rfstdtc) %>%
  distinct()

# =============================================================================
# Source 1: PR — Medical Procedures (MedDRA coded)
# =============================================================================
pr_coded <- pr1 %>%
  left_join(
    pr_med1 %>%
      select(subjectid, prspid,
             llt_name, llt_code, pt_name, pt_code,
             hlt_name, hlt_code, hlgt_name, hlgt_code,
             soc_name, soc_code, pt_soc_name, pt_soc_code, soc_list,
             codedondate, initiateddate_med = initiateddate,
             lastediteddate_med = lastediteddate),
    by = c("subjectid", "prspid")
  ) %>%
  mutate(
    sourceds = "PR",
    PRTRT    = prtrt,
    PRSPID   = as.character(prspid),
    PRSTDAT  = prstdat,
    PRENDAT  = prendat,
    PRCAT    = "MEDICAL PROCEDURE",
    PRORIG   = "CRF",
    PREVAL   = "INVESTIGATOR",

    # Indication
    PRINDC = case_when(
      toupper(prindicat) == "PREVENTIVE / FOR SCREENING PURPOSES" ~ "OTHER",
      TRUE ~ prindicat
    ),
    PRINDCX = if_else(
      toupper(prindicat) == "PREVENTIVE / FOR SCREENING PURPOSES",
      prindicat, NA_character_
    ),

    # PRENRTPT + PRENTPT
    PRENRTPT = case_when(
      prongo == "Y" ~ "ONGOING",
      prongo == "N" ~ "BEFORE",
      TRUE ~ NA_character_
    ),
    PRENTPT = if_else(!is.na(prongo), "END OF STUDY", NA_character_),

    # MedDRA coded fields
    PRLLT    = llt_name,
    PRLLTCD  = as.numeric(llt_code),
    PRDECOD  = pt_name,
    PRPTCD   = as.numeric(pt_code),
    PRHLT    = hlt_name,
    PRHLTCD  = as.numeric(hlt_code),
    PRHLGT   = hlgt_name,
    PRHLGTCD = as.numeric(hlgt_code),
    PRBODSYS = soc_name,
    PRBDSYCD = as.numeric(soc_code),
    PRSOC    = pt_soc_name,
    PRSOCCD  = as.numeric(pt_soc_code),
    PRSOCLST = soc_list,

    PRDICT  = if_else(!is.na(PRDECOD), paste("MedDRA", meddra_ver), NA_character_),
    PRPRESP = NA_character_,
    PROCCUR = NA_character_,
    PRREASOC = NA_character_,
    eventid  = NA_character_,

    # Audit
    PRCODED  = format_iso_dtc(parse_partial_date(codedondate)),
    PRINITD  = format_iso_dtc(parse_partial_date(
      if_else(!is.na(initiateddate_med), initiateddate_med, initiateddate)
    )),
    PRLTEDID = format_iso_dtc(parse_partial_date(
      if_else(!is.na(lastediteddate_med), lastediteddate_med, lastediteddate)
    ))
  )

# =============================================================================
# Source 2: LBS_IMG — Images and Videos (5 assessments per visit, transpose)
# =============================================================================
# Reshape from wide (1 row per visit) to long (1 row per image/video)
occur_cols  <- c("lbs_imdhndnd", "lbs_imphndnd", "lbs_vllndnd", "lbs_vlelndnd", "lbs_vtndnd")
reason_cols <- c("lbs_imdhndr",  "lbs_imphndr",  "lbs_vllndr",  "lbs_vllelndr", "lbs_vtndr")
date_cols   <- c("lbs_dhdate",   "lbs_phdate",   "lbs_vlldate", "lbs_vlyldate", "lbs_vtdate")

lbs_img_long <- list()
for (k in seq_along(occur_cols)) {
  lbs_img_long[[k]] <- lbs_img1 %>%
    select(subjectid, eventid,
           occur = !!sym(occur_cols[k]),
           reason = !!sym(reason_cols[k]),
           pr_date = !!sym(date_cols[k]),
           initiateddate, lastediteddate) %>%
    mutate(
      img_var = occur_cols[k],
      PRTRT   = img_prtrt_map$PRTRT[img_prtrt_map$img_var == occur_cols[k]],
      PROCCUR = if_else(!is.na(occur), "N", "Y"),
      PRREASOC = reason,
      PRSTDAT  = pr_date
    )
}
lbs_img_all <- bind_rows(lbs_img_long) %>%
  mutate(
    sourceds = "LBS_IMG",
    PRPRESP  = "Y",
    PRCAT    = "IMAGES AND VIDEOS",
    PRORIG   = "CRF",
    PREVAL   = "INVESTIGATOR",
    PRENDAT  = NA_character_,
    PRSPID   = NA_character_,
    PRINDC   = NA_character_,
    PRINDCX  = NA_character_,
    PRENRTPT = NA_character_,
    PRENTPT  = NA_character_,
    PRLLT = NA_character_, PRLLTCD = NA_real_, PRDECOD = NA_character_,
    PRPTCD = NA_real_, PRHLT = NA_character_, PRHLTCD = NA_real_,
    PRHLGT = NA_character_, PRHLGTCD = NA_real_, PRBODSYS = NA_character_,
    PRBDSYCD = NA_real_, PRSOC = NA_character_, PRSOCCD = NA_real_,
    PRSOCLST = NA_character_, PRDICT = NA_character_,
    PRCODED  = NA_character_,
    PRINITD  = format_iso_dtc(parse_partial_date(initiateddate)),
    PRLTEDID = format_iso_dtc(parse_partial_date(lastediteddate))
  )

# =============================================================================
# Source 3: LBS_SIMG — Collection Stool Image (diary entries)
# =============================================================================
lbs_simg_all <- lbs_simg1 %>%
  mutate(
    sourceds = "LBS_SIMG",
    PRTRT    = "STOOL SAMPLE IMAGE",
    PRSTDAT  = lbs_dimg,
    PRENDAT  = NA_character_,
    PRCAT    = "COLLECTION STOOL IMAGE",
    PREVAL   = "PARENT",
    PRORIG   = "ePRO",
    PRPRESP  = NA_character_,
    PROCCUR  = NA_character_,
    PRREASOC = NA_character_,
    PRSPID   = NA_character_,
    PRINDC   = NA_character_,
    PRINDCX  = NA_character_,
    PRENRTPT = NA_character_,
    PRENTPT  = NA_character_,
    eventid  = NA_character_,
    PRLLT = NA_character_, PRLLTCD = NA_real_, PRDECOD = NA_character_,
    PRPTCD = NA_real_, PRHLT = NA_character_, PRHLTCD = NA_real_,
    PRHLGT = NA_character_, PRHLGTCD = NA_real_, PRBODSYS = NA_character_,
    PRBDSYCD = NA_real_, PRSOC = NA_character_, PRSOCCD = NA_real_,
    PRSOCLST = NA_character_, PRDICT = NA_character_,
    PRCODED  = NA_character_,
    PRINITD  = format_iso_dtc(parse_partial_date(initiateddate)),
    PRLTEDID = format_iso_dtc(parse_partial_date(lastediteddate))
  )

# =============================================================================
# Combine all sources and derive final SDTM variables
# =============================================================================
common_cols <- c("subjectid", "sourceds", "eventid",
                 "PRTRT", "PRSPID", "PRSTDAT", "PRENDAT",
                 "PRCAT", "PRORIG", "PREVAL", "PRPRESP", "PROCCUR", "PRREASOC",
                 "PRINDC", "PRINDCX", "PRENRTPT", "PRENTPT",
                 "PRLLT", "PRLLTCD", "PRDECOD", "PRPTCD",
                 "PRHLT", "PRHLTCD", "PRHLGT", "PRHLGTCD",
                 "PRBODSYS", "PRBDSYCD", "PRSOC", "PRSOCCD",
                 "PRSOCLST", "PRDICT",
                 "PRCODED", "PRINITD", "PRLTEDID")

pr_all <- bind_rows(
  pr_coded     %>% select(any_of(common_cols)),
  lbs_img_all  %>% select(any_of(common_cols)),
  lbs_simg_all %>% select(any_of(common_cols))
)

# Derive final PR variables ----
pr2 <- pr_all %>%
  left_join(dm_slim, by = c("subjectid" = "subjid")) %>%
  mutate(
    # --- Identifiers ---
    STUDYID  = study,
    DOMAIN   = sdtm_domain,
    SUBJID   = subjectid,
    USUBJID  = paste(study, subjectid, sep = "-"),
    SOURCEID = paste("CRF:", sourceds),

    # --- Dates ---
    PRSTDTC = format_iso_dtc(parse_partial_date(PRSTDAT)),
    PRENDTC = format_iso_dtc(parse_partial_date(PRENDAT)),

    # --- Link ID for stool images ---
    PRLNKID = if_else(sourceds == "LBS_SIMG",
                       paste(PRTRT, PRSTDTC, sep = "-"),
                       NA_character_)
  ) %>%

  # --- Visit variables (for records with eventid) ---
  left_join(visit_lookup, by = "eventid") %>%

  # --- Study Day ---
  derive_dy("PRSTDY", "PRSTDTC", "rfstdtc") %>%
  derive_dy("PRENDY", "PRENDTC", "rfstdtc") %>%

  # --- Sequence ---
  derive_seq("PRSEQ", by = "USUBJID",
             order_by = c("PRCAT", "PRTRT", "VISITNUM", "PRSTDTC"))

# Finalize ----
pr_final <- export_domain(
  data        = pr2,
  domain      = sdtm_domain,
  output_dir  = "../../sdtm/datasets",
  formats     = c("xpt", "rds", "csv"),
  xpt_version = 8L,
  target_meta = target_meta,
  domain_meta = domain_meta,
  keys        = c("STUDYID", "USUBJID", "PRCAT", "PRTRT", "VISITNUM", "PRSTDTC"),
  drop_empty_perm = TRUE
)

cli::cli_alert_success("PR domain created: {nrow(pr2)} rows")
