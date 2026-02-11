# =============================================================================
# STUDY-PILOT Study — PR Domain Demo
# =============================================================================
# This script demonstrates how to use sdtmbuilder to:
#   1. Read study metadata and controlled terminology
#   2. Load and prepare raw data (merging CRF datasets as in PR.sas)
#   3. Build the PR (Procedures) SDTM domain
#   4. Export the PR dataset as XPT
#   5. Auto-generate the PR derivation R program
#
# Prerequisites:
#   - sdtmbuilder package installed (devtools::install("."))
#   - Raw datasets in study-pilot/raw/ (run create_dummy_raw_pr.R first, or
#     point RAW_DATA_DIR to your real export folder)
#
# Raw datasets:
#   - pr.csv         : Medical Procedures CRF page
#   - pr_meddra.csv  : MedDRA coded procedures
#   - lbs_img.csv    : Images and Videos CRF page
#   - lbs_simg.csv   : Collection Stool Image CRF page
# =============================================================================

library(sdtmbuilder)

# ---- 0. Configuration -------------------------------------------------------

# >>> CHANGE THIS to the path where your raw datasets are stored <<<
# For the dummy demo, use the local raw/ folder.
# For the real study, point to your EDC export folder.
RAW_DATA_DIR <- "raw"

# Working directory: the study-pilot folder
DEMO_DIR <- tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) getwd()
)
setwd(DEMO_DIR)

STUDYID <- "STUDY-PILOT"

# ---- 1. Read metadata & controlled terminology ------------------------------

cli::cli_h1("Step 1: Reading metadata")

study_meta <- read_study_metadata_excel("metadata/Study_Metadata.xlsx")
ct_lib     <- read_study_ct_excel("metadata/Study_CT.xlsx")

target_meta      <- study_meta$target_meta
domain_meta      <- study_meta$domain_meta
value_level_meta <- study_meta$value_level_meta
study_name       <- study_meta$study_name

cli::cli_alert_success("Study: {study_name}")
cli::cli_alert_info(
  "Target metadata: {nrow(target_meta)} variables across {length(unique(target_meta$domain))} domains"
)
cli::cli_alert_info(
  "Controlled terminology: {nrow(ct_lib)} terms across {length(unique(ct_lib$codelist_id))} codelists"
)

# ---- 2. Load raw data -------------------------------------------------------

cli::cli_h1("Step 2: Loading raw data")

# Load all datasets from the raw data directory
all_raw <- load_raw_datasets(RAW_DATA_DIR)
cli::cli_alert_info(
  "Found {length(all_raw)} raw datasets: {paste(names(all_raw), collapse = ', ')}"
)

# Standardise column names to lowercase for all datasets
for (nm in names(all_raw)) {
  all_raw[[nm]] <- all_raw[[nm]] %>%
    standardize_names() %>%
    convert_blanks_to_na()
}

pr_crf      <- all_raw[["pr"]]
pr_meddra   <- all_raw[["pr_meddra"]]
lbs_img_crf <- all_raw[["lbs_img"]]
lbs_simg    <- all_raw[["lbs_simg"]]
dm_data     <- all_raw[["dm"]]
ic_data     <- all_raw[["ic"]]

# =============================================================================
# 2a. Replicate PR.sas data preparation logic
# =============================================================================
# PR.sas merges/transforms 4 raw datasets into a single pr_all2 before the
# final SDTM derivation step. We replicate that logic here.

# --- Source 1: PR CRF + PR_MEDDRA merge (by subjectid, prspid) ---------------
# SAS: merge pr_1(drop=initiateddate lastediteddate)
#            pr_meddra_1(keep=subjectid prspid soc_: pt_: hlgt_: hlt_: llt_:
#                        DictInstance version codedondate initiateddate lastediteddate)
#      by subjectid prspid;
cli::cli_alert_info("Merging pr + pr_meddra by subjectid + prspid ...")

# Drop initiateddate/lastediteddate from pr (keep the ones from pr_meddra)
pr_merge <- pr_crf[, setdiff(names(pr_crf), c("initiateddate", "lastediteddate")),
                   drop = FALSE]

# Keep relevant columns from pr_meddra
meddra_keep_patterns <- c("subjectid", "prspid",
                          "^soc_", "^pt_", "^hlgt_", "^hlt_", "^llt_",
                          "dictinstance", "version", "codedondate",
                          "initiateddate", "lastediteddate")
meddra_cols <- unique(unlist(lapply(meddra_keep_patterns, function(p) {
  grep(p, names(pr_meddra), value = TRUE, ignore.case = TRUE)
})))
pr_meddra_slim <- pr_meddra[, meddra_cols, drop = FALSE]

pr_all <- dplyr::left_join(pr_merge, pr_meddra_slim,
                           by = c("subjectid", "prspid"))

# Audit variables from pr_meddra: SAS %sdtm_dt2dtc macros
pr_all$prcoded  <- format_iso_dtc(pr_all$codedondate)
pr_all$prinitd  <- format_iso_dtc(pr_all$initiateddate)
pr_all$prltedid <- format_iso_dtc(pr_all$lastediteddate)

# Get MedDRA version dynamically (like %get_meddra_version macro)
meddra_version <- get_meddra_version(pr_meddra)
if (nzchar(meddra_version)) {
  cli::cli_alert_info("MedDRA version detected: {meddra_version}")
} else {
  meddra_version_raw <- unique(pr_all$version[!is.na(pr_all$version)])
  meddra_version <- if (length(meddra_version_raw) > 0) meddra_version_raw[1] else "27.0"
  cli::cli_alert_warning("Using fallback MedDRA version: {meddra_version}")
}

# Add sourceds column (PR.sas: indsname = _inds -> sourceds = "PR")
pr_all$sourceds <- "PR"

cli::cli_alert_success(
  "Merged pr + pr_meddra: {nrow(pr_all)} rows x {ncol(pr_all)} cols"
)

# --- Source 2: LBS_IMG — pivot from wide to long (one row per image type) -----
# SAS logic: array over 5 occur/reasoc/date triplets, output 1 record each
# prtrt from $lbs_img_prtrt format, proccur = ifc(^missing(occur), "N", "Y")
# (Note: SAS reverses the logic: if occur IS populated → proccur = "N" = not done?
#  Actually in the CRF, the occur variable = "Y" means "done". The SAS code says:
#  proccur = ifc(^missing(occur[i]), "N", "Y") which means:
#    - occur is NOT missing → proccur = "N" [i.e. the *not-done* is FALSE]
#    - occur IS missing     → proccur = "Y" [i.e. procedure was NOT done → "Y"]
#  Wait — re-reading the SAS: proccur = ifc(^missing(occur[i]), "N", "Y")
#  ^missing = NOT missing. So if occur IS filled → "N", if missing → "Y".
#  But PR.PROCCUR = "Y" typically means "procedure occurred". In this CRF context:
#   - lbs_imdhndnd = "Y" means "not done" (ndnd = "not done not done"?)
#  Let's look at the variable names: imdhndnd likely = "image dorsal hand ND ND"
#  So the occur var = "Y" means the image was NOT done (it's a "not done" flag).
#  Then proccur = ifc(^missing(occur[i]), "N", "Y"):
#    - occur filled (="Y", i.e. not-done=Y) → proccur = "N" (procedure NOT occurred)
#    - occur missing → proccur = "Y" (procedure occurred by default)
#  This makes sense! The ndnd vars are "not done" indicators.
cli::cli_alert_info("Pivoting lbs_img from wide to long (5 image types) ...")

# LBS_IMG format map
lbs_img_prtrt_map <- c(
  "imdhndnd" = "IMAGE DORSAL VIEW HANDS",
  "imphndnd" = "IMAGE PALM VIEW HANDS",
  "vllndnd"  = "VIDEO CHILD'S LOWER LIP",
  "vlelndnd" = "VIDEO CHILD'S LOWER EYELID",
  "vtndnd"   = "VIDEO CHILD'S TONGUE"
)

# Arrays matching the SAS code
occur_cols  <- c("lbs_imdhndnd", "lbs_imphndnd", "lbs_vllndnd",
                 "lbs_vlelndnd", "lbs_vtndnd")
reasoc_cols <- c("lbs_imdhndr",  "lbs_imphndr",  "lbs_vllndr",
                 "lbs_vllelndr", "lbs_vtndr")
date_cols   <- c("lbs_dhdate",   "lbs_phdate",   "lbs_vlldate",
                 "lbs_vlyldate", "lbs_vtdate")

# Extract the short key from occur column name: lbs_XXXndnd -> XXX part
occur_keys <- gsub("^lbs_", "", occur_cols)

lbs_img_long_rows <- list()
row_counter <- 0
for (r in seq_len(nrow(lbs_img_crf))) {
  for (k in seq_along(occur_cols)) {
    row_counter <- row_counter + 1
    occur_val  <- lbs_img_crf[[occur_cols[k]]][r]
    reasoc_val <- lbs_img_crf[[reasoc_cols[k]]][r]
    date_val   <- lbs_img_crf[[date_cols[k]]][r]

    # SAS: prtrt = put(scan(upcase(vname(occur[i])), 2, '_'), $lbs_img_prtrt.)
    prtrt_key <- occur_keys[k]
    prtrt_val <- lbs_img_prtrt_map[prtrt_key]

    # SAS: proccur = ifc(^missing(occur[i]), "N", "Y")
    # If the "not-done" flag is filled → procedure NOT occurred → "N"
    proccur_val <- ifelse(!is.na(occur_val) & nchar(trimws(occur_val)) > 0,
                          "N", "Y")

    lbs_img_long_rows[[row_counter]] <- tibble::tibble(
      subjectid = lbs_img_crf$subjectid[r],
      eventid   = lbs_img_crf$eventid[r],
      prtrt     = unname(prtrt_val),
      proccur   = proccur_val,
      prreasoc  = reasoc_val,
      prstdat   = date_val,
      prcoded   = NA_character_,
      prinitd   = format_iso_dtc(lbs_img_crf$initiateddate[r]),
      prltedid  = format_iso_dtc(lbs_img_crf$lastediteddate[r])
    )
  }
}
lbs_img_all <- dplyr::bind_rows(lbs_img_long_rows)
lbs_img_all$sourceds <- "LBS_IMG"

cli::cli_alert_success(
  "LBS_IMG pivoted: {nrow(lbs_img_all)} rows (5 image types per visit)"
)

# --- Source 3: LBS_SIMG — stool sample images ---------------------------------
# SAS: set lbs_simg_all; rename lbs_dimg=prstdat
# Then later: prtrt = 'STOOL SAMPLE IMAGE'; preval = 'PARENT'; prorig = 'ePRO'
cli::cli_alert_info("Preparing lbs_simg (stool sample images) ...")

lbs_simg_all <- lbs_simg
lbs_simg_all$prstdat  <- lbs_simg_all$lbs_dimg
lbs_simg_all$prcoded  <- NA_character_
lbs_simg_all$prinitd  <- format_iso_dtc(lbs_simg_all$initiateddate)
lbs_simg_all$prltedid <- format_iso_dtc(lbs_simg_all$lastediteddate)
lbs_simg_all$sourceds <- "LBS_SIMG"

cli::cli_alert_success(
  "LBS_SIMG prepared: {nrow(lbs_simg_all)} rows"
)

# --- Append all sources (SAS: data pr_all2; set pr_all lbs_simg_all lbs_img_all)
# SAS: sourceds = tranwrd(scan(_inds,2,'.'),"_ALL","");
# Ensure all three datasets have compatible columns before binding
common_cols <- c("subjectid", "prstdat", "sourceds",
                 "prcoded", "prinitd", "prltedid")

# PR source keeps all its columns (MedDRA, pr-specific, etc.)
# LBS_IMG and LBS_SIMG have their specific columns
pr_all2 <- dplyr::bind_rows(
  pr_all,
  lbs_img_all,
  lbs_simg_all
)

cli::cli_alert_success(
  "Combined all PR sources: {nrow(pr_all2)} rows x {ncol(pr_all2)} cols"
)

# =============================================================================
# 2b. Derive USUBJID
# =============================================================================
# USUBJID derivation:
#   catx('-', "&studyid", scan(subjectid,-3,'-'), scan(subjectid,-2,'-'),
#        put(input(scan(subjectid,-1,'-'), best.), z4.))
# SubjectId format: CC-SITE-NN-SSSS  (e.g. NL-AMC-01-0001)
# USUBJID format:   STUDY-PILOT-AMC-01-0001  (studyid + last 3 segments)
derive_usubjid_study <- function(subjectid, studyid = "STUDY-PILOT") {
  parts <- strsplit(subjectid, "-")
  vapply(parts, function(p) {
    n <- length(p)
    if (n < 4) return(NA_character_)
    site_name <- p[n - 2]   # e.g. "AMC"
    site_num  <- p[n - 1]   # e.g. "01"
    subj_num  <- sprintf("%04d", as.integer(p[n]))  # z4. format
    paste(studyid, site_name, site_num, subj_num, sep = "-")
  }, character(1))
}

pr_all2$usubjid <- derive_usubjid_study(pr_all2$subjectid, STUDYID)
pr_all2$subjid  <- pr_all2$subjectid  # keep original as SUBJID source

# =============================================================================
# 2c. Prepare DM raw for RFSTDTC
# =============================================================================
# In the real study, RFSTDTC = icdat (informed consent date) from DM.sas
dm_raw <- dm_data
dm_raw$usubjid <- derive_usubjid_study(dm_raw$subjectid, STUDYID)
dm_raw$subjid  <- dm_raw$subjectid

# Join IC data to get RFSTDTC
if (!is.null(ic_data) && "icdat" %in% names(ic_data)) {
  ic_data$usubjid <- derive_usubjid_study(ic_data$subjectid, STUDYID)
  dm_raw <- dplyr::left_join(dm_raw, ic_data[, c("usubjid", "icdat")],
                              by = "usubjid")
  dm_raw$rfstdtc <- format_iso_dtc(dm_raw$icdat)
} else if ("icdat" %in% names(dm_raw)) {
  dm_raw$rfstdtc <- format_iso_dtc(dm_raw$icdat)
} else {
  # Fallback: use a fixed date
  dm_raw$rfstdtc <- "2025-06-01"
}

# =============================================================================
# 2d. Pre-derive PR-specific SAS logic
# =============================================================================
# The SAS PR.sas program performs several transformations in the DATA step
# BEFORE the metadata-driven `%sdtm_create_domain_v2`. We replicate them here
# so that the columns exist as source columns for build_domain.

# DOMAIN
pr_all2$DOMAIN <- "PR"

# STUDYID
pr_all2$STUDYID <- STUDYID

# PRSPID: numeric to character (SAS: prspid = put(_prspid, best.-l))
pr_all2$prspid <- as.character(pr_all2$prspid)

# SOURCEID: CRF source identifier
pr_all2$sourceid <- paste0("CRF: ", pr_all2$sourceds)
pr_all2$SOURCEID <- pr_all2$sourceid

# SUBJID: keep original subject id
pr_all2$SUBJID <- pr_all2$subjectid

# PRCAT: category based on source dataset
# SAS format: $prcat: PR="MEDICAL PROCEDURE", LBS_IMG="IMAGES AND VIDEOS",
#             LBS_SIMG="COLLECTION STOOL IMAGE"
prcat_map <- c(
  "PR"       = "MEDICAL PROCEDURE",
  "LBS_IMG"  = "IMAGES AND VIDEOS",
  "LBS_SIMG" = "COLLECTION STOOL IMAGE"
)
pr_all2$PRCAT <- prcat_map[pr_all2$sourceds]

# --- MedDRA fields: map from merged pr_meddra columns (lowercase sources) ----
pr_all2$prllt    <- pr_all2$llt_name
pr_all2$prlltcd  <- suppressWarnings(as.numeric(pr_all2$llt_code))
pr_all2$prdecod  <- pr_all2$pt_name
pr_all2$prptcd   <- suppressWarnings(as.numeric(pr_all2$pt_code))
pr_all2$prhlt    <- pr_all2$hlt_name
pr_all2$prhltcd  <- suppressWarnings(as.numeric(pr_all2$hlt_code))
pr_all2$prhlgt   <- pr_all2$hlgt_name
pr_all2$prhlgtcd <- suppressWarnings(as.numeric(pr_all2$hlgt_code))
pr_all2$prbodsys <- pr_all2$soc_name
pr_all2$prbdsycd <- suppressWarnings(as.numeric(pr_all2$soc_code))
pr_all2$prsoc    <- pr_all2$pt_soc_name
pr_all2$prsoccd  <- suppressWarnings(as.numeric(pr_all2$pt_soc_code))
pr_all2$PRSOCLST <- pr_all2$soc_list

# PRDICT: MedDRA dictionary version — use derive_dict_version()
# SAS: prdict = ifc(^missing(prdecod), "MedDRA &meddra_version_data.", " ")
pr_all2 <- derive_dict_version(pr_all2, "PRDICT", "prdecod", "meddra", meddra_version)

# --- Source-specific derivations (SAS data step) -----

# CRF 'Collection Stool Image' page specific derivations
idx_simg <- pr_all2$sourceds == "LBS_SIMG"
pr_all2$prtrt[idx_simg]  <- "STOOL SAMPLE IMAGE"
pr_all2$PREVAL  <- NA_character_
pr_all2$PRORIG  <- NA_character_
pr_all2$PREVAL[idx_simg]  <- "PARENT"
pr_all2$PRORIG[idx_simg]  <- "ePRO"

# CRF 'Images and Videos' page specific derivations
idx_img <- pr_all2$sourceds == "LBS_IMG"
pr_all2$PRPRESP <- NA_character_
pr_all2$PRPRESP[idx_img] <- "Y"
pr_all2$PRORIG[idx_img]  <- "CRF"
pr_all2$PREVAL[idx_img]  <- "INVESTIGATOR"

# CRF 'Medical Procedure' page specific derivations
idx_pr <- pr_all2$sourceds == "PR"
pr_all2$PRORIG[idx_pr]  <- "CRF"
pr_all2$PREVAL[idx_pr]  <- "INVESTIGATOR"

# PRTRT: for PR source, it comes from prtrt raw col already

# PRINDC: indication (SAS: prindc = prindicat; if "PREVENTIVE..." → "OTHER")
pr_all2$prindc  <- NA_character_
pr_all2$prindcx <- NA_character_
if ("prindicat" %in% names(pr_all2)) {
  pr_all2$prindc[idx_pr] <- pr_all2$prindicat[idx_pr]
  prev_mask <- idx_pr &
    !is.na(pr_all2$prindc) &
    toupper(trimws(pr_all2$prindc)) == "PREVENTIVE / FOR SCREENING PURPOSES"
  pr_all2$prindcx[prev_mask] <- pr_all2$prindc[prev_mask]
  pr_all2$prindc[prev_mask]  <- "OTHER"
}
pr_all2$PRINDC  <- pr_all2$prindc
pr_all2$PRINDCX <- pr_all2$prindcx

# PRENRTPT + PRENTPT: end relative to reference time point
# SAS: if prongo = "Y" then prenrtpt = 'ONGOING';
#      if prongo = "N" then prenrtpt = 'BEFORE';
#      prentpt = 'END OF STUDY';
pr_all2$PRENRTPT <- NA_character_
pr_all2$PRENTPT  <- NA_character_
prongo_present <- idx_pr & !is.na(pr_all2$prongo)
pr_all2$PRENRTPT[prongo_present & toupper(pr_all2$prongo) == "Y"] <- "ONGOING"
pr_all2$PRENRTPT[prongo_present & toupper(pr_all2$prongo) == "N"] <- "BEFORE"
pr_all2$PRENTPT[!is.na(pr_all2$PRENRTPT)] <- "END OF STUDY"

# Date variables: PRSTDTC, PRSTDY, PRENDTC, PRENDY
# SAS: %sdtm_dt2dtc(dt=prstdat, dtc=prstdtc)
pr_all2$PRSTDTC <- format_iso_dtc(pr_all2$prstdat)
pr_all2$PRENDTC <- NA_character_
if ("prendat" %in% names(pr_all2)) {
  pr_all2$PRENDTC <- format_iso_dtc(pr_all2$prendat)
}

# Join RFSTDTC from DM for DY calculation
dm_slim <- dplyr::distinct(dm_raw[, c("usubjid", "rfstdtc"), drop = FALSE])
pr_all2 <- dplyr::left_join(pr_all2, dm_slim, by = "usubjid")

# Derive DY variables
# SAS: %sdtm_dtc2dy(dtc= prstdtc) and %sdtm_dtc2dy(dtc= prendtc)
pr_all2$RFSTDTC <- pr_all2$rfstdtc  # ensure uppercase version too
pr_all2 <- derive_dy(pr_all2, "PRSTDY", "PRSTDTC", "RFSTDTC")
pr_all2 <- derive_dy(pr_all2, "PRENDY", "PRENDTC", "RFSTDTC")

# PROCCUR: for LBS_IMG it's already set from the pivot; for others default NA
# (PR CRF data and LBS_SIMG don't have PROCCUR in the SAS)

# PRREASOC: reason for occurrence (only from LBS_IMG)
# Already set from pivot for LBS_IMG; for others it's NA

# PRLNKID: SAS: if sourceds = "LBS_SIMG" then prlnkid = catx('-', prtrt, prstdtc)
pr_all2$PRLNKID <- NA_character_
pr_all2$PRLNKID[idx_simg] <- paste(
  pr_all2$prtrt[idx_simg],
  pr_all2$PRSTDTC[idx_simg],
  sep = "-"
)

# PRPRESP (pre-specified): only for LBS_IMG (already set)

# Audit trail variables (already derived above for all sources)
pr_all2$PRCODED  <- pr_all2$prcoded
pr_all2$PRINITD  <- pr_all2$prinitd
pr_all2$PRLTEDID <- pr_all2$prltedid

# PRTERM: direct from raw prtrt
pr_all2$prterm <- pr_all2$prtrt

# VISITDY: derive from VISITNUM and RFSTDTC (if available)
# Since PR doesn't have a consistent visit-date mapping, VISITDY is left NA
# for records without visit context. For LBS_IMG records with PRSTDTC, VISITDY = PRSTDY.
pr_all2$VISITDY <- NA_real_

# Derive VISIT variables using eventid
# SAS: %derive_visit_vars() — uses eventid to derive VISIT, VISITNUM, VISITDY
# For PR CRF records there's no eventid. For LBS_IMG records eventid maps to visits.
# We derive visit vars where eventid is available.
visit_map <- list(
  "SCR"     = list(visit = "SCREENING",               visitnum = 1),
  "V1"      = list(visit = "VISIT 1",                 visitnum = 2),
  "V1_IMG"  = list(visit = "VISIT 1",                 visitnum = 2),
  "PW1"     = list(visit = "PHONE CALL WEEK 1",       visitnum = 3),
  "PW5"     = list(visit = "PHONE CALL WEEK 5",       visitnum = 4),
  "V2"      = list(visit = "VISIT 2",                 visitnum = 5),
  "PW15"    = list(visit = "PHONE CALL WEEK 15",      visitnum = 6),
  "PW20"    = list(visit = "PHONE CALL WEEK 20",      visitnum = 7),
  "V3"      = list(visit = "VISIT 3",                 visitnum = 8),
  "V3_IMG"  = list(visit = "VISIT 3",                 visitnum = 8),
  "PFU"     = list(visit = "POST-STUDY FOLLOW UP",    visitnum = 9)
)

pr_all2$VISIT    <- NA_character_
pr_all2$VISITNUM <- NA_real_
if ("eventid" %in% names(pr_all2)) {
  for (eid in names(visit_map)) {
    mask <- !is.na(pr_all2$eventid) & toupper(trimws(pr_all2$eventid)) == eid
    pr_all2$VISIT[mask]    <- visit_map[[eid]]$visit
    pr_all2$VISITNUM[mask] <- visit_map[[eid]]$visitnum
  }
}

cli::cli_alert_success("Pre-derived PR variables from SAS logic")

# =============================================================================
# 2e. Assemble raw_data list for sdtmbuilder
# =============================================================================
raw_data <- list(
  pr_raw = pr_all2,
  dm_raw = dm_raw
)

cli::cli_alert_info("Raw data prepared: {paste(names(raw_data), collapse = ', ')}")

# ---- 3. Build configuration -------------------------------------------------

cli::cli_h1("Step 3: Building configuration")

cfg_yaml <- yaml::read_yaml("metadata/config.yaml")

config <- new_sdtm_config(
  studyid        = toupper(cfg_yaml$studyid),
  timezone       = if (!is.null(cfg_yaml$timezone)) cfg_yaml$timezone else "UTC",
  ref_start_rule = list(
    var    = if (!is.null(cfg_yaml$ref_start_rule$column)) cfg_yaml$ref_start_rule$column else "rfstdtc",
    source = if (!is.null(cfg_yaml$ref_start_rule$dataset)) cfg_yaml$ref_start_rule$dataset else "dm_raw"
  ),
  create_supp = if (!is.null(cfg_yaml$create_supp)) cfg_yaml$create_supp else TRUE
)

cli::cli_alert_success("Config: studyid = {config$studyid}")

# ---- 4. Expand value-level metadata (if any) --------------------------------

if (!is.null(value_level_meta) && nrow(value_level_meta) > 0L) {
  cli::cli_alert_info("Expanding {nrow(value_level_meta)} value-level rows...")
  target_meta <- expand_value_level_meta(target_meta, value_level_meta)
}

# ---- 5. Compile rules -------------------------------------------------------

cli::cli_h1("Step 4: Compiling derivation rules")

rule_set <- compile_rules(target_meta, ct_lib = ct_lib)

pr_rules <- rule_set$rules[["PR"]]
if (is.null(pr_rules)) {
  cli::cli_alert_warning("No PR rules found in rule_set. Check metadata has PR domain variables.")
} else {
  cli::cli_alert_success("PR domain: {length(pr_rules)} derivation rules compiled")
}

# ---- 6. Build PR domain -----------------------------------------------------

cli::cli_h1("Step 5: Building PR domain")

pr_result <- build_domain(
  domain           = "PR",
  target_meta      = target_meta,
  raw_data         = raw_data,
  config           = config,
  rule_set         = rule_set,
  domain_meta      = domain_meta,
  value_level_meta = value_level_meta,
  create_supp      = TRUE,
  validate         = TRUE,
  skip_existing    = TRUE,
  verbose          = TRUE
)

cli::cli_alert_success("PR built: {nrow(pr_result$data)} rows x {ncol(pr_result$data)} cols")

# Show validation summary
if (!is.null(pr_result$report)) {
  n_err  <- sum(pr_result$report$findings$severity == "ERROR", na.rm = TRUE)
  n_warn <- sum(pr_result$report$findings$severity == "WARNING", na.rm = TRUE)
  cli::cli_alert_info("Validation: {n_err} errors, {n_warn} warnings")
}

# Preview first rows
cat("\n--- PR Domain Preview ---\n")
print(head(pr_result$data, 10))

# ---- 7. Export PR dataset ----------------------------------------------------

cli::cli_h1("Step 6: Exporting PR dataset")

# Export PR in XPT v8 + RDS + CSV
pr_final <- export_domain(
  data             = pr_result$data,
  domain           = "PR",
  output_dir       = "sdtm/datasets",
  formats          = c("xpt", "rds", "csv"),
  xpt_version      = 8L,
  target_meta      = target_meta,
  domain_meta      = domain_meta,
  drop_empty_perm  = TRUE
)

# Export SUPPPR if applicable
if (!is.null(pr_result$supp) && nrow(pr_result$supp) > 0L) {
  export_domain(pr_result$supp, "SUPPPR", "sdtm/datasets", formats = "xpt")
  cli::cli_alert_success("SUPPPR exported ({nrow(pr_result$supp)} rows)")
}

# ---- 8. Generate PR R program -----------------------------------------------

cli::cli_h1("Step 7: Generating PR derivation program")

dir.create("sdtm/programs", recursive = TRUE, showWarnings = FALSE)

gen_domain_script(
  domain      = "PR",
  rule_set    = rule_set,
  target_meta = target_meta,
  config      = config,
  output_path = "sdtm/programs/pr.R"
)
cli::cli_alert_success("Generated: sdtm/programs/pr.R")

# ---- Done! -------------------------------------------------------------------

cli::cli_h1("Demo Complete")
cli::cli_alert_success("PR domain built and exported successfully!")
cli::cli_alert_info("Outputs:")
cli::cli_bullets(c(
  " " = "Dataset:  sdtm/datasets/pr.xpt",
  " " = "RDS:      sdtm/datasets/pr.rds",
  " " = "Program:  sdtm/programs/pr.R"
))
if (!is.null(pr_result$supp) && nrow(pr_result$supp) > 0L) {
  cli::cli_bullets(c(" " = "SUPPPR:   sdtm/datasets/supppr.xpt"))
}
