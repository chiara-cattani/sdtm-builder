# =============================================================================
# STUDY-PILOT Study — AE Domain Demo
# =============================================================================
# This script demonstrates how to use sdtmbuilder to:
#   1. Read study metadata and controlled terminology
#   2. Load and prepare raw data (merging CRF datasets)
#   3. Build the AE (Adverse Events) SDTM domain
#   4. Export the AE dataset as XPT
#   5. Auto-generate the AE derivation R program
#
# Prerequisites:
#   - sdtmbuilder package installed (devtools::install("."))
#   - Raw datasets in study-pilot/raw/ (run create_dummy_raw.R first, or
#     point RAW_DATA_DIR to your real export folder)
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

ae_crf    <- all_raw[["ae"]]
sae_data  <- all_raw[["sae"]]
ae_meddra <- all_raw[["ae_meddra"]]
dm_data   <- all_raw[["dm"]]
ic_data   <- all_raw[["ic"]]

# --- 2a. Pre-merge AE sources ------------------------------------------------
# The SAS program (AE.sas) merges three raw datasets:
#   1. ae        — AE CRF page (main AE data: term, severity, dates, actions)
#   2. ae_meddra — MedDRA coded terms (by subjectid + aespid)
#   3. sae       — SAE seriousness criteria (by subjectid + saespid → aespid)
# Then joins DM for USUBJID and RFSTDTC.

# Step 1: Merge ae CRF + ae_meddra by subjectid + aespid
ae_raw <- dplyr::left_join(ae_crf, ae_meddra, by = c("subjectid", "aespid"))

# Step 2: Merge with SAE data (rename saespid → aespid)
sae_cols_to_keep <- intersect(
  names(sae_data),
  c("subjectid", "saespid", "aesdth", "aeslife", "aeshosp",
    "aesdisab", "aescong", "aesmie", "aesdtdat")
)
sae_slim <- sae_data[, sae_cols_to_keep, drop = FALSE]
if ("saespid" %in% names(sae_slim)) {
  sae_slim <- dplyr::rename(sae_slim, aespid = saespid)
}
ae_raw <- dplyr::left_join(ae_raw, sae_slim, by = c("subjectid", "aespid"))

cli::cli_alert_success("Merged ae + ae_meddra + sae: {nrow(ae_raw)} rows x {ncol(ae_raw)} cols")

# --- 2b. Derive USUBJID -------------------------------------------------------
# USUBJID derivation:
#   catx('-', "&studyid", scan(subjectid,-3,'-'), scan(subjectid,-2,'-'),
#        put(input(scan(subjectid,-1,'-'), best.), z4.))
# SubjectId format: CC-SITE-NN-SSSS  (e.g. XX-SITE-01-0001)
# USUBJID format:   STUDY-PILOT-SITEA-01-0001  (studyid + last 3 segments)
derive_usubjid_study <- function(subjectid, studyid = "STUDY-PILOT") {
  parts <- strsplit(subjectid, "-")
  vapply(parts, function(p) {
    n <- length(p)
    if (n < 4) return(NA_character_)
    site_name <- p[n - 2]   # scan(subjectid, -3, '-')  e.g. "SITEA"
    site_num  <- p[n - 1]   # scan(subjectid, -2, '-')  e.g. "01"
    subj_num  <- sprintf("%04d", as.integer(p[n]))  # z4. format
    paste(studyid, site_name, site_num, subj_num, sep = "-")
  }, character(1))
}

ae_raw$usubjid <- derive_usubjid_study(ae_raw$subjectid, STUDYID)
ae_raw$subjid  <- ae_raw$subjectid  # keep original as SUBJID source

# --- 2c. Prepare DM raw for RFSTDTC ------------------------------------------
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

# --- 2d. Pre-derive AE-specific SAS logic ------------------------------------
# The SAS AE.sas program performs several transformations in the data step
# BEFORE the metadata-driven `%sdtm_create_domain_v2`. We replicate them here
# so that the columns referenced by the metadata METHOD exist in ae_raw.
#
# Lowercase columns = SOURCE columns for the builder's direct_map rules.
# UPPERCASE columns = FINAL SDTM values the builder should NOT overwrite
#                     (used with skip_existing = TRUE in build_domain).

# AESPID: numeric to character (SAS: put(_aespid, best.-l))
ae_raw$aespid <- as.character(ae_raw$aespid)

# SOURCEID: CRF source identifier (lowercase source for direct_map)
ae_raw$sourceid <- "CRF: AE"

# AEACN: tranwrd(aeacnp, "NA", "NOT APPLICABLE")
ae_raw$aeacn <- ifelse(
  !is.na(ae_raw$aeacnp) & toupper(trimws(ae_raw$aeacnp)) == "NA",
  "NOT APPLICABLE",
  ae_raw$aeacnp
)

# AEACNOTH: ifc(^missing(aeacns0), aeacns0, catx(";", of aeacns:))
# If aeacns0 has a value, use it. Otherwise, concatenate checkbox columns.
aeacns_checkbox_cols <- grep("^aeacns\\d+$", names(ae_raw), value = TRUE)
aeacns_checkbox_cols <- setdiff(aeacns_checkbox_cols, "aeacns0")
if (length(aeacns_checkbox_cols) > 0) {
  ae_raw$aeacnoth <- ifelse(
    !is.na(ae_raw$aeacns0) & nchar(trimws(ae_raw$aeacns0)) > 0,
    ae_raw$aeacns0,
    apply(ae_raw[, aeacns_checkbox_cols, drop = FALSE], 1, function(row) {
      vals <- row[!is.na(row) & nchar(trimws(row)) > 0]
      if (length(vals) > 0) paste(vals, collapse = "; ") else NA_character_
    })
  )
} else {
  ae_raw$aeacnoth <- ifelse(
    !is.na(ae_raw$aeacns0) & nchar(trimws(ae_raw$aeacns0)) > 0,
    ae_raw$aeacns0,
    NA_character_
  )
}
ae_raw$AEACNOTH <- ae_raw$aeacnoth

# AEACNOTX = aeacnssp (other action specify text)
ae_raw$aeacnotx <- if ("aeacnssp" %in% names(ae_raw)) ae_raw$aeacnssp else NA_character_

# AEACNX = aeacnpsp (action taken specify)
ae_raw$aeacnx <- if ("aeacnpsp" %in% names(ae_raw)) ae_raw$aeacnpsp else NA_character_

# AEACNEVL: not in raw — initialize to NA
ae_raw$aeacnevl <- NA_character_

# AERELX = aerel1co (relationship comment)
ae_raw$aerelx <- if ("aerel1co" %in% names(ae_raw)) ae_raw$aerel1co else NA_character_

# AEREL: tranwrd(aerel1,"NA","NOT APPLICABLE") + " RELATED" suffix
# The metadata case_when has case-sensitivity mismatches, so we pre-derive
# the corrected SDTM values and protect with UPPERCASE.
ae_raw$aerel <- ae_raw$aerel1
ae_raw$aerel <- ifelse(
  !is.na(ae_raw$aerel) & toupper(trimws(ae_raw$aerel)) == "NA",
  "NOT APPLICABLE",
  ae_raw$aerel
)
needs_suffix <- !is.na(ae_raw$aerel) &
  !(toupper(trimws(ae_raw$aerel)) %in% c("", "NOT RELATED", "NOT APPLICABLE"))
ae_raw$aerel[needs_suffix] <- paste0(
  toupper(ae_raw$aerel[needs_suffix]), " RELATED"
)
ae_raw$aerel[!needs_suffix & !is.na(ae_raw$aerel)] <- toupper(
  ae_raw$aerel[!needs_suffix & !is.na(ae_raw$aerel)]
)
ae_raw$AEREL <- ae_raw$aerel

# AERELP: tranwrd(aerel2, "NA", "NOT APPLICABLE") + uppercase
ae_raw$aerelp <- toupper(ifelse(
  !is.na(ae_raw$aerel2) & toupper(trimws(ae_raw$aerel2)) == "NA",
  "NOT APPLICABLE",
  ae_raw$aerel2
))

# AERELPX = aerel2co
ae_raw$aerelpx <- if ("aerel2co" %in% names(ae_raw)) ae_raw$aerel2co else NA_character_

# AEOUTX = aesequel1 (outcome text)
ae_raw$aeoutx <- if ("aesequel1" %in% names(ae_raw)) ae_raw$aesequel1 else NA_character_

# AEDESC = aecoval (description)
ae_raw$aedesc <- ae_raw$aecoval

# AEFUNEED = aefu (follow-up needed)
ae_raw$aefuneed <- if ("aefu" %in% names(ae_raw)) ae_raw$aefu else NA_character_

# AEOUT: tranwrd(aeout, " / ", "/") — clean the slash formatting + uppercase
ae_raw$aeout <- toupper(gsub(" / ", "/", ae_raw$aeout, fixed = TRUE))

# AESEV: uppercase for CT compliance
ae_raw$aesev <- toupper(ae_raw$aesev)

# AESER: derive from SAE seriousness flags (Y if any SAE criterion is Y)
sae_flags <- c("aesdth", "aeslife", "aeshosp", "aesdisab", "aescong", "aesmie")
sae_flag_cols <- intersect(sae_flags, names(ae_raw))
if (length(sae_flag_cols) > 0) {
  ae_raw$aeser <- apply(ae_raw[, sae_flag_cols, drop = FALSE], 1, function(row) {
    if (any(toupper(row) == "Y", na.rm = TRUE)) "Y" else "N"
  })
} else {
  ae_raw$aeser <- "N"
}

# MedDRA fields: direct mapping from merged ae_meddra columns (lowercase sources)
ae_raw$aellt    <- ae_raw$llt_name
ae_raw$aelltcd  <- as.numeric(ae_raw$llt_code)
ae_raw$aedecod  <- ae_raw$pt_name
ae_raw$aeptcd   <- as.numeric(ae_raw$pt_code)
ae_raw$aehlt    <- ae_raw$hlt_name
ae_raw$aehltcd  <- as.numeric(ae_raw$hlt_code)
ae_raw$aehlgt   <- ae_raw$hlgt_name
ae_raw$aehlgtcd <- as.numeric(ae_raw$hlgt_code)
ae_raw$aebodsys <- ae_raw$soc_name
ae_raw$aebdsycd <- as.numeric(ae_raw$soc_code)
ae_raw$aesoc    <- ae_raw$pt_soc_name
ae_raw$aesoccd  <- as.numeric(ae_raw$pt_soc_code)
ae_raw$aesoclst <- ae_raw$soc_list

# AEDICT: MedDRA dictionary version — detect from ae_meddra data dynamically
# Uses get_meddra_version() from sdtmbuilder
meddra_version <- get_meddra_version(ae_meddra)
if (nzchar(meddra_version)) {
  cli::cli_alert_info("MedDRA version detected: {meddra_version}")
} else {
  meddra_version <- "27.0"
  cli::cli_alert_warning("MedDRA version could not be detected; defaulting to {meddra_version}")
}
ae_raw <- derive_dict_version(ae_raw, "AEDICT", "aedecod", "meddra", meddra_version)

# SAE seriousness criterion flags: uppercase for CT compliance
for (fl in sae_flag_cols) {
  ae_raw[[fl]] <- toupper(ae_raw[[fl]])
}

# AESTRTPT + AESTTPT: start relative to reference time point
# SAS: if index(upcase(aestper), "BEFORE") then aestrtpt = "BEFORE";
#      else if index(upcase(aestper), "AFTER") then aestrtpt = "AFTER";
#      aesttpt = "FIRST PRODUCT INTAKE";
ae_raw$AESTRTPT <- NA_character_
ae_raw$AESTTPT  <- NA_character_
idx_per <- !is.na(ae_raw$aestper)
ae_raw$AESTRTPT[idx_per & grepl("before", ae_raw$aestper, ignore.case = TRUE)] <- "BEFORE"
ae_raw$AESTRTPT[idx_per & grepl("after",  ae_raw$aestper, ignore.case = TRUE)] <- "AFTER"
ae_raw$AESTTPT[!is.na(ae_raw$AESTRTPT)] <- "FIRST PRODUCT INTAKE"

# AEENRTPT + AEENTPT: end relative to reference time point
# SAS: if aeongo = "Y" then aeenrtpt = "ONGOING";
#      else if aeongo = "N" then aeenrtpt = "BEFORE";
#      aeentpt = "END OF STUDY";
ae_raw$AEENRTPT <- NA_character_
ae_raw$AEENTPT  <- NA_character_
idx_ongo <- !is.na(ae_raw$aeongo)
ae_raw$AEENRTPT[idx_ongo & toupper(ae_raw$aeongo) == "Y"] <- "ONGOING"
ae_raw$AEENRTPT[idx_ongo & toupper(ae_raw$aeongo) == "N"] <- "BEFORE"
ae_raw$AEENTPT[!is.na(ae_raw$AEENRTPT)] <- "END OF STUDY"

# AETERM: direct from raw (just ensure lowercase source exists)
ae_raw$aeterm <- ae_raw$aeterm  # already exists from standardize_names

# Audit trail variables: ISO date conversion
# SAS: %sdtm_dt2dtc(dt=CodedOnDate, dtc=aecoded)
ae_raw$AECODED  <- format_iso_dtc(ae_raw$codedondate)
ae_raw$AEINITD  <- format_iso_dtc(ae_raw$initiateddate)
ae_raw$AELTEDID <- format_iso_dtc(ae_raw$lastediteddate)

cli::cli_alert_success("Pre-derived AE variables from SAS logic")

# --- 2e. Assemble raw_data list for sdtmbuilder ------------------------------
raw_data <- list(
  ae_raw = ae_raw,
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

ae_rules <- rule_set$rules[["AE"]]
cli::cli_alert_success("AE domain: {length(ae_rules)} derivation rules compiled")

# ---- 6. Build AE domain -----------------------------------------------------

cli::cli_h1("Step 5: Building AE domain")

ae_result <- build_domain(
  domain           = "AE",
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

cli::cli_alert_success("AE built: {nrow(ae_result$data)} rows x {ncol(ae_result$data)} cols")

# Show validation summary
if (!is.null(ae_result$report)) {
  n_err  <- sum(ae_result$report$findings$severity == "ERROR", na.rm = TRUE)
  n_warn <- sum(ae_result$report$findings$severity == "WARNING", na.rm = TRUE)
  cli::cli_alert_info("Validation: {n_err} errors, {n_warn} warnings")
}

# Preview first rows
cat("\n--- AE Domain Preview ---\n")
print(head(ae_result$data, 5))

# ---- 7. Export AE dataset ----------------------------------------------------

cli::cli_h1("Step 6: Exporting AE dataset")

# Export AE in XPT v8 + RDS + CSV (metadata-driven ordering, labels, PERM drop)
ae_final <- export_domain(
  data             = ae_result$data,
  domain           = "AE",
  output_dir       = "sdtm/datasets",
  formats          = c("xpt", "rds", "csv"),
  xpt_version      = 8L,
  target_meta      = target_meta,
  domain_meta      = domain_meta,
  drop_empty_perm  = TRUE
)

# Export SUPPAE if applicable
if (!is.null(ae_result$supp) && nrow(ae_result$supp) > 0L) {
  export_domain(ae_result$supp, "SUPPAE", "sdtm/datasets", formats = "xpt")
  cli::cli_alert_success("SUPPAE exported ({nrow(ae_result$supp)} rows)")
}

# ---- 8. Generate AE R program -----------------------------------------------

cli::cli_h1("Step 7: Generating AE derivation program")

dir.create("sdtm/programs", recursive = TRUE, showWarnings = FALSE)

gen_domain_script(
  domain      = "AE",
  rule_set    = rule_set,
  target_meta = target_meta,
  config      = config,
  output_path = "sdtm/programs/ae.R"
)
cli::cli_alert_success("Generated: sdtm/programs/ae.R")

# ---- Done! -------------------------------------------------------------------

cli::cli_h1("Demo Complete")
cli::cli_alert_success("AE domain built and exported successfully!")
cli::cli_alert_info("Outputs:")
cli::cli_bullets(c(
  " " = "Dataset:  sdtm/datasets/ae.xpt",
  " " = "RDS:      sdtm/datasets/ae.rds",
  " " = "Program:  sdtm/programs/ae.R"
))
if (!is.null(ae_result$supp) && nrow(ae_result$supp) > 0L) {
  cli::cli_bullets(c(" " = "SUPPAE:   sdtm/datasets/suppae.xpt"))
}
