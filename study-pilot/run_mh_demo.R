# =============================================================================
# STUDY-PILOT Study — MH (Medical History) Domain Demo
# =============================================================================
# This script demonstrates how to use sdtmbuilder to:
#   1. Read study metadata and controlled terminology
#   2. Load and prepare raw data (merging CRF datasets as in MH.sas)
#   3. Build the MH (Medical History) SDTM domain
#   4. Export the MH dataset as XPT
#   5. Auto-generate the MH derivation R program
#
# Prerequisites:
#   - sdtmbuilder package installed (devtools::install("."))
#   - Raw datasets in raw/ (run create_dummy_raw.R + create_dummy_raw_mh.R
#     first, or point RAW_DATA_DIR to your real EDC export folder)
#
# Raw datasets:
#   - mh_meddra.csv  : Medical History CRF + MedDRA coded data
#   - dm.csv         : Demographics (for RFSTDTC)
#   - ic.csv         : Informed consent (for RFSTDTC = icdat)
# =============================================================================

library(sdtmbuilder)

# ---- 0. Configuration -------------------------------------------------------

# >>> CHANGE THIS to the path where your raw datasets are stored <<<
RAW_DATA_DIR <- "raw"

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

mh_meddra <- all_raw[["mh_meddra"]]
dm_data   <- all_raw[["dm"]]
ic_data   <- all_raw[["ic"]]

# =============================================================================
# 2a. Replicate MH.sas data preparation logic
# =============================================================================
# MH.sas merges mh_meddra and derives MH variables before the final
# %sdtm_create_domain_v2. We replicate that logic here.

# --- 2b. Derive USUBJID -------------------------------------------------------
# USUBJID derivation:
#   catx('-', "&studyid", scan(subjectid,-3,'-'), scan(subjectid,-2,'-'),
#        put(input(scan(subjectid,-1,'-'), best.), z4.))
derive_usubjid_study <- function(subjectid, studyid = "STUDY-PILOT") {
  parts <- strsplit(subjectid, "-")
  vapply(parts, function(p) {
    n <- length(p)
    if (n < 4) return(NA_character_)
    site_name <- p[n - 2]
    site_num  <- p[n - 1]
    subj_num  <- sprintf("%04d", as.integer(p[n]))
    paste(studyid, site_name, site_num, subj_num, sep = "-")
  }, character(1))
}

# Filter: keep only rows with non-missing MHTERM (SAS: if not missing(mhterm))
mh_raw <- dplyr::filter(mh_meddra, !is.na(mhterm) & nchar(trimws(mhterm)) > 0)

# Derive USUBJID and SUBJID
mh_raw$usubjid <- derive_usubjid_study(mh_raw$subjectid, STUDYID)
mh_raw$subjid  <- mh_raw$subjectid

cli::cli_alert_success("MH raw data: {nrow(mh_raw)} rows x {ncol(mh_raw)} cols")

# --- 2c. Prepare DM raw for RFSTDTC ------------------------------------------
dm_raw <- dm_data
dm_raw$usubjid <- derive_usubjid_study(dm_raw$subjectid, STUDYID)
dm_raw$subjid  <- dm_raw$subjectid

if (!is.null(ic_data) && "icdat" %in% names(ic_data)) {
  ic_data$usubjid <- derive_usubjid_study(ic_data$subjectid, STUDYID)
  dm_raw <- dplyr::left_join(dm_raw, ic_data[, c("usubjid", "icdat")],
                              by = "usubjid")
  dm_raw$rfstdtc <- format_iso_dtc(dm_raw$icdat)
} else if ("icdat" %in% names(dm_raw)) {
  dm_raw$rfstdtc <- format_iso_dtc(dm_raw$icdat)
} else {
  dm_raw$rfstdtc <- "2025-06-01"
}

# --- 2d. Pre-derive MH-specific SAS logic ------------------------------------
# Replicate MH.sas DATA step derivations

# DOMAIN and STUDYID
mh_raw$DOMAIN  <- "MH"
mh_raw$STUDYID <- STUDYID

# MHCAT: constant
mh_raw$MHCAT <- "RELEVANT MEDICAL HISTORY AND PRE-EXISTING CONDITIONS"

# SOURCEID
mh_raw$SOURCEID <- "CRF: MH"

# MHTERM: direct from raw (SAS: mhterm = strip(mhterm))
mh_raw$mhterm <- trimws(mh_raw$mhterm)

# MHSPID: numeric to character (SAS: mhspid = strip(put(_mhspid, best.)))
mh_raw$mhspid <- as.character(mh_raw$mhspid)

# MHCURMED: direct (SAS: mhcurmed = strip(mhcurmed))
mh_raw$mhcurmed <- trimws(mh_raw$mhcurmed)

# MHMEDINT: from mhpr (SAS: mhmedint = strip(mhpr))
mh_raw$mhmedint <- trimws(mh_raw$mhpr)

# MedDRA fields: direct mapping from merged mh_meddra columns
mh_raw$mhllt    <- mh_raw$llt_name
mh_raw$mhlltcd  <- suppressWarnings(as.numeric(mh_raw$llt_code))
mh_raw$mhdecod  <- mh_raw$pt_name
mh_raw$mhptcd   <- suppressWarnings(as.numeric(mh_raw$pt_code))
mh_raw$mhhlt    <- mh_raw$hlt_name
mh_raw$mhhltcd  <- suppressWarnings(as.numeric(mh_raw$hlt_code))
mh_raw$mhhlgt   <- mh_raw$hlgt_name
mh_raw$mhhlgtcd <- suppressWarnings(as.numeric(mh_raw$hlgt_code))
mh_raw$mhbodsys <- mh_raw$soc_name
mh_raw$mhbdsycd <- suppressWarnings(as.numeric(mh_raw$soc_code))
mh_raw$mhsoc    <- mh_raw$pt_soc_name
mh_raw$mhsoccd  <- suppressWarnings(as.numeric(mh_raw$pt_soc_code))
mh_raw$MHSOCLST <- mh_raw$soc_list

# MHDICT: MedDRA dictionary version — detect from mh_meddra data dynamically
# Uses get_meddra_version() from sdtmbuilder
meddra_version <- get_meddra_version(mh_meddra)
if (nzchar(meddra_version)) {
  cli::cli_alert_info("MedDRA version detected: {meddra_version}")
} else {
  # Fallback: try raw version column
  meddra_version_raw <- unique(mh_raw$version[!is.na(mh_raw$version)])
  meddra_version <- if (length(meddra_version_raw) > 0) meddra_version_raw[1] else "27.0"
  cli::cli_alert_warning("Using fallback MedDRA version: {meddra_version}")
}
mh_raw <- derive_dict_version(mh_raw, "MHDICT", "mhdecod", "meddra", meddra_version)

# Date variables: ISO DTC conversion
# SAS: %sdtm_dt2dtc(dt=mhstdat, dtc=mhstdtc)
mh_raw$MHSTDTC <- format_iso_dtc(mh_raw$mhstdat)
mh_raw$MHENDTC <- format_iso_dtc(mh_raw$mhendat)

# End relative time point
# SAS: if ^missing(mhongo) then do;
#        mhentpt = "INFORMED CONSENT";
#        if lowcase(mhongo) = "y" then mhenrtpt = "ONGOING";
#        else if lowcase(mhongo) = "n" then mhenrtpt = "BEFORE";
#      end;
mh_raw$MHENRTPT <- NA_character_
mh_raw$MHENTPT  <- NA_character_
idx_ongo <- !is.na(mh_raw$mhongo)
mh_raw$MHENRTPT[idx_ongo & tolower(mh_raw$mhongo) == "y"] <- "ONGOING"
mh_raw$MHENRTPT[idx_ongo & tolower(mh_raw$mhongo) == "n"] <- "BEFORE"
mh_raw$MHENTPT[!is.na(mh_raw$MHENRTPT)] <- "INFORMED CONSENT"

# Audit variables: ISO date conversion
# SAS: %sdtm_dt2dtc(dt=CodedOnDate, dtc=mhcoded)
mh_raw$MHCODED  <- format_iso_dtc(mh_raw$codedondate)
mh_raw$MHINITD  <- format_iso_dtc(mh_raw$initiateddate)
mh_raw$MHLTEDID <- format_iso_dtc(mh_raw$lastediteddate)

# Join RFSTDTC from DM for DY calculation
dm_slim <- dplyr::distinct(dm_raw[, c("usubjid", "rfstdtc"), drop = FALSE])
mh_raw <- dplyr::left_join(mh_raw, dm_slim, by = "usubjid")

# Derive DY variables
# SAS: %sdtm_dtc2dy(dtc=mhstdtc) and %sdtm_dtc2dy(dtc=mhendtc)
mh_raw$RFSTDTC <- mh_raw$rfstdtc
mh_raw <- derive_dy(mh_raw, "MHSTDY", "MHSTDTC", "RFSTDTC")
mh_raw <- derive_dy(mh_raw, "MHENDY", "MHENDTC", "RFSTDTC")

cli::cli_alert_success("Pre-derived MH variables from SAS logic")

# --- 2e. Assemble raw_data list for sdtmbuilder ------------------------------
raw_data <- list(
  mh_raw = mh_raw,
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

mh_rules <- rule_set$rules[["MH"]]
if (is.null(mh_rules)) {
  cli::cli_alert_warning("No MH rules found in rule_set. Check metadata has MH domain variables.")
} else {
  cli::cli_alert_success("MH domain: {length(mh_rules)} derivation rules compiled")
}

# ---- 6. Build MH domain -----------------------------------------------------

cli::cli_h1("Step 5: Building MH domain")

mh_result <- build_domain(
  domain           = "MH",
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

cli::cli_alert_success("MH built: {nrow(mh_result$data)} rows x {ncol(mh_result$data)} cols")

# Show validation summary
if (!is.null(mh_result$report)) {
  n_err  <- sum(mh_result$report$findings$severity == "ERROR", na.rm = TRUE)
  n_warn <- sum(mh_result$report$findings$severity == "WARNING", na.rm = TRUE)
  cli::cli_alert_info("Validation: {n_err} errors, {n_warn} warnings")
}

# Preview first rows
cat("\n--- MH Domain Preview ---\n")
print(head(mh_result$data, 5))

# ---- 7. Export MH dataset ----------------------------------------------------

cli::cli_h1("Step 6: Exporting MH dataset")

# Export MH in XPT v8 + RDS + CSV
mh_final <- export_domain(
  data             = mh_result$data,
  domain           = "MH",
  output_dir       = "sdtm/datasets",
  formats          = c("xpt", "rds", "csv"),
  xpt_version      = 8L,
  target_meta      = target_meta,
  domain_meta      = domain_meta,
  drop_empty_perm  = TRUE
)

# Export SUPPMH if applicable
if (!is.null(mh_result$supp) && nrow(mh_result$supp) > 0L) {
  export_domain(mh_result$supp, "SUPPMH", "sdtm/datasets", formats = "xpt")
  cli::cli_alert_success("SUPPMH exported ({nrow(mh_result$supp)} rows)")
}

# ---- 8. Generate MH R program -----------------------------------------------

cli::cli_h1("Step 7: Generating MH derivation program")

dir.create("sdtm/programs", recursive = TRUE, showWarnings = FALSE)

gen_domain_script(
  domain      = "MH",
  rule_set    = rule_set,
  target_meta = target_meta,
  config      = config,
  output_path = "sdtm/programs/mh.R"
)
cli::cli_alert_success("Generated: sdtm/programs/mh.R")

# ---- Done! -------------------------------------------------------------------

cli::cli_h1("Demo Complete")
cli::cli_alert_success("MH domain built and exported successfully!")
cli::cli_alert_info("Outputs:")
cli::cli_bullets(c(
  " " = "Dataset:  sdtm/datasets/mh.xpt",
  " " = "RDS:      sdtm/datasets/mh.rds",
  " " = "Program:  sdtm/programs/mh.R"
))
if (!is.null(mh_result$supp) && nrow(mh_result$supp) > 0L) {
  cli::cli_bullets(c(" " = "SUPPMH:   sdtm/datasets/suppmh.xpt"))
}
