# =============================================================================
# STUDY-PILOT Study — CM (Concomitant Medications) Domain Demo
# =============================================================================
# This script demonstrates how to use sdtmbuilder to:
#   1. Read study metadata and controlled terminology
#   2. Load and prepare raw data (merging CRF datasets as in CM.sas)
#   3. Build the CM (Concomitant Medications) SDTM domain
#   4. Export the CM dataset as XPT
#   5. Auto-generate the CM derivation R program
#
# Prerequisites:
#   - sdtmbuilder package installed (devtools::install("."))
#   - Raw datasets in raw/ (run create_dummy_raw.R + create_dummy_raw_cm.R
#     first, or point RAW_DATA_DIR to your real EDC export folder)
#
# Raw datasets:
#   - cm_whodrug.csv : Concomitant Medications CRF + WHODrug coded data
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

cm_whodrug <- all_raw[["cm_whodrug"]]
dm_data    <- all_raw[["dm"]]
ic_data    <- all_raw[["ic"]]

# =============================================================================
# 2a. Replicate CM.sas data preparation logic
# =============================================================================
# CM.sas merges cm_whodrug and derives CM variables before the final
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

# Derive USUBJID and SUBJID
cm_raw <- cm_whodrug
cm_raw$usubjid <- derive_usubjid_study(cm_raw$subjectid, STUDYID)
cm_raw$subjid  <- cm_raw$subjectid

cli::cli_alert_success("CM raw data: {nrow(cm_raw)} rows x {ncol(cm_raw)} cols")

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

# --- 2d. Pre-derive CM-specific SAS logic ------------------------------------
# Replicate CM.sas DATA step derivations

# DOMAIN and STUDYID
cm_raw$DOMAIN  <- "CM"
cm_raw$STUDYID <- STUDYID

# CMCAT: constant (SAS: cmcat = "CONCOMITANT MEDICATIONS AND NUTRITIONAL SUPPLEMENTS")
cm_raw$CMCAT <- "CONCOMITANT MEDICATIONS AND NUTRITIONAL SUPPLEMENTS"

# SOURCEID
cm_raw$SOURCEID <- "CRF: CM"

# CMTRT: direct from raw (SAS: cmtrt = strip(cmtrt))
cm_raw$cmtrt <- trimws(cm_raw$cmtrt)

# CMSPID: numeric to character (SAS: cmspid = strip(put(_cmspid, best.)))
cm_raw$cmspid <- as.character(cm_raw$cmspid)

# CMINDC: indication from raw (SAS: cmindc = strip(indicat))
cm_raw$cmindc <- trimws(cm_raw$indicat)

# CMINDCX: indication other specify (SAS: cmindcx = strip(cmindsp))
cm_raw$CMINDCX <- if ("cmindsp" %in% names(cm_raw)) trimws(cm_raw$cmindsp) else ""

# CMDOSFRQ: dosing frequency (SAS: cmdosfrq = strip(cmfrq))
cm_raw$cmdosfrq <- if ("cmfrq" %in% names(cm_raw)) trimws(cm_raw$cmfrq) else ""

# CMDSFRQX: frequency other specify (SAS: cmdsfrqx = strip(cmfrqsp))
cm_raw$CMDSFRQX <- if ("cmfrqsp" %in% names(cm_raw)) trimws(cm_raw$cmfrqsp) else ""

# CMDOSE / CMDOSTXT: In original SAS, a single "cmdos" field is split into
# numeric CMDOSE and text CMDOSTXT. In our raw data, they are already separate.
# If a unified "cmdos" column exists, apply the split; otherwise use as-is.
if ("cmdos" %in% names(cm_raw) && !"cmdose" %in% names(cm_raw)) {
  numeric_part <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", cm_raw$cmdos)))
  cm_raw$cmdose   <- ifelse(!is.na(numeric_part), numeric_part, NA_real_)
  cm_raw$cmdostxt <- ifelse(is.na(numeric_part), trimws(cm_raw$cmdos), "")
}
# Ensure numeric type for cmdose
if ("cmdose" %in% names(cm_raw)) {
  cm_raw$cmdose <- suppressWarnings(as.numeric(cm_raw$cmdose))
}

# CMDOSU: dose unit (SAS: cmdosu = strip(cmunit))
cm_raw$cmdosu <- if ("cmunit" %in% names(cm_raw)) trimws(cm_raw$cmunit) else ""

# CMROUTE: route (SAS: cmroute = strip(cmroute))
cm_raw$cmroute <- if ("cmroute" %in% names(cm_raw)) trimws(cm_raw$cmroute) else ""

# WHODrug fields: In our raw data these already use SDTM column names.
# If legacy names exist (drugname, atc1 etc.), map them; otherwise leave as-is.
if ("drugname" %in% names(cm_raw) && !"cmdecod" %in% names(cm_raw)) {
  cm_raw$cmdecod <- cm_raw$drugname
}
for (.atc_pair in list(c("atc1","cmatc1"), c("atc2","cmatc2"),
                       c("atc3","cmatc3"), c("atc4","cmatc4"))) {
  if (.atc_pair[1] %in% names(cm_raw) && !.atc_pair[2] %in% names(cm_raw)) {
    cm_raw[[.atc_pair[2]]] <- cm_raw[[.atc_pair[1]]]
  }
}

# CMDICT: WHODrug dictionary version — detect from cm_whodrug data dynamically
# Uses get_whodrug_version() from sdtmbuilder
whodrug_version <- get_whodrug_version(cm_raw)
if (nzchar(whodrug_version)) {
  cli::cli_alert_info("WHODrug version detected: {whodrug_version}")
} else {
  # Fallback: try raw version column directly
  if ("version" %in% names(cm_raw)) {
    ver_vals <- unique(cm_raw$version[!is.na(cm_raw$version)])
    if (length(ver_vals) > 0) whodrug_version <- ver_vals[1]
  }
  if (!nzchar(whodrug_version)) {
    whodrug_version <- "B3"
    cli::cli_alert_warning("WHODrug version could not be detected; defaulting to {whodrug_version}")
  }
}
cm_raw <- derive_dict_version(cm_raw, "CMDICT", "cmdecod", "whodrug", whodrug_version)

# Date variables: ISO DTC conversion
# SAS: %sdtm_dt2dtc(dt=cmstdat, dtc=cmstdtc)
cm_raw$CMSTDTC <- format_iso_dtc(cm_raw$cmstdat)
cm_raw$CMENDTC <- format_iso_dtc(cm_raw$cmendat)

# End relative time point
# SAS: if ^missing(cmongo) then do;
#        cmentpt = "END OF STUDY";
#        if lowcase(cmongo) = "y" then cmenrtpt = "ONGOING";
#        else if lowcase(cmongo) = "n" then cmenrtpt = "BEFORE";
#      end;
cm_raw$CMENRTPT <- NA_character_
cm_raw$CMENTPT  <- NA_character_
idx_ongo <- !is.na(cm_raw$cmongo)
cm_raw$CMENRTPT[idx_ongo & tolower(cm_raw$cmongo) == "y"] <- "ONGOING"
cm_raw$CMENRTPT[idx_ongo & tolower(cm_raw$cmongo) == "n"] <- "BEFORE"
cm_raw$CMENTPT[!is.na(cm_raw$CMENRTPT)] <- "END OF STUDY"

# Audit variables: ISO date conversion
# SAS: %sdtm_dt2dtc(dt=CodedOnDate, dtc=cmcoded)
cm_raw$CMCODED  <- format_iso_dtc(cm_raw$codedondate)
cm_raw$CMINITD  <- format_iso_dtc(cm_raw$initiateddate)
cm_raw$CMLTEDID <- format_iso_dtc(cm_raw$lastediteddate)

# Join RFSTDTC from DM for DY calculation
dm_slim <- dplyr::distinct(dm_raw[, c("usubjid", "rfstdtc"), drop = FALSE])
cm_raw <- dplyr::left_join(cm_raw, dm_slim, by = "usubjid")

# Derive DY variables
# SAS: %sdtm_dtc2dy(dtc=cmstdtc) and %sdtm_dtc2dy(dtc=cmendtc)
cm_raw$RFSTDTC <- cm_raw$rfstdtc
cm_raw <- derive_dy(cm_raw, "CMSTDY", "CMSTDTC", "RFSTDTC")
cm_raw <- derive_dy(cm_raw, "CMENDY", "CMENDTC", "RFSTDTC")

cli::cli_alert_success("Pre-derived CM variables from SAS logic")

# --- 2e. Assemble raw_data list for sdtmbuilder ------------------------------
raw_data <- list(
  cm_raw = cm_raw,
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

cm_rules <- rule_set$rules[["CM"]]
if (is.null(cm_rules)) {
  cli::cli_alert_warning("No CM rules found in rule_set. Check metadata has CM domain variables.")
} else {
  cli::cli_alert_success("CM domain: {length(cm_rules)} derivation rules compiled")
}

# ---- 6. Build CM domain -----------------------------------------------------

cli::cli_h1("Step 5: Building CM domain")

cm_result <- build_domain(
  domain           = "CM",
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

cli::cli_alert_success("CM built: {nrow(cm_result$data)} rows x {ncol(cm_result$data)} cols")

# Show validation summary
if (!is.null(cm_result$report)) {
  n_err  <- sum(cm_result$report$findings$severity == "ERROR", na.rm = TRUE)
  n_warn <- sum(cm_result$report$findings$severity == "WARNING", na.rm = TRUE)
  cli::cli_alert_info("Validation: {n_err} errors, {n_warn} warnings")
}

# Preview first rows
cat("\n--- CM Domain Preview ---\n")
print(head(cm_result$data, 5))

# ---- 7. Export CM dataset ----------------------------------------------------

cli::cli_h1("Step 6: Exporting CM dataset")

# Export CM in XPT v8 + RDS + CSV
cm_final <- export_domain(
  data             = cm_result$data,
  domain           = "CM",
  output_dir       = "sdtm/datasets",
  formats          = c("xpt", "rds", "csv"),
  xpt_version      = 8L,
  target_meta      = target_meta,
  domain_meta      = domain_meta,
  drop_empty_perm  = TRUE
)

# Export SUPPCM if applicable
if (!is.null(cm_result$supp) && nrow(cm_result$supp) > 0L) {
  export_domain(cm_result$supp, "SUPPCM", "sdtm/datasets", formats = "xpt")
  cli::cli_alert_success("SUPPCM exported ({nrow(cm_result$supp)} rows)")
}

# ---- 8. Generate CM R program -----------------------------------------------

cli::cli_h1("Step 7: Generating CM derivation program")

dir.create("sdtm/programs", recursive = TRUE, showWarnings = FALSE)

gen_domain_script(
  domain      = "CM",
  rule_set    = rule_set,
  target_meta = target_meta,
  config      = config,
  output_path = "sdtm/programs/cm.R"
)
cli::cli_alert_success("Generated: sdtm/programs/cm.R")

# ---- Done! -------------------------------------------------------------------

cli::cli_h1("Demo Complete")
cli::cli_alert_success("CM domain built and exported successfully!")
cli::cli_alert_info("Outputs:")
cli::cli_bullets(c(
  " " = "Dataset:  sdtm/datasets/cm.xpt",
  " " = "RDS:      sdtm/datasets/cm.rds",
  " " = "Program:  sdtm/programs/cm.R"
))
if (!is.null(cm_result$supp) && nrow(cm_result$supp) > 0L) {
  cli::cli_bullets(c(" " = "SUPPCM:   sdtm/datasets/suppcm.xpt"))
}
