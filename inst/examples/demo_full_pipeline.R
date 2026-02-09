# ==============================================================================
# sdtmbuilder — Full Pipeline Demo
# ==============================================================================
# This script demonstrates the complete sdtmbuilder pipeline:
#   1. Generate dummy clinical data (or load your own)
#   2. Compile derivation rules from metadata
#   3. Build 10 SDTM domains end-to-end
#   4. Inspect and validate results
#   5. Export to XPT format
#
# Run this after installing the package:
#   devtools::install_github("chiara-cattani/sdtm-builder")
# ==============================================================================

library(sdtmbuilder)

cat("
================================================================================
  sdtmbuilder — Full Pipeline Demo
  Building 10 SDTM domains from metadata-driven rules
================================================================================
\n")

# =============================================================================
# STEP 1: Load or generate study data
# =============================================================================
cat("STEP 1: Generating dummy study data (30 subjects, seed=123)...\n\n")

study <- make_dummy_study(seed = 123)

# study contains:
#   $raw_data    — 10 raw source datasets (dm_raw, ae_raw, cm_raw, ds_raw, qs_raw, etc.)
#   $target_meta — SDTM variable definitions + derivation rules
#   $ct_lib      — controlled terminology codelists
#   $config      — study configuration (sdtm_config object)
#   $domain_meta — domain-level metadata (build order, keys, etc.)
#   $value_level_meta — value-level conditions

config      <- study$config
target_meta <- study$target_meta
ct_lib      <- study$ct_lib
raw_data    <- study$raw_data
domain_meta <- study$domain_meta

cat("  Study ID:", config$studyid, "\n")
cat("  Subjects:", nrow(raw_data$dm_raw), "\n")
cat("  Source datasets:\n")
for (nm in names(raw_data)) {
  cat(sprintf("    %-10s %4d rows x %2d cols\n", nm,
              nrow(raw_data[[nm]]), ncol(raw_data[[nm]])))
}

cat("\n  Domains in metadata:",
    paste(unique(target_meta$domain), collapse = ", "), "\n")
cat("  Controlled terminology codelists:",
    paste(unique(ct_lib$codelist_id), collapse = ", "), "\n\n")


# =============================================================================
# STEP 2: Compile derivation rules from metadata
# =============================================================================
cat("STEP 2: Compiling derivation rules from metadata...\n\n")

rule_set <- compile_rules(target_meta, ct_lib = ct_lib)

for (dom in names(rule_set$rules)) {
  n <- length(rule_set$rules[[dom]])
  types <- unique(vapply(rule_set$rules[[dom]], `[[`, character(1), "type"))
  cat(sprintf("  %-4s %2d rules  [types: %s]\n", dom, n,
              paste(types, collapse = ", ")))
}
cat("\n")


# =============================================================================
# STEP 3: Build all domains (automatic dependency ordering)
# =============================================================================
cat("STEP 3: Building SDTM domains (DM first, then all others)...\n")
cat(strrep("-", 60), "\n\n")

# build_all_domains() auto-detects that DM must be built first and
# passes dm_data downstream to every other domain.
# Set create_supp = FALSE to keep all variables in the main domain
# (useful when company guidelines do not require SUPP-- datasets).

all_results <- build_all_domains(
  target_meta = target_meta,
  raw_data    = raw_data,
  config      = config,
  rule_set    = rule_set,
  domain_meta = domain_meta,
  verbose     = FALSE
)

all_domains <- names(all_results)
built_data  <- all_results
build_ok    <- character()
build_fail  <- character()

for (dom in all_domains) {
  result <- all_results[[dom]]
  if (is.null(result)) {
    build_fail <- c(build_fail, dom)
    next
  }
  n_err  <- sum(result$report$findings$severity == "ERROR")
  n_warn <- sum(result$report$findings$severity == "WARNING")
  status <- if (n_err == 0) "PASS" else "WARN"
  cat(sprintf("  [%s] %-4s %4d rows x %2d cols  |  Errors: %d  Warnings: %d\n",
              status, dom, nrow(result$data), ncol(result$data), n_err, n_warn))
  build_ok <- c(build_ok, dom)
}

cat(sprintf("\n  Built: %d/%d domains\n", length(build_ok), length(all_domains)))
if (length(build_fail) > 0) {
  cat("  Failed:", paste(build_fail, collapse = ", "), "\n")
}
cat("\n")


# =============================================================================
# STEP 4: Inspect built domains
# =============================================================================
cat("STEP 4: Inspecting built domains...\n")
cat(strrep("-", 60), "\n\n")

# --- DM (Demographics) ---
if ("DM" %in% build_ok) {
  dm <- built_data$DM$data
  cat("DM — Demographics (spine domain)\n")
  cat(sprintf("  %d subjects, %d variables\n", nrow(dm), ncol(dm)))
  cat("  Variables:", paste(names(dm), collapse = ", "), "\n")
  cat("  Arms:", paste(sort(unique(dm$ARMCD)), collapse = ", "), "\n")
  cat("  Sites:", paste(sort(unique(dm$SITEID)), collapse = ", "), "\n")
  cat("  Sex distribution:", paste(table(dm$SEX), collapse = "/"), "\n")
  cat("  First 3 rows:\n")
  print(dm[1:3, c("USUBJID", "SEX", "AGE", "ARMCD", "RFSTDTC")])
  cat("\n")
}

# --- AE (Adverse Events) ---
if ("AE" %in% build_ok) {
  ae <- built_data$AE$data
  cat("AE — Adverse Events\n")
  cat(sprintf("  %d events across %d subjects\n", nrow(ae),
              length(unique(ae$USUBJID))))
  cat("  Severity distribution:\n")
  print(table(ae$AESEV))
  cat("  Causality distribution:\n")
  print(table(ae$AEREL))
  cat("  First 3 rows:\n")
  print(ae[1:3, c("USUBJID", "AESEQ", "AETERM", "AESEV", "AESTDTC")])
  cat("\n")
}

# --- CM (Concomitant Medications) ---
if ("CM" %in% build_ok) {
  cm <- built_data$CM$data
  cat("CM — Concomitant Medications\n")
  cat(sprintf("  %d records, %d unique meds\n", nrow(cm),
              length(unique(cm$CMTRT))))
  cat("  Route distribution:\n")
  print(table(cm$CMROUTE))
  cat("\n")
}

# --- EX (Exposure) ---
if ("EX" %in% build_ok) {
  ex <- built_data$EX$data
  cat("EX — Exposure\n")
  cat(sprintf("  %d dosing records across %d subjects\n", nrow(ex),
              length(unique(ex$USUBJID))))
  cat("  Treatments:\n")
  print(table(ex$EXTRT))
  cat("\n")
}

# --- VS (Vital Signs) ---
if ("VS" %in% build_ok) {
  vs <- built_data$VS$data
  cat("VS — Vital Signs\n")
  cat(sprintf("  %d measurements across %d subjects, %d visits\n", nrow(vs),
              length(unique(vs$USUBJID)), length(unique(vs$VISIT))))
  cat("  Tests:\n")
  print(table(vs$VSTESTCD))
  cat("  Visits:\n")
  print(table(vs$VISIT))
  cat("  Baseline records:", sum(vs$VSBLFL == "Y", na.rm = TRUE), "\n\n")
}

# --- LB (Laboratory) ---
if ("LB" %in% build_ok) {
  lb <- built_data$LB$data
  cat("LB — Laboratory\n")
  cat(sprintf("  %d results across %d subjects, %d visits\n", nrow(lb),
              length(unique(lb$USUBJID)), length(unique(lb$VISIT))))
  cat("  Tests:\n")
  print(table(lb$LBTESTCD))
  cat("\n")
}

# --- MH (Medical History) ---
if ("MH" %in% build_ok) {
  mh <- built_data$MH$data
  cat("MH — Medical History\n")
  cat(sprintf("  %d records\n", nrow(mh)))
  # Show partial dates
  partial_ct <- sum(nchar(mh$MHSTDTC) < 10, na.rm = TRUE)
  cat(sprintf("  Partial dates: %d/%d (%.0f%%)\n", partial_ct, nrow(mh),
              100 * partial_ct / nrow(mh)))
  cat("\n")
}

# --- PR (Procedures) ---
if ("PR" %in% build_ok) {
  pr <- built_data$PR$data
  cat("PR — Procedures\n")
  cat(sprintf("  %d records\n", nrow(pr)))
  cat("  Categories:\n")
  print(table(pr$PRCAT))
  cat("\n")
}

# --- DS (Disposition) ---
if ("DS" %in% build_ok) {
  ds <- built_data$DS$data
  cat("DS — Disposition\n")
  cat(sprintf("  %d records across %d subjects\n", nrow(ds),
              length(unique(ds$USUBJID))))
  cat("  Rule types demonstrated: coalesce, case_when, if_else, concat\n")
  cat("  Category distribution (case_when):\n")
  print(table(ds$DSCAT))
  cat("  Sub-category distribution (if_else):\n")
  print(table(ds$DSSCAT))
  cat("  Sample DSREFID (concat):", head(ds$DSREFID, 3), "\n")
  cat("  Sample DSDECOD (coalesce):", head(ds$DSDECOD, 3), "\n")
  cat("\n")
}

# --- QS (Questionnaires) ---
if ("QS" %in% build_ok) {
  qs <- built_data$QS$data
  cat("QS — Questionnaires\n")
  cat(sprintf("  %d records across %d subjects, %d visits\n", nrow(qs),
              length(unique(qs$USUBJID)), length(unique(qs$VISIT))))
  cat("  Rule types demonstrated: ct_decode, occurrence, baseline_flag\n")
  cat("  Tests (ct_decode from QSTESTCD):\n")
  print(table(qs$QSTEST))
  cat("  Status (occurrence):\n")
  print(table(qs$QSSTAT, useNA = "always"))
  cat("  Baseline flags:", sum(qs$QSBLFL == "Y", na.rm = TRUE), "records\n")
  cat("\n")
}


# =============================================================================
# STEP 5: Export to XPT (optional)
# =============================================================================
cat("STEP 5: Exporting to XPT...\n")
cat(strrep("-", 60), "\n\n")

output_dir <- tempfile("sdtm_output_")
dir.create(output_dir)

for (dom in build_ok) {
  tryCatch({
    export_xpt(built_data[[dom]]$data, dom, output_dir, target_meta)
    fpath <- file.path(output_dir, paste0(tolower(dom), ".xpt"))
    fsize <- file.size(fpath)
    cat(sprintf("  %-4s -> %s (%s bytes)\n", dom, basename(fpath),
                format(fsize, big.mark = ",")))
  }, error = function(e) {
    cat(sprintf("  %-4s export error: %s\n", dom, e$message))
  })
}

cat(sprintf("\n  XPT files written to: %s\n\n", output_dir))


# =============================================================================
# SUMMARY
# =============================================================================
cat("
================================================================================
  SUMMARY
================================================================================

")

total_rows <- sum(vapply(build_ok, function(d) nrow(built_data[[d]]$data), numeric(1)))
total_errs <- sum(vapply(build_ok, function(d) {
  sum(built_data[[d]]$report$findings$severity == "ERROR")
}, numeric(1)))

cat(sprintf("  Domains built:         %d / %d\n", length(build_ok), length(all_domains)))
cat(sprintf("  Total records:         %s\n", format(total_rows, big.mark = ",")))
cat(sprintf("  Validation errors:     %d\n", total_errs))
cat(sprintf("  XPT files exported:    %d\n", length(build_ok)))
cat(sprintf("  Output directory:      %s\n", output_dir))

cat("\n  Domain breakdown:\n")
for (dom in build_ok) {
  d <- built_data[[dom]]$data
  cat(sprintf("    %-4s  %4d rows x %2d cols\n", dom, nrow(d), ncol(d)))
}

cat("
================================================================================
  Pipeline complete!
  sdtmbuilder v0.1.0 — metadata-driven SDTM domain builder for R
================================================================================
\n")
