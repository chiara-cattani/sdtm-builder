# ==============================================================================
# sdtmbuilder — Your Own Study Pipeline Example
# ==============================================================================
# This script shows exactly how to use sdtmbuilder on YOUR OWN clinical study.
#
# Prerequisites:
#   - Your raw data files (sas7bdat, csv, xpt, or xlsx) in a "raw/" folder
#   - Study_Metadata.xlsx filled in (see package vignette for sheet structure)
#   - Study_CT.xlsx filled in (Codelists + Codelists_terms sheets)
#   - config.yaml filled in (copy template with get_template_config())
#
# Expected folder structure:
#   my_study/
#   ├── metadata/
#   │   ├── config.yaml
#   │   ├── Study_Metadata.xlsx
#   │   └── Study_CT.xlsx
#   ├── raw/
#   │   ├── dm.sas7bdat
#   │   ├── ae.sas7bdat
#   │   ├── cm.sas7bdat
#   │   ├── lb.sas7bdat
#   │   └── ...
#   └── sdtm/
#       ├── datasets/   <- XPT + RDA files will go here
#       └── programs/   <- Generated R scripts will go here
# ==============================================================================

library(sdtmbuilder)

# ==============================================================================
# OPTION A: One-Call Pipeline (recommended)
# ==============================================================================
# If you have a config.yaml, a single call does everything:

# Set working directory to your study root
# setwd("C:/Users/you/my_study")

out <- run_study("metadata/config.yaml")

# That's it! Check your sdtm/datasets/ and sdtm/programs/ folders.
# Results are also in memory:
names(out$results)           # domains built (e.g., "DM", "AE", "LB", ...)
head(out$results$DM$data)    # inspect DM dataset
head(out$results$AE$data)    # inspect AE dataset

# If you only want datasets (no R programs generated):
out <- run_study("metadata/config.yaml", generate_programs = FALSE)


# ==============================================================================
# OPTION B: One-Call Pipeline without config.yaml
# ==============================================================================
# You can skip config.yaml and pass paths directly:

out <- run_study(
  metadata_path = "metadata/Study_Metadata.xlsx",
  ct_path       = "metadata/Study_CT.xlsx",
  raw_dir       = "raw",
  output_dir    = "sdtm/datasets",
  programs_dir  = "sdtm/programs"
)


# ==============================================================================
# OPTION C: Step-by-Step (full control)
# ==============================================================================
# For more control, run each step individually.

# ── Step 1: Read metadata ────────────────────────────────────────────────────
study_meta <- read_study_metadata_excel("metadata/Study_Metadata.xlsx")
ct_lib     <- read_study_ct_excel("metadata/Study_CT.xlsx")

target_meta      <- study_meta$target_meta
domain_meta      <- study_meta$domain_meta
value_level_meta <- study_meta$value_level_meta

# Expand value-level metadata (if you use VLM)
if (!is.null(value_level_meta) && nrow(value_level_meta) > 0) {
  target_meta <- expand_value_level_meta(target_meta, value_level_meta)
}

cat("Domains in metadata:", paste(unique(target_meta$domain), collapse = ", "), "\n")

# ── Step 2: Load raw datasets ────────────────────────────────────────────────
raw_data <- load_raw_datasets("raw")
cat("Raw datasets loaded:", paste(names(raw_data), collapse = ", "), "\n")

# ── Step 3: Create configuration ─────────────────────────────────────────────
# Read from config.yaml (in metadata/ folder):
cfg_yaml <- yaml::read_yaml("metadata/config.yaml")

config <- new_sdtm_config(
  studyid        = cfg_yaml$studyid,
  timezone       = cfg_yaml$timezone %||% "UTC",
  ref_start_rule = list(
    var    = cfg_yaml$ref_start_rule$column %||% "rfstdtc",
    source = cfg_yaml$ref_start_rule$dataset %||% "dm_raw"
  )
)

# ── Step 4: Compile rules ────────────────────────────────────────────────────
rule_set <- compile_rules(target_meta, ct_lib = ct_lib)
cat("Rule set compiled for", length(names(rule_set$rules)), "domains\n")

# ── Step 5: Build all domains ────────────────────────────────────────────────
results <- build_all_domains(
  target_meta      = target_meta,
  raw_data         = raw_data,
  config           = config,
  rule_set         = rule_set,
  domain_meta      = domain_meta,
  value_level_meta = value_level_meta
)

cat("Built", length(results), "domains:", paste(names(results), collapse = ", "), "\n")

# ── Step 6: Export each domain as XPT v8 + RDA ──────────────────────────────
for (dom in names(results)) {
  dom_data <- results[[dom]]$data
  if (is.null(dom_data)) next

  # Export as XPT v8 + RDA
  export_domain(dom_data, dom, "sdtm/datasets",
                formats = c("xpt", "rda"),
                target_meta = target_meta, domain_meta = domain_meta)

  # Export SUPP if present
  supp <- results[[dom]]$supp
  if (!is.null(supp) && nrow(supp) > 0) {
    export_domain(supp, paste0("SUPP", dom), "sdtm/datasets",
                  formats = c("xpt", "rda"))
  }
}

# ── Step 7: Generate R programs ──────────────────────────────────────────────
for (dom in names(results)) {
  gen_domain_script(
    domain      = dom,
    rule_set    = rule_set,
    target_meta = target_meta,
    config      = config,
    output_path = file.path("sdtm/programs", paste0(tolower(dom), ".R"))
  )
}

cat("\n===== Pipeline complete! =====\n")
cat("Datasets:  sdtm/datasets/\n")
cat("Programs:  sdtm/programs/\n")
