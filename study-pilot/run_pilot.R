# ******************************************************************************
# Run study-pilot pipeline
# ******************************************************************************
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(sdtmbuilder)
devtools::load_all("..")

out <- run_study(

  # -- Config ------------------------------------------------------------------
  # Path to the YAML config file. NULL = auto-discover config.yaml or
  # metadata/config.yaml in the working directory.
  config_path       = NULL,

  # -- Paths (override config.yaml if set) -------------------------------------
  # metadata_path = Path to Study_Metadata.xlsx
  # ct_path       = Path to Study_CT.xlsx
  # raw_dir       = Directory with raw datasets (.sas7bdat / .csv / .xpt)
  # output_dir    = Where to write exported XPT/RDA files
  # programs_dir  = Where to write generated R programs
  metadata_path     = NULL,
  ct_path           = NULL,
  raw_dir           = NULL,
  output_dir        = NULL,
  programs_dir      = NULL,

  # -- Export ------------------------------------------------------------------
  # export_formats = Output file types: "xpt" (SAS transport) and/or "rda" (R binary)
  # xpt_version    = SAS transport version: 8 (v8, default) or 5 (v5 legacy)
  # generate_programs = TRUE to write an R script per domain
  export_formats    = c("xpt", "rda"),
  xpt_version       = 8L,
  generate_programs = TRUE,

  # -- Domains -----------------------------------------------------------------
  # Which domains to build. NULL = build all found in metadata.
  domains           = c("AE"),

  # -- Options -----------------------------------------------------------------
  # create_supp    = TRUE to generate SUPP-- datasets; FALSE to skip
  # drop_empty_perm= TRUE to drop PERM variables that are entirely empty;
  #                  FALSE to keep all; or a named list for per-domain overrides
  # validate       = TRUE to run validation checks after building
  # verbose        = TRUE for progress messages in the console
  create_supp       = FALSE,
  drop_empty_perm   = FALSE,
  validate          = TRUE,
  verbose           = TRUE
)

