# ******************************************************************************
# Run study-pilot pipeline
# ******************************************************************************

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(sdtmbuilder)
devtools::load_all("..")

out <- run_study(
  config_path       = NULL,
  metadata_path     = NULL,
  ct_path           = NULL,
  raw_dir           = NULL,
  output_dir        = NULL,
  programs_dir      = NULL,
  export_formats    = c("xpt", "rda"),
  xpt_version       = 8L,
  generate_programs = TRUE,
  domains           = c("AE"),
  create_supp       = FALSE,
  drop_empty_perm   = FALSE,
  validate          = TRUE,
  verbose           = TRUE
)
