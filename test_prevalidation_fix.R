# Quick test of pre-validation fix
setwd("c:\\Users\\chiar\\OneDrive - Alma Mater Studiorum Università di Bologna\\Desktop\\Repo\\sdtm-builder\\study-pilot")
library(sdtmbuilder)
devtools::load_all("..")

cat("\n--- Testing Pre-Validation Fix ---\n\n")

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

cat("\n--- Test Complete: No errors! ---\n")
