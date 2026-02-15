# tests/testthat/helper-dummy.R
# ============================================================================
# Shared test fixtures — loaded automatically by testthat before test files.
# ============================================================================
# Uses the package's make_dummy_study() to generate deterministic test data.
# All tests share the same datasets to ensure consistency and speed.
# ============================================================================

# ---------------------------------------------------------------------------
# Resolve starter kit path (works in devtools::test and R CMD check)
# ---------------------------------------------------------------------------
starter_kit_path <- system.file("extdata", "starter_kit",
                                package = "sdtmbuilder")
if (starter_kit_path == "" || !dir.exists(starter_kit_path)) {
  # Fallback for devtools::test() where inst/ is not installed
  starter_kit_path <- file.path(
    testthat::test_path(), "..", "..", "inst", "extdata", "starter_kit"
  )
}

# ---------------------------------------------------------------------------
# Generate clean dummy study
# ---------------------------------------------------------------------------
if (dir.exists(starter_kit_path) &&
    file.exists(file.path(starter_kit_path, "Study_Metadata.xlsx"))) {
  dummy_study <- make_dummy_study(seed = 123,
                                  starter_kit_dir = starter_kit_path)
  dummy_cfg   <- dummy_study$config
  dummy_meta  <- dummy_study$target_meta
  dummy_ct    <- dummy_study$ct_lib
  dummy_raw   <- dummy_study$raw_data
} else {
  # If starter kit is not available, set NULLs so tests can skip gracefully
  dummy_study <- NULL
  dummy_cfg   <- NULL
  dummy_meta  <- NULL
  dummy_ct    <- NULL
  dummy_raw   <- NULL
}

# ---------------------------------------------------------------------------
# Helper: skip if starter kit not available
# ---------------------------------------------------------------------------
skip_if_no_starter_kit <- function() {
  if (is.null(dummy_study)) {
    testthat::skip("Starter kit not available")
  }
}
