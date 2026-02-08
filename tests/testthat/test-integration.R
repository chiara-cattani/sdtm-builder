# tests/testthat/test-integration.R
# ============================================================================
# Integration tests: all 8 domains build end-to-end, SUPP, new validation
# ============================================================================

# ---------------------------------------------------------------------------
# DM domain
# ---------------------------------------------------------------------------
test_that("DM builds correctly with expected columns", {
  skip_if_no_starter_kit()
  rs <- compile_rules(dummy_meta, dummy_smeta, dummy_ct)
  result <- build_domain("DM", dummy_meta, dummy_smeta, dummy_raw,
                         dummy_cfg, rs, verbose = FALSE)

  expect_s3_class(result, "build_result")
  expect_true(nrow(result$data) > 0)
  expect_true("STUDYID" %in% names(result$data))
  expect_true("USUBJID" %in% names(result$data))
  expect_true("DOMAIN" %in% names(result$data))
  expect_true("SEX" %in% names(result$data))
  expect_true("AGE" %in% names(result$data))
  expect_true("ARMCD" %in% names(result$data))
  expect_equal(unique(result$data$DOMAIN), "DM")
  # DM should have one row per subject
  expect_equal(nrow(result$data),
               length(unique(result$data$USUBJID)))
})

# ---------------------------------------------------------------------------
# EX domain
# ---------------------------------------------------------------------------
test_that("EX builds correctly with expected columns", {
  skip_if_no_starter_kit()
  rs <- compile_rules(dummy_meta, dummy_smeta, dummy_ct)
  result <- build_domain("EX", dummy_meta, dummy_smeta, dummy_raw,
                         dummy_cfg, rs, verbose = FALSE)

  expect_s3_class(result, "build_result")
  expect_true(nrow(result$data) > 0)
  expect_true("EXTRT" %in% names(result$data))
  expect_true("EXDOSE" %in% names(result$data))
  expect_true("EXSTDTC" %in% names(result$data))
  expect_equal(unique(result$data$DOMAIN), "EX")
  # EXSEQ should be positive
  expect_true(all(result$data$EXSEQ > 0, na.rm = TRUE))
})

# ---------------------------------------------------------------------------
# VS domain
# ---------------------------------------------------------------------------
test_that("VS builds correctly with expected columns", {
  skip_if_no_starter_kit()
  rs <- compile_rules(dummy_meta, dummy_smeta, dummy_ct)
  result <- build_domain("VS", dummy_meta, dummy_smeta, dummy_raw,
                         dummy_cfg, rs, verbose = FALSE)

  expect_s3_class(result, "build_result")
  expect_true(nrow(result$data) > 0)
  expect_true("VSTESTCD" %in% names(result$data))
  expect_true("VSORRES" %in% names(result$data))
  expect_true("VSDTC" %in% names(result$data))
  expect_true("VSBLFL" %in% names(result$data))
  expect_true("VISIT" %in% names(result$data))
  expect_true("VISITNUM" %in% names(result$data))
  expect_equal(unique(result$data$DOMAIN), "VS")

  # Baseline flag: should have some "Y" values
  bl_vals <- result$data$VSBLFL[!is.na(result$data$VSBLFL)]
  expect_true(length(bl_vals) > 0)
  expect_true(all(bl_vals == "Y"))
})

# ---------------------------------------------------------------------------
# LB domain
# ---------------------------------------------------------------------------
test_that("LB builds correctly with expected columns", {
  skip_if_no_starter_kit()
  rs <- compile_rules(dummy_meta, dummy_smeta, dummy_ct)
  result <- build_domain("LB", dummy_meta, dummy_smeta, dummy_raw,
                         dummy_cfg, rs, verbose = FALSE)

  expect_s3_class(result, "build_result")
  expect_true(nrow(result$data) > 0)
  expect_true("LBTESTCD" %in% names(result$data))
  expect_true("LBORRES" %in% names(result$data))
  expect_true("LBDTC" %in% names(result$data))
  expect_true("VISIT" %in% names(result$data))
  expect_equal(unique(result$data$DOMAIN), "LB")
  # LBSEQ should be positive
  expect_true(all(result$data$LBSEQ > 0, na.rm = TRUE))
})

# ---------------------------------------------------------------------------
# All 8 domains: zero validation errors
# ---------------------------------------------------------------------------
test_that("All 8 domains build with zero validation errors", {
  skip_if_no_starter_kit()
  rs <- compile_rules(dummy_meta, dummy_smeta, dummy_ct)
  domains <- c("DM", "AE", "CM", "MH", "PR", "EX", "VS", "LB")
  dm_result <- build_domain("DM", dummy_meta, dummy_smeta, dummy_raw,
                            dummy_cfg, rs, verbose = FALSE)
  dm_data <- dm_result$data

  for (dom in domains) {
    result <- build_domain(dom, dummy_meta, dummy_smeta, dummy_raw,
                           dummy_cfg, rs, dm_data = dm_data, verbose = FALSE)
    n_errors <- sum(result$report$findings$severity == "ERROR")
    expect_equal(n_errors, 0,
                 info = paste0(dom, " has ", n_errors, " validation errors"))
  }
})

# ---------------------------------------------------------------------------
# SUPPAE generation
# ---------------------------------------------------------------------------
test_that("AE build generates SUPPAE from to_supp metadata", {
  skip_if_no_starter_kit()
  rs <- compile_rules(dummy_meta, dummy_smeta, dummy_ct)
  result <- build_domain("AE", dummy_meta, dummy_smeta, dummy_raw,
                         dummy_cfg, rs, verbose = FALSE)

  # SUPPAE should be present
  expect_false(is.null(result$supp),
               info = "SUPPAE should be generated")

  if (!is.null(result$supp)) {
    expect_true(nrow(result$supp) > 0)
    # Standard SUPP columns
    expect_true(all(c("STUDYID", "RDOMAIN", "USUBJID", "IDVAR",
                       "IDVARVAL", "QNAM", "QLABEL", "QVAL",
                       "QORIG", "QEVAL") %in% names(result$supp)))
    expect_equal(unique(result$supp$RDOMAIN), "AE")
    # QNAM values should be the SUPP var names
    expect_true("AESSION" %in% result$supp$QNAM)
    expect_true("AETRTEM" %in% result$supp$QNAM)
  }

  # Main AE data should NOT contain SUPP vars
  expect_false("AESSION" %in% names(result$data))
  expect_false("AETRTEM" %in% names(result$data))
})

# ---------------------------------------------------------------------------
# Non-AE domains: no SUPP
# ---------------------------------------------------------------------------
test_that("Domains without to_supp vars have NULL supp", {
  skip_if_no_starter_kit()
  rs <- compile_rules(dummy_meta, dummy_smeta, dummy_ct)
  result <- build_domain("CM", dummy_meta, dummy_smeta, dummy_raw,
                         dummy_cfg, rs, verbose = FALSE)
  expect_null(result$supp)
})

# ---------------------------------------------------------------------------
# New validation checks
# ---------------------------------------------------------------------------
test_that("validate_domain_value detects wrong DOMAIN value", {
  rpt <- new_validation_report(domain = "AE")
  df <- data.frame(DOMAIN = c("AE", "AE", "XX"), stringsAsFactors = FALSE)
  rpt <- validate_domain_value(df, "AE", rpt)
  expect_true(any(rpt$findings$severity == "ERROR"))
  expect_true(any(grepl("XX", rpt$findings$message)))
})

test_that("validate_domain_value passes for correct DOMAIN", {
  rpt <- new_validation_report(domain = "AE")
  df <- data.frame(DOMAIN = c("AE", "AE"), stringsAsFactors = FALSE)
  rpt <- validate_domain_value(df, "AE", rpt)
  expect_equal(nrow(rpt$findings), 0)
})

test_that("validate_studyid_constant detects multiple STUDYIDs", {
  rpt <- new_validation_report(domain = "AE")
  df <- data.frame(STUDYID = c("S1", "S1", "S2"), stringsAsFactors = FALSE)
  rpt <- validate_studyid_constant(df, "AE", rpt)
  expect_true(any(rpt$findings$severity == "ERROR"))
})

test_that("validate_studyid_constant passes for single STUDYID", {
  rpt <- new_validation_report(domain = "AE")
  df <- data.frame(STUDYID = c("S1", "S1", "S1"), stringsAsFactors = FALSE)
  rpt <- validate_studyid_constant(df, "AE", rpt)
  expect_equal(nrow(rpt$findings), 0)
})

test_that("validate_no_allna_reqexp flags all-NA required var", {
  rpt <- new_validation_report(domain = "TEST")
  tm <- data.frame(domain = "TEST", var = c("STUDYID", "TESTVAR"),
                   label = c("A", "B"), type = c("char", "char"),
                   core = c("REQ", "EXP"), rule_type = c("constant", "direct_map"),
                   stringsAsFactors = FALSE)
  df <- data.frame(STUDYID = c("S1", "S1"), TESTVAR = c(NA, NA),
                   stringsAsFactors = FALSE)
  rpt <- validate_no_allna_reqexp(df, tm, "TEST", rpt)
  expect_true(any(rpt$findings$severity == "ERROR"))
  expect_true(any(grepl("TESTVAR", rpt$findings$message)))
})

test_that("validate_seq_integrity flags non-positive SEQ", {
  rpt <- new_validation_report(domain = "AE")
  df <- data.frame(AESEQ = c(1, 0, 3))
  rpt <- validate_seq_integrity(df, "AE", rpt)
  expect_true(any(rpt$findings$severity == "ERROR"))
})

test_that("validate_seq_integrity passes for valid SEQ", {
  rpt <- new_validation_report(domain = "AE")
  df <- data.frame(AESEQ = c(1, 2, 3))
  rpt <- validate_seq_integrity(df, "AE", rpt)
  expect_equal(nrow(rpt$findings), 0)
})

test_that("validate_no_duplicate_rows detects full duplicates", {
  rpt <- new_validation_report(domain = "TEST")
  df <- data.frame(A = c(1, 1, 2), B = c("x", "x", "y"),
                   stringsAsFactors = FALSE)
  rpt <- validate_no_duplicate_rows(df, "TEST", rpt)
  expect_true(any(rpt$findings$severity == "ERROR"))
})

test_that("validate_no_duplicate_rows passes for unique rows", {
  rpt <- new_validation_report(domain = "TEST")
  df <- data.frame(A = c(1, 2, 3), B = c("x", "y", "z"),
                   stringsAsFactors = FALSE)
  rpt <- validate_no_duplicate_rows(df, "TEST", rpt)
  expect_equal(nrow(rpt$findings), 0)
})

# ---------------------------------------------------------------------------
# Cross-domain validation
# ---------------------------------------------------------------------------
test_that("Cross-domain validation works with all 8 domains", {
  skip_if_no_starter_kit()
  rs <- compile_rules(dummy_meta, dummy_smeta, dummy_ct)

  dm_result <- build_domain("DM", dummy_meta, dummy_smeta, dummy_raw,
                            dummy_cfg, rs, verbose = FALSE)
  ae_result <- build_domain("AE", dummy_meta, dummy_smeta, dummy_raw,
                            dummy_cfg, rs, dm_data = dm_result$data,
                            verbose = FALSE)

  domain_data <- list(DM = dm_result$data, AE = ae_result$data)
  cross_rpt <- validate_cross_domain(domain_data, dummy_cfg)
  n_errors <- sum(cross_rpt$findings$severity == "ERROR")
  expect_equal(n_errors, 0)
})
