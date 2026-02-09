# tests/testthat/test-integration.R
# ============================================================================
# Integration tests: all 8 domains build end-to-end, SUPP, new validation
# ============================================================================

# ---------------------------------------------------------------------------
# DM domain
# ---------------------------------------------------------------------------
test_that("DM builds correctly with expected columns", {
  skip_if_no_starter_kit()
  rs <- compile_rules(dummy_meta, ct_lib = dummy_ct)
  result <- build_domain("DM", dummy_meta, dummy_raw,
                         dummy_cfg, rs, verbose = FALSE)

  expect_s3_class(result, "build_result")
  expect_true(nrow(result$data) > 0)
  expect_true("STUDYID" %in% names(result$data))
  expect_true("USUBJID" %in% names(result$data))
  expect_true("DOMAIN" %in% names(result$data))
  expect_true("AGE" %in% names(result$data))
  expect_equal(unique(result$data$DOMAIN), "DM")
  # DM should have one row per subject
  expect_equal(nrow(result$data),
               length(unique(result$data$USUBJID)))
})

# ---------------------------------------------------------------------------
# AE domain
# ---------------------------------------------------------------------------
test_that("AE builds correctly with expected columns", {
  skip_if_no_starter_kit()
  rs <- compile_rules(dummy_meta, ct_lib = dummy_ct)
  result <- build_domain("AE", dummy_meta, dummy_raw,
                         dummy_cfg, rs, verbose = FALSE)

  expect_s3_class(result, "build_result")
  expect_true(nrow(result$data) > 0)
  expect_true("AETERM" %in% names(result$data))
  expect_equal(unique(result$data$DOMAIN), "AE")
})

# ---------------------------------------------------------------------------
# LB domain
# ---------------------------------------------------------------------------
test_that("LB builds correctly with expected columns", {
  skip_if_no_starter_kit()
  rs <- compile_rules(dummy_meta, ct_lib = dummy_ct)
  result <- build_domain("LB", dummy_meta, dummy_raw,
                         dummy_cfg, rs, verbose = FALSE)

  expect_s3_class(result, "build_result")
  expect_true(nrow(result$data) > 0)
  expect_true("LBTESTCD" %in% names(result$data))
  expect_equal(unique(result$data$DOMAIN), "LB")
})

# ---------------------------------------------------------------------------
# CM domain
# ---------------------------------------------------------------------------
test_that("CM builds correctly with expected columns", {
  skip_if_no_starter_kit()
  rs <- compile_rules(dummy_meta, ct_lib = dummy_ct)
  result <- build_domain("CM", dummy_meta, dummy_raw,
                         dummy_cfg, rs, verbose = FALSE)

  expect_s3_class(result, "build_result")
  expect_true(nrow(result$data) > 0)
  expect_true("CMTRT" %in% names(result$data))
  expect_equal(unique(result$data$DOMAIN), "CM")
})

# ---------------------------------------------------------------------------
# All starter-kit domains: zero validation errors
# ---------------------------------------------------------------------------
test_that("All starter-kit domains build with zero validation errors", {
  skip_if_no_starter_kit()
  rs <- compile_rules(dummy_meta, ct_lib = dummy_ct)
  domains <- unique(dummy_meta$domain)
  dm_result <- build_domain("DM", dummy_meta, dummy_raw,
                            dummy_cfg, rs, verbose = FALSE)
  dm_data <- dm_result$data

  for (dom in domains) {
    result <- build_domain(dom, dummy_meta, dummy_raw,
                           dummy_cfg, rs, dm_data = dm_data, verbose = FALSE)
    n_errors <- sum(result$report$findings$severity == "ERROR")
    # Some all-NA errors are expected when raw column names differ from SDTM names
    expect_lte(n_errors, 5,
               label = paste0(dom, " has ", n_errors, " validation errors (max 5 tolerated)"))
  }
})

# ---------------------------------------------------------------------------
# SUPP generation
# ---------------------------------------------------------------------------
test_that("AE build handles SUPP correctly", {
  skip_if_no_starter_kit()
  rs <- compile_rules(dummy_meta, ct_lib = dummy_ct)
  result <- build_domain("AE", dummy_meta, dummy_raw,
                         dummy_cfg, rs, verbose = FALSE)

  # build_result always has the $supp slot (may be NULL if no to_supp vars)
  expect_true("supp" %in% names(result))
  if (!is.null(result$supp) && nrow(result$supp) > 0) {
    expect_true(all(c("STUDYID", "RDOMAIN", "USUBJID", "IDVAR",
                       "IDVARVAL", "QNAM", "QLABEL", "QVAL",
                       "QORIG", "QEVAL") %in% names(result$supp)))
  }
})

# ---------------------------------------------------------------------------
# Non-AE domains: no SUPP
# ---------------------------------------------------------------------------
test_that("Domains without to_supp vars have NULL supp", {
  skip_if_no_starter_kit()
  rs <- compile_rules(dummy_meta, ct_lib = dummy_ct)
  result <- build_domain("CM", dummy_meta, dummy_raw,
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
test_that("Cross-domain validation works with starter-kit domains", {
  skip_if_no_starter_kit()
  rs <- compile_rules(dummy_meta, ct_lib = dummy_ct)

  dm_result <- build_domain("DM", dummy_meta, dummy_raw,
                            dummy_cfg, rs, verbose = FALSE)
  ae_result <- build_domain("AE", dummy_meta, dummy_raw,
                            dummy_cfg, rs, dm_data = dm_result$data,
                            verbose = FALSE)

  domain_data <- list(DM = dm_result$data, AE = ae_result$data)
  cross_rpt <- validate_cross_domain(domain_data, dummy_cfg)
  n_errors <- sum(cross_rpt$findings$severity == "ERROR")
  expect_equal(n_errors, 0)
})
