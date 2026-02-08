# tests/testthat/test-mod_i_validation.R

test_that("validate_domain_structure returns a validation_report", {
  skip_if_no_starter_kit()

  rs <- compile_rules(dummy_meta, dummy_smeta, dummy_ct)
  br <- build_domain("AE", dummy_meta, dummy_smeta, dummy_raw,
                     dummy_cfg, rs, verbose = FALSE)

  rpt <- validate_domain_structure(br$data, dummy_meta, "AE", dummy_cfg)
  expect_s3_class(rpt, "validation_report")
})

test_that("validate_required_vars flags missing required variable", {
  rpt <- new_validation_report(domain = "TEST")
  tm <- data.frame(domain = "TEST", var = "STUDYID", label = "Study",
                   type = "char", core = "REQ", rule_type = "constant",
                   stringsAsFactors = FALSE)
  df <- data.frame(X = 1)
  rpt <- validate_required_vars(df, tm, "TEST", rpt)
  expect_true(nrow(rpt$findings) >= 1)
  expect_true(any(rpt$findings$severity == "ERROR"))
})

test_that("validate_iso8601 flags non-ISO dates", {
  rpt <- new_validation_report(domain = "TEST")
  tm <- data.frame(domain = "TEST", var = "AESTDTC", label = "Start",
                   type = "char", core = "PERM", rule_type = "iso_dtc",
                   stringsAsFactors = FALSE)
  df <- data.frame(AESTDTC = c("2024-01-15", "03/15/2024"),
                   stringsAsFactors = FALSE)
  rpt <- validate_iso8601(df, tm, "TEST", rpt)
  expect_true(nrow(rpt$findings) >= 1)
})

test_that("validate_keys_unique detects duplicates", {
  rpt <- new_validation_report(domain = "TEST")
  tm <- data.frame(domain = "TEST", var = c("STUDYID", "USUBJID", "TESTSEQ"),
                   label = c("A", "B", "C"), type = c("char", "char", "num"),
                   core = c("REQ", "REQ", "REQ"),
                   rule_type = rep("constant", 3),
                   stringsAsFactors = FALSE)
  df <- data.frame(
    STUDYID = c("S", "S"), USUBJID = c("U1", "U1"), TESTSEQ = c(1, 1),
    stringsAsFactors = FALSE
  )
  rpt <- validate_keys_unique(df, tm, "TEST", rpt)
  expect_true(nrow(rpt$findings) >= 1)
})

test_that("summarize_validation_report works on empty report", {
  rpt <- new_validation_report(domain = "CLEAN")
  s <- summarize_validation_report(rpt, verbose = FALSE)
  expect_equal(nrow(s), 0)
})
