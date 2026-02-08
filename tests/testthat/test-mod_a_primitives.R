# tests/testthat/test-mod_a_primitives.R

test_that("new_sdtm_config creates valid config with minimal args", {
  cfg <- new_sdtm_config(studyid = "STUDY01",
                          ref_start_rule = list(var = "RFSTDTC", source = "DM"))
  expect_s3_class(cfg, "sdtm_config")
  expect_equal(cfg$studyid, "STUDY01")
  expect_equal(cfg$timezone, "UTC")
  expect_equal(cfg$log_level, "INFO")
})

test_that("new_sdtm_config rejects missing studyid", {
  expect_error(
    new_sdtm_config(studyid = NULL, ref_start_rule = list(var = "X", source = "Y"))
  )
})

test_that("new_sdtm_config rejects bad log_level", {
  expect_error(
    new_sdtm_config(studyid = "S", ref_start_rule = list(var = "X", source = "Y"),
                    log_level = "TRACE")
  )
})

test_that("new_sdtm_config captures sponsor_overrides", {
  cfg <- new_sdtm_config(studyid = "S",
                          ref_start_rule = list(var = "X", source = "Y"),
                          sponsor_overrides = list(ae_max_length = 500))
  expect_equal(cfg$sponsor_overrides$ae_max_length, 500)
})

test_that("new_meta_bundle assembles metadata frames", {
  mb <- new_meta_bundle(
    target_meta = tibble::tibble(domain = "DM", var = "STUDYID"),
    source_meta = tibble::tibble(dataset = "dm_raw", column = "study")
  )
  expect_s3_class(mb, "meta_bundle")
})

test_that("new_rule_set wraps compiled rules", {
  rs <- new_rule_set(rules = list(AE = list(STUDYID = list(type = "constant"))))
  expect_s3_class(rs, "rule_set")
})

test_that("new_validation_report starts empty", {
  vr <- new_validation_report(domain = "AE")
  expect_s3_class(vr, "validation_report")
  expect_equal(nrow(vr$findings), 0)
})

test_that("add_finding appends rows", {
  vr <- new_validation_report(domain = "AE")
  vr <- add_finding(vr, rule_id = "test_check", severity = "ERROR", message = "bad")
  expect_equal(nrow(vr$findings), 1)
  expect_equal(vr$findings$severity[1], "ERROR")
})

test_that("print.sdtm_config runs without error", {
  cfg <- new_sdtm_config(studyid = "S", ref_start_rule = list(var = "X", source = "Y"))
  expect_no_error(print(cfg))
})

test_that("print.validation_report runs without error", {
  vr <- new_validation_report(domain = "AE")
  vr <- add_finding(vr, rule_id = "c1", severity = "WARNING", message = "m1")
  expect_no_error(print(vr))
})
