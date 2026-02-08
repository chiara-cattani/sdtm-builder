# tests/testthat/test-make_dummy_study.R
# Unit tests for the deterministic dummy data generator

test_that("make_dummy_study returns expected structure", {
  skip_if_no_starter_kit()
  study <- make_dummy_study(seed = 123, starter_kit_dir = starter_kit_path)
  expect_type(study, "list")
  expect_named(study, c("raw_data", "target_meta", "source_meta", "ct_lib", "config"))
})

test_that("raw_data contains all expected datasets", {
  skip_if_no_starter_kit()
  study <- make_dummy_study(seed = 123, starter_kit_dir = starter_kit_path)
  expect_named(study$raw_data,
               c("dm_raw", "ae_raw", "cm_raw", "mh_raw", "pr_raw",
                 "ex_raw", "vs_raw", "lb_raw"))
})

test_that("dm_raw has one row per subject", {
  skip_if_no_starter_kit()
  dm <- dummy_raw$dm_raw
  expect_equal(nrow(dm), 30)
  expect_equal(length(unique(dm$usubjid)), 30)
})

test_that("dm_raw has required columns", {
  skip_if_no_starter_kit()
  dm <- dummy_raw$dm_raw
  expected_cols <- c("usubjid", "subjid", "siteid", "rfstdtc", "rfendtc",
                     "age", "sex", "race", "armcd", "arm")
  expect_true(all(expected_cols %in% names(dm)))
})

test_that("ae_raw has plausible row count", {
  skip_if_no_starter_kit()
  ae <- dummy_raw$ae_raw
  # 30 subjects × Poisson(2.5) ≈ 75, expect between 40 and 130

  expect_gte(nrow(ae), 40)
  expect_lte(nrow(ae), 130)
})

test_that("ae_raw contains expected columns", {
  skip_if_no_starter_kit()
  ae <- dummy_raw$ae_raw
  expected_cols <- c("usubjid", "aeid", "aeterm", "aedecod", "aesev_raw",
                     "aeser_raw", "aerel_raw", "aestdat", "aestim",
                     "aeendat", "aeentim")
  expect_true(all(expected_cols %in% names(ae)))
})

test_that("cm_raw has plausible row count", {
  skip_if_no_starter_kit()
  cm <- dummy_raw$cm_raw
  expect_gte(nrow(cm), 20)
  expect_lte(nrow(cm), 100)
})

test_that("mh_raw contains partial dates", {
  skip_if_no_starter_kit()
  mh <- dummy_raw$mh_raw
  # Some dates should be year-only (4 chars) or year-month (7 chars)
  lens <- nchar(mh$mhstdat)
  expect_true(any(lens == 4))   # year-only
  expect_true(any(lens == 7))   # year-month
  expect_true(any(lens == 10))  # full date
})

test_that("pr_raw has plausible row count", {
  skip_if_no_starter_kit()
  pr <- dummy_raw$pr_raw
  expect_gte(nrow(pr), 20)
  expect_lte(nrow(pr), 100)
})

test_that("output is deterministic (same seed → same data)", {
  skip_if_no_starter_kit()
  s1 <- make_dummy_study(seed = 42, starter_kit_dir = starter_kit_path)
  s2 <- make_dummy_study(seed = 42, starter_kit_dir = starter_kit_path)
  expect_identical(s1$raw_data$dm_raw, s2$raw_data$dm_raw)
  expect_identical(s1$raw_data$ae_raw, s2$raw_data$ae_raw)
})

test_that("different seeds produce different data", {
  skip_if_no_starter_kit()
  s1 <- make_dummy_study(seed = 1, starter_kit_dir = starter_kit_path)
  s2 <- make_dummy_study(seed = 2, starter_kit_dir = starter_kit_path)
  expect_false(identical(s1$raw_data$dm_raw$rfstdtc, s2$raw_data$dm_raw$rfstdtc))
})

test_that("target_meta loads correctly", {
  skip_if_no_starter_kit()
  expect_s3_class(dummy_meta, "tbl_df")
  expect_true(all(c("domain", "var", "type", "rule_type", "rule_params")
                  %in% names(dummy_meta)))
  domains <- unique(dummy_meta$domain)
  expect_true(all(c("AE", "CM", "MH", "PR", "DM", "EX", "VS", "LB") %in% domains))
})

test_that("ct_lib loads correctly", {
  skip_if_no_starter_kit()
  expect_s3_class(dummy_ct, "tbl_df")
  expect_true("codelist_id" %in% names(dummy_ct))
  expect_true("C66769" %in% dummy_ct$codelist_id)
})

test_that("config is a valid sdtm_config", {
  skip_if_no_starter_kit()
  expect_s3_class(dummy_cfg, "sdtm_config")
  expect_equal(dummy_cfg$studyid, "STUDY-XYZ")
})

# ---------------------------------------------------------------------------
# Bad-case injection tests
# ---------------------------------------------------------------------------

test_that("bad_case='dup_dm_key' duplicates a DM row", {
  skip_if_no_starter_kit()
  study <- make_dummy_study(seed = 123, bad_case = "dup_dm_key",
                            starter_kit_dir = starter_kit_path)
  dm <- study$raw_data$dm_raw
  expect_equal(nrow(dm), 31)  # 30 + 1 duplicate
  expect_true(any(duplicated(dm$usubjid)))
})

test_that("bad_case='bad_ct_value' injects unmapped severity", {
  skip_if_no_starter_kit()
  study <- make_dummy_study(seed = 123, bad_case = "bad_ct_value",
                            starter_kit_dir = starter_kit_path)
  ae <- study$raw_data$ae_raw
  expect_true("TERRIBLE" %in% ae$aesev_raw)
})

test_that("bad_case='invalid_date' injects bad date", {
  skip_if_no_starter_kit()
  study <- make_dummy_study(seed = 123, bad_case = "invalid_date",
                            starter_kit_dir = starter_kit_path)
  ae <- study$raw_data$ae_raw
  expect_true("2025-13-01" %in% ae$aestdat)
})

test_that("bad_case='missing_req_var' drops aeterm", {
  skip_if_no_starter_kit()
  study <- make_dummy_study(seed = 123, bad_case = "missing_req_var",
                            starter_kit_dir = starter_kit_path)
  ae <- study$raw_data$ae_raw
  expect_false("aeterm" %in% names(ae))
})

# ---------------------------------------------------------------------------
# New domain dataset tests
# ---------------------------------------------------------------------------

test_that("ex_raw has plausible row count and columns", {
  skip_if_no_starter_kit()
  ex <- dummy_raw$ex_raw
  # 30 subjects × 4-8 records each ≈ 120-240
  expect_gte(nrow(ex), 100)
  expect_lte(nrow(ex), 300)
  expected_cols <- c("usubjid", "exid", "extrt", "exdose", "exdosu_raw",
                     "exdosfrq_raw", "exroute_raw", "exstdat", "exendat")
  expect_true(all(expected_cols %in% names(ex)))
})

test_that("vs_raw has expected row count (30 subj x 5 visits x 5 tests)", {
  skip_if_no_starter_kit()
  vs <- dummy_raw$vs_raw
  expect_equal(nrow(vs), 750)
  expected_cols <- c("usubjid", "vstestcd", "vstest", "vsorres", "vsorresu",
                     "vsdat", "visit", "visitnum", "vsblfl")
  expect_true(all(expected_cols %in% names(vs)))
  # Baseline flag present for visit 2
  expect_true(any(vs$vsblfl == "Y", na.rm = TRUE))
})

test_that("lb_raw has expected row count (30 subj x 4 visits x 5 tests)", {
  skip_if_no_starter_kit()
  lb <- dummy_raw$lb_raw
  expect_equal(nrow(lb), 600)
  expected_cols <- c("usubjid", "lbtestcd", "lbtest", "lborres", "lborresu",
                     "lbdat", "visit", "visitnum")
  expect_true(all(expected_cols %in% names(lb)))
})

test_that("target_meta includes all 8 domains", {
  skip_if_no_starter_kit()
  domains <- unique(dummy_meta$domain)
  expect_true(all(c("AE", "CM", "MH", "PR", "DM", "EX", "VS", "LB") %in% domains))
})
