# tests/testthat/test-mod_b_metadata.R

test_that("read_target_meta reads CSV file", {
  skip_if_no_starter_kit()
  path <- file.path(starter_kit_path, "target_meta.csv")
  result <- read_target_meta(path)
  expect_s3_class(result, "tbl_df")
  expect_true("domain" %in% names(result))
  expect_true(nrow(result) > 0)
})

test_that("read_source_meta reads CSV file", {
  skip_if_no_starter_kit()
  path <- file.path(starter_kit_path, "source_meta.csv")
  result <- read_source_meta(path)
  expect_s3_class(result, "tbl_df")
  expect_true("dataset" %in% names(result))
})

test_that("read_ct_library reads CSV CT", {
  skip_if_no_starter_kit()
  path <- file.path(starter_kit_path, "ct_codelist.csv")
  result <- read_ct_library(path)
  expect_s3_class(result, "tbl_df")
  expect_true("codelist_id" %in% names(result))
})

test_that("validate_target_meta passes with starter kit", {
  skip_if_no_starter_kit()
  expect_silent(validate_target_meta(dummy_meta))
})

test_that("normalize_target_meta standardizes types", {
  skip_if_no_starter_kit()
  result <- normalize_target_meta(dummy_meta)
  expect_true(all(result$type %in% c("char", "num", NA)))
})

test_that("resolve_domain_model filters target_meta to one domain", {
  skip_if_no_starter_kit()
  result <- resolve_domain_model(dummy_meta, domain = "AE")
  expect_true(all(result$domain == "AE"))
})
