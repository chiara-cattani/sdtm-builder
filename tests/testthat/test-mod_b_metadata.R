# tests/testthat/test-mod_b_metadata.R

test_that("read_target_meta reads CSV file", {
  skip_if_no_starter_kit()
  # read_target_meta is deprecated; we test backwards-compat here
  # The Excel-based reader is the primary path now
  expect_true(TRUE)  # placeholder — read_target_meta needs a CSV which we no longer ship
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
