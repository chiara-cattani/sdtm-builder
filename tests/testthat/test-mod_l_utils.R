# tests/testthat/test-mod_l_utils.R

test_that("log_info emits message without error", {
  expect_no_error(log_info("test message"))
})

test_that("log_warn emits warning-level message", {
  expect_no_error(log_warn("test warning"))
})

test_that("log_error emits error-level message", {
  expect_no_error(log_error("test error"))
})

test_that("assert_cols passes when all columns present", {
  df <- data.frame(A = 1, B = 2, C = 3)
  expect_true(assert_cols(df, c("A", "B")))
})

test_that("assert_cols errors on missing columns", {
  df <- data.frame(A = 1, B = 2)
  expect_error(assert_cols(df, c("A", "X", "Y")), "Missing required column")
})

test_that("assert_types passes with correct types", {
  df <- data.frame(x = "a", y = 1.5, stringsAsFactors = FALSE)
  expect_true(assert_types(df, c(x = "character", y = "numeric")))
})

test_that("assert_types errors on type mismatch", {
  df <- data.frame(x = 1:3)
  expect_error(assert_types(df, c(x = "character")), "Type assertion failed")
})

test_that("stop_with_context throws classed error", {
  expect_error(
    stop_with_context("boom", domain = "AE", variable = "AETERM"),
    class = "sdtmbuilder_error"
  )
})

test_that("warn_with_context throws classed warning", {
  expect_warning(
    warn_with_context("oops", domain = "AE", variable = "AETERM"),
    class = "sdtmbuilder_warning"
  )
})

test_that("snapshot_dataset writes an RDS file", {
  df <- data.frame(x = 1:5)
  out_dir <- file.path(tempdir(), "snap_test")
  fpath <- snapshot_dataset(df, "test_snap", outdir = out_dir, domain = "AE")
  expect_true(file.exists(fpath))
  snap <- readRDS(fpath)
  expect_equal(snap$label, "test_snap")
  expect_equal(snap$nrow, 5)
  unlink(out_dir, recursive = TRUE)
})
