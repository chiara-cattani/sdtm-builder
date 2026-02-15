# tests/testthat/test-mod_e_data_access.R

test_that("load_raw_datasets reads CSV from directory", {
  tmpdir <- tempfile("test_raw_")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  write.csv(data.frame(a = 1:3, b = letters[1:3]),
            file.path(tmpdir, "test.csv"), row.names = FALSE)
  result <- load_raw_datasets(tmpdir, verbose = FALSE)
  expect_true("test" %in% names(result))
  expect_equal(nrow(result$test), 3)
})

test_that("standardize_names lowercases", {
  df <- data.frame(MyVar = 1, AnotherVar = 2)
  result <- standardize_names(df)
  expect_true(all(names(result) == tolower(names(result))))
})

test_that("standardize_types strips factors", {
  df <- data.frame(x = factor(c("a", "b")))
  result <- standardize_types(df)
  expect_true(is.character(result$x))
})

test_that("apply_missing_conventions maps blanks to NA", {
  df <- data.frame(x = c("", "  ", "val"), stringsAsFactors = FALSE)
  result <- apply_missing_conventions(df, blank_to_na = TRUE)
  expect_true(is.na(result$x[1]))
  expect_equal(result$x[3], "val")
})

test_that("derive_core_keys adds STUDYID", {
  skip_if_no_starter_kit()
  df <- data.frame(subjid = c("001", "002"), stringsAsFactors = FALSE)
  result <- derive_core_keys(df, dummy_cfg)
  expect_true("STUDYID" %in% names(result))
  expect_true(all(result$STUDYID == dummy_cfg$studyid))
})
