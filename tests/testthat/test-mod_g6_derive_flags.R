# tests/testthat/test-mod_g6_derive_flags.R

test_that("derive_baseline_flag sets Y for BASELINE visit", {
  df <- data.frame(
    USUBJID = "S1",
    VISIT   = c("SCREENING", "BASELINE", "WEEK 4"),
    stringsAsFactors = FALSE
  )
  result <- derive_baseline_flag(df, "BLFL", baseline_visit = "BASELINE")
  expect_equal(result$BLFL, c(NA_character_, "Y", NA_character_))
})

test_that("derive_lastobs_flag sets Y for last row per subject", {
  df <- data.frame(
    USUBJID = c("S1", "S1", "S1", "S2", "S2"),
    VAL     = 1:5,
    stringsAsFactors = FALSE
  )
  result <- derive_lastobs_flag(df, "LOCF", by = "USUBJID", order_var = "VAL")
  expect_equal(result$LOCF, c(NA, NA, "Y", NA, "Y"))
})

test_that("derive_occurrence sets Y for non-NA source", {
  df <- data.frame(src = c("yes", NA, "no"), stringsAsFactors = FALSE)
  result <- derive_occurrence(df, "AEOCCUR", source_var = "src")
  expect_equal(result$AEOCCUR, c("Y", NA_character_, "Y"))
})

test_that("derive_status marks NOT DONE for NA results", {
  df <- data.frame(RESULT = c("ok", NA, "ok"), stringsAsFactors = FALSE)
  result <- derive_status(df, "STAT", "RESULT")
  expect_equal(result$STAT, c(NA_character_, "NOT DONE", NA_character_))
})

test_that("derive_reason copies source when present", {
  df <- data.frame(rsn = c(NA, "adverse event"), stringsAsFactors = FALSE)
  result <- derive_reason(df, "REASND", source_var = "rsn")
  expect_equal(result$REASND, c(NA_character_, "adverse event"))
})
