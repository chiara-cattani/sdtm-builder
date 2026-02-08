# tests/testthat/test-mod_g3_derive_dates.R

test_that("parse_partial_date parses YYYY-MM-DD", {
  res <- parse_partial_date(c("2024-03-15", "2023-01-01"))
  expect_equal(res$year,  c(2024L, 2023L))
  expect_equal(res$month, c(3L, 1L))
  expect_equal(res$day,   c(15L, 1L))
  expect_equal(res$precision, c("YMD", "YMD"))
  expect_true(all(res$valid))
})

test_that("parse_partial_date handles partial dates", {
  res <- parse_partial_date(c("2024-03", "2024"))
  expect_equal(res$precision, c("YM", "Y"))
  expect_true(is.na(res$day[1]))
  expect_true(is.na(res$month[2]))
})

test_that("parse_partial_date returns valid=FALSE for unparseable", {
  res <- parse_partial_date("not-a-date")
  expect_false(res$valid[1])
})

test_that("parse_partial_date handles NA", {
  res <- parse_partial_date(NA_character_)
  expect_true(is.na(res$valid[1]))
})

test_that("format_iso_dtc returns ISO strings", {
  parsed <- parse_partial_date(c("2024-03-15", "2024-03", "2024"))
  iso <- format_iso_dtc(parsed)
  expect_equal(iso, c("2024-03-15", "2024-03", "2024"))
})

test_that("format_iso_dtc accepts character input", {
  iso <- format_iso_dtc(c("2024-03-15", "2024-06"))
  expect_equal(iso, c("2024-03-15", "2024-06"))
})

test_that("derive_dy computes study day with no Day 0", {
  df <- data.frame(
    AESTDTC  = c("2024-01-10", "2024-01-01", "2023-12-30"),
    RFSTDTC  = c("2024-01-01", "2024-01-01", "2024-01-01"),
    stringsAsFactors = FALSE
  )
  result <- derive_dy(df, "AESTDY", "AESTDTC", "RFSTDTC")
  # Day 10: diff=9, >=0 -> 9+1=10
  expect_equal(result$AESTDY[1], 10)
  # Day 1: diff=0, >=0 -> 0+1=1
  expect_equal(result$AESTDY[2], 1)
  # Before: diff=-2, <0 -> -2
  expect_equal(result$AESTDY[3], -2)
})

test_that("derive_duration computes P{n}D", {
  df <- data.frame(
    AESTDTC = "2024-01-01", AEENDTC = "2024-01-11",
    stringsAsFactors = FALSE
  )
  result <- derive_duration(df, "ADURN", "AESTDTC", "AEENDTC")
  expect_equal(result$ADURN, "P10D")
})

test_that("apply_imputation_policy imputes day=1", {
  parsed <- parse_partial_date("2024-03")
  imputed <- apply_imputation_policy(parsed, policy = list(day = "first"))
  expect_equal(imputed$day[1], 1L)
  expect_equal(imputed$precision[1], "YMD")
})

test_that("combine_date_time adds time info", {
  parsed <- parse_partial_date("2024-03-15")
  result <- combine_date_time(parsed, "14:30:00")
  expect_equal(result$hour[1], 14L)
  expect_equal(result$minute[1], 30L)
  expect_equal(result$second[1], 0L)
})
