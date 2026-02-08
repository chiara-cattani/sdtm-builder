# tests/testthat/test-mod_g1_derive_map.R

test_that("map_direct copies column with new name", {
  df <- data.frame(src = c("A", "B"), stringsAsFactors = FALSE)
  result <- map_direct(df, "TGT", "src")
  expect_true("TGT" %in% names(result))
  expect_equal(result$TGT, c("A", "B"))
})

test_that("map_direct applies toupper transform", {
  df <- data.frame(term = c("headache", "nausea"), stringsAsFactors = FALSE)
  result <- map_direct(df, "AETERM", "term", transform = toupper)
  expect_equal(result$AETERM, c("HEADACHE", "NAUSEA"))
})

test_that("derive_constant fills column", {
  df <- data.frame(x = 1:3)
  result <- derive_constant(df, "DOMAIN", value = "AE")
  expect_true(all(result$DOMAIN == "AE"))
})

test_that("derive_coalesce picks first non-NA", {
  df <- data.frame(a = c(NA, "x", NA), b = c("y", NA, NA),
                   c = c("z", "z", "z"), stringsAsFactors = FALSE)
  result <- derive_coalesce(df, "OUT", c("a", "b", "c"))
  expect_equal(result$OUT, c("y", "x", "z"))
})

test_that("derive_if_else applies binary condition", {
  df <- data.frame(val = c(1, NA, 3))
  result <- derive_if_else(df, "STATUS", "!is.na(val)", "DONE", "NOT DONE")
  expect_equal(result$STATUS, c("DONE", "NOT DONE", "DONE"))
})

test_that("derive_concat joins columns", {
  df <- data.frame(a = "hello", b = "world", stringsAsFactors = FALSE)
  result <- derive_concat(df, "FULL", c("a", "b"), sep = " ")
  expect_equal(result$FULL, "hello world")
})

test_that("derive_regex_extract captures pattern", {
  df <- data.frame(txt = c("Dose: 100mg", "Dose: 50mg"), stringsAsFactors = FALSE)
  result <- derive_regex_extract(df, "DOSE", "txt", "(\\d+)mg")
  expect_equal(result$DOSE, c("100", "50"))
})
