# tests/testthat/test-mod_g2_derive_ct.R

test_that("assign_ct maps raw values to coded terms", {
  df <- data.frame(raw_sev = c("mild", "MODERATE", "severe"), stringsAsFactors = FALSE)
  ct <- tibble::tibble(
    codelist_id = rep("C66769", 3),
    input_value = c("mild", "MODERATE", "severe"),
    coded_value = c("MILD", "MODERATE", "SEVERE")
  )
  result <- assign_ct(df, "AESEV", "raw_sev", "C66769", ct)
  expect_equal(result$AESEV, c("MILD", "MODERATE", "SEVERE"))
})

test_that("assign_ct warns on unmapped values", {
  df <- data.frame(raw = c("UNKNOWN_VAL"), stringsAsFactors = FALSE)
  ct <- tibble::tibble(
    codelist_id = "CL1", input_value = "a", coded_value = "A"
  )
  expect_warning(
    assign_ct(df, "OUT", "raw", "CL1", ct, unknown_policy = "warn_and_keep")
  )
})

test_that("map_yes_no converts logical to Y/N", {
  df <- data.frame(flag = c(TRUE, FALSE, NA))
  result <- map_yes_no(df, "AESER", "flag")
  expect_equal(result$AESER, c("Y", "N", NA_character_))
})

test_that("validate_ct_values checks conformance", {
  df <- data.frame(AESEV = c("MILD", "SEVERE", "EXTREME"), stringsAsFactors = FALSE)
  ct <- tibble::tibble(
    codelist_id = rep("C66769", 3),
    coded_value = c("MILD", "MODERATE", "SEVERE")
  )
  findings <- validate_ct_values(df, "AESEV", "C66769", ct)
  expect_true(nrow(findings) >= 1)
})
