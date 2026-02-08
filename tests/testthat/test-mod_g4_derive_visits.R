# tests/testthat/test-mod_g4_derive_visits.R

test_that("derive_visit maps day to visit via visit_map", {
  df <- data.frame(USUBJID = "S1", DY = c(1, 15, 30), stringsAsFactors = FALSE)
  vmap <- data.frame(
    VISIT     = c("SCREENING", "WEEK 2", "WEEK 4"),
    VISITNUM  = c(1, 2, 3),
    START_DAY = c(1, 10, 25),
    END_DAY   = c(9, 24, 40),
    stringsAsFactors = FALSE
  )
  result <- derive_visit(df, "VISIT", visit_map = vmap, dy_var = "DY")
  expect_equal(result$VISIT, c("SCREENING", "WEEK 2", "WEEK 4"))
})

test_that("derive_visit returns NA when visit_map is NULL", {
  df <- data.frame(USUBJID = "S1", DY = 1, stringsAsFactors = FALSE)
  result <- derive_visit(df, "VISIT", visit_map = NULL)
  expect_true(is.na(result$VISIT[1]))
})

test_that("derive_visitnum looks up VISITNUM from visit_map", {
  df <- data.frame(VISIT = c("SCREENING", "WEEK 2"), stringsAsFactors = FALSE)
  vmap <- data.frame(
    VISIT    = c("SCREENING", "WEEK 2"),
    VISITNUM = c(1, 2),
    stringsAsFactors = FALSE
  )
  result <- derive_visitnum(df, "VISITNUM", visit_map = vmap)
  expect_equal(result$VISITNUM, c(1, 2))
})
