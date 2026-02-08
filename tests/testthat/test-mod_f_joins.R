# tests/testthat/test-mod_f_joins.R

test_that("safe_join performs left join", {
  left  <- data.frame(k = c("001", "002"), val = c(1, 2), stringsAsFactors = FALSE)
  right <- data.frame(k = c("001", "003"), info = c("a", "b"), stringsAsFactors = FALSE)
  result <- safe_join(left, right, by = "k", type = "left")
  expect_equal(nrow(result), 2)
  expect_true(is.na(result$info[2]))
})

test_that("safe_join checks cardinality", {
  left  <- data.frame(k = c(1, 1), a = c("x", "y"))
  right <- data.frame(k = c(1, 1), b = c("m", "n"))
  expect_error(
    safe_join(left, right, by = "k", type = "left", cardinality = "1:1")
  )
})

test_that("deduplicate_by_rule keeps first", {
  df <- data.frame(k = c("A", "A", "B"), val = c(1, 2, 3), stringsAsFactors = FALSE)
  result <- deduplicate_by_rule(df, keys = "k", strategy = "first")
  expect_equal(nrow(result), 2)
})

test_that("bind_sources stacks compatible data frames", {
  df1 <- data.frame(a = 1, stringsAsFactors = FALSE)
  df2 <- data.frame(a = 2, stringsAsFactors = FALSE)
  result <- bind_sources(list(df1, df2))
  expect_equal(nrow(result), 2)
})

test_that("split_records expands delimited fields", {
  df <- data.frame(k = "001", items = "A;B;C", stringsAsFactors = FALSE)
  result <- split_records(df, col = "items", sep = ";")
  expect_equal(nrow(result), 3)
})
