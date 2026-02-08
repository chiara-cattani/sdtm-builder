# tests/testthat/test-mod_d_dependency.R

test_that("build_dependency_graph returns order for AE domain", {
  skip_if_no_starter_kit()
  rs <- compile_rules(dummy_meta, dummy_smeta, dummy_ct)
  result <- build_dependency_graph(rs, "AE")
  expect_type(result, "list")
  expect_true("order" %in% names(result))
  expect_true(length(result$order) > 0)
})

test_that("build_dependency_graph respects AESTDY depends on AESTDTC", {
  skip_if_no_starter_kit()
  rs <- compile_rules(dummy_meta, dummy_smeta, dummy_ct)
  result <- build_dependency_graph(rs, "AE")
  idx_dtc <- which(result$order == "AESTDTC")
  idx_dy  <- which(result$order == "AESTDY")
  expect_true(idx_dtc < idx_dy)
})

test_that("detect_cycles returns empty for starter kit", {
  skip_if_no_starter_kit()
  rs <- compile_rules(dummy_meta, dummy_smeta, dummy_ct)
  result <- build_dependency_graph(rs, "AE")
  expect_length(result$cycles, 0)
})

test_that("plan_build_steps returns tibble", {
  skip_if_no_starter_kit()
  rs <- compile_rules(dummy_meta, dummy_smeta, dummy_ct)
  dep <- build_dependency_graph(rs, "AE")
  plan <- plan_build_steps("AE", dep, rs, dummy_smeta)
  expect_s3_class(plan, "tbl_df")
})
