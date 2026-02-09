# tests/testthat/test-mod_d_dependency.R

test_that("build_dependency_graph returns order for AE domain", {
  skip_if_no_starter_kit()
  rs <- compile_rules(dummy_meta, ct_lib = dummy_ct)
  result <- build_dependency_graph(rs, "AE")
  expect_type(result, "list")
  expect_true("order" %in% names(result))
  expect_true(length(result$order) > 0)
})

test_that("build_dependency_graph respects SEQ depends on other variables", {
  skip_if_no_starter_kit()
  rs <- compile_rules(dummy_meta, ct_lib = dummy_ct)
  result <- build_dependency_graph(rs, "AE")
  # AESEQ should appear in the order (it's a SEQ derivation)
  expect_true("AESEQ" %in% result$order)
})

test_that("detect_cycles returns empty for starter kit", {
  skip_if_no_starter_kit()
  rs <- compile_rules(dummy_meta, ct_lib = dummy_ct)
  result <- build_dependency_graph(rs, "AE")
  expect_length(result$cycles, 0)
})

test_that("plan_build_steps returns tibble", {
  skip_if_no_starter_kit()
  rs <- compile_rules(dummy_meta, ct_lib = dummy_ct)
  dep <- build_dependency_graph(rs, "AE")
  plan <- plan_build_steps("AE", dep, rs)
  expect_s3_class(plan, "tbl_df")
})
