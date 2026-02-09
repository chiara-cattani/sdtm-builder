# tests/testthat/test-mod_h_builders.R

test_that("build_domain returns a build_result for AE", {
  skip_if_no_starter_kit()

  rs   <- compile_rules(dummy_meta, ct_lib = dummy_ct)
  result <- build_domain("AE", dummy_meta, dummy_raw,
                         dummy_cfg, rs, verbose = FALSE)

  expect_s3_class(result, "build_result")
  expect_true("data" %in% names(result))
  expect_true("report" %in% names(result))
  expect_true(nrow(result$data) > 0)
})

test_that("build_domain produces correct columns for AE", {
  skip_if_no_starter_kit()

  rs   <- compile_rules(dummy_meta, ct_lib = dummy_ct)
  result <- build_domain("AE", dummy_meta, dummy_raw,
                         dummy_cfg, rs, verbose = FALSE)

  ae_vars <- dplyr::filter(dummy_meta, domain == "AE")$var
  expect_true(all(names(result$data) %in% ae_vars))
})

test_that("derive_variable dispatches constant rule", {
  df <- data.frame(x = 1:3)
  rule <- list(type = "constant", params = list(value = "AE"))
  out <- derive_variable(df, "DOMAIN", rule, context = list())
  expect_equal(out$DOMAIN, rep("AE", 3))
})

test_that("derive_variable dispatches direct_map rule", {
  df <- data.frame(src_col = c("a", "b"), stringsAsFactors = FALSE)
  rule <- list(type = "direct_map",
               params = list(column = "src_col", dataset = "d", transform = "toupper"))
  out <- derive_variable(df, "VAR", rule, context = list())
  expect_equal(out$VAR, c("A", "B"))
})

test_that("finalize_domain selects only target variables", {
  skip_if_no_starter_kit()

  df <- data.frame(STUDYID = "X", DOMAIN = "AE", AETERM = "H",
                   EXTRA_COL = 1, stringsAsFactors = FALSE)
  ae_meta <- dplyr::filter(dummy_meta, domain == "AE")
  fin <- finalize_domain(df, "AE", ae_meta, dummy_cfg,
                         create_seq = FALSE, create_supp = FALSE)
  # EXTRA_COL should not be in output

  expect_false("EXTRA_COL" %in% names(fin$data))
})
