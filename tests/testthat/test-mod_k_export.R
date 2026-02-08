# tests/testthat/test-mod_k_export.R

test_that("export_xpt writes an XPT file", {
  skip_if_no_starter_kit()

  rs <- compile_rules(dummy_meta, dummy_smeta, dummy_ct)
  br <- build_domain("AE", dummy_meta, dummy_smeta, dummy_raw,
                     dummy_cfg, rs, verbose = FALSE)

  out_dir <- file.path(tempdir(), "xpt_test")
  fpath <- export_xpt(br$data, "AE", out_dir)
  expect_true(file.exists(fpath))
  expect_true(grepl("ae\\.xpt$", fpath))

  # Read back to verify
  read_back <- haven::read_xpt(fpath)
  expect_equal(nrow(read_back), nrow(br$data))
  unlink(out_dir, recursive = TRUE)
})

test_that("export_rds_csv writes both formats", {
  df <- data.frame(STUDYID = "S", USUBJID = "U1", stringsAsFactors = FALSE)
  out_dir <- file.path(tempdir(), "rds_csv_test")
  paths <- export_rds_csv(df, "DM", out_dir, formats = c("rds", "csv"))

  expect_true(file.exists(paths$rds))
  expect_true(file.exists(paths$csv))

  rds_back <- readRDS(paths$rds)
  expect_equal(nrow(rds_back), 1)
  unlink(out_dir, recursive = TRUE)
})

test_that("write_origin_support creates origin file", {
  tm <- data.frame(
    domain    = "AE",
    var       = c("STUDYID", "AETERM"),
    rule_type = c("constant", "direct_map"),
    stringsAsFactors = FALSE
  )
  out_path <- file.path(tempdir(), "origin_test.csv")
  write_origin_support(tm, out_path)
  expect_true(file.exists(out_path))
  origins <- readr::read_csv(out_path, show_col_types = FALSE)
  expect_true("origin" %in% names(origins))
  expect_equal(origins$origin[1], "Assigned")
  expect_equal(origins$origin[2], "CRF")
  unlink(out_path)
})
