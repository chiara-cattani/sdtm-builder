# tests/testthat/test-mod_g5_derive_ids.R

test_that("derive_usubjid creates STUDYID-SUBJID", {
  df <- data.frame(subjid = c("001", "002"), stringsAsFactors = FALSE)
  result <- derive_usubjid(df, "STUDY-XYZ", "subjid")
  expect_equal(result$USUBJID, c("STUDY-XYZ-001", "STUDY-XYZ-002"))
})

test_that("derive_usubjid keeps existing USUBJID", {
  df <- data.frame(USUBJID = "X-001", subjid = "001", stringsAsFactors = FALSE)
  result <- derive_usubjid(df, "X", "subjid", validate_existing = TRUE)
  expect_equal(result$USUBJID, "X-001")
})

test_that("derive_seq assigns sequential integers", {
  df <- data.frame(
    USUBJID = c("S1", "S1", "S1", "S2", "S2"),
    AETERM  = c("a", "b", "c", "d", "e"),
    stringsAsFactors = FALSE
  )
  result <- derive_seq(df, "AESEQ", by = "USUBJID", order_by = "AETERM")
  expect_equal(result$AESEQ[result$USUBJID == "S1"], c(1L, 2L, 3L))
  expect_equal(result$AESEQ[result$USUBJID == "S2"], c(1L, 2L))
})

test_that("derive_domain_keys sets STUDYID and DOMAIN", {
  df <- data.frame(x = 1:3)
  cfg <- new_sdtm_config(studyid = "TEST",
                         ref_start_rule = list(var = "RFSTDTC", source = "DM"))
  result <- derive_domain_keys(df, "AE", cfg)
  expect_equal(result$STUDYID, rep("TEST", 3))
  expect_equal(result$DOMAIN,  rep("AE", 3))
})

test_that("derive_spid concatenates columns", {
  df <- data.frame(a = "X", b = "Y", stringsAsFactors = FALSE)
  result <- derive_spid(df, "SPID", c("a", "b"), sep = "-")
  expect_equal(result$SPID, "X-Y")
})

test_that("derive_grpid assigns group IDs", {
  df <- data.frame(grp = c("A", "B", "A", "B"), stringsAsFactors = FALSE)
  result <- derive_grpid(df, "GID", group_by = "grp")
  # Group IDs should be consistent within groups

  expect_equal(result$GID[1], result$GID[3])
  expect_equal(result$GID[2], result$GID[4])
  expect_true(result$GID[1] != result$GID[2])
})
