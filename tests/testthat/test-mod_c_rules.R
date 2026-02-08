# tests/testthat/test-mod_c_rules.R

test_that("compile_rules parses JSON rules from starter kit", {
  skip_if_no_starter_kit()
  rs <- compile_rules(dummy_meta, dummy_smeta, dummy_ct)
  expect_s3_class(rs, "rule_set")
  expect_true("AE" %in% names(rs$rules))
  expect_true(length(rs$rules$AE) > 0)
})

test_that("compile_rules generates all 4 MVP domains", {
  skip_if_no_starter_kit()
  rs <- compile_rules(dummy_meta, dummy_smeta, dummy_ct)
  expect_true(all(c("AE", "CM", "MH", "PR") %in% names(rs$rules)))
})

test_that("parse_rule_json parses valid JSON", {
  json <- '{"type":"constant","params":{"value":"Y"}}'
  parsed <- parse_rule_json(json)
  expect_type(parsed, "list")
  expect_equal(parsed$type, "constant")
})

test_that("parse_rule_json errors on invalid JSON", {
  expect_error(parse_rule_json("{broken"))
})

test_that("parse_rule_dsl handles colon-separated format", {
  dsl <- "direct_map: ae_raw.term"
  parsed <- parse_rule_dsl(dsl)
  expect_equal(parsed$type, "direct_map")
})

test_that("canonicalize_rules lowercases rule types", {
  skip_if_no_starter_kit()
  rs <- compile_rules(dummy_meta, dummy_smeta, dummy_ct)
  canon <- canonicalize_rules(rs)
  expect_true(all(canon$rule_types == tolower(canon$rule_types)))
})
