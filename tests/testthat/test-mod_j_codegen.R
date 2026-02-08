# tests/testthat/test-mod_j_codegen.R

test_that("gen_domain_script returns a character string", {
  skip_if_no_starter_kit()

  rs <- compile_rules(dummy_meta, dummy_smeta, dummy_ct)
  script <- gen_domain_script("AE", rs, dummy_meta, dummy_smeta, dummy_cfg)
  expect_type(script, "character")
  expect_true(nchar(script) > 100)
  expect_true(grepl("SDTM AE Domain", script))
})

test_that("gen_domain_script contains library calls", {
  skip_if_no_starter_kit()

  rs <- compile_rules(dummy_meta, dummy_smeta, dummy_ct)
  script <- gen_domain_script("AE", rs, dummy_meta, dummy_smeta, dummy_cfg)
  expect_true(grepl("library\\(dplyr\\)", script))
  expect_true(grepl("library\\(sdtmbuilder\\)", script))
})

test_that("render_rule_code handles constant rule", {
  rule <- list(type = "constant", params = list(value = "AE"))
  code <- render_rule_code("DOMAIN", rule)
  expect_true(grepl("DOMAIN", code))
  expect_true(grepl("AE", code))
})

test_that("serialize_rules_to_yaml returns YAML text", {
  skip_if_no_starter_kit()

  rs <- compile_rules(dummy_meta, dummy_smeta, dummy_ct)
  yml <- serialize_rules_to_yaml(rs)
  expect_type(yml, "character")
  expect_true(grepl("AE", yml))
})

test_that("serialize_rules_to_json returns JSON text", {
  skip_if_no_starter_kit()

  rs <- compile_rules(dummy_meta, dummy_smeta, dummy_ct)
  jsn <- serialize_rules_to_json(rs)
  expect_type(jsn, "character")
  # Should be valid JSON
  parsed <- jsonlite::fromJSON(jsn)
  expect_true(is.list(parsed))
})

test_that("gen_shared_utils_script contains STUDYID", {
  cfg <- new_sdtm_config(studyid = "DEMO-001",
                         ref_start_rule = list(var = "RFSTDTC", source = "DM"))
  script <- gen_shared_utils_script(cfg)
  expect_true(grepl("DEMO-001", script))
})
