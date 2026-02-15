# tests/testthat/test-mod_j_codegen.R

test_that("gen_domain_script returns a character string", {
  skip_if_no_starter_kit()

  rs <- compile_rules(dummy_meta, ct_lib = dummy_ct)
  script <- gen_domain_script("AE", rs, dummy_meta, dummy_cfg)
  expect_type(script, "character")
  expect_true(nchar(script) > 100)
  expect_true(grepl("SDTM AE Domain", script))
})

test_that("gen_domain_script contains library calls", {
  skip_if_no_starter_kit()

  rs <- compile_rules(dummy_meta, ct_lib = dummy_ct)
  script <- gen_domain_script("AE", rs, dummy_meta, dummy_cfg)
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

  rs <- compile_rules(dummy_meta, ct_lib = dummy_ct)
  yml <- serialize_rules_to_yaml(rs)
  expect_type(yml, "character")
  expect_true(grepl("AE", yml))
})

test_that("serialize_rules_to_json returns JSON text", {
  skip_if_no_starter_kit()

  rs <- compile_rules(dummy_meta, ct_lib = dummy_ct)
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

test_that("gen_domain_script produces parseable R code for AE", {
  skip_if_no_starter_kit()

  rs <- compile_rules(dummy_meta, ct_lib = dummy_ct)
  script <- gen_domain_script("AE", rs, dummy_meta, dummy_cfg)
  # Should parse without error
  parsed <- tryCatch(parse(text = script), error = function(e) NULL)
  expect_false(is.null(parsed),
               info = "Generated AE script should be parseable R code")
})

test_that("gen_domain_script works for all starter-kit domains", {
  skip_if_no_starter_kit()

  rs <- compile_rules(dummy_meta, ct_lib = dummy_ct)
  domains <- unique(dummy_meta$domain)
  for (dom in domains) {
    script <- gen_domain_script(dom, rs, dummy_meta, dummy_cfg)
    expect_true(nchar(script) > 50,
                info = paste0("Script for ", dom, " should have content"))
    expect_true(grepl(dom, script),
                info = paste0("Script should reference domain ", dom))
    # Should be parseable
    parsed <- tryCatch(parse(text = script), error = function(e) NULL)
    expect_false(is.null(parsed),
                 info = paste0(dom, " script should be parseable R code"))
  }
})

test_that("gen_domain_script writes to file", {
  skip_if_no_starter_kit()

  rs <- compile_rules(dummy_meta, ct_lib = dummy_ct)
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp))
  script <- gen_domain_script("AE", rs, dummy_meta, dummy_cfg,
                              output_path = tmp)
  expect_true(file.exists(tmp))
  content <- readLines(tmp)
  expect_true(length(content) > 5)
})

test_that("gen_qmd_domain produces Quarto markdown", {
  skip_if_no_starter_kit()

  rs <- compile_rules(dummy_meta, ct_lib = dummy_ct)
  qmd <- gen_qmd_domain("AE", rs, dummy_meta)
  expect_true(grepl("title:", qmd))
  expect_true(grepl("SDTM AE", qmd))
  expect_true(grepl("AETERM", qmd))
})
