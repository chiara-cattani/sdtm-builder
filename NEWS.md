# sdtmbuilder 0.1.0

## Phase 1 — Metadata Schema & Rule Compiler

* S3 classes for study config, meta bundle, rule set, validation report, and
  build result (`mod_a_primitives.R`).
* Readers and validators for target metadata, source metadata, and controlled
  terminology library (`mod_b_metadata.R`).
* `compile_rules()` parses JSON rule parameters, resolves dependencies, and
  enriches CT rules with codelist mappings (`mod_c_rules.R`).
* `build_dependency_graph()` and `topo_sort_rules()` using igraph to
  guarantee correct derivation order (`mod_d_dependency.R`).

## Phase 2 — build_domain MVP

* `build_domain()` orchestrator: load, join DM, dependency-sort, derive,
  finalize, validate (`mod_h_builders.R`).
* Core derivation functions: `map_direct()`, `derive_constant()`,
  `derive_coalesce()`, `derive_if_else()`, `derive_case_when()`
  (`mod_g1_derive_map.R`).
* Controlled terminology: `assign_ct()`, `decode_ct()` with configurable
  unknown-value policy (`mod_g2_derive_ct.R`).
* Date/time handling: `format_iso_dtc()`, `parse_partial_date()`,
  `derive_dy()` for study-day calculation (`mod_g3_derive_dates.R`).
* Visit derivation: `derive_visit()`, `derive_visitnum()`
  (`mod_g4_derive_visits.R`).
* Identifiers: `derive_seq()` with stable sort and per-subject numbering
  (`mod_g5_derive_ids.R`).
* Flags: `derive_baseline_flag()`, `derive_status()`
  (`mod_g6_derive_flags.R`).
* Data access: `load_raw_data()`, `standardize_names()`,
  `standardize_types()`, `apply_missing_conv()` (`mod_e_data_access.R`).
* Joins: `safe_join()` with cardinality checks (`mod_f_joins.R`).
* Export: `export_xpt()` via haven, `export_rds_csv()` (`mod_k_export.R`).
* All 8 domains build end-to-end from dummy data: DM, AE, CM, MH, PR, EX,
  VS, LB.
* `make_dummy_study()` generates deterministic 30-subject test data
  (seed = 123).

## Phase 3 — Multi-Source Joins

* `bind_sources()` stacks multiple raw datasets with fill and provenance
  tagging.
* `split_records()` and `expand_checkbox()` for record expansion.
* `add_provenance_cols()` tracks source and rule origin per row.

## Phase 4 — Dates, Visits & Study Day

* `derive_epoch()` assigns epoch labels (SCREENING, TREATMENT, FOLLOW-UP)
  based on study-day windows from `config$epoch_map`.
* EPOCH rows added to `target_meta.csv` for 6 domains (AE, CM, PR, EX, VS,
  LB).
* `epoch_map` configuration in `config.yaml`.
* Fixed `format_iso_dtc()` condition order to eliminate 1 092 warnings from
  uninitialised column access.

## Phase 5 — SUPP-- and RELREC

* `build_supp()` generates CDISC-compliant supplemental qualifier datasets
  (RDOMAIN, IDVAR, IDVARVAL, QNAM, QLABEL, QVAL, QORIG, QEVAL).
* Automatic detection of `to_supp = "Y"` metadata flags for SUPPAE
  generation.
* `build_relrec()` scaffold for relationship records (full linking deferred).

## Phase 6 — Validation & Code Generation

* 10+ validation checks:
    - Required variables present
    - Key variables unique
    - DOMAIN value constant and correct
    - STUDYID constant
    - ISO 8601 date conformance
    - CT value conformance
    - No all-NA Required/Expected columns
    - SEQ integrity (positive integer, no gaps)
    - No duplicate rows
    - Cross-domain USUBJID consistency
* `gen_domain_script()` produces parseable, reproducible R code for all 8
  domains.
* `gen_qmd_domain()` generates Quarto report templates.
* `gen_project_scaffold()` creates full project directory structure.
* All 8 domains build with 0 validation errors.

## Phase 7 — Documentation & CI

* Vignettes:
    - "Build AE from Metadata" — full end-to-end tutorial.
    - "Architecture Overview" — module diagram and data flow.
    - "Build Pipeline" — step-by-step pipeline walkthrough.
    - "Metadata Schema" — field definitions for all metadata inputs.
    - "Multi-Source EX" — join patterns and bind_sources workflow.
    - "SUPP and RELREC" — supplemental qualifier generation.
    - "Custom Rules and Sponsor Overrides" — extending the rule engine.
* Demo script `inst/examples/demo_full_pipeline.R`.
* `pkgdown` site configuration.
* GitHub Actions CI: R-CMD-check on ubuntu/windows/macos (R release + devel).
* Test coverage reporting with `covr`.
* README with quick-start guide and architecture diagram.

## Phase 8 — Delivery Readiness Audit

* Expanded `derive_variable()` dispatcher from 8 to 21 rule types including
  `visit`, `visitnum`, `baseline_flag`, `coalesce`, `if_else`, `case_when`,
  `ct_decode`, `concat`, `occurrence`, `status`, and `duration`.
* Enriched `target_meta.csv` starter kit:
    - New variables: AEBODSYS, AEENDDY, CMENDDY, EXENDDY, LBBLFL.
    - Corrected rule types: VS/LB VISIT (`visit`), VISITNUM (`visitnum`),
      VSBLFL/LBBLFL (`baseline_flag`).
    - 10+ distinct rule types now exercised across the starter kit.
* Fixed BASELINE visit day offsets in dummy study generator so VS/LB
  observations correctly map to the BASELINE window (study day 1).
* Added AEBODSYS (Body System or Organ Class) to `ae_raw` dummy data and
  `source_meta.csv`.
* Fixed `emit_log_messages()` bug (`f$check` → `f$rule_id`).
* Full roxygen documentation for all 11 functions in `mod_b_metadata.R`.
* Regenerated NAMESPACE and man pages (18 new `.Rd` files).
* Replaced TODO comment in code generator with professional `[MANUAL]` label.
* Added `inform()` messages to stub plugins (TA/TV/TE/TS, SV).

## Test Suite

* 289 tests passing, 0 failures, 0 warnings.
* Unit tests for all derivation functions, CT mapping, ISO date handling.
* Integration tests covering all 8 domains end-to-end.
* Snapshot-style validation report checks.
