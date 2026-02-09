# sdtmbuilder

<!-- badges: start -->
[![R-CMD-check](https://github.com/chiara-cattani/sdtm-builder/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/chiara-cattani/sdtm-builder/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

> **A metadata-driven R package for building SDTM domains from raw clinical
> data.**

`sdtmbuilder` turns structured metadata files (Study\_Metadata.xlsx,
source\_meta, Study\_CT.xlsx) into validated SDTM datasets.  Every derivation
rule is declared in the metadata — **no hand-coding per domain** — so a single
engine (`build_domain()`) processes any CDISC domain.

---

## Table of Contents

1. [Installation](#installation)
2. [Quick Start — 5 Minutes](#quick-start--5-minutes)
3. [Architecture](#architecture)
4. [Workflow](#workflow)
5. [Metadata Files](#metadata-files)
6. [Rule Types Reference](#rule-types-reference)
7. [Function Reference](#function-reference)
8. [Adding a New Domain](#adding-a-new-domain)
9. [Validation](#validation)
10. [Code Generation](#code-generation)
11. [Export](#export)
12. [Running the Demo](#running-the-demo)
13. [Domains Included](#domains-included)
14. [Running Tests](#running-tests)
15. [Troubleshooting](#troubleshooting)

---

## Installation

```r
# Install from GitHub (requires devtools)
devtools::install_github("chiara-cattani/sdtm-builder")

# Or clone and install locally
devtools::install("path/to/sdtm-builder")

# Or load for development
devtools::load_all("path/to/sdtm-builder")
```

### Dependencies

| Package   | Purpose                          |
|-----------|----------------------------------|
| dplyr     | Data manipulation                |
| tibble    | Tibbles                          |
| rlang     | Tidy evaluation, error handling  |
| cli       | Pretty console messages          |
| glue      | String interpolation             |
| stringr   | String operations                |
| readxl    | Read Excel (.xlsx) metadata      |
| haven     | Export SAS .xpt files            |
| yaml      | Read config.yaml                 |
| jsonlite  | Parse rule\_params JSON          |

---

## Quick Start — 5 Minutes

```r
library(sdtmbuilder)

# 1. Generate dummy study data (30 subjects, 10 raw datasets)
study <- make_dummy_study(seed = 42)

# 2. Compile derivation rules from metadata
rule_set <- compile_rules(
  study$target_meta,
  study$source_meta,
  study$ct_lib
)
print(rule_set)  # shows 10 domains, 146 rules

# 3. Build ALL domains in dependency order (DM first, then the rest)
results <- build_all_domains(
  target_meta = study$target_meta,
  source_meta = study$source_meta,
  raw_data    = study$raw_data,
  config      = study$config,
  rule_set    = rule_set
)

# 4. Inspect results
head(results$AE$data)       # the SDTM AE dataset
results$AE$supp             # SUPPAE (if any SUPP variables)
results$AE$report           # validation report

# 5. Build without SUPP domains (keep all vars in main domain)
results_no_supp <- build_all_domains(
  target_meta = study$target_meta,
  source_meta = study$source_meta,
  raw_data    = study$raw_data,
  config      = study$config,
  rule_set    = rule_set,
  create_supp = FALSE
)

# 6. Or build a single domain manually
ae <- build_domain("AE", study$target_meta, study$source_meta,
                   study$raw_data, study$config, rule_set,
                   dm_data = results$DM$data)

# 7. Export
export_xpt(results$AE$data, "AE", "output/")
```

---

## Architecture

```
+------------------------------------------------------------------+
|                         Metadata Layer                            |
|  Study_Metadata.xlsx    source_meta.csv/xlsx   Study_CT.xlsx     |
|  config.yaml                                                     |
+----------------+--------------------------+----------------------+
                 |                          |
          +------v------+           +-------v------+
          | compile_rules|           | make_dummy   |
          |  (Module C)  |           | _study()     |
          +------+------+           +-------+------+
                 | rule_set                 | raw_data
          +------v------------------------------v------+
          |            build_domain()                   |
          |  +----------------------------------------+ |
          |  | derive_variable() dispatcher           | |
          |  |  -> 19 rule types                      | |
          |  |  -> topological order                  | |
          |  +----------------------------------------+ |
          |  +----------+ +--------+ +-----------+      |
          |  |finalize  | |validate| |build_supp |      |
          |  |_domain() | |_domain | |()         |      |
          |  +----------+ +--------+ +-----------+      |
          +-----------------+---------------------------+
                            |
               +------------v------------+
               |  SDTM domain + SUPP +   |
               |  validation report +    |
               |  provenance log         |
               +------------+------------+
                            |
                +-----------v-----------+
                |   export_xpt()        |
                |   gen_domain_script() |
                |   gen_qmd_domain()    |
                +-----------------------+
```

### Module Map

| Module | File | Purpose |
|--------|------|---------|
| A | `mod_a_primitives.R` | S3 classes: `sdtm_config`, `meta_bundle`, `rule_set`, `build_context`, `validation_report`, `log_sink` |
| B | `mod_b_metadata.R` | Read, validate, normalize metadata files (CSV & Excel) — legacy readers deprecated |
| — | `read_study_metadata_excel.R` | Read multi-sheet Study\_Metadata.xlsx (Meta, Domains, Variables, Value Level, Where Clauses) |
| — | `read_study_ct_excel.R` | Read multi-sheet Study\_CT.xlsx (Codelists, Codelists\_terms) |
| — | `method_mapping.R` | Map METHOD column to internal rule\_type values |
| — | `validate_study_metadata.R` | Cross-sheet validation (domains, VLM, CT refs, extensibility) |
| C | `mod_c_rules.R` | Parse JSON rule\_params into compiled rule\_set |
| D | `mod_d_dependency.R` | Build dependency graph, topological sort |
| E | `mod_e_data_access.R` | Load raw data, standardize names/types, apply missing conventions |
| F | `mod_f_joins.R` | Safe joins, key resolution, record assembly |
| G1 | `mod_g1_derive_map.R` | Mapping & transform derivations (direct\_map, coalesce, case\_when, if\_else, concat, regex) |
| G2 | `mod_g2_derive_ct.R` | Controlled terminology (ct\_assign, ct\_decode, yes/no, unknown) |
| G3 | `mod_g3_derive_dates.R` | ISO 8601 dates, DY, EPOCH, duration |
| G4 | `mod_g4_derive_visits.R` | VISIT, VISITNUM, VISITDY, timepoints |
| G5 | `mod_g5_derive_ids.R` | USUBJID, --SEQ, SPID, GRPID |
| G6 | `mod_g6_derive_flags.R` | Baseline flag, last-obs flag, occurrence, status |
| H | `mod_h_builders.R` | `build_domain()`, `derive_variable()` dispatcher, `finalize_domain()`, SUPP/RELREC builders, plugins |
| I | `mod_i_validation.R` | Domain-level validation (15 checks) |
| J | `mod_j_codegen.R` | Generate R scripts, Quarto docs, YAML/JSON serialization |
| K | `mod_k_export.R` | Export to XPT, RDS, CSV; define.xml support files |
| L | `mod_l_utils.R` | Logging, error helpers, assertions, snapshots |

---

## Workflow

### Step 1 — Prepare Metadata

The package ships with starter-kit metadata in `inst/extdata/starter_kit/`.
The primary metadata format uses multi-sheet Excel workbooks:

- **`Study_Metadata.xlsx`** — Contains sheets: Meta, Standards, Domains,
  Variables, Value Level, Where Clauses, Method
- **`Study_CT.xlsx`** — Contains sheets: Codelists, Codelists\_terms
- **`source_meta.csv`** — Raw data column descriptions (CSV or Excel)

```r
# Read study metadata from multi-sheet Excel workbook
study_meta <- read_study_metadata_excel("metadata/Study_Metadata.xlsx")
# Returns: list(study_name, target_meta, domain_meta, value_level_meta)

# Read controlled terminology from multi-sheet Excel workbook
ct_lib <- read_study_ct_excel("metadata/Study_CT.xlsx")
# Returns: flat tibble with codelist_id, coded_value, input_value, decode, ...

# Read source metadata (unchanged — CSV or single-sheet Excel)
source_meta <- read_source_meta("metadata/source_meta.csv")

# Validate cross-references between sheets
validate_study_metadata(study_meta, ct_lib)
```

> **Legacy CSV readers** (`read_target_meta()`, `read_ct_library()`) are
> deprecated. Use `read_study_metadata_excel()` and `read_study_ct_excel()`
> instead.

### Step 2 — Create Configuration

```r
config <- new_sdtm_config(
  studyid        = "STUDY-XYZ",
  timezone       = "UTC",
  ref_start_rule = list(var = "rfstdtc", source = "dm_raw"),
  visit_map      = visit_map_tibble,  # tibble with VISITNUM, VISIT, START_DAY, END_DAY
  create_supp    = TRUE               # set FALSE to keep all vars in main domain
)
```

Or load from YAML:

```r
config_yaml <- yaml::read_yaml("config.yaml")
config <- new_sdtm_config(
  studyid        = config_yaml$studyid,
  timezone       = config_yaml$timezone %||% "UTC",
  ref_start_rule = config_yaml$ref_start_rule,
  visit_map      = config_yaml$visit_map
)
```

### Step 3 — Compile Rules

```r
rule_set <- compile_rules(target_meta, source_meta, ct_lib)
print(rule_set)
```

The compiler reads `rule_type` and `rule_params` (JSON) from each row of
`target_meta`, parses them into an internal rule list, resolves dependencies
declared in `depends_on`, and enriches rules with codelist information.

### Step 4 — Build Domains

The simplest way is `build_all_domains()` — it auto-detects that DM must be
built first (other domains need `RFSTDTC`) and passes `dm_data` downstream.

```r
# Build all domains in dependency order
results <- build_all_domains(
  target_meta, source_meta, raw_data, config, rule_set
)

# Build only specific domains
results <- build_all_domains(
  target_meta, source_meta, raw_data, config, rule_set,
  domains = c("DM", "AE", "VS")
)

# Build without SUPP domains (keep all vars in main domain)
results <- build_all_domains(
  target_meta, source_meta, raw_data, config, rule_set,
  create_supp = FALSE
)
```

Or build domains individually:

```r
# Build DM first
dm <- build_domain("DM", target_meta, source_meta, raw_data, config, rule_set)

# Build other domains, passing DM data for RFSTDTC
ae <- build_domain("AE", target_meta, source_meta, raw_data, config, rule_set,
                   dm_data = dm$data)
```

`build_domain()` returns a named list:

| Element | Description |
|---------|-------------|
| `$data` | The SDTM dataset (tibble) |
| `$supp` | SUPP-- dataset or NULL |
| `$relrec` | RELREC rows or NULL |
| `$report` | Validation report object |
| `$log` | Build log messages |
| `$provenance` | Column-level derivation trace |
| `$artifacts` | Any extra artefacts |

> **Tip:** You can also pass the full build result as `dm_data` — the function
> automatically extracts `$data` from list objects.

### Step 5 — Validate & Export

```r
# Export as SAS XPT
export_xpt(ae$data, "AE", "output/xpt/", target_meta = target_meta)

# Export as RDS + CSV
export_rds_csv(ae$data, "AE", "output/rds/")

# Generate a standalone R script that reproduces the domain
gen_domain_script("AE", rule_set, target_meta, source_meta, config,
                  output_path = "programs/ae.R")
```

---

## Metadata Files

> **Extra columns are tolerated.** Metadata readers only require certain
> columns (see tables below). Any additional columns in your files are
> silently preserved — no error, no data loss.

### Study\_Metadata.xlsx — Multi-Sheet Target Metadata

The primary metadata workbook contains all study-level, domain-level,
and variable-level definitions across multiple sheets.

#### Sheet: Meta

| Column | Required | Description |
|--------|----------|-------------|
| `Select` | **Yes** | `Y` for the active study row |
| `STUDY_NAME` | **Yes** | Study identifier |

#### Sheet: Domains

| Column | Required | Description |
|--------|----------|-------------|
| `Select` | **Yes** | `Y` to include this domain |
| `CLASS_ORDER` | **Yes** | Numeric sort order by domain class |
| `DOMAIN_LEVEL_ORDER` | **Yes** | Numeric sort order within class |
| `CLASS` | **Yes** | Domain class (SPECIAL PURPOSE, EVENTS, FINDINGS, ...) |
| `DOMAIN` | **Yes** | Domain abbreviation (DM, AE, LB, ...) |
| `KEYS` | No | Comma-separated sort keys (e.g., `STUDYID, USUBJID, LBTESTCD`) |
| `DESCRIPTION` | No | Dataset label (e.g., "Laboratory Test Results") |
| `STRUCTURE` | No | Dataset structure (e.g., "One record per lab test per subject") |

#### Sheet: Variables

| Column | Required | Description |
|--------|----------|-------------|
| `Select` | **Yes** | `Y` to include this variable |
| `CLASS_ORDER` | No | Sort order (1st priority) |
| `DOMAIN_LEVEL_ORDER` | No | Sort order (2nd priority) |
| `ROLE_ORDER` | No | Sort order (3rd priority) |
| `VAR_MODEL_ORDER` | No | Sort order (4th priority) |
| `SEQORDER` | No | Sort order (5th priority) |
| `DOMAIN` | **Yes** | Domain abbreviation |
| `VAR_MODEL` | No | Variable model name without domain prefix |
| `VARNAME` | **Yes** | Full variable name (e.g., `LBTESTCD`) |
| `VARLABEL` | **Yes** | Variable label |
| `DATA_TYPE` | **Yes** | `text`, `integer`, `float`, `datetime`, `durationDatetime` |
| `LENGTH` | No | Maximum length |
| `SIGNIFICANT_DIGITS` | No | Decimal precision for numerics |
| `CODELIST_ID` | No | Codelist identifier |
| `ROLE` | No | CDISC role (Identifier, Topic, etc.) |
| `CORE` | No | `Req`, `Exp`, or `Perm` |
| `VLM_ID` | No | Value-level metadata identifier |
| `METHOD` | No | Derivation method (SEQ, STRESN, USUBJID, DY, etc.) |

#### Sheet: Value Level

| Column | Required | Description |
|--------|----------|-------------|
| `Select` | **Yes** | `Y` to include |
| `DOMAIN` | **Yes** | Domain abbreviation |
| `VARNAME` | **Yes** | Variable the VLM applies to |
| `VLM_ID` | **Yes** | Matches VLM\_ID in Variables sheet |
| `WHERE_CLAUSE_ID` | **Yes** | Links to Where Clauses sheet |
| `DATA_TYPE` | No | Override data type for this condition |
| `LENGTH` | No | Override length |
| `SIGNIFICANT_DIGITS` | No | Override precision |
| `CODELIST_ID` | No | Override codelist |

#### Sheet: Where Clauses

| Column | Required | Description |
|--------|----------|-------------|
| `Select` | **Yes** | `Y` to include |
| `WHERE_CLAUSE_ID` | **Yes** | Unique identifier (e.g., `LB.LBTESTCD.GLUC`) |
| `DOMAIN` | **Yes** | Domain abbreviation |
| `VARNAME` | **Yes** | Variable to filter on |
| `COMPARATOR` | **Yes** | `EQ`, `IN`, `NE`, `NOTIN` |
| `VALUE` | **Yes** | Value(s) — pipe-separated for IN (e.g., `GLUC\|WBC\|HGB`) |

#### Sheet: Method (informational)

| Column | Description |
|--------|-------------|
| `Select` | `Y` to include |
| `METHOD` | Method identifier (SEQ, STRESN, etc.) |
| `DESCRIPTION` | Human-readable derivation description |

### Study\_CT.xlsx — Multi-Sheet Controlled Terminology

#### Sheet: Codelists

| Column | Required | Description |
|--------|----------|-------------|
| `Select` | **Yes** | `Y` to include |
| `CODELIST_NAME` | No | Human-readable name |
| `CODELIST_CODE` | No | CDISC code (e.g., C66731) |
| `CODELIST_ID` | **Yes** | Primary identifier (e.g., `YN`, `SEX`, `LBTESTCD`) |
| `IS_EXTENSIBLE` | No | `Yes` or `No` |

#### Sheet: Codelists\_terms

| Column | Required | Description |
|--------|----------|-------------|
| `Select` | **Yes** | `Y` to include |
| `CODELIST_NAME` | No | Codelist name |
| `CODELIST_ID` | **Yes** | Matches Codelists sheet |
| `TERM_CODE` | No | CDISC term code (e.g., C49488) |
| `SUBMISSION_VALUE` | **Yes** | The SDTM standard term |
| `DECODE` | No | Collected value (if different from submission value) |

### SELECT Column — Study-Specific Filtering

All metadata sheets support a `Select` column. When present,
only rows with `Select = "Y"` (case-insensitive) are loaded; all other rows
are silently excluded. This allows you to maintain a **global metadata set**
covering all studies and select the study-specific subset via the `Select`
column.

### source\_meta.csv / source\_meta.xlsx

Each row describes one column in a raw dataset.

| Column | Type | Required | Description |
|--------|------|----------|-------------|
| `select` | char | No | `Y` to include this row for the study |
| `dataset` | char | **Yes** | Raw dataset name (dm\_raw, ae\_raw, ...) |
| `column` | char | **Yes** | Column name |
| `type` | char | **Yes** | `character` or `numeric` |
| `label` | char | No | Description |
| `is_key` | char | No | `Y` if key column |
| `notes` | char | No | Free-text notes |

### config.yaml

```yaml
studyid: "STUDY-XYZ"
timezone: "UTC"
ref_start_rule:
  var: rfstdtc
  source: dm_raw
visit_map:
  - VISITNUM: 1
    VISIT: SCREENING
    START_DAY: -14
    END_DAY: -1
  - VISITNUM: 2
    VISIT: BASELINE
    START_DAY: 1
    END_DAY: 1
epoch_map:
  - epoch: SCREENING
    end_day: -1
  - epoch: TREATMENT
    start_day: 1
    end_day: 92
  - epoch: FOLLOW-UP
    start_day: 93
```

---

## Rule Types Reference

The `rule_type` column in `target_meta` determines how each SDTM variable is
derived.  The `rule_params` column is a JSON string that configures the
derivation.  Below are all **19 rule types** with example JSON:

### constant

Set a fixed value.

```json
{"value": "STUDY-XYZ"}
```

### direct\_map

Copy a column from raw data, optionally transforming.

```json
{"dataset": "ae_raw", "column": "aeterm", "transform": "toupper"}
```

`transform` options: `toupper`, `tolower`, `trimws`, or omit.

### ct\_assign

Map raw values to controlled terminology coded values.

```json
{
  "dataset": "ae_raw",
  "column": "aesev_raw",
  "codelist_id": "C66769",
  "unknown_policy": "warn_and_keep"
}
```

### ct\_decode

Look up the decode (display name) for a coded value via codelist.

```json
{"column": "QSTESTCD", "codelist_id": "QSTEST"}
```

### iso\_dtc

Combine date and optional time into ISO 8601 DTC format.

```json
{
  "date_col": {"dataset": "ae_raw", "column": "aestdat"},
  "time_col": {"dataset": "ae_raw", "column": "aestim"},
  "partial_allowed": true
}
```

### dy

Calculate study day (DTC minus RFSTDTC, with no Day 0).

```json
{
  "dtc_var": "AESTDTC",
  "ref_var": "RFSTDTC",
  "ref_dataset": "dm_raw",
  "ref_column": "rfstdtc"
}
```

### seq

Generate sequential integers (`--SEQ`) per subject.

```json
{
  "by": ["STUDYID", "USUBJID"],
  "order_by": ["AESTDTC", "AEDECOD"],
  "ties": "dense"
}
```

### epoch

Assign EPOCH based on study day and the epoch\_map in config.

```json
{"dtc_var": "AESTDTC", "ref_var": "RFSTDTC"}
```

### visit

Map study day to VISIT name using the visit\_map in config.

```json
{"dy_var": "VSDY"}
```

### visitnum

Map VISIT name to VISITNUM using the visit\_map in config.

```json
{"visit_var": "VISIT"}
```

### baseline\_flag

Set `"Y"` for baseline visit rows, `NA` otherwise.

```json
{
  "visit_var": "VISIT",
  "baseline_visit": "BASELINE",
  "by": ["USUBJID", "VSTESTCD"]
}
```

### coalesce

Use the first non-missing value from multiple sources.

```json
{"sources": ["dsdecod_raw", "dsterm"]}
```

### case\_when

Apply sequential conditions (first match wins).

```json
{
  "conditions": {
    "grepl('CONSENT|RANDOMIZ', dsterm, ignore.case = TRUE)": "PROTOCOL MILESTONE",
    "TRUE": "DISPOSITION EVENT"
  },
  "default": "DISPOSITION EVENT"
}
```

Conditions are valid R expressions evaluated against the working dataset.

### if\_else

Simple two-way conditional.

```json
{
  "condition": "DSCAT == 'DISPOSITION EVENT'",
  "true_value": "STUDY TREATMENT",
  "false_value": "INFORMED CONSENT",
  "missing_value": "NA"
}
```

### concat

Concatenate columns with a separator.

```json
{"sources": ["STUDYID", "dsid"], "sep": "-"}
```

### occurrence

Flag presence/absence of a value.

```json
{
  "source_var": "qsorres",
  "present_value": "NA",
  "absent_value": "NOT DONE"
}
```

Use `"NA"` (the string) to set the value to R's `NA` when present.  This is
common for SDTM `--STAT` variables which are blank when done and `"NOT DONE"`
when not done.

### status

Derive completion status from a result variable.

```json
{
  "result_var": "QSORRES",
  "done_value": "",
  "not_done_value": "NOT DONE"
}
```

### duration

Calculate ISO 8601 duration between two DTC variables.

```json
{"start_dtc": "CMSTDTC", "end_dtc": "CMENDTC", "units": "auto"}
```

### join

Join data from another dataset.

```json
{
  "dataset": "dm_raw",
  "column": "arm",
  "by": {"usubjid": "usubjid"}
}
```

---

## Function Reference

### Module A — S3 Classes & Constructors (`mod_a_primitives.R`)

| Function | Description |
|----------|-------------|
| `new_sdtm_config(studyid, timezone, ref_start_rule, visit_map, ...)` | Create study configuration |
| `new_meta_bundle(target_meta, source_meta, ct_lib, value_level_meta)` | Bundle metadata objects |
| `new_rule_set(rules, dependency_info, rule_types, compile_log)` | Create a rule\_set (usually via `compile_rules()`) |
| `new_build_context(domain, config, meta_bundle, rule_set, ...)` | Create a build context |
| `new_validation_report(domain, title)` | Initialize a validation report |
| `add_finding(report, rule_id, severity, domain, variable, ...)` | Add a finding to a validation report |
| `new_log_sink(log_level, file, context_fields)` | Create a log sink |
| `print.sdtm_config(x)` | Print config summary |
| `print.meta_bundle(x)` | Print bundle summary |
| `print.rule_set(x)` | Print rule\_set summary |
| `print.build_context(x)` | Print context summary |
| `print.validation_report(x)` | Print validation report |
| `print.log_sink(x)` | Print log sink info |

### Module B — Metadata Ingestion (`mod_b_metadata.R`)

| Function | Description |
|----------|-------------|
| `read_target_meta(path, sheet, domain, colmap, encoding)` | Read target\_meta from CSV or Excel |
| `read_source_meta(path, sheet, colmap, encoding)` | Read source\_meta from CSV or Excel |
| `read_ct_library(path, sheet, colmap, version, sponsor_extension)` | Read CT codelist from CSV or Excel |
| `validate_target_meta(target_meta, strict)` | Validate target\_meta structure |
| `validate_source_meta(source_meta, strict)` | Validate source\_meta structure |
| `validate_ct_library(ct_lib)` | Validate CT codelist structure |
| `normalize_target_meta(target_meta, config)` | Normalize column names/types |
| `normalize_source_meta(source_meta, config)` | Normalize column names/types |
| `expand_value_level_meta(target_meta, value_level_meta)` | Expand value-level metadata |
| `apply_study_overrides(target_meta, config)` | Apply sponsor overrides |
| `resolve_domain_model(domain, target_meta, config, ig_version)` | Resolve domain model |

### Module C — Rule Compilation (`mod_c_rules.R`)

| Function | Description |
|----------|-------------|
| `compile_rules(target_meta, source_meta, ct_lib, dsl, strict)` | **Compile metadata into a rule\_set** — the main entry point for rule compilation |
| `parse_rule_json(rule_json, var)` | Parse a single JSON rule\_params string |
| `parse_rule_dsl(rule_text, var)` | Parse a DSL rule expression |
| `validate_rules(rule_set, source_meta, ct_lib)` | Validate compiled rules against metadata |
| `enrich_rules_with_ct(rule_set, ct_lib)` | Enrich rules with CT information |
| `infer_rule_dependencies(rule_set)` | Infer dependencies not explicitly declared |
| `canonicalize_rules(rule_set)` | Normalize rule structures |

### Module D — Dependency & Ordering (`mod_d_dependency.R`)

| Function | Description |
|----------|-------------|
| `build_dependency_graph(rule_set, domain, allow_cycles, late_bind_vars)` | Build derivation dependency graph |
| `topo_sort_rules(graph)` | Topological sort of derivation rules |
| `detect_cycles(graph)` | Detect circular dependencies |
| `resolve_late_binding(graph, cycles, late_bind_vars)` | Resolve late-binding cycles |
| `plan_build_steps(domain, dep_graph, rule_set, source_meta)` | Plan derivation execution steps |

### Module E — Raw Data Access (`mod_e_data_access.R`)

| Function | Description |
|----------|-------------|
| `load_raw_data(paths, formats, encoding, db_con, db_tables)` | Load raw data from files or database |
| `standardize_names(data, dataset_name, source_meta, ...)` | Standardize column names |
| `standardize_types(data, source_meta, dataset_name, ...)` | Coerce column types per source\_meta |
| `apply_missing_conventions(data, config, blank_to_na, ...)` | Apply missing value conventions |
| `derive_core_keys(data, config, subjid_col, sep)` | Derive USUBJID and core key variables |
| `get_subject_level(raw_data, config, dm_dataset, key_vars)` | Extract subject-level DM data |

### Module F — Joins & Record Assembly (`mod_f_joins.R`)

| Function | Description |
|----------|-------------|
| `safe_join(x, y, by, type, cardinality, on_violation, ...)` | Safe join with cardinality checks |
| `resolve_keys(data, domain, target_meta, config)` | Resolve natural key columns |
| `deduplicate_by_rule(data, keys, strategy, order_by, ...)` | Deduplicate rows |
| `bind_sources(..., source_names, fill_missing)` | Bind multiple source datasets |
| `add_provenance_cols(data, source_name, rule_id)` | Add provenance tracking columns |
| `split_records(data, col, sep, trim, id_cols, ...)` | Split multi-valued columns into rows |
| `expand_checkbox(data, cols, map, id_cols, ...)` | Expand checkbox columns |
| `expand_visits(data, visit_map, by, filter_fn)` | Expand visit schedule |

### Module G1 — Mapping & Transform (`mod_g1_derive_map.R`)

| Function | Description |
|----------|-------------|
| `map_direct(data, target_var, source_var, transform, type, blank_to_na)` | Direct column mapping |
| `derive_constant(data, target_var, value, when, type)` | Set a constant value |
| `derive_coalesce(data, target_var, sources, blank_to_na)` | First non-missing from list |
| `derive_if_else(data, target_var, condition, true_value, false_value, missing_value)` | Two-way conditional |
| `derive_case_when(data, target_var, conditions, default)` | Multi-way conditional (first match wins) |
| `derive_regex_extract(data, target_var, source_var, pattern, group, transform)` | Extract via regex |
| `derive_regex_replace(data, target_var, source_var, pattern, replacement, all)` | Replace via regex |
| `derive_concat(data, target_var, sources, sep, na_rm, trim)` | Concatenate columns |
| `derive_trim_pad(data, target_var, source_var, width, side, pad)` | Trim or pad strings |
| `derive_numeric_round(data, target_var, source_var, digits)` | Round numeric values |
| `derive_unit_standardize(data, target_var, source_var, unit_map)` | Standardize units |

### Module G2 — Controlled Terminology (`mod_g2_derive_ct.R`)

| Function | Description |
|----------|-------------|
| `assign_ct(data, target_var, source_var, codelist_id, ct_lib, ...)` | Assign CT coded value |
| `decode_ct(data, target_var, source_var, codelist_id, ct_lib, ...)` | Decode CT to display value |
| `map_yes_no(data, target_var, source_var, yes_values, no_values, na_policy)` | Map Y/N values |
| `map_unknown(data, target_var, source_var, unknown_tokens, sdtm_value)` | Map unknown values |
| `validate_ct_values(data, var, codelist_id, ct_lib, check_type, ...)` | Validate against CT |

### Module G3 — Dates & Times (`mod_g3_derive_dates.R`)

| Function | Description |
|----------|-------------|
| `parse_partial_date(x, formats, partial, unknown_tokens, day_first)` | Parse partial dates |
| `combine_date_time(date, time, seconds, tz)` | Combine date + time |
| `format_iso_dtc(parsed, time, keep_partial, impute)` | Format as ISO 8601 DTC |
| `derive_dy(data, target_var, dtc_var, ref_var, impute_policy)` | Calculate study day |
| `derive_epoch(data, target_var, dtc_var, epoch_map, ref_var, by)` | Assign EPOCH |
| `derive_duration(data, target_var, start_dtc, end_dtc, units)` | Calculate duration |
| `apply_imputation_policy(parsed, policy)` | Apply date imputation |

### Module G4 — Visits & Timing (`mod_g4_derive_visits.R`)

| Function | Description |
|----------|-------------|
| `derive_visit(data, target_var, visit_map, dy_var, by)` | Derive VISIT from study day |
| `derive_visitnum(data, target_var, visit_map, visit_var)` | Derive VISITNUM from VISIT |
| `derive_visitdy(data, target_var, visit_var, dy_var)` | Derive VISITDY |
| `derive_tpt(data, target_var, source_var, tpt_map)` | Derive timepoint |
| `derive_elapsed_time(data, target_var, start_var, end_var, units)` | Calculate elapsed time |
| `derive_rel_time(data, target_var, dtc_var, ref_dtc_var)` | Calculate relative time |

### Module G5 — Identifiers & Sequences (`mod_g5_derive_ids.R`)

| Function | Description |
|----------|-------------|
| `derive_usubjid(data, studyid, subjid_col, sep, validate_existing)` | Derive USUBJID |
| `derive_domain_keys(data, domain, config, idvar, idvarval_col)` | Derive domain key variables |
| `derive_seq(data, target_var, by, order_by, ties, start_at)` | Generate --SEQ |
| `derive_spid(data, target_var, source_cols, sep, prefix)` | Derive SPID |
| `derive_grpid(data, target_var, group_by, method)` | Derive GRPID |

### Module G6 — Flags & Status (`mod_g6_derive_flags.R`)

| Function | Description |
|----------|-------------|
| `derive_baseline_flag(data, target_var, by, baseline_visit, visit_var)` | Derive baseline flag |
| `derive_lastobs_flag(data, target_var, by, order_var)` | Derive last-observation flag |
| `derive_occurrence(data, target_var, source_var, present_value, absent_value)` | Flag presence/absence |
| `derive_status(data, target_var, result_var, done_value, not_done_value)` | Derive completion status |
| `derive_reason(data, target_var, source_var, stat_var)` | Derive reason variable |

### Module H — Domain Builders (`mod_h_builders.R`)

| Function | Description |
|----------|-------------|
| `build_domain(domain, target_meta, source_meta, raw_data, config, rule_set, dm_data, sv_data, ...)` | **Main entry point** — build a complete SDTM domain |
| `build_domain_from_sources(domain, rule_set, raw_data, source_meta, config)` | Build domain from multiple sources |
| `apply_domain_rules(data, domain_rules, context, stop_on_error)` | Apply derivation rules |
| `derive_variable(data, var, rule, context)` | **Dispatcher** — routes to the correct derivation function by `rule_type` |
| `finalize_domain(data, domain, target_meta, config, ...)` | Select/order/label/type columns, enforce lengths |
| `derive_domain_defaults(data, domain, config)` | Set domain defaults |
| `build_supp(domain_data, domain, target_meta, vars_to_supp, ...)` | Build SUPP-- dataset |
| `build_relrec(relationship_specs, domain_data, config)` | Build RELREC rows |
| `build_dm_plugin(target_meta, source_meta, raw_data, config, rule_set)` | DM-specific builder |
| `build_ta_tv_te_ts_plugins(domains, target_meta, config, rule_set, ...)` | Trial-design domain plugins |
| `build_sv_plugin(target_meta, source_meta, raw_data, config, rule_set)` | SV domain plugin |

### Module I — Validation (`mod_i_validation.R`)

| Function | Description |
|----------|-------------|
| `validate_domain_structure(data, target_meta, domain, config, ct_lib, checks)` | Run all validation checks |
| `validate_required_vars(data, target_meta, domain, report)` | Check Req variables present |
| `validate_keys_unique(data, target_meta, domain, report)` | Check key uniqueness |
| `validate_iso8601(data, target_meta, domain, report)` | Check ISO 8601 format |
| `validate_lengths_types_labels(data, target_meta, domain, report)` | Check lengths, types, labels |
| `validate_ct_conformance(data, target_meta, domain, ct_lib, report)` | Check CT conformance |
| `validate_value_level(data, vlm_meta, domain, report)` | Check value-level metadata |
| `validate_cross_domain(domain_data, config)` | Cross-domain consistency |
| `summarize_validation_report(report, verbose)` | Summarize a report |
| `emit_log_messages(report, sink)` | Emit report to log |
| `validate_domain_value(data, domain, report)` | Check DOMAIN column |
| `validate_studyid_constant(data, domain, report)` | Check STUDYID constant |
| `validate_no_allna_reqexp(data, target_meta, domain, report)` | Check no all-NA required vars |
| `validate_seq_integrity(data, domain, report)` | Check --SEQ integrity |
| `validate_no_duplicate_rows(data, domain, report)` | Check for duplicate rows |

### Module J — Code Generation (`mod_j_codegen.R`)

| Function | Description |
|----------|-------------|
| `gen_domain_script(domain, rule_set, target_meta, source_meta, config, style, include_comments, output_path)` | Generate a standalone R script for a domain |
| `gen_shared_utils_script(config, output_path)` | Generate shared utility functions script |
| `gen_qmd_domain(domain, rule_set, target_meta, build_result, output_path)` | Generate Quarto document for a domain |
| `gen_project_scaffold(output_dir, config, domains)` | Generate full project scaffold |
| `render_rule_comments(rule)` | Render comments for a rule |
| `serialize_rules_to_yaml(rule_set, output_path)` | Export rules as YAML |
| `serialize_rules_to_json(rule_set, output_path)` | Export rules as JSON |

### Module K — Export (`mod_k_export.R`)

| Function | Description |
|----------|-------------|
| `export_xpt(data, domain, output_dir, target_meta, max_label_length, version)` | Export to SAS XPT v5 |
| `export_rds_csv(data, domain, output_dir, formats)` | Export to RDS and/or CSV |
| `write_define_support(domains, target_meta, config, output_path)` | Write define.xml support CSV |
| `write_codelist_support(ct_lib, output_path)` | Write codelist support CSV |
| `write_value_level_support(vlm, output_path)` | Write VLM support CSV |
| `write_origin_support(target_meta, output_path)` | Write origin support CSV |

### Module L — Utilities (`mod_l_utils.R`)

| Function | Description |
|----------|-------------|
| `log_info(message, ..., module, domain, .sink)` | Log info message |
| `log_warn(message, ..., module, domain, .sink)` | Log warning |
| `log_error(message, ..., module, domain, .sink)` | Log error |
| `stop_with_context(message, domain, variable, rule_id, data, call)` | Error with context |
| `warn_with_context(message, domain, variable, rule_id, data, call)` | Warning with context |
| `assert_cols(data, cols, context)` | Assert columns exist |
| `assert_types(data, type_map, context)` | Assert column types |
| `snapshot_dataset(data, label, outdir, domain, step)` | Save a snapshot for debugging |

### Top-Level Functions

| Function | Description |
|----------|-------------|
| `make_dummy_study(seed, n_subjects, bad_case, starter_kit_dir)` | Generate a complete dummy study with 10 raw datasets + metadata |
| `check_end_to_end(verbose, return_data, starter_kit_dir, output_dir)` | Run end-to-end pipeline verification on all 10 domains |

---

## Adding a New Domain

To add a new SDTM domain (e.g., **FA**), you need to edit three metadata
files.  **No code changes to the package are required** — the metadata drives
everything.

### 1. Add raw data

Either add a generator to `make_dummy_study()` or supply your own raw data as
a named element in the `raw_data` list.

### 2. Add source\_meta rows

In `source_meta.csv`, describe each raw column:

```csv
fa_raw,usubjid,character,Unique Subject Identifier,Y,
fa_raw,fatestcd,character,Assessment Test Short Name,N,e.g. TUMSTATE
fa_raw,faorres,character,Result or Finding in Original Units,N,
fa_raw,fadat,character,Date of Assessment,N,
```

### 3. Add target\_meta rows

In `target_meta.csv`, define each SDTM variable with its derivation rule:

```csv
FA,STUDYID,char,20,Study Identifier,Identifier,Req,,1,Y,N,constant,"{""value"":""STUDY-XYZ""}",
FA,DOMAIN,char,2,Domain Abbreviation,Identifier,Req,,2,Y,N,constant,"{""value"":""FA""}",
FA,USUBJID,char,40,Unique Subject Identifier,Identifier,Req,,3,Y,N,direct_map,"{""dataset"":""fa_raw"",""column"":""usubjid""}",
FA,FASEQ,num,8,Sequence Number,Identifier,Req,,4,Y,N,seq,"{""by"":[""STUDYID"",""USUBJID""],""order_by"":[""FADTC"",""FATESTCD""],""ties"":""dense""}",FADTC;FATESTCD
FA,FATESTCD,char,8,Assessment Short Name,Topic,Req,,5,Y,N,direct_map,"{""dataset"":""fa_raw"",""column"":""fatestcd"",""transform"":""toupper""}",
FA,FATEST,char,40,Assessment Test Name,Qualifier,Req,FATEST,6,N,N,ct_decode,"{""column"":""FATESTCD"",""codelist_id"":""FATEST""}",FATESTCD
FA,FAORRES,char,200,Result in Original Units,Result,Exp,,7,N,N,direct_map,"{""dataset"":""fa_raw"",""column"":""faorres""}",
FA,FADTC,char,19,Date/Time of Assessment,Timing,Exp,,8,N,N,iso_dtc,"{""date_col"":{""dataset"":""fa_raw"",""column"":""fadat""}}",
FA,FADY,num,8,Study Day of Assessment,Timing,Perm,,9,N,N,dy,"{""dtc_var"":""FADTC"",""ref_var"":""RFSTDTC""}",FADTC
FA,EPOCH,char,20,Epoch,Timing,Perm,,10,N,N,epoch,"{""dtc_var"":""FADTC"",""ref_var"":""RFSTDTC""}",FADTC
```

### 4. Add CT rows (if needed)

If the domain uses controlled terminology not yet in `ct_codelist.csv`:

```csv
FATEST,Assessment Test Name,TUMSTATE,TUMSTATE,Tumor State,N,FA decode
```

### 5. Build

```r
fa <- build_domain("FA", target_meta, source_meta, raw_data, config, rule_set,
                   dm_data = dm$data)
```

---

## Validation

`build_domain()` automatically validates each built domain.  The validation
report contains findings from these checks:

| Check | Description |
|-------|-------------|
| Required variables | All `core = "Req"` variables must exist and have data |
| Key uniqueness | No duplicate keys |
| ISO 8601 format | DTC variables are valid ISO 8601 |
| Lengths & types | Character lengths respected, types correct |
| CT conformance | Coded values match the codelist |
| DOMAIN column | DOMAIN = domain code on every row |
| STUDYID constant | Single STUDYID value per domain |
| SEQ integrity | --SEQ is sequential per subject |
| No duplicate rows | Full row uniqueness |
| No all-NA Req/Exp | Required/Expected variables have some data |

```r
# Access the validation report from build results
report <- ae$report
print(report)

# Or run stand-alone validation
report <- validate_domain_structure(ae$data, target_meta, "AE", config, ct_lib)
summarize_validation_report(report)
```

---

## Code Generation

Generate human-readable R scripts that reproduce the domain derivation:

```r
# Single domain script
gen_domain_script("AE", rule_set, target_meta, source_meta, config,
                  output_path = "programs/ae.R")

# Quarto document with narrative
gen_qmd_domain("AE", rule_set, target_meta, build_result = ae,
               output_path = "docs/ae.qmd")

# Full project scaffold
gen_project_scaffold("output/sdtm_project/", config,
                     domains = c("DM", "AE", "CM"))

# Export rules as machine-readable formats
serialize_rules_to_yaml(rule_set, "output/rules.yaml")
serialize_rules_to_json(rule_set, "output/rules.json")
```

---

## Export

```r
# SAS Transport v5 (.xpt)
export_xpt(ae$data, "AE", "output/xpt/", target_meta = target_meta)

# RDS + CSV
export_rds_csv(ae$data, "AE", "output/", formats = c("rds", "csv"))

# define.xml support files
write_define_support(list(dm$data, ae$data), target_meta, config,
                     "output/define_support.csv")
write_codelist_support(ct_lib, "output/codelist_support.csv")
write_origin_support(target_meta, "output/origin_support.csv")
```

---

## Running the Demo

### Option 1 — End-to-End Check (fastest)

```r
library(sdtmbuilder)
result <- check_end_to_end(verbose = TRUE, return_data = TRUE)
# Builds all 10 domains: DM, AE, CM, MH, PR, EX, VS, LB, DS, QS
# Prints validation results for each domain
```

### Option 2 — Full Pipeline Script

```r
source(system.file("examples", "demo_full_pipeline.R", package = "sdtmbuilder"))
```

This script:

1. Generates dummy data for 30 subjects across 10 raw datasets
2. Compiles 146 derivation rules from metadata
3. Builds all 10 SDTM domains
4. Demonstrates SUPP datasets (SUPPAE)
5. Runs validation on each domain
6. Generates standalone R scripts for each domain
7. Exports XPT files
8. Prints a summary table

### Option 3 — Interactive Step-by-Step

```r
library(sdtmbuilder)

# Generate data
study <- make_dummy_study(seed = 42)

# Inspect metadata
View(study$target_meta)  # all 146 variable definitions
View(study$source_meta)  # raw data column descriptions
View(study$ct_lib)       # controlled terminology

# Compile rules
rs <- compile_rules(study$target_meta, study$source_meta, study$ct_lib)
print(rs)

# Build DM
dm <- build_domain("DM", study$target_meta, study$source_meta,
                   study$raw_data, study$config, rs)
head(dm$data)

# Build AE (with SUPP)
ae <- build_domain("AE", study$target_meta, study$source_meta,
                   study$raw_data, study$config, rs, dm_data = dm$data)
print(ae$report)    # validation
head(ae$supp)       # SUPPAE

# DS domain — demonstrates case_when, if_else, coalesce, concat
ds <- build_domain("DS", study$target_meta, study$source_meta,
                   study$raw_data, study$config, rs, dm_data = dm$data)
table(ds$data$DSCAT)   # PROTOCOL MILESTONE vs DISPOSITION EVENT
table(ds$data$DSSCAT)  # INFORMED CONSENT vs STUDY TREATMENT

# QS domain — demonstrates ct_decode, occurrence, baseline_flag
qs <- build_domain("QS", study$target_meta, study$source_meta,
                   study$raw_data, study$config, rs, dm_data = dm$data)
table(qs$data$QSTEST)  # decoded from QSTESTCD via codelist
table(qs$data$QSSTAT, useNA = "always")  # NA (done) vs NOT DONE
table(qs$data$QSBLFL, useNA = "always")  # Y for baseline visits

# Generate a reproducible script for AE
gen_domain_script("AE", rs, study$target_meta, study$source_meta,
                  study$config, output_path = "ae_program.R")
cat(readLines("ae_program.R"), sep = "\n")
```

---

## Domains Included

The starter kit includes complete metadata and dummy data for **10 CDISC
SDTM domains**:

| Domain | Rows | Description | Rule Types Exercised |
|--------|------|-------------|---------------------|
| DM | 30 | Demographics | constant, direct\_map, ct\_assign |
| AE | 64 | Adverse Events | constant, direct\_map, ct\_assign, iso\_dtc, dy, seq, epoch |
| CM | 36 | Concomitant Medications | ct\_assign, iso\_dtc, dy, seq, epoch |
| MH | 71 | Medical History | direct\_map, iso\_dtc, seq |
| PR | 53 | Procedures | ct\_assign, iso\_dtc, dy, seq, epoch |
| EX | 188 | Exposure | ct\_assign, iso\_dtc, dy, seq, epoch |
| VS | 750 | Vital Signs | direct\_map, iso\_dtc, dy, visit, visitnum, baseline\_flag, seq, epoch |
| LB | 600 | Lab Results | direct\_map, iso\_dtc, dy, visit, visitnum, baseline\_flag, seq, epoch |
| DS | 90 | Disposition | **coalesce**, **case\_when**, **if\_else**, **concat**, iso\_dtc, dy, seq, epoch |
| QS | 360 | Questionnaires | **ct\_decode**, **occurrence**, visit, visitnum, baseline\_flag, iso\_dtc, dy, seq, epoch |

**16 out of 19** rule types are exercised in the starter kit.  The remaining
three (`join`, `status`, `duration`) are fully implemented and unit-tested but
not yet used in the 10-domain metadata.

---

## Running Tests

```r
devtools::test()
# Expected: FAIL 0 | WARN 0 | SKIP 0 | PASS 289
```

---

## Troubleshooting

### "Domain 'XX' not found in target\_meta"

The domain abbreviation must match a `domain` value in target\_meta exactly
(case-sensitive).

### "No compiled rules for domain 'XX'"

Run `compile_rules()` with target\_meta that includes rows for this domain.

### "Source dataset 'xx\_raw' not found in raw\_data"

The `dataset` value in `rule_params` JSON must match a key in the `raw_data`
named list.

### DY / VISIT / EPOCH columns are all NA

The domain needs RFSTDTC from DM.  Pass `dm_data = dm$data` (or the full DM
build result — it will be auto-extracted) when building non-DM domains.

### Controlled terminology values not mapping

Check that `codelist_id` in target\_meta matches a `codelist_id` in
ct\_codelist.  Check `case_sensitive` — set to `"N"` for case-insensitive
matching.

### Excel files not loading

Ensure the `readxl` package is installed (`install.packages("readxl")`).
The metadata readers automatically detect `.xlsx` vs `.csv` by file extension.

---

## License

MIT
