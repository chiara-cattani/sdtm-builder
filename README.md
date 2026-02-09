# sdtmbuilder

<!-- badges: start -->
[![R-CMD-check](https://github.com/chiara-cattani/sdtm-builder/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/chiara-cattani/sdtm-builder/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

> **A metadata-driven R package for building SDTM domains from raw clinical
> data.**

`sdtmbuilder` turns two structured Excel workbooks — **Study\_Metadata.xlsx**
and **Study\_CT.xlsx** — into validated SDTM datasets.  Every derivation rule
is declared in the metadata — **no hand-coding per domain** — so a single
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

# 1. Generate dummy study data (30 subjects, raw datasets + metadata)
study <- make_dummy_study(seed = 42)

# 2. Compile derivation rules from metadata
rule_set <- compile_rules(study$target_meta, ct_lib = study$ct_lib)
print(rule_set)

# 3. Build ALL domains in dependency order (DM first, then the rest)
results <- build_all_domains(
  target_meta = study$target_meta,
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
  raw_data    = study$raw_data,
  config      = study$config,
  rule_set    = rule_set,
  create_supp = FALSE
)

# 6. Or build a single domain manually
ae <- build_domain("AE", study$target_meta, study$raw_data,
                   study$config, rule_set, dm_data = results$DM$data)

# 7. Export
export_xpt(results$AE$data, "AE", "output/")
```

---

## Architecture

```
+------------------------------------------------------------------+
|                         Metadata Layer                            |
|  Study_Metadata.xlsx              Study_CT.xlsx                  |
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
| B | `mod_b_metadata.R` | Validate, normalize metadata |
| — | `read_study_metadata_excel.R` | Read multi-sheet Study\_Metadata.xlsx |
| — | `read_study_ct_excel.R` | Read multi-sheet Study\_CT.xlsx |
| — | `method_mapping.R` | Map METHOD column to internal rule\_type values |
| — | `validate_study_metadata.R` | Cross-sheet validation |
| C | `mod_c_rules.R` | Parse rule\_params, auto-assign rule\_type, compile into rule\_set |
| D | `mod_d_dependency.R` | Build dependency graph, topological sort |
| E | `mod_e_data_access.R` | Load raw data, standardize, `infer_source_meta()`, `load_raw_datasets()` |
| F | `mod_f_joins.R` | Safe joins, key resolution, record assembly |
| G1–G6 | `mod_g*.R` | Derivation functions (mapping, CT, dates, visits, IDs, flags) |
| H | `mod_h_builders.R` | `build_domain()`, `derive_variable()`, `finalize_domain()`, SUPP/RELREC, plugins |
| I | `mod_i_validation.R` | Domain-level validation (15 checks) |
| J | `mod_j_codegen.R` | Generate R scripts, Quarto docs, YAML/JSON serialization |
| K | `mod_k_export.R` | Export to XPT, RDS, CSV; define.xml support files |
| L | `mod_l_utils.R` | Logging, error helpers, assertions, snapshots |

---

## Workflow

### Step 1 — Prepare Metadata

The package ships with starter-kit metadata in `inst/extdata/starter_kit/`:

- **`Study_Metadata.xlsx`** — Sheets: Meta, Standards, Domains, Variables, Value Level, Where Clauses, Method
- **`Study_CT.xlsx`** — Sheets: Codelists, Codelists\_terms
- **`config.yaml`** — Study-wide configuration (studyid, timezone, visit map, etc.)

```r
# Read study metadata from multi-sheet Excel workbook
study_meta <- read_study_metadata_excel("metadata/Study_Metadata.xlsx")
# Returns: list(study_name, target_meta, domain_meta, value_level_meta)

# Read controlled terminology from multi-sheet Excel workbook
ct_lib <- read_study_ct_excel("metadata/Study_CT.xlsx")
# Returns: flat tibble with codelist_id, coded_value, input_value, decode, ...

# Validate cross-references between sheets
validate_study_metadata(study_meta, ct_lib)
```

> **Note:** Source metadata (column descriptions for raw datasets) is
> automatically inferred from your raw data via `infer_source_meta()`. You do
> **not** need a separate source metadata file.

### Step 2 — Create Configuration

```r
config <- new_sdtm_config(
  studyid        = "STUDY-XYZ",
  timezone       = "UTC",
  ref_start_rule = list(var = "rfstdtc", source = "dm_raw"),
  visit_map      = visit_map_tibble,
  create_supp    = TRUE
)
```

### Step 3 — Compile Rules

```r
rule_set <- compile_rules(target_meta, ct_lib = ct_lib)
print(rule_set)
```

The compiler reads `rule_type` and `rule_params` (JSON) from each row of
`target_meta`.   When the `METHOD` column is NA, the compiler auto-assigns
rule types: `STUDYID`/`DOMAIN` → `"constant"`, all others → `"direct_map"`
with convention-based parameters pointing to `{domain_lower}_raw`.

### Step 4 — Build Domains

```r
# Build all domains in dependency order
results <- build_all_domains(
  target_meta = target_meta,
  raw_data    = raw_data,
  config      = config,
  rule_set    = rule_set
)

# Or build individually
dm <- build_domain("DM", target_meta, raw_data, config, rule_set)
ae <- build_domain("AE", target_meta, raw_data, config, rule_set,
                   dm_data = dm$data)
```

### Step 5 — Validate & Export

```r
export_xpt(ae$data, "AE", "output/xpt/", target_meta = target_meta)
gen_domain_script("AE", rule_set, target_meta, config,
                  output_path = "programs/ae.R")
```

---

## Metadata Files

> **Extra columns are tolerated.** Metadata readers only require certain
> columns. Any additional columns are silently preserved.

### Study\_Metadata.xlsx

#### Sheet: Meta

| Column | Required | Description |
|--------|----------|-------------|
| `Select` | **Yes** | `Y` for the active study row |
| `STUDY_NAME` | **Yes** | Study identifier |

#### Sheet: Domains

| Column | Required | Description |
|--------|----------|-------------|
| `Select` | **Yes** | `Y` to include this domain |
| `CLASS_ORDER` | **Yes** | Sort order by domain class |
| `DOMAIN_LEVEL_ORDER` | **Yes** | Sort order within class |
| `CLASS` | **Yes** | Domain class |
| `DOMAIN` | **Yes** | Domain abbreviation |
| `KEYS` | No | Comma-separated sort keys |
| `DESCRIPTION` | No | Dataset label |

#### Sheet: Variables

| Column | Required | Description |
|--------|----------|-------------|
| `Select` | **Yes** | `Y` to include this variable |
| `DOMAIN` | **Yes** | Domain abbreviation |
| `VARNAME` | **Yes** | Full variable name |
| `VARLABEL` | **Yes** | Variable label |
| `DATA_TYPE` | **Yes** | `text`, `integer`, `float`, `datetime`, `durationDatetime` |
| `LENGTH` | No | Maximum length |
| `SIGNIFICANT_DIGITS` | No | Decimal precision for numerics |
| `CODELIST_ID` | No | Codelist identifier |
| `ROLE` | No | CDISC role |
| `CORE` | No | `Req`, `Exp`, or `Perm` |
| `VLM_ID` | No | Value-level metadata identifier |
| `METHOD` | No | Derivation method (SEQ, USUBJID, DY, etc.) |

#### Sheet: Value Level

| Column | Required | Description |
|--------|----------|-------------|
| `Select` | **Yes** | `Y` to include |
| `DOMAIN` | **Yes** | Domain abbreviation |
| `VARNAME` | **Yes** | Variable the VLM applies to |
| `VLM_ID` | **Yes** | Matches VLM\_ID in Variables sheet |
| `WHERE_CLAUSE_ID` | **Yes** | Links to Where Clauses sheet |

#### Sheet: Where Clauses

| Column | Required | Description |
|--------|----------|-------------|
| `WHERE_CLAUSE_ID` | **Yes** | Unique identifier |
| `DOMAIN` | **Yes** | Domain abbreviation |
| `VARNAME` | **Yes** | Variable to filter on |
| `COMPARATOR` | **Yes** | `EQ`, `IN`, `NE`, `NOTIN` |
| `VALUE` | **Yes** | Value(s) — pipe-separated for IN |

### Study\_CT.xlsx

#### Sheet: Codelists

| Column | Required | Description |
|--------|----------|-------------|
| `Select` | **Yes** | `Y` to include |
| `CODELIST_ID` | **Yes** | Primary identifier (e.g., `YN`, `SEX`) |
| `IS_EXTENSIBLE` | No | `Yes` or `No` |

#### Sheet: Codelists\_terms

| Column | Required | Description |
|--------|----------|-------------|
| `Select` | **Yes** | `Y` to include |
| `CODELIST_ID` | **Yes** | Matches Codelists sheet |
| `SUBMISSION_VALUE` | **Yes** | The SDTM standard term |
| `DECODE` | No | Collected value |

### SELECT Column

All metadata sheets support a `Select` column. Only rows with `Select = "Y"`
are loaded, allowing you to maintain a global metadata set and select
study-specific subsets.

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

The `rule_type` / `METHOD` column determines how each variable is derived.
The `rule_params` column is a JSON string configuring the derivation.

| Rule type | Purpose | Example params |
|-----------|---------|----------------|
| `constant` | Fixed value | `{"value": "STUDY-XYZ"}` |
| `direct_map` | Copy + transform | `{"dataset": "ae_raw", "column": "aeterm", "transform": "toupper"}` |
| `ct_assign` | CT coded value | `{"column": "aesev_raw", "codelist_id": "SEV"}` |
| `ct_decode` | CT decode lookup | `{"column": "QSTESTCD", "codelist_id": "QSTEST"}` |
| `iso_dtc` | ISO 8601 date/time | `{"date_col": {...}, "time_col": {...}}` |
| `dy` | Study day | `{"dtc_var": "AESTDTC", "ref_var": "RFSTDTC"}` |
| `seq` | Sequence number | `{"by": ["USUBJID"], "order_by": ["AESTDTC"]}` |
| `epoch` | Epoch assignment | `{"dtc_var": "AESTDTC", "ref_var": "RFSTDTC"}` |
| `visit` | Visit from study day | `{"dy_var": "VSDY"}` |
| `visitnum` | VISITNUM from VISIT | `{"visit_var": "VISIT"}` |
| `baseline_flag` | Baseline flag | `{"visit_var": "VISIT", "baseline_visit": "BASELINE"}` |
| `coalesce` | First non-missing | `{"sources": ["col1", "col2"]}` |
| `case_when` | Multi-condition | `{"conditions": {...}, "default": "..."}` |
| `if_else` | Two-way conditional | `{"condition": "...", "true_value": "...", "false_value": "..."}` |
| `concat` | Concatenate | `{"sources": ["STUDYID", "id"], "sep": "-"}` |
| `occurrence` | Presence/absence | `{"source_var": "qsorres"}` |
| `status` | Completion status | `{"result_var": "QSORRES"}` |
| `duration` | ISO duration | `{"start_dtc": "CMSTDTC", "end_dtc": "CMENDTC"}` |
| `join` | Join from dataset | `{"dataset": "dm_raw", "column": "arm"}` |

---

## Function Reference

See `?build_domain`, `?compile_rules`, `?make_dummy_study` for complete
documentation of all exported functions.

### Core Pipeline

| Function | Description |
|----------|-------------|
| `make_dummy_study()` | Generate dummy study with metadata + raw data |
| `compile_rules(target_meta, ct_lib)` | Compile metadata into rule\_set |
| `build_domain(domain, target_meta, raw_data, config, rule_set, ...)` | Build one SDTM domain |
| `build_all_domains(target_meta, raw_data, config, rule_set, ...)` | Build all domains |
| `check_end_to_end()` | End-to-end pipeline verification |

### Metadata

| Function | Description |
|----------|-------------|
| `read_study_metadata_excel(path)` | Read Study\_Metadata.xlsx |
| `read_study_ct_excel(path)` | Read Study\_CT.xlsx |
| `validate_study_metadata(study_meta, ct_lib)` | Validate metadata |
| `infer_source_meta(raw_data)` | Auto-infer source metadata from raw data |
| `load_raw_datasets(dir, ...)` | Load all raw data files from a directory |

### Export & Code Generation

| Function | Description |
|----------|-------------|
| `export_xpt(data, domain, output_dir, ...)` | Export to SAS XPT v5 |
| `export_rds_csv(data, domain, output_dir, ...)` | Export to RDS/CSV |
| `gen_domain_script(domain, rule_set, target_meta, config, ...)` | Generate R script |
| `gen_qmd_domain(...)` | Generate Quarto document |
| `gen_project_scaffold(...)` | Generate full project scaffold |

---

## Adding a New Domain

Edit **Study\_Metadata.xlsx** (Variables + Domains sheets) and optionally
**Study\_CT.xlsx**. No R code changes needed — the metadata drives everything.

---

## Validation

`build_domain()` automatically validates each built domain, checking:
required variables, key uniqueness, ISO 8601 format, lengths/types,
CT conformance, DOMAIN/STUDYID constants, SEQ integrity, and more.

---

## Code Generation

```r
gen_domain_script("AE", rule_set, target_meta, config,
                  output_path = "programs/ae.R")
```

---

## Export

```r
export_xpt(ae$data, "AE", "output/xpt/", target_meta = target_meta)
export_rds_csv(ae$data, "AE", "output/")
```

---

## Running the Demo

```r
library(sdtmbuilder)

# Option 1: End-to-end check
result <- check_end_to_end(verbose = TRUE, return_data = TRUE)

# Option 2: Full pipeline script
source(system.file("examples", "demo_full_pipeline.R", package = "sdtmbuilder"))

# Option 3: Interactive
study <- make_dummy_study(seed = 42)
rs <- compile_rules(study$target_meta, ct_lib = study$ct_lib)
dm <- build_domain("DM", study$target_meta, study$raw_data, study$config, rs)
ae <- build_domain("AE", study$target_meta, study$raw_data,
                   study$config, rs, dm_data = dm$data)
```

---

## Domains Included

The starter kit includes metadata for: **DM**, **AE**, **LB**, **CM**.
Additional domains can be added by editing Study\_Metadata.xlsx.

---

## Running Tests

```r
devtools::test()
# Expected: FAIL 0 | WARN 0 | SKIP 0 | PASS 244
```

---

## Troubleshooting

| Problem | Solution |
|---------|----------|
| "Domain 'XX' not found in target\_meta" | Domain must match exactly (case-sensitive) |
| "No compiled rules for domain 'XX'" | Ensure target\_meta has rows for this domain |
| DY / VISIT / EPOCH all NA | Pass `dm_data = dm$data` for non-DM domains |
| CT values not mapping | Check `codelist_id` matches Study\_CT.xlsx |
| Excel not loading | Install `readxl`: `install.packages("readxl")` |

---

## License

MIT License. Copyright (c) 2025 Chiara Cattani.
