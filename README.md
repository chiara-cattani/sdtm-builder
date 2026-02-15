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
2. [Quick Demo (5 minutes)](#quick-demo-5-minutes)
3. [Using sdtmbuilder on Your Own Study](#using-sdtmbuilder-on-your-own-study)
   - [Step 0 — Set Up Your Folder Structure](#step-0--set-up-your-folder-structure)
   - [Step 1 — Get the Template config.yaml](#step-1--get-the-template-configyaml)
   - [Step 2 — Prepare Your Metadata](#step-2--prepare-your-metadata)
   - [Step 3 — Place Your Raw Data](#step-3--place-your-raw-data)
   - [Step 4 — Run the Pipeline](#step-4--run-the-pipeline)
   - [Step 5 — Inspect Results](#step-5--inspect-results)
   - [Step 6 — Step-by-Step Alternative](#step-6--step-by-step-alternative)
4. [Architecture](#architecture)
5. [Metadata Files Reference](#metadata-files-reference)
6. [Rule Types Reference](#rule-types-reference)
7. [Function Reference](#function-reference)
8. [Adding a New Domain](#adding-a-new-domain)
9. [Validation](#validation)
10. [Export Formats](#export-formats)
11. [Running Tests](#running-tests)
12. [Troubleshooting](#troubleshooting)

---

## Installation

```r
# Install from GitHub
devtools::install_github("chiara-cattani/sdtm-builder")

# Or clone and install locally
devtools::install("path/to/sdtm-builder")
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
| haven     | Read .sas7bdat, export .xpt      |
| yaml      | Read config.yaml                 |
| jsonlite  | Parse rule\_params JSON          |

---

## Quick Demo (5 minutes)

The package ships with built-in dummy data so you can test the full pipeline
immediately — no external files needed.

```r
library(sdtmbuilder)

# 1. Generate dummy study (30 subjects, raw datasets + metadata)
study <- make_dummy_study(seed = 42)

# 2. Compile derivation rules from metadata
rule_set <- compile_rules(study$target_meta, ct_lib = study$ct_lib)
print(rule_set)

# 3. Build ALL domains in dependency order
results <- build_all_domains(
  target_meta = study$target_meta,
  raw_data    = study$raw_data,
  config      = study$config,
  rule_set    = rule_set
)

# 4. Inspect results
head(results$DM$data)       # the SDTM DM dataset
head(results$AE$data)       # the SDTM AE dataset
results$AE$report           # validation report

# 5. Export
export_domain(results$AE$data, "AE", "output/")

# Or use the built-in end-to-end check
result <- check_end_to_end(verbose = TRUE)
```

See also the full demo script:

```r
source(system.file("examples", "demo_full_pipeline.R", package = "sdtmbuilder"))
```

---

## Using sdtmbuilder on Your Own Study

This section walks you through using `sdtmbuilder` on your **real study data**.

### Step 0 — Set Up Your Folder Structure

Create the following folder structure for your study:

```
my_study/
├── config.yaml                  <- study configuration (template provided)
├── metadata/
│   ├── Study_Metadata.xlsx      <- your target metadata + domains + VLM
│   └── Study_CT.xlsx            <- your controlled terminology
├── raw/
│   ├── dm.sas7bdat              <- raw DM data (or .csv, .xpt, .xlsx)
│   ├── ae.sas7bdat              <- raw AE data
│   ├── cm.sas7bdat              <- raw CM data
│   ├── lb.sas7bdat              <- raw LB data
│   └── ...                      <- any other raw datasets
└── sdtm/
    ├── datasets/                <- OUTPUT: XPT + RDA files (created automatically)
    └── programs/                <- OUTPUT: generated R scripts (created automatically)
```

**Notes on raw data file naming:**

- Files can have **any name** — the file name (without extension) becomes the
  dataset name used in the pipeline (e.g., `dm.sas7bdat` → `dm`, `lbs.sas7bdat` → `lbs`).
- Supported formats: `.sas7bdat`, `.csv`, `.xlsx`, `.xls`, `.xpt`, `.rds`, `.rda`.
- If your raw file is named the same as the final SDTM domain (e.g., `dm.sas7bdat`),
  the auto-mapping convention `{domain}_raw` won't match.  You can either:
  - Rename files to `dm_raw.sas7bdat`, `ae_raw.sas7bdat`, etc. (recommended), **or**
  - Set explicit `dataset` in the METHOD column of Study_Metadata.xlsx.

### Step 1 — Get the Template config.yaml

```r
library(sdtmbuilder)

# Copy the template to your study folder
get_template_config(copy_to = "my_study/config.yaml")
```

This creates a `config.yaml` with all available settings and comments.
Open it in any editor and fill in:

| Field | What to fill in |
|-------|-----------------|
| `studyid` | Your study identifier (e.g., `"ABC-001"`) |
| `paths.metadata` | Path to Study\_Metadata.xlsx (default: `metadata/Study_Metadata.xlsx`) |
| `paths.ct` | Path to Study\_CT.xlsx (default: `metadata/Study_CT.xlsx`) |
| `paths.raw_data` | Directory with raw data files (default: `raw`) |
| `paths.output` | Where to write SDTM datasets (default: `sdtm/datasets`) |
| `paths.programs` | Where to write R programs (default: `sdtm/programs`) |
| `ref_start_rule` | How to find RFSTDTC — usually `dataset: dm_raw`, `column: rfstdtc` |
| `visit_map` | Your visit schedule (visitnum, visit name, start/end day windows) |
| `epoch_map` | Your epoch definitions (epoch name, start/end day windows) |
| `export.formats` | `["xpt", "rda"]` for both formats (default) |
| `export.generate_programs` | `true` to generate R scripts (default) |

### Step 2 — Prepare Your Metadata

#### Study\_Metadata.xlsx

This Excel workbook has these sheets (all use a `Select = "Y"` column to
activate rows):

| Sheet | Purpose |
|-------|---------|
| **Meta** | Study name, select flag |
| **Domains** | Which domains to build (DM, AE, LB, CM, …), class, build order, keys |
| **Variables** | Every target variable: name, label, type, length, codelist, derivation method |
| **Value Level** | Value-level metadata (e.g., different rules for LBTEST=ALB vs LBTEST=GLUC) |
| **Where Clauses** | Conditions for value-level rows |
| **Method** | Optional method descriptions |

The critical sheet is **Variables**. Each row defines one SDTM variable:

| Column | Required | Example | Description |
|--------|----------|---------|-------------|
| Select | Yes | `Y` | Include this variable |
| DOMAIN | Yes | `AE` | Domain code |
| VARNAME | Yes | `AETERM` | SDTM variable name |
| VARLABEL | Yes | `Reported Term` | Variable label |
| DATA\_TYPE | Yes | `text` | `text`, `integer`, `float`, `datetime` |
| LENGTH | No | `200` | Max character length |
| CORE | No | `Req` | `Req`, `Exp`, `Perm`, or `SUPP` |
| CODELIST\_ID | No | `SEV` | Links to Study\_CT.xlsx |
| METHOD | No | `SEQ` | Derivation method (see below) |

**METHOD column values:**

| METHOD | What it does |
|--------|-------------|
| *(empty/NA)* | Auto-assigned: `STUDYID`/`DOMAIN` → constant, others → direct\_map |
| `SEQ` | Sequence number (AESEQ, CMSEQ, etc.) |
| `USUBJID` | Derives USUBJID from STUDYID + subject ID |
| `DY` | Study day from date vs RFSTDTC |
| `EPOCH` | Epoch from study day |
| `DURATION` | ISO 8601 duration between start/end dates |
| JSON string | Any custom rule, e.g., `{"rule_type": "case_when", ...}` |

#### Study\_CT.xlsx

Two sheets:

| Sheet | Key Columns |
|-------|-------------|
| **Codelists** | `Select`, `CODELIST_ID`, `CODELIST_NAME`, `IS_EXTENSIBLE` |
| **Codelists\_terms** | `Select`, `CODELIST_ID`, `SUBMISSION_VALUE`, `DECODE` |

Example terms:

| CODELIST\_ID | SUBMISSION\_VALUE | DECODE |
|-------------|-------------------|--------|
| SEV | MILD | Mild |
| SEV | MODERATE | Moderate |
| SEV | SEVERE | Severe |
| YN | Y | Yes |
| YN | N | No |

### Step 3 — Place Your Raw Data

Copy your raw SAS datasets (`.sas7bdat`) into the `raw/` folder.  Any of these
formats work: `.sas7bdat`, `.csv`, `.xlsx`, `.xls`, `.xpt`, `.rds`, `.rda`.

The file name (without extension) becomes the dataset name in the pipeline.
For automatic mapping to work, name them with a `_raw` suffix:

```
raw/
├── dm_raw.sas7bdat    → loaded as "dm_raw" → maps to DM domain
├── ae_raw.sas7bdat    → loaded as "ae_raw" → maps to AE domain
├── cm_raw.sas7bdat    → loaded as "cm_raw" → maps to CM domain
└── lb_raw.sas7bdat    → loaded as "lb_raw" → maps to LB domain
```

Or, if your files are named `dm.sas7bdat`, `ae.sas7bdat` etc., you can
override the dataset mapping in the METHOD column of Study\_Metadata.xlsx:

```json
{"rule_type": "direct_map", "params": {"dataset": "dm", "column": "subjid"}}
```

### Step 4 — Run the Pipeline

```r
library(sdtmbuilder)

# Set working directory to your study root
setwd("C:/Users/you/my_study")

# ONE CALL DOES EVERYTHING:
out <- run_study("config.yaml")
```

This single call:

1. Reads `Study_Metadata.xlsx` and `Study_CT.xlsx`
2. Loads all raw `.sas7bdat` files from `raw/`
3. Compiles derivation rules from the metadata
4. Builds every SDTM domain defined in the metadata
5. Exports each domain as **XPT** and **RDA** to `sdtm/datasets/`
6. Generates an **R script** per domain to `sdtm/programs/`

After running, your folder will look like:

```
my_study/
├── config.yaml
├── metadata/
│   ├── Study_Metadata.xlsx
│   └── Study_CT.xlsx
├── raw/
│   ├── dm_raw.sas7bdat
│   ├── ae_raw.sas7bdat
│   └── ...
└── sdtm/
    ├── datasets/
    │   ├── dm.xpt           <- SAS transport v5
    │   ├── dm.rda           <- R binary format
    │   ├── ae.xpt
    │   ├── ae.rda
    │   ├── lb.xpt
    │   ├── lb.rda
    │   └── ...
    └── programs/
        ├── dm.R             <- standalone R script to reproduce DM
        ├── ae.R             <- standalone R script to reproduce AE
        └── ...
```

**Options:**

```r
# Datasets only (no R programs generated)
out <- run_study("config.yaml", generate_programs = FALSE)

# Build specific domains only
out <- run_study("config.yaml", domains = c("DM", "AE"))

# Export as XPT only (no RDA)
out <- run_study("config.yaml", export_formats = "xpt")

# Without config.yaml — pass paths directly
out <- run_study(
  metadata_path = "metadata/Study_Metadata.xlsx",
  ct_path       = "metadata/Study_CT.xlsx",
  raw_dir       = "raw",
  output_dir    = "sdtm/datasets",
  programs_dir  = "sdtm/programs"
)
```

### Step 5 — Inspect Results

```r
# What domains were built?
names(out$results)
# [1] "DM" "AE" "LB" "CM"

# Inspect a domain
head(out$results$DM$data)
head(out$results$AE$data)

# Validation report
out$results$AE$report
# → shows any validation errors/warnings (required vars, ISO dates, CT, etc.)

# SUPP datasets (if any)
out$results$AE$supp

# Export paths
out$exported$DM
# $xpt [1] "sdtm/datasets/dm.xpt"
# $rda [1] "sdtm/datasets/dm.rda"

# Generated programs
out$programs$DM
# [1] "sdtm/programs/dm.R"
```

### Step 6 — Step-by-Step Alternative

If you prefer full control over each step, here is the manual pipeline:

```r
library(sdtmbuilder)

# ── 1. Read metadata ─────────────────────────────────────────────────────
study_meta <- read_study_metadata_excel("metadata/Study_Metadata.xlsx")
ct_lib     <- read_study_ct_excel("metadata/Study_CT.xlsx")

target_meta      <- study_meta$target_meta
domain_meta      <- study_meta$domain_meta
value_level_meta <- study_meta$value_level_meta

# Expand value-level metadata (if you use VLM)
if (!is.null(value_level_meta) && nrow(value_level_meta) > 0) {
  target_meta <- expand_value_level_meta(target_meta, value_level_meta)
}

# ── 2. Load raw data (sas7bdat, csv, xpt, ...) ───────────────────────────
raw_data <- load_raw_datasets("raw")
names(raw_data)  # see what was loaded

# ── 3. Configuration ─────────────────────────────────────────────────────
config <- new_sdtm_config(
  studyid        = "MY-STUDY-001",
  timezone       = "UTC",
  ref_start_rule = list(var = "rfstdtc", source = "dm_raw")
)

# ── 4. Compile rules ─────────────────────────────────────────────────────
rule_set <- compile_rules(target_meta, ct_lib = ct_lib)

# ── 5. Build all domains ─────────────────────────────────────────────────
results <- build_all_domains(
  target_meta      = target_meta,
  raw_data         = raw_data,
  config           = config,
  rule_set         = rule_set,
  domain_meta      = domain_meta,
  value_level_meta = value_level_meta
)

# ── 6. Export each domain ─────────────────────────────────────────────────
for (dom in names(results)) {
  export_domain(results[[dom]]$data, dom, "sdtm/datasets",
                formats = c("xpt", "rda"),
                target_meta = target_meta, domain_meta = domain_meta)
}

# ── 7. Generate R programs (optional) ─────────────────────────────────────
for (dom in names(results)) {
  gen_domain_script(dom, rule_set, target_meta, config,
                    output_path = file.path("sdtm/programs", paste0(tolower(dom), ".R")))
}
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
          | compile_rules|           | load_raw     |
          |  (Module C)  |           | _datasets()  |
          +------+------+           +-------+------+
                 | rule_set                 | raw_data
          +------v------------------------------v------+
          |            build_domain()                   |
          |  +----------------------------------------+ |
          |  | derive_variable() dispatcher           | |
          |  |  -> 26 rule types                      | |
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
                |  export_domain()      |
                |  gen_domain_script()  |
                +-----------------------+
```

### Module Map

| Module | File | Purpose |
|--------|------|---------|
| — | `run_study.R` | **High-level orchestrator**: `run_study()`, `get_template_config()` |
| A | `mod_a_primitives.R` | S3 classes: `sdtm_config`, `meta_bundle`, `rule_set`, `validation_report` |
| B | `mod_b_metadata.R` | Read Study\_Metadata.xlsx & Study\_CT.xlsx, validate, normalize |
| — | `method_mapping.R` | Function registry & METHOD column parsing |
| C | `mod_c_rules.R` | Parse rules, auto-assign rule\_type, compile rule\_set |
| D | `mod_d_dependency.R` | Build dependency graph, topological sort |
| E | `mod_e_data_access.R` | Load raw data, `load_raw_datasets()`, `infer_source_meta()` |
| F | `mod_f_joins.R` | Safe joins, key resolution, record assembly |
| G1–G6 | `mod_g*.R` | Derivation functions (mapping, CT, dates, visits, IDs, flags) |
| H | `mod_h_builders.R` | `build_domain()`, `build_all_domains()`, plugins |
| I | `mod_i_validation.R` | Domain-level validation (15 checks) |
| J | `mod_j_codegen.R` | Generate R scripts, Quarto docs |
| K | `mod_k_export.R` | Export to XPT, RDA, RDS, CSV; define.xml support files |
| L | `mod_l_utils.R` | Logging, error helpers, assertions, snapshots |

---

## Metadata Files Reference

### Study\_Metadata.xlsx

| Sheet | Key Columns | Purpose |
|-------|-------------|---------|
| Meta | Select, STUDY\_NAME | Study-level info |
| Domains | Select, DOMAIN, CLASS, CLASS\_ORDER, DOMAIN\_LEVEL\_ORDER, KEYS | Domain definitions + build order |
| Variables | Select, DOMAIN, VARNAME, VARLABEL, DATA\_TYPE, LENGTH, CODELIST\_ID, METHOD | Variable definitions + derivation rules |
| Value Level | Select, DOMAIN, VARNAME, VLM\_ID, WHERE\_CLAUSE\_ID | Value-level metadata |
| Where Clauses | WHERE\_CLAUSE\_ID, DOMAIN, VARNAME, COMPARATOR, VALUE | Conditions for VLM |
| Method | (informational) | Derivation descriptions |

### Study\_CT.xlsx

| Sheet | Key Columns | Purpose |
|-------|-------------|---------|
| Codelists | Select, CODELIST\_ID, CODELIST\_NAME, IS\_EXTENSIBLE | Codelist definitions |
| Codelists\_terms | Select, CODELIST\_ID, SUBMISSION\_VALUE, DECODE | Individual terms |

### config.yaml

```yaml
studyid: "MY-STUDY-001"
timezone: "UTC"

paths:
  metadata: "metadata/Study_Metadata.xlsx"
  ct:       "metadata/Study_CT.xlsx"
  raw_data: "raw"
  output:   "sdtm/datasets"
  programs: "sdtm/programs"

ref_start_rule:
  dataset: "dm_raw"
  column:  "rfstdtc"

visit_map:
  - visitnum: 1
    visit: "SCREENING"
    start_day: -14
    end_day: -1

epoch_map:
  - epoch: "SCREENING"
    start_day: null
    end_day: -1

export:
  formats: ["xpt", "rda"]
  generate_programs: true

log_level: "INFO"
```

Get the template:

```r
get_template_config(copy_to = "my_study/config.yaml")
```

---

## Rule Types Reference

| Rule type | METHOD value | Purpose | Example params |
|-----------|-------------|---------|----------------|
| `constant` | (auto for STUDYID/DOMAIN) | Fixed value | `{"value": "STUDY-XYZ"}` |
| `direct_map` | (auto for most vars) | Copy + transform | `{"dataset": "ae_raw", "column": "aeterm"}` |
| `ct_assign` | (auto when CODELIST\_ID set) | CT coded value | `{"column": "aesev_raw", "codelist_id": "SEV"}` |
| `ct_decode` | | CT decode lookup | `{"column": "QSTESTCD", "codelist_id": "QSTEST"}` |
| `iso_dtc` | | ISO 8601 date/time | `{"date_col": {...}, "time_col": {...}}` |
| `dy` | `DY` | Study day | `{"dtc_var": "AESTDTC", "ref_var": "RFSTDTC"}` |
| `seq` | `SEQ` | Sequence number | `{"by": ["USUBJID"], "order_by": ["AESTDTC"]}` |
| `epoch` | `EPOCH` | Epoch assignment | `{"dtc_var": "AESTDTC", "ref_var": "RFSTDTC"}` |
| `visit` | | Visit from study day | `{"dy_var": "VSDY"}` |
| `visitnum` | | VISITNUM from VISIT | `{"visit_var": "VISIT"}` |
| `baseline_flag` | | Baseline flag | `{"visit_var": "VISIT", "baseline_visit": "BASELINE"}` |
| `coalesce` | | First non-missing | `{"sources": ["col1", "col2"]}` |
| `case_when` | | Multi-condition | `{"conditions": {...}, "default": "..."}` |
| `if_else` | | Two-way conditional | `{"condition": "...", "true_value": "..."}` |
| `concat` | `USUBJID` | Concatenate | `{"sources": ["STUDYID", "id"], "sep": "-"}` |
| `duration` | `DURATION` | ISO duration | `{"start_dtc": "CMSTDTC", "end_dtc": "CMENDTC"}` |

---

## Function Reference

### High-Level

| Function | Description |
|----------|-------------|
| `run_study(config_path, ...)` | **One call does everything**: read, build, export, generate programs |
| `get_template_config(copy_to)` | Copy the template config.yaml to your study folder |
| `check_end_to_end()` | End-to-end verification on built-in dummy data |

### Metadata

| Function | Description |
|----------|-------------|
| `read_study_metadata_excel(path)` | Read Study\_Metadata.xlsx |
| `read_study_ct_excel(path)` | Read Study\_CT.xlsx |
| `validate_study_metadata(study_meta, ct_lib)` | Cross-sheet validation |
| `load_raw_datasets(dir, ...)` | Load all raw data files from a directory |
| `infer_source_meta(raw_data)` | Auto-infer source metadata from raw data |

### Core Pipeline

| Function | Description |
|----------|-------------|
| `compile_rules(target_meta, ct_lib)` | Compile metadata into rule\_set |
| `build_domain(domain, target_meta, raw_data, config, rule_set, ...)` | Build one SDTM domain |
| `build_all_domains(target_meta, raw_data, config, rule_set, ...)` | Build all domains |
| `make_dummy_study(seed)` | Generate dummy study for testing |

### Export & Code Generation

| Function | Description |
|----------|-------------|
| `export_domain(data, domain, output_dir, formats, ...)` | Export to XPT v8 (default), RDS, CSV, or RDA |
| `gen_domain_script(domain, rule_set, target_meta, config, ...)` | Generate R script |
| `gen_qmd_domain(...)` | Generate Quarto document |

---

## Adding a New Domain

1. **Study\_Metadata.xlsx → Domains sheet**: add a row with Select=Y, DOMAIN, CLASS, etc.
2. **Study\_Metadata.xlsx → Variables sheet**: add rows for each variable in the domain
3. **Study\_CT.xlsx** (if needed): add codelists for the domain
4. Place the raw data file in `raw/` (e.g., `xx_raw.sas7bdat`)
5. Re-run `run_study("config.yaml")` — the new domain is picked up automatically

No R code changes required.

---

## Validation

`build_domain()` automatically validates each built domain, checking:

- Required variables present
- Key uniqueness (no duplicate records for the same keys)
- ISO 8601 date format (`--DTC` variables)
- Character lengths and numeric types match metadata
- Controlled terminology conformance
- STUDYID and DOMAIN constants
- SEQ integrity (unique, sequential)
- No all-NA required/expected columns
- Value-level metadata conformance

Validation results are in `result$report` for each domain.

---

## Export Formats

Each domain can be exported in multiple formats:

| Format | Function | Extension | Description |
|--------|----------|-----------|-------------|
| XPT | `export_domain(..., formats = "xpt")` | `.xpt` | SAS transport v8 (default, for regulatory submission) |
| RDA | `export_domain(..., formats = "rda")` | `.rda` | R binary (preserves attributes, labels) |
| RDS | `export_domain(..., formats = "rds")` | `.rds` | R single-object binary |
| CSV | `export_domain(..., formats = "csv")` | `.csv` | Plain text |

`run_study()` exports **XPT + RDA** by default. Change with `export_formats`:

```r
run_study("config.yaml", export_formats = c("xpt", "rda", "csv"))
```

---

## Running Tests

```r
devtools::test()
# Expected: FAIL 0 | WARN 0 | SKIP 0 | PASS 263
```

---

## Troubleshooting

| Problem | Solution |
|---------|----------|
| `could not find function "run_study"` | Run `devtools::load_all()` or reinstall the package |
| `could not find function "read_study_metadata_excel"` | Same — reload with `devtools::load_all()` |
| "Domain 'XX' not found in target\_meta" | Check DOMAIN column in Variables sheet (case-sensitive) |
| "No compiled rules for domain 'XX'" | Ensure target\_meta has rows with Select=Y for this domain |
| DY / VISIT / EPOCH all NA | Pass `dm_data = dm$data` for non-DM domains (automatic in `build_all_domains`) |
| CT values not mapping | Check `CODELIST_ID` matches between Variables and Study\_CT.xlsx |
| Raw data column not found | Confirm column names in raw data match the lower-case variable names |
| "File not found" for Excel | Check the paths in config.yaml or function arguments |
| R programs not generated | Set `generate_programs = TRUE` (default), or check `export.generate_programs` in config.yaml |

---

## License

MIT License. Copyright (c) 2026 Chiara Cattani.
