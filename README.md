# sdtmbuilder

<!-- badges: start -->
<!-- badges: end -->

> **Metadata-Driven SDTM Domain Builder and Code Generator**

`sdtmbuilder` auto-generates SDTM domain datasets and R build scripts from
structured metadata. Point it at your target metadata, source metadata,
controlled terminology, and raw clinical data — it builds every domain,
validates the output, and exports XPT files.

---

## Installation

```r
# Install from GitHub (requires devtools)
devtools::install_github("chiara-cattani/sdtm-builder")
```

Or clone and install locally:

```r
# From the cloned repo root
devtools::install(".")
```

---

## Quick Start (Dummy Data)

The package ships with a starter kit that lets you see the full pipeline in
action *without any real data*:

```r
library(sdtmbuilder)

# 1. Run the end-to-end check (builds all 8 domains)
result <- check_end_to_end(verbose = TRUE, return_data = TRUE)
# Prints: DM -> PASS, AE -> PASS, CM -> PASS, MH -> PASS,
#         PR -> PASS, EX -> PASS, VS -> PASS, LB -> PASS

# 2. Inspect built domains
str(result$DM$data)   # 30 rows x 16 columns (Demographics)
str(result$AE$data)   # 65 rows x 12 columns (Adverse Events)
str(result$VS$data)   # 750 rows x 16 columns (Vital Signs)
str(result$LB$data)   # 600 rows x 15 columns (Laboratory)

# 3. Or run the full demo script
source(system.file("examples", "demo_full_pipeline.R", package = "sdtmbuilder"))
```

---

## Using with Real Data

### Step 1 — Prepare Your Metadata Files

You need **four** files. Use the starter kit as templates
(`inst/extdata/starter_kit/`):

#### `target_meta.csv`

One row per SDTM variable you want to produce. Required columns:

| Column        | Description                                         | Example           |
|---------------|-----------------------------------------------------|-------------------|
| `domain`      | Two-letter SDTM domain code                         | `AE`              |
| `var`         | SDTM variable name                                  | `AESTDTC`         |
| `label`       | Variable label (max 40 chars for XPT)                | `Start Date/Time` |
| `type`        | `char` or `num`                                      | `char`            |
| `core`        | `REQ`, `PERM`, or `EXP`                              | `REQ`             |
| `rule_type`   | Derivation type (see table below)                    | `iso_dtc`         |
| `rule_params` | JSON object with derivation parameters               | (see below)       |

**Supported `rule_type` values:**

| `rule_type`   | What it does                                     | Key `rule_params` fields                                   |
|---------------|--------------------------------------------------|-------------------------------------------------------------|
| `constant`    | Set a fixed value                                | `{"value": "AE"}`                                           |
| `direct_map`  | Copy from source column                          | `{"dataset": "ae_raw", "column": "aeterm"}`                 |
| `ct_assign`   | Map through controlled terminology               | `{"dataset":"ae_raw","column":"aesev","codelist_id":"C66769"}` |
| `iso_dtc`     | Parse date to ISO 8601                           | `{"date_col": {"dataset":"ae_raw","column":"aestdt"}}`      |
| `dy`          | Compute study day (no Day 0)                     | `{"dtc_var":"AESTDTC","ref_var":"RFSTDTC"}`                 |
| `seq`         | Sequence number within subject                   | `{"by":["USUBJID"],"order_by":["AESTDTC"]}`                |

#### `source_meta.csv`

Maps raw dataset columns to their target variables. Required columns:

| Column           | Description                               |
|------------------|-------------------------------------------|
| `domain`         | Target SDTM domain                        |
| `source_dataset` | Name of the raw dataset (e.g., `ae_raw`)  |
| `source_column`  | Column name in the raw dataset            |
| `target_var`     | SDTM variable this feeds into             |

#### `ct_codelist.csv`

Your controlled terminology codelist. Required columns:

| Column        | Description                              |
|---------------|------------------------------------------|
| `codelist_id` | Unique codelist identifier (e.g., NCI C-code) |
| `coded_value` | The SDTM standard value                  |
| `input_value` | Raw data value that maps to it           |
| `decode`      | Human-readable decode (optional)         |

#### `config.yaml`

Study-level settings:

```yaml
studyid: "YOUR-STUDY-001"
timezone: "UTC"
ref_start_rule:
  var: "RFSTDTC"
  source: "dm_raw"
imputation_policy:
  day: "first"
visit_map:
  - VISIT: "SCREENING"
    VISITNUM: 1
    START_DAY: -28
    END_DAY: -1
  - VISIT: "BASELINE"
    VISITNUM: 2
    START_DAY: 1
    END_DAY: 1
```

### Step 2 — Load Everything

```r
library(sdtmbuilder)

# Read study config
config      <- yaml::read_yaml("path/to/config.yaml")
cfg         <- new_sdtm_config(
  studyid        = config$studyid,
  ref_start_rule = config$ref_start_rule,
  timezone       = config$timezone %||% "UTC",
  visit_map      = config$visit_map,
  imputation_policy = config$imputation_policy %||% list()
)

# Read metadata
target_meta <- read_target_meta("path/to/target_meta.csv")
source_meta <- read_source_meta("path/to/source_meta.csv")
ct_lib      <- read_ct_library("path/to/ct_codelist.csv")

# Load raw clinical data (supports CSV, XPT, RDS, XLSX)
raw_data <- load_raw_data(list(
  dm_raw = "data/raw/dm_raw.csv",
  ae_raw = "data/raw/ae_raw.csv",
  cm_raw = "data/raw/cm_raw.csv",
  mh_raw = "data/raw/mh_raw.csv"
))
```

### Step 3 — Compile Rules and Build Domains

```r
# Compile derivation rules from metadata
rule_set <- compile_rules(target_meta, source_meta, ct_lib)

# Build each domain
domains <- c("AE", "CM", "MH")  # list your domains here

results <- list()
for (dom in domains) {
  results[[dom]] <- build_domain(
    domain      = dom,
    target_meta = target_meta,
    source_meta = source_meta,
    raw_data    = raw_data,
    config      = cfg,
    rule_set    = rule_set,
    verbose     = TRUE
  )
  cat(sprintf("%s: %d rows x %d cols\n",
              dom, nrow(results[[dom]]$data), ncol(results[[dom]]$data)))
}
```

### Step 4 — Validate

```r
for (dom in names(results)) {
  rpt <- results[[dom]]$report
  cat(sprintf("\n--- %s ---\n", dom))
  summarize_validation_report(rpt)
}
```

### Step 5 — Export XPT Files

```r
output_dir <- "output/sdtm"

for (dom in names(results)) {
  export_xpt(results[[dom]]$data, dom, output_dir, target_meta)
}
# Creates: output/sdtm/ae.xpt, cm.xpt, mh.xpt, etc.
```

### Step 6 — Generate Standalone R Scripts (Optional)

Generate a self-contained R script that reproduces each domain build —
useful for filing with a regulatory submission:

```r
for (dom in names(results)) {
  gen_domain_script(
    domain      = dom,
    rule_set    = rule_set,
    target_meta = target_meta,
    source_meta = source_meta,
    config      = cfg,
    output_path = sprintf("programs/%s.R", tolower(dom))
  )
}
# Creates: programs/ae.R, programs/cm.R, programs/mh.R
```

---

## Package Architecture

```
Metadata Files (CSV/YAML)
  target_meta - source_meta - ct_codelist - config
         |
   B. Metadata Ingestion     read, validate, normalize
         |
   C. Rule Compilation       compile derivation rules
         |
   D. Dependency System      DAG, topological sort
         |
   H. Domain Builders        orchestrate per-domain builds
         |
   +---------+-----------+
   |         |           |
 E.Data   F.Join    G.Derivation
 Access              Library
         |
   I. Validation              P21-style checks
         |
   K. Export                   XPT, RDS, CSV, define support
         |
   J. Code Generation         standalone R scripts, Quarto docs
```

## Package Modules

| Module | Purpose |
|--------|---------|
| **A. Primitives** | S3 classes and constructors (config, meta bundle, rule set, build context, validation report) |
| **B. Metadata Ingestion** | Read, validate, normalize target/source metadata and controlled terminology |
| **C. Rule Compilation** | Convert derivation descriptions to machine-readable rules |
| **D. Dependency System** | Build DAG, topological sort, cycle detection |
| **E. Data Access** | Load raw data, standardize names/types, derive core keys |
| **F. Join & Assembly** | Safe joins, record expansion, checkbox handling, deduplication |
| **G. Derivation Library** | Atomic derivation functions (mapping, CT, dates, visits, sequences, flags) |
| **H. Domain Builders** | Orchestrators and domain-specific plugins |
| **I. Validation** | Structure, keys, ISO dates, CT conformance, cross-domain checks |
| **J. Code Generation** | Generate R scripts, Quarto documents, project scaffolds |
| **K. Export** | XPT output, define.xml support artifacts |
| **L. Utilities** | Logging, assertions, error handling, snapshots |

---

## Running Tests

```r
devtools::test()
# FAIL 0 | WARN 25 | SKIP 0 | PASS 185
```

The 25 warnings are a harmless igraph deprecation notice (`is.igraph()` -> `is_igraph()`).

---

## Project Status

| Component          | Status |
|--------------------|--------|
| Core functions     | Complete — 18 module files, 80+ exported functions |
| End-to-end pipeline| Verified — 8 domains: DM, AE, CM, MH, PR, EX, VS, LB all PASS |
| XPT export         | Working — all 8 domains export to SAS transport |
| Test suite         | 194 tests passing (FAIL 0) |
| Code generation    | Generates standalone R scripts |
| Validation         | P21-style checks (required vars, keys, ISO dates, types, CT) |
| Demo               | `inst/examples/demo_full_pipeline.R` — full 8-domain walkthrough |
| Vignettes          | 4 vignettes including "Build AE from Metadata" tutorial |

---

## License

MIT
