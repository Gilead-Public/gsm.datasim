# gsm.datasim

<!-- badges: start -->

<div class="pkgdown-release">

[![R-CMD-check](https://github.com/Gilead-BioStats/gsm.datasim/actions/workflows/R-CMD-check.yaml/badge.svg?branch=main)](https://github.com/Gilead-BioStats/gsm.datasim/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/Gilead-BioStats/gsm.datasim/actions/workflows/test-coverage.yaml/badge.svg?branch=main)](https://github.com/Gilead-BioStats/gsm.datasim/actions/workflows/test-coverage.yaml)
[![pkgdown-all](https://github.com/Gilead-BioStats/gsm.datasim/actions/workflows/pkgdown-all.yaml/badge.svg?branch=main)](https://github.com/Gilead-BioStats/gsm.datasim/actions/workflows/pkgdown-all.yaml)

</div>

<div class="pkgdown-devel">

[![R-CMD-check](https://github.com/Gilead-BioStats/gsm.datasim/actions/workflows/R-CMD-check.yaml/badge.svg?branch=dev)](https://github.com/Gilead-BioStats/gsm.datasim/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/Gilead-BioStats/gsm.datasim/actions/workflows/test-coverage.yaml/badge.svg?branch=dev)](https://github.com/Gilead-BioStats/gsm.datasim/actions/workflows/test-coverage.yaml)
[![pkgdown-all](https://github.com/Gilead-BioStats/gsm.datasim/actions/workflows/pkgdown-all.yaml/badge.svg?branch=dev)](https://github.com/Gilead-BioStats/gsm.datasim/actions/workflows/pkgdown-all.yaml)

</div>

<!-- badges: end -->

## Overview

`{gsm.datasim}` generates synthetic clinical trial data for testing and
development of Risk-Based Quality Monitoring (RBQM) applications and
packages. It produces multi-snapshot longitudinal datasets across a
configurable set of clinical domains and runs the full gsm analytics and
reporting pipelines on the generated data.

## Installation

You can install the latest release of gsm.datasim from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("Gilead-BioStats/gsm.datasim@*release")
```

<div class="pkgdown-devel">

You can install the development version of gsm.datasim from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("Gilead-BioStats/gsm.datasim")
```

</div>

## Quick Start

```r
library(gsm.datasim)

# Generate a standard 6-month study with analytics + reporting
study <- quick_longitudinal_study(
  study_name       = "DEMO-001",
  participants     = 200,
  sites            = 15,
  months_duration  = 6,
  study_type       = "standard"
)

# Access raw data, analytics, and reporting results
names(study)          # $study_id, $config, $raw_data, $analytics, $reporting
names(study$raw_data) # one entry per snapshot date

# Inspect the first snapshot
snap <- get_snapshot_data(study, 1)
nrow(snap$Raw_SUBJ)
nrow(snap$Raw_AE)

# Track a domain across all snapshots
ae_counts <- sapply(get_domain_timeline(study, "AE"), nrow)
```

## Key Functions

### Study generation

| Function | Description |
|---|---|
| `quick_longitudinal_study()` | Single-call entry point: raw data + analytics + reporting |
| `create_longitudinal_study()` | Full control over domains, intervals, and pipelines |
| `create_multiple_longitudinal_studies()` | Generate a portfolio of studies in one call |
| `create_study_config()` | Build a config object for low-level control |
| `generate_study_data()` | Generate raw data from a config object |

### Study access helpers

| Function | Description |
|---|---|
| `get_snapshot_data()` | Extract data for a specific snapshot |
| `get_domain_timeline()` | All snapshots for a single domain |
| `get_available_domains()` | List all domains present in the study |
| `summarize_longitudinal_study()` | Print a study summary |

### Pipelines

| Function | Description |
|---|---|
| `run_longitudinal_analytics()` | Run (or re-run) the gsm.kri analytics pipeline |
| `run_longitudinal_reporting()` | Run (or re-run) the gsm.reporting pipeline |
| `generate_analytics_layers()` | Run analytics on already-generated raw data |
| `generate_reporting_layers()` | Run reporting on analytics results |

### Export

| Function | Description |
|---|---|
| `export_study_data()` | Write study to structured folder hierarchy (CSV or Parquet) |

## Common Patterns

### Data generation only (no pipeline)

```r
data_only <- create_longitudinal_study(
  study_id      = "DATA-001",
  participants  = 200,
  sites         = 15,
  snapshots     = 6,
  interval      = "1 month",
  domains       = c("AE", "LB", "VISIT", "QUERY"),
  run_analytics = FALSE,
  run_reporting = FALSE
)
```

### Increased outlier prevalence

```r
study <- quick_longitudinal_study(
  study_name        = "DEMO-OUTLIER-HIGH",
  participants      = 200,
  sites             = 15,
  months_duration   = 6,
  outlier_intensity = 2.5   # default is 1
)
```

### Stepwise pipeline execution

```r
# Step 1 — raw data
study <- create_longitudinal_study(
  study_id      = "STEP-001",
  participants  = 100,
  sites         = 10,
  snapshots     = 4,
  interval      = "1 month",
  domains       = c("AE", "LB", "VISIT"),
  run_analytics = FALSE,
  run_reporting = FALSE
)

# Step 2 — add analytics
study <- run_longitudinal_analytics(study)

# Step 3 — add reporting
study <- run_longitudinal_reporting(study)
```

### Low-level config API

```r
config <- create_study_config(
  study_id          = "CUSTOM-001",
  participant_count = 300,
  site_count        = 20,
  analytics_package = "gsm.kri"
) |>
  set_temporal_config(start_date = "2023-01-01", snapshot_count = 12, snapshot_width = "months") |>
  add_dataset_config("Raw_AE",    enabled = TRUE) |>
  add_dataset_config("Raw_LB",    enabled = TRUE) |>
  add_dataset_config("Raw_VISIT", enabled = TRUE)

raw_data  <- generate_study_data(config)
analytics <- generate_analytics_layers(raw_data, config)
reporting <- generate_reporting_layers(analytics, config)
```

### Multiple studies

```r
studies <- create_multiple_longitudinal_studies(
  study_names   = c("TRIAL-001", "TRIAL-002", "TRIAL-003"),
  participants  = 200,
  sites         = 12,
  snapshots     = 6,
  domains       = c("AE", "LB", "VISIT", "PD"),
  run_analytics = TRUE
)
```

### Export

```r
export_study_data(
  study      = study,
  output_dir = "./output",
  format     = "parquet",   # or "csv" (default) or "both"
  overwrite  = TRUE
)
```

## Examples

Full worked examples are in the Examples section of the website:

- `example_demo.html` — complete walkthrough of all major features
- `example_longitudinal.html` — longitudinal data generation patterns
- `example_domain_registry.html` — extending the Domain Registry

## Domain Registry

The Domain Registry is an extensible system for per-domain data
generation. Each entry defines how data should be generated for a single
`Raw_*` dataset — independently testable and overridable without
modifying core generation code.

```r
registry <- get_domain_registry()
cat("Registry-backed domains:", paste(names(registry), collapse = ", "), "\n")
```

Domains not yet in the registry are handled by a legacy dispatcher and
can be migrated incrementally.

