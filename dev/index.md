# gsm.datasim

## Overview

[gsm.datasim](https://github.com/Gilead-BioStats/gsm.datasim) generates
synthetic clinical trial data for testing and development of clinical
monitoring applications. It produces multi-snapshot longitudinal
datasets across a configurable set of clinical domains (SDTM-style and
custom) and can run configurable analytics and reporting pipelines on
the generated data via the
[`workr`](https://github.com/Gilead-BioStats/workr) workflow engine.

## Installation

You can install the latest release of gsm.datasim from
[GitHub](https://github.com/) with:

``` r

# install.packages("pak")
pak::pak("Gilead-BioStats/gsm.datasim@*release")
```

You can install the development version of gsm.datasim from
[GitHub](https://github.com/) with:

``` r

# install.packages("pak")
pak::pak("Gilead-BioStats/gsm.datasim")
```

## Quick Start

``` r

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
|----|----|
| [`quick_longitudinal_study()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/quick_longitudinal_study.md) | Single-call entry point: raw data + analytics + reporting |
| [`create_longitudinal_study()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/create_longitudinal_study.md) | Full control over domains, intervals, and pipelines |
| [`create_multiple_longitudinal_studies()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/create_multiple_longitudinal_studies.md) | Generate a portfolio of studies in one call |
| [`create_study_config()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/create_study_config.md) | Build a config object for low-level control |
| [`generate_study_data()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/generate_study_data.md) | Generate raw data from a config object |

### Study access helpers

| Function | Description |
|----|----|
| [`get_snapshot_data()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/get_snapshot_data.md) | Extract data for a specific snapshot |
| [`get_domain_timeline()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/get_domain_timeline.md) | All snapshots for a single domain |
| [`get_available_domains()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/get_available_domains.md) | List all domains present in the study |
| [`summarize_longitudinal_study()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/summarize_longitudinal_study.md) | Print a study summary |

### Pipelines

| Function | Description |
|----|----|
| [`run_longitudinal_analytics()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/run_longitudinal_analytics.md) | Run (or re-run) the analytics pipeline (powered by `workr`) |
| [`run_longitudinal_reporting()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/run_longitudinal_reporting.md) | Run (or re-run) the reporting pipeline |
| [`generate_analytics_layers()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/generate_analytics_layers.md) | Run analytics on already-generated raw data |
| [`generate_reporting_layers()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/generate_reporting_layers.md) | Run reporting on analytics results |

### Export

| Function | Description |
|----|----|
| [`export_study_data()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/export_study_data.md) | Write study to structured folder hierarchy (CSV or Parquet) |

## Common Patterns

### Data generation only (no pipeline)

``` r

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

``` r

study <- quick_longitudinal_study(
  study_name        = "DEMO-OUTLIER-HIGH",
  participants      = 200,
  sites             = 15,
  months_duration   = 6,
  outlier_intensity = 2.5   # default is 1
)
```

### Stepwise pipeline execution

``` r

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

``` r

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

``` r

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

``` r

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

``` r

registry <- get_domain_registry()
cat("Registry-backed domains:", paste(names(registry), collapse = ", "), "\n")
```

Domains not yet in the registry are handled by a legacy dispatcher and
can be migrated incrementally.
