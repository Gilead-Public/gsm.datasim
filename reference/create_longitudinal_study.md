# High-Level Study Creation API

This file contains convenience functions for creating studies with
minimal configuration. These functions provide simplified interfaces for
common study types. Create longitudinal study data

## Usage

``` r
create_longitudinal_study(
  study_id = "STUDY-001",
  participants = 100,
  sites = 10,
  snapshots = 5,
  interval = "1 month",
  domains = c("AE", "LB", "VISIT"),
  run_analytics = TRUE,
  analytics_package = NULL,
  analytics_workflows = NULL,
  run_reporting = FALSE,
  verbose = FALSE
)
```

## Arguments

- study_id:

  Study identifier

- participants:

  Number of participants

- sites:

  Number of sites

- snapshots:

  Number of snapshots

- interval:

  Time between snapshots (e.g., "1 month", "2 weeks")

- domains:

  Clinical domains to include

- run_analytics:

  Whether to run the analytics pipeline (default TRUE)

- analytics_package:

  Package containing workflows (optional)

- analytics_workflows:

  Specific workflows to run (optional)

- run_reporting:

  Whether to run the reporting pipeline after analytics (default FALSE)

- verbose:

  Whether to print progress/output messages

## Value

LongitudinalStudy object with generated data

## Details

Creates a complete longitudinal study with multiple snapshots
