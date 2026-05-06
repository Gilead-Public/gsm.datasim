# Create Multiple Longitudinal Studies

Creates multiple longitudinal studies simultaneously, allowing for
efficient batch generation of study data with shared or per-study
configuration.

## Usage

``` r
create_multiple_longitudinal_studies(
  study_names,
  participants = 100,
  sites = 10,
  snapshots = 5,
  interval = "1 month",
  domains = c("AE", "LB", "VISIT"),
  run_analytics = TRUE,
  analytics_package = NULL,
  analytics_workflows = NULL,
  run_reporting = FALSE,
  outlier_intensity = 1,
  study_configs = NULL,
  parallel = FALSE,
  export_studies = FALSE,
  export_dir = ".",
  verbose = FALSE
)
```

## Arguments

- study_names:

  Character vector of study names/identifiers

- participants:

  Number of participants per study (default 100). Can be a single value
  applied to all studies or a vector of values per study.

- sites:

  Number of sites per study (default 10). Can be a single value applied
  to all studies or a vector of values per study.

- snapshots:

  Number of snapshots per study (default 5). Can be a single value
  applied to all studies or a vector of values per study.

- interval:

  Time between snapshots (default "1 month"). Can be a single value
  applied to all studies or a vector of values per study.

- domains:

  Clinical domains to include (default c("AE", "LB", "VISIT")). Applied
  to all studies unless overridden in study_configs.

- run_analytics:

  Whether to run the analytics pipeline (default TRUE)

- analytics_package:

  Package containing workflows (optional)

- analytics_workflows:

  Specific workflows to run (optional)

- run_reporting:

  Whether to run the reporting pipeline after analytics (default FALSE)

- outlier_intensity:

  Global multiplier for outlier-like values (default 1). Can be a single
  value applied to all studies or a vector of values per study.

- study_configs:

  Optional named list of per-study configurations to override defaults.
  Names should match study_names. Each element can contain any of the
  parameters above to override the global settings for that specific
  study.

- parallel:

  Whether to generate studies in parallel (default FALSE). Requires
  parallel processing setup if TRUE.

- export_studies:

  Whether to automatically export all studies to disk (default FALSE)

- export_dir:

  Directory to export studies to if export_studies = TRUE

- verbose:

  Whether to print progress/output messages (default FALSE)

## Value

Named list of LongitudinalStudy objects, with names corresponding to
study_names

## Examples

``` r
if (FALSE) { # \dontrun{
# Create three studies with shared configuration
studies <- create_multiple_longitudinal_studies(
  study_names = c("STUDY-001", "STUDY-002", "STUDY-003"),
  participants = 150,
  sites = 12,
  snapshots = 6,
  domains = c("AE", "LB", "VISIT", "PD"),
  verbose = TRUE
)

# Create studies with per-study configuration
studies <- create_multiple_longitudinal_studies(
  study_names = c("PHASE2-001", "PHASE3-001"),
  participants = c(100, 500),
  sites = c(8, 25),
  snapshots = c(4, 12),
  study_configs = list(
    "PHASE2-001" = list(domains = c("AE", "LB")),
    "PHASE3-001" = list(domains = c("AE", "LB", "VISIT", "PD", "PK"))
  ),
  verbose = TRUE
)
} # }
```
