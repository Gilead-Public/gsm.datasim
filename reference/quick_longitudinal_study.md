# Quick longitudinal study creation

Creates a complete longitudinal study with sensible defaults and runs
analytics

## Usage

``` r
quick_longitudinal_study(
  study_name = "GS-US-000-0001",
  participants = 1000,
  sites = 150,
  months_duration = 24,
  study_type = "standard",
  include_pipeline = TRUE,
  verbose = FALSE
)
```

## Arguments

- study_name:

  Name of the study

- participants:

  Number of participants (default 1000)

- sites:

  Number of sites (default 150)

- months_duration:

  Duration in months (default 24)

- study_type:

  Type of study - "standard" or "endpoints"

- include_pipeline:

  Whether to run both the analytics and reporting pipelines (default
  TRUE)

- verbose:

  Whether to print progress/output messages

## Value

LongitudinalStudy object with complete data and analytics
