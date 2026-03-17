# Create Study Configuration

Creates a study configuration list with study parameters, temporal
configuration, and dataset specifications for clinical trial data
generation.

## Usage

``` r
create_study_config(
  study_id = "STUDY001",
  participant_count = 100,
  site_count = 10,
  analytics_package = NULL,
  analytics_workflows = NULL,
  reporting_package = NULL,
  reporting_workflows = NULL
)
```

## Arguments

- study_id:

  Study identifier

- participant_count:

  Number of participants

- site_count:

  Number of sites

- analytics_package:

  Analytics package to use

- analytics_workflows:

  Specific workflows to run

- reporting_package:

  Reporting package to use (default: `"gsm.reporting"`)

- reporting_workflows:

  Specific reporting workflows to run (default: all)

## Value

A list containing study configuration
