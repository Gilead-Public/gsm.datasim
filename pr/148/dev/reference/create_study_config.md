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
  reporting_workflows = NULL,
  outlier_intensity = 1,
  risk_profile = NULL
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

- outlier_intensity:

  Global multiplier for outlier-like values in domain generators. Use
  `1` for current baseline, values `>1` to increase outlier prevalence.

- risk_profile:

  Optional named list controlling site-targeted consecutive-run
  injection. Only `Raw_VS` currently supports it. `NULL` uses the
  generator defaults. Recognized fields are `dPctRed` and `dPctAmber`
  (share of sites in each band), `nWindowLength` (rolling window length,
  whole number `>= 2`), `dRateNormal` / `dRateAmber` / `dRateRed`
  (target repeat rate per band), and `vVitals` (character vector of
  vitals to target, or `NULL` for all eight). Fields can be given at
  three levels, with more specific levels overriding less specific ones:

  - Top level: applies to every domain that supports it.

  - Domain level: `list(Raw_VS = list(dPctRed = 0.3))`.

  - Vital level: `list(Raw_VS = list(sysbp = list(dRateRed = 0.6)))`.
    Each vital's bands are allocated independently, so vitals can have
    different profiles. The targeted vitals are the domain's `vVitals`
    if given (named vitals must then be among them), else the vitals the
    domain names, else the top-level `vVitals`, else all eight.
    `vVitals` cannot be set inside a vital.

## Value

A list containing study configuration

## Examples

``` r
config <- create_study_config("STUDY001", participant_count = 200, site_count = 15)
config$study_params$study_id
#> [1] "STUDY001"
config$temporal_config$snapshot_count
#> [1] 5
```
