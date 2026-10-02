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
  include_pipeline = FALSE,
  outlier_intensity = 1,
  vs_risk_profile = NULL,
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
  FALSE)

- outlier_intensity:

  Global multiplier for outlier-like values in domain generators.

- vs_risk_profile:

  Optional named list controlling site-targeted consecutive-run
  injection in `Raw_VS`. Recognized fields are `dPctRed` and `dPctAmber`
  (share of sites in each band), `nWindowLength` (rolling window length,
  whole number `>= 2`), `dRateNormal` / `dRateAmber` / `dRateRed`
  (target repeat rate per band), and `vVitals` (character vector of
  vitals to target, or `NULL` for all eight). `NULL` uses the generator
  defaults.

- verbose:

  Whether to print progress/output messages

## Value

LongitudinalStudy object with complete data and analytics

## Examples

``` r
if (FALSE) { # \dontrun{
study <- quick_longitudinal_study(
  study_name = "GS-US-123-4567",
  participants = 200,
  sites = 20,
  months_duration = 12
)
study$study_id
length(study$raw_data)
} # }
```
