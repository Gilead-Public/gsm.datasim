# Generate study data across multiple snapshots

Generate study data across multiple snapshots

## Usage

``` r
generate_study_snapshots(
  study_id,
  participants,
  sites,
  snapshots,
  interval,
  mappings,
  base_date = NULL,
  outlier_intensity = 1,
  risk_profile = NULL,
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

  Time interval between snapshots

- mappings:

  Vector of mapping names to use

- base_date:

  Base date for snapshot generation (defaults to "2012-01-31" if NULL)

- outlier_intensity:

  Global multiplier for outlier-like values in domain generators.

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

- verbose:

  Whether to print progress/output messages

## Value

List of raw data for each snapshot

## Examples

``` r
if (FALSE) { # \dontrun{
mappings <- ensure_core_mappings(c("AE", "LB"))
snapshots <- generate_study_snapshots(
  study_id = "STUDY-001",
  participants = 100, sites = 10, snapshots = 3,
  interval = "1 month", mappings = mappings
)
length(snapshots)
} # }
```
