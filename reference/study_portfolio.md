# Create a portfolio of studies from a base config and per-study variants

A convenience wrapper around
[`create_multiple_longitudinal_studies()`](https://gilead-biostats.github.io/gsm.datasim/reference/create_multiple_longitudinal_studies.md)
that lets you define a shared base configuration and a single named list
of per-study overrides (variants). Only the fields that differ between
studies need to appear in each variant entry – everything else falls
back to the base.

## Usage

``` r
study_portfolio(
  variants,
  participants = 100,
  sites = 10,
  snapshots = 6,
  interval = "1 month",
  domains = c("AE", "LB", "VISIT"),
  ...
)
```

## Arguments

- variants:

  Named list of per-study override lists. Names become the study
  identifiers. Each element may contain any combination of:
  `participants`, `sites`, `snapshots`, `interval`, `domains`,
  `outlier_intensity`, `run_analytics`, `run_reporting`, or any other
  argument accepted by
  [`create_longitudinal_study()`](https://gilead-biostats.github.io/gsm.datasim/reference/create_longitudinal_study.md).

- participants:

  Default participant count for studies that do not specify their own
  (default 100).

- sites:

  Default site count (default 10).

- snapshots:

  Default number of snapshots (default 6).

- interval:

  Default snapshot interval (default `"1 month"`).

- domains:

  Default domain vector (default `c("AE", "LB", "VISIT")`).

- ...:

  Additional arguments forwarded verbatim to
  [`create_multiple_longitudinal_studies()`](https://gilead-biostats.github.io/gsm.datasim/reference/create_multiple_longitudinal_studies.md)
  (e.g. `run_analytics`, `run_reporting`, `parallel`, `verbose`).

## Value

A `multiple_longitudinal_studies` object (named list of
`longitudinal_study` objects).

## Details

Vectorised parameters (`participants`, `sites`, `snapshots`,
`outlier_intensity`, `interval`) are automatically extracted from the
variant list so you never have to maintain a parallel vector alongside
`study_configs`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Define shared defaults; each variant only specifies what changes
studies <- study_portfolio(
  variants = list(
    "PHASE2-SMALL" = list(
      participants = 80, sites = 8, snapshots = 4,
      domains = c("AE", "LB")
    ),
    "PHASE3-LARGE" = list(
      participants = 400, sites = 25, snapshots = 12,
      domains = c("AE", "LB", "VISIT", "PD")
    ),
    "SAFETY-RUN" = list(
      participants = 50, sites = 3, snapshots = 8,
      outlier_intensity = 2
    )
  ),
  # Shared defaults (used when a variant does not override)
  participants = 100,
  sites = 10,
  snapshots = 6,
  interval = "1 month",
  domains = c("AE", "LB", "VISIT"),
  run_analytics = FALSE,
  verbose = TRUE
)

names(studies) # "PHASE2-SMALL" "PHASE3-LARGE" "SAFETY-RUN"
studies[["PHASE3-LARGE"]]$config # inspect per-study config
} # }
```
