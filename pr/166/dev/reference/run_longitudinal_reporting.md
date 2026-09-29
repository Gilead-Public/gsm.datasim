# Run Reporting Pipeline on Longitudinal Study

Execute the gsm.reporting pipeline on study analytics results, producing
a `reporting` list of data frames on the study object (one entry per
snapshot). The study must already have analytics results (run
`run_longitudinal_analytics` first, or call `create_longitudinal_study`
with `run_analytics = TRUE`).

## Usage

``` r
run_longitudinal_reporting(study, verbose = FALSE)
```

## Arguments

- study:

  Longitudinal study data structure

- verbose:

  Whether to print progress output

## Value

Updated study structure with `study$reporting` populated

## Examples

``` r
if (FALSE) { # \dontrun{
study <- create_longitudinal_study(
  "STUDY-001", participants = 50, sites = 5, snapshots = 2,
  run_analytics = TRUE, analytics_package = "gsm.kri"
)
study <- run_longitudinal_reporting(study)
names(study$reporting)
} # }
```
