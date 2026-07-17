# Run Analytics Pipeline on Longitudinal Study

Execute full analytics pipeline on study data.

## Usage

``` r
run_longitudinal_analytics(study, verbose = FALSE)
```

## Arguments

- study:

  Longitudinal study data structure

- verbose:

  Whether to print progress output

## Value

Updated study structure with analytics results

## Examples

``` r
if (FALSE) { # \dontrun{
study <- create_longitudinal_study(
  "STUDY-001", participants = 50, sites = 5, snapshots = 2,
  analytics_package = "gsm.kri"
)
study <- run_longitudinal_analytics(study)
names(study$analytics)
} # }
```
