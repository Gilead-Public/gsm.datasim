# Get Summary of Longitudinal Study

Display comprehensive study summary.

## Usage

``` r
summarize_longitudinal_study(study, verbose = TRUE)
```

## Arguments

- study:

  Longitudinal study data structure

- verbose:

  Whether to print summary output

## Value

Invisibly returns the study structure

## Examples

``` r
if (FALSE) { # \dontrun{
study <- create_longitudinal_study("STUDY-001", participants = 50, sites = 5, snapshots = 2)
summarize_longitudinal_study(study)
} # }
```
