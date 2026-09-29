# Get Domain Timeline Data

Get specific domain data across all snapshots.

## Usage

``` r
get_domain_timeline(study, domain_name)
```

## Arguments

- study:

  Longitudinal study data structure

- domain_name:

  Domain mapping name (e.g., "AE", "LB")

## Value

Timeline data for the specified domain

## Examples

``` r
if (FALSE) { # \dontrun{
study <- create_longitudinal_study("STUDY-001", participants = 50, sites = 5,
                                    snapshots = 3, domains = c("AE", "LB"))
ae_timeline <- get_domain_timeline(study, "AE")
length(ae_timeline) # one entry per snapshot
} # }
```
