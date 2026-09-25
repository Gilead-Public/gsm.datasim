# Create Longitudinal Study Data Structure

Creates a longitudinal study data structure that encapsulates study data
and provides intuitive access methods for different analysis
perspectives.

## Usage

``` r
create_longitudinal_study_data(study_id, raw_data, config)
```

## Arguments

- study_id:

  Study identifier

- raw_data:

  Raw study data snapshots

- config:

  Configuration parameters

## Value

A longitudinal study data structure

## Examples

``` r
config <- list(participants = 50, sites = 5, snapshots = 2, interval = "1 month",
               domains = c("AE", "LB"))
study <- create_longitudinal_study_data("MY-STUDY", raw_data = list(), config = config)
study$study_id
#> [1] "MY-STUDY"
```
