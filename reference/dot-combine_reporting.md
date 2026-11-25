# Combine reporting tables across snapshots

Results and Bounds accumulate across snapshots; Groups and Metrics
describe the final snapshot.

## Usage

``` r
.combine_reporting(reporting)
```

## Arguments

- reporting:

  List of per-snapshot reporting outputs.

## Value

List with the four combined `Reporting_*` tables.
