# Sort a bundle table on explicit keys

Sort a table so reruns never produce row-order-only diffs. Reporting
Metrics/Results/Bounds/Groups use curated keys (natural `MetricID` rank
with `srs` last, `SnapshotDate` descending, curated Study `Param`
order); anything else is sorted on every column.

## Usage

``` r
.sort_reporting(x, name)
```

## Arguments

- x:

  A data frame.

- name:

  Table name, e.g. `"reportingResults_country"` or
  `"Reporting_Results"`.

## Value

`x`, reordered.
