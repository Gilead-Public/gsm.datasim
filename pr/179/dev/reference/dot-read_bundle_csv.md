# Read a bundle CSV back with standard column types

`GroupID` is read as character for `analytics*` and `reportingResults`,
and converted to character for the other `reportingResults*` tables;
`SnapshotDate` is converted to `Date`. Other columns keep
[`utils::read.csv()`](https://rdrr.io/r/utils/read.table.html) types.

## Usage

``` r
.read_bundle_csv(name, dir)
```

## Arguments

- name:

  Table name, e.g. `"reportingResults_country"`.

- dir:

  Directory containing `<name>.csv`.

## Value

A data frame.
