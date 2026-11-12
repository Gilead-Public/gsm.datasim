# Read one object from a test data bundle

Looks `name` up in the bundle's `snapshot.json`, verifies its checksum,
and reads it: `.rds` files with
[`readRDS()`](https://rdrr.io/r/base/readRDS.html), `.csv` files with
the bundle's standard column types (`GroupID` character, `SnapshotDate`
Date).

## Usage

``` r
read_testdata_object(path, name)
```

## Arguments

- path:

  A bundle directory.

- name:

  An `objects` key: an object name such as `"reportingResults"`, or a
  CSV file name such as `"reportingResults.csv"`.

## Value

The object.
