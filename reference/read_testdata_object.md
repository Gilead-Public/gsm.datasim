# Read one object from a test data bundle

Looks `name` up in the bundle's `snapshot.json`, reads it with
[`readRDS()`](https://rdrr.io/r/base/readRDS.html), and verifies the
object against its
[`gsm.core::fresh_digest()`](https://gilead-public.github.io/gsm.core/reference/fresh_digest.html).
CSV copies (e.g. `"reportingResults.csv"`) are not in `objects`; they
are read with the bundle's standard column types (`GroupID` character,
`SnapshotDate` Date) without verification.

## Usage

``` r
read_testdata_object(path, name)
```

## Arguments

- path:

  A bundle directory.

- name:

  An object name such as `"reportingResults"`, or a CSV file name such
  as `"reportingResults.csv"`.

## Value

The object.
