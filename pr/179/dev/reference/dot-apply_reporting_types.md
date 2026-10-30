# Apply reporting column types

Coerces `GroupID` to character and `SnapshotDate` to `Date` when
present.

## Usage

``` r
.apply_reporting_types(x)
```

## Arguments

- x:

  A data frame.

## Value

`x` with coerced types and reset row names.
