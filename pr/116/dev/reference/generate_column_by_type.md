# Generate a Single Column by Inferred Type

Produces a vector of length `n` with realistic simulated values based on
the resolved column type. For columns that look like foreign keys (e.g.
`subjid`, `invid`), values are sampled from existing parent domain data
when available in `context$data`.

## Usage

``` r
generate_column_by_type(col_name, col_spec = list(), n, context = list())
```

## Arguments

- col_name:

  Character. The column name.

- col_spec:

  List. Spec metadata for the column.

- n:

  Integer. Number of rows to generate.

- context:

  List with element `data` (named list of data.frames already generated
  for earlier domains), and `start_date`/`end_date` (Date or character
  coercible to Date).

## Value

A vector of length `n`.

## Lifecycle

**\[experimental\]**
