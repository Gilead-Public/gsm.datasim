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

## Examples

``` r
# Generate date values
generate_column_by_type("ae_stdt", list(type = "date"), n = 5,
  context = list(start_date = "2023-01-01", end_date = "2023-12-31"))
#> [1] "2023-06-22" "2023-03-04" "2023-12-24" "2023-02-16" "2023-07-20"

# Generate integer values using name-pattern inference
generate_column_by_type("subject_count", n = 10)
#>  [1] 69  5 24 79 77  2 62 55 43 62

# Generate character values with explicit type
generate_column_by_type("category", list(type = "character"), n = 5)
#> [1] "CATE-0001" "CATE-0002" "CATE-0003" "CATE-0004" "CATE-0005"
```
