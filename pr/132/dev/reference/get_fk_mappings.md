# Get Foreign Key Mappings

Returns a list mapping common FK column names to the domain and column
they should reference. Used by
[`generate_column_by_type()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/generate_column_by_type.md)
to maintain referential integrity when generating data for unknown
domains.

## Usage

``` r
get_fk_mappings()
```

## Value

A named list where each element has `domain` and `column` fields.
