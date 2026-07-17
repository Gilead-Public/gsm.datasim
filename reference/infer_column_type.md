# Infer Column Type from Spec and Name Patterns

Resolves the data type for a column by checking (1) the explicit `type`
field in the spec, (2) column name patterns, then (3) falling back to
`"character"`.

## Usage

``` r
infer_column_type(col_name, col_spec = list())
```

## Arguments

- col_name:

  Character. The column name.

- col_spec:

  List. Spec metadata for the column (may contain `type`, `source_col`,
  `required`).

## Value

A single character string: one of `"date"`, `"numeric"`, `"integer"`,
`"logical"`, `"yn"`, `"character"`, or `"timestamp"`.
