# Summarise a test data bundle

Inventories every table and column in a bundle written by
[`build_testdata_bundle()`](https://gilead-public.github.io/gsm.datasim/dev/reference/build_testdata_bundle.md):
one row per table with its size, and one row per column with its type,
missingness, cardinality, and an example value. This is the data behind
[`explore_testdata()`](https://gilead-public.github.io/gsm.datasim/dev/reference/explore_testdata.md)
and is useful on its own for a quick look at what a bundle version
contains.

## Usage

``` r
summarize_testdata_bundle(bundle)
```

## Arguments

- bundle:

  A `testdata_bundle` (from
  [`build_testdata_bundle()`](https://gilead-public.github.io/gsm.datasim/dev/reference/build_testdata_bundle.md)
  or
  [`read_testdata_bundle()`](https://gilead-public.github.io/gsm.datasim/dev/reference/read_testdata_bundle.md))
  or the path to a bundle folder.

## Value

An object of class `testdata_summary`: a list with `manifest`, `tables`
(data frame: `snapshot`, `layer`, `table`, `rows`, `cols`, `file`),
`columns` (data frame: `snapshot`, `layer`, `table`, `column`, `type`,
`n_missing`, `pct_missing`, `n_distinct`, `example`), and `path`.

## Examples

``` r
if (FALSE) { # \dontrun{
s <- summarize_testdata_bundle("~/gsm-testdata/AA-AA-000-0000")
s$tables
subset(s$columns, table == "Raw_SUBJ")
} # }
```
