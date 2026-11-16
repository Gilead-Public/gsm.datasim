# Check whether R code sets the test data builder option

Parses (without evaluating) the code and looks for an
[`options()`](https://rdrr.io/r/base/options.html) call with an argument
named exactly `gsm.testdata_builder`. Code that doesn't parse falls back
to a plain text search, so the file is left alone.

## Usage

``` r
.sets_testdata_builder(lines)
```

## Arguments

- lines:

  Lines of R code.

## Value

`TRUE` or `FALSE`.
