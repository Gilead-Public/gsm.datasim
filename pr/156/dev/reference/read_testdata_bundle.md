# Read a test data bundle from disk

Rehydrates a bundle written by
[`build_testdata_bundle()`](https://gilead-public.github.io/gsm.datasim/dev/reference/build_testdata_bundle.md)
(or downloaded from a release) so it can be summarised or explored. The
data itself is read lazily by the consuming functions.

## Usage

``` r
read_testdata_bundle(path)
```

## Arguments

- path:

  The bundle folder containing `manifest.json`.

## Value

An object of class `testdata_bundle` with `path` and `manifest`.

## Examples

``` r
if (FALSE) { # \dontrun{
bundle <- read_testdata_bundle("~/gsm-testdata/AA-AA-000-0000")
summarize_testdata_bundle(bundle)
} # }
```
