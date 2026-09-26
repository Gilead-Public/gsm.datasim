# Locate the shipped test data bundle configuration

Returns the path to `inst/testdata/bundle-config.yaml`, the
configuration that
[`build_testdata_bundle()`](https://gilead-public.github.io/gsm.datasim/dev/reference/build_testdata_bundle.md)
uses to reproduce the shared gsm test data.

## Usage

``` r
testdata_config_path()
```

## Value

A file path.

## Examples

``` r
testdata_config_path()
#> [1] "/home/runner/work/_temp/Library/gsm.datasim/testdata/bundle-config.yaml"
```
