# Read a bundle manifest

Read a bundle manifest

## Usage

``` r
read_testdata_manifest(path)
```

## Arguments

- path:

  A bundle folder (containing `manifest.json`) or the manifest file
  itself.

## Value

The parsed manifest as a list; `tables` and `files` are data frames.

## Examples

``` r
if (FALSE) { # \dontrun{
m <- read_testdata_manifest("~/gsm-testdata/AA-AA-000-0000")
m$bundle_version
} # }
```
