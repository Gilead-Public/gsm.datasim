# Read an installed package's DESCRIPTION

Thin wrapper around
[`utils::packageDescription()`](https://rdrr.io/r/utils/packageDescription.html)
so installed metadata can be mocked in tests.

## Usage

``` r
.pkg_description(pkg)
```

## Arguments

- pkg:

  Package name.

## Value

A `packageDescription` object.
