# Canonicalize a path for comparison

Resolves the deepest existing ancestor with
[`fs::path_real()`](https://fs.r-lib.org/reference/path_math.html) and
appends the remaining components; case-folded on Windows.

## Usage

``` r
.canonical_path(path)
```

## Arguments

- path:

  A path, which need not exist.

## Value

A single normalized path string.
