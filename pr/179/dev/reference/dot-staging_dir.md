# Create a staging directory next to the bundle output directory

A sibling (rather than
[`tempdir()`](https://rdrr.io/r/base/tempfile.html)) keeps the final
swap a same-volume rename.

## Usage

``` r
.staging_dir(output_dir)
```

## Arguments

- output_dir:

  Final bundle directory.

## Value

Path to a new, empty staging directory.
