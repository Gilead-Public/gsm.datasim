# Replace the bundle output directory with a completed staging directory

Copies (rather than renames, since
[`tempdir()`](https://rdrr.io/r/base/tempfile.html) may be on another
volume) the staged files into `output_dir`. Existing contents are backed
up to [`tempdir()`](https://rdrr.io/r/base/tempfile.html) first and
restored if the copy fails.

## Usage

``` r
.publish_bundle(staging, output_dir)
```

## Arguments

- staging:

  Completed staging directory.

- output_dir:

  Final bundle directory.

## Value

`output_dir`, invisibly.
