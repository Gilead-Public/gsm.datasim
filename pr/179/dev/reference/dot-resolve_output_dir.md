# Resolve the bundle output directory

Paths are compared after canonicalization so case, short names,
symlinks, and separators don't hide a match.
[`tempdir()`](https://rdrr.io/r/base/tempfile.html) itself is redirected
to a `testdata-bundle` subdirectory, since it holds unrelated files that
overwriting must not touch. Ancestors of
[`tempdir()`](https://rdrr.io/r/base/tempfile.html) are rejected because
replacing their contents would delete the session's temporary files
(including the build workspace).

## Usage

``` r
.resolve_output_dir(output_dir)
```

## Arguments

- output_dir:

  Requested output directory.

## Value

The directory to publish into.
