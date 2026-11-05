# Create the private build workspace

A single [`tempdir()`](https://rdrr.io/r/base/tempfile.html)
subdirectory holding `staging/` (the new bundle) and `backup/` (the
previous bundle while publishing), so nothing is written loose in the
[`tempdir()`](https://rdrr.io/r/base/tempfile.html) root.

## Usage

``` r
.bundle_workspace()
```

## Value

Path to the workspace.
