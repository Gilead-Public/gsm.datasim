# Replace the bundle output directory with a completed staging directory

Moves any existing `output_dir` aside, moves `staging` into place, then
deletes the old bundle. Restores the old bundle if the move fails.

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
