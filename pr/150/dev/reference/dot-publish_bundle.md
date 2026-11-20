# Replace the bundle output directory's contents

Copies (rather than renames, since
[`tempdir()`](https://rdrr.io/r/base/tempfile.html) may be on another
volume) the staged files into `output_dir`, replacing its contents but
not the directory itself. Existing contents are backed up to the
workspace first and restored if the copy fails. If restoring also fails,
the workspace is kept and the error reports where the backup is.

## Usage

``` r
.publish_bundle(staging, output_dir, workspace)
```

## Arguments

- staging:

  Completed staging directory.

- output_dir:

  Final bundle directory.

- workspace:

  Workspace from
  [`.bundle_workspace()`](https://gilead-public.github.io/gsm.datasim/dev/reference/dot-bundle_workspace.md).

## Value

`output_dir`, invisibly.
