# Build the bundle manifest

The fingerprint is a sha256 over per-file checksums of the data files
only, so identical data yields an identical fingerprint regardless of
build metadata.

## Usage

``` r
.build_manifest(output_dir, config)
```

## Arguments

- output_dir:

  Bundle directory.

- config:

  A validated config from
  [`read_testdata_config()`](https://gilead-public.github.io/gsm.datasim/dev/reference/read_testdata_config.md).

## Value

Manifest list.
