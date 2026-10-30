# Read a test data bundle manifest

Reads the `manifest.json` written by
[`build_testdata_bundle()`](https://gilead-public.github.io/gsm.datasim/dev/reference/build_testdata_bundle.md),
which records the bundle fingerprint, simulation config, package
versions, and per-file checksums.

## Usage

``` r
read_testdata_manifest(path)
```

## Arguments

- path:

  A bundle directory (`output_dir`) or a path to `manifest.json`.

## Value

The parsed manifest list.
