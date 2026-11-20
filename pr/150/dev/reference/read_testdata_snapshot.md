# Read a test data bundle snapshot description

Reads the `snapshot.json` written by
[`build_testdata_bundle()`](https://gilead-public.github.io/gsm.datasim/dev/reference/build_testdata_bundle.md).

## Usage

``` r
read_testdata_snapshot(path)
```

## Arguments

- path:

  A bundle directory (`output_dir`) or a path to `snapshot.json`.

## Value

The parsed snapshot list.
