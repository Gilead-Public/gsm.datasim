# Simulate raw snapshots for the bundle

Simulates raw data under the config seed without changing the caller's
RNG state, then marks every site Active.

## Usage

``` r
.simulate_bundle_snapshots(config)
```

## Arguments

- config:

  A validated config from
  [`read_testdata_config()`](https://gilead-public.github.io/gsm.datasim/dev/reference/read_testdata_config.md).

## Value

Unnamed list of raw-data snapshots.
