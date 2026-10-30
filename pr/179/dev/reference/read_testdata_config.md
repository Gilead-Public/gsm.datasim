# Read and validate a test data bundle config

Reads the simulation parameters used by
[`build_testdata_bundle()`](https://gilead-public.github.io/gsm.datasim/dev/reference/build_testdata_bundle.md).
The supplied default should almost always be used.

## Usage

``` r
read_testdata_config(
  config = system.file("testdata", "bundle-config.yaml", package = "gsm.datasim")
)
```

## Arguments

- config:

  Path to a YAML config file, or an already-parsed config list
  (re-validated).

## Value

A list with elements `seed`, `study_id`, `participants`, `sites`,
`snapshots`, `interval`, `snapshot_dates` (`Date`), `domains`, and
`extra_mappings`.

## Examples

``` r
cfg <- read_testdata_config()
cfg$snapshot_dates
#> [1] "2025-02-01" "2025-03-01" "2025-04-01"
```
