# Read and validate a test data bundle configuration

Reads a bundle configuration YAML (by default the one shipped with the
package, see
[`testdata_config_path()`](https://gilead-public.github.io/gsm.datasim/dev/reference/testdata_config_path.md)),
applies any overrides passed through `...`, coerces types, and validates
the result.

## Usage

``` r
read_testdata_config(path = testdata_config_path(), ...)
```

## Arguments

- path:

  Path to a bundle configuration YAML file.

- ...:

  Named overrides for individual fields, e.g. `participants = 40`. Only
  fields present in the file can be overridden.

## Value

A list of class `testdata_config` with (at least) `bundle_version`,
`study_id`, `seed`, `participants`, `sites`, `snapshots`, `interval`,
`start_date`, `snapshot_dates`, `domains`, `mapping_package`,
`analytics_packages`, `reporting_package`, `attach_packages`,
`post_processing`, `format`, and `package_refs`.

## Examples

``` r
cfg <- read_testdata_config()
cfg$participants
#> [1] 1000

# a reduced configuration for local experiments
small <- read_testdata_config(
  participants = 40, sites = 4, snapshots = 2,
  snapshot_dates = c("2025-02-01", "2025-03-01")
)
```
