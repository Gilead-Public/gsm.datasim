# Build the shared gsm test data bundle

Reproduces the test data that used to ship inside `gsm.core` (`lSource`
and the `reporting*` / `analytics*` objects) from a checked-in
configuration, running the mapping, metric, and reporting workflows
through `workr`, and writes the result as a versioned bundle:

## Usage

``` r
build_testdata_bundle(
  config = read_testdata_config(),
  output_dir = tempdir(),
  overwrite = FALSE,
  verbose = FALSE
)
```

## Arguments

- config:

  A `testdata_config`, see
  [`read_testdata_config()`](https://gilead-public.github.io/gsm.datasim/dev/reference/read_testdata_config.md).

- output_dir:

  Directory under which the `<study_id>` bundle folder is created.

- overwrite:

  If `TRUE`, an existing bundle folder is removed and rebuilt.

- verbose:

  Print progress and pipeline output.

## Value

An object of class `testdata_bundle`: a list with `path` (the bundle
folder), `manifest` (the parsed `manifest.json`), `config`, and `study`
(the in-memory `longitudinal_study` that was exported).

## Details

    <output_dir>/<study_id>/
      manifest.json
      <snapshot_date>/
        raw/          Raw_*.parquet / .csv
        mapped/       Mapped_*.parquet / .csv
        analytics/    Analysis_<metric>_<table>.parquet / .csv
        reporting/    Reporting_*.parquet / .csv

The two edits the original maintainer script applied silently are
explicit, configurable steps: `Raw_SITE$site_status` is forced to
`config$post_processing$site_status` on every snapshot, and
`SnapshotDate` on the reporting tables is stamped with
`config$snapshot_dates`, which also name the snapshot folders.

## See also

[`read_testdata_bundle()`](https://gilead-public.github.io/gsm.datasim/dev/reference/read_testdata_bundle.md),
[`summarize_testdata_bundle()`](https://gilead-public.github.io/gsm.datasim/dev/reference/summarize_testdata_bundle.md),
[`explore_testdata()`](https://gilead-public.github.io/gsm.datasim/dev/reference/explore_testdata.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# the production bundle (takes a while)
bundle <- build_testdata_bundle(output_dir = "~/gsm-testdata")

# a reduced bundle for local work
cfg <- read_testdata_config(
  participants = 40, sites = 4, snapshots = 2,
  snapshot_dates = c("2025-02-01", "2025-03-01")
)
bundle <- build_testdata_bundle(cfg, output_dir = tempdir(), overwrite = TRUE)
bundle$manifest$tables
} # }
```
