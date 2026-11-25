# Build the shared gsm test data bundle

Generate shared example data and write it to disk with a `snapshot.json`
describing the build.

## Usage

``` r
build_testdata_bundle(
  output_dir,
  config = read_testdata_config(),
  overwrite = FALSE,
  allow_local = FALSE
)
```

## Arguments

- output_dir:

  Directory to write the bundle into. Created if needed.

- config:

  A config list or path to a YAML file; see
  [`read_testdata_config()`](https://gilead-public.github.io/gsm.datasim/reference/read_testdata_config.md).
  Defaults to the checked-in config.

- overwrite:

  If `FALSE` (default), error when `output_dir` exists and is not empty.
  If `TRUE`, replace the contents of `output_dir`. The bundle is built
  in a [`tempdir()`](https://rdrr.io/r/base/tempfile.html) subdirectory
  and copied into `output_dir` only once complete, so a failed build
  leaves the existing bundle untouched. If `output_dir` is
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html) itself, the bundle
  is written to `file.path(tempdir(), "testdata-bundle")` instead;
  ancestors of [`tempdir()`](https://rdrr.io/r/base/tempfile.html) are
  rejected.

- allow_local:

  If `FALSE` (default), error when any pipeline package lacks a
  `RemoteSha` (a local or uncommitted install). Set `TRUE` only for
  local builds that won't be published.

## Value

The snapshot description (a list), invisibly.

## Details

The pipeline is:

1.  Simulate raw data with
    [`create_longitudinal_study()`](https://gilead-public.github.io/gsm.datasim/reference/create_longitudinal_study.md)
    using the config's seed and parameters.

2.  Set `Raw_SITE$site_status` to `"Active"` on every snapshot.

3.  Map each snapshot with
    [`gsm.mapping::Ingest()`](https://gilead-public.github.io/gsm.mapping/reference/Ingest.html)
    and the `gsm.mapping` mapping workflows (config `domains` plus
    `extra_mappings`).

4.  Run every `gsm.kri` metric workflow, including inactive ones (so the
    example reports show them), and the `gsm.qtl` metrics.

5.  Run the `gsm.reporting` workflows, then stamp `SnapshotDate` from
    `snapshot_dates` onto `Reporting_Results` and `Reporting_Bounds`.

6.  Combine reporting tables across snapshots and split them by group
    level.

7.  Write CSVs of the `kri0001` and `cou0001` analytics
    (`Analysis_Input` and `Analysis_Summary`) from the final snapshot
    and of the 12 `reporting*` tables.

8.  Write RDS files of `lSource` (the final raw snapshot) and of 14
    tables read back from those CSVs.

Rows are sorted on explicit keys so identical inputs always produce
byte-identical files.

## Local test data builder

This function is also gsm.core's local test data builder. When installed
gsm packages don't match a published build and at least one is local,
gsm.core calls
`build_testdata_bundle(output_dir, overwrite = TRUE, allow_local = TRUE)`
via the `gsm.testdata_builder` option and caches the result. Run
[`use_gsm_datasim()`](https://gilead-public.github.io/gsm.datasim/reference/use_gsm_datasim.md)
to set that option in your `.Rprofile`.

## Layout

    <output_dir>/
      snapshot.json
      analytics{Input,Summary}{,_country}.csv
      reporting{Results,Groups,Bounds,Metrics}{,_country,_study}.csv
      lSource.rds
      analytics{Input,Summary}.rds
      reporting{Results,Groups,Bounds,Metrics}{,_country,_study}.rds

## Snapshot

`snapshot.json` uses gsm.core's refresh snapshot format (version 2),
written by
[`gsm.core::write_snapshot_json()`](https://gilead-public.github.io/gsm.core/reference/write_snapshot_json.html):
`refresh_snapshot`, `key` (`"sha256:<fingerprint>"`), `created`,
`objects` (each `.rds` file with the
[`gsm.core::fresh_digest()`](https://gilead-public.github.io/gsm.core/reference/fresh_digest.html)
of its R object), and `packages` (installed `version` and `remote_sha`
of each pipeline package, including gsm.core). gsm-specific details live
under `metadata`: `fingerprint`, `inputs_hash`, `simulation`,
`r_version`, and `size`. CSVs are a convenience copy: they stay in the
bundle (always written with LF line endings) but are not in `objects` or
the fingerprint.

The bundle records only what built it, not when or where it was built or
how it is published; release tags and run provenance belong to the
release.

- `fingerprint` is
  [`gsm.core::fresh_fingerprint()`](https://gilead-public.github.io/gsm.core/reference/fresh_fingerprint.html)
  of the object digests, so two builds with identical data share a
  fingerprint (and `key`) regardless of build time, origin, R version,
  or RDS compression.

- `inputs_hash` is
  [`gsm.core::fresh_inputs_hash()`](https://gilead-public.github.io/gsm.core/reference/fresh_inputs_hash.html)
  of `packages`: the SHA-256 of the UTF-8 text formed by one
  `"<package> <version> <remote_sha>"` line per entry (a missing
  `remote_sha` written as `NA`), sorted by package name in C-locale
  order and joined with `"\n"` (no trailing newline). Consumers can
  compute it from their installed packages to look up the matching
  bundle.

## Examples

``` r
if (FALSE) { # \dontrun{
build_testdata_bundle("bundle")
} # }
```
