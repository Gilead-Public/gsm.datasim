# Build the shared gsm test data bundle

Generate shared example data and write it to disk with a `snapshot.json`
describing the build.

## Usage

``` r
build_testdata_bundle(
  output_dir,
  config = read_testdata_config(),
  overwrite = FALSE,
  channel = "dev",
  key = NULL,
  allow_local = FALSE
)
```

## Arguments

- output_dir:

  Directory to write the bundle into. Created if needed.

- config:

  A config list or path to a YAML file; see
  [`read_testdata_config()`](https://gilead-public.github.io/gsm.datasim/dev/reference/read_testdata_config.md).
  Defaults to the checked-in config.

- overwrite:

  If `FALSE` (default), error when `output_dir` exists and is not empty.
  If `TRUE`, replace `output_dir` and its contents. The bundle is built
  in a staging directory next to `output_dir` and swapped in only once
  complete, so a failed build leaves the existing bundle untouched.

- channel:

  Release channel (lowercase letters and digits), e.g. `"dev"` or
  `"prod"`.

- key:

  Release tag recorded as the snapshot `key`. Defaults to
  `testdata-<channel>-<YYYYMMDD>T<HHMMSS>Z` (UTC) at build time; it must
  match the tag the bundle is published under.

- allow_local:

  If `FALSE` (default), error when any pipeline package lacks a
  `RemoteSha` (a local or uncommitted install). Set `TRUE` only for
  local builds that won't be published.

## Value

The snapshot description (a list), invisibly.

## Details

The pipeline is:

1.  Simulate raw data with
    [`create_longitudinal_study()`](https://gilead-public.github.io/gsm.datasim/dev/reference/create_longitudinal_study.md)
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

## Layout

    <output_dir>/
      snapshot.json
      analytics{Input,Summary}{,_country}.csv
      reporting{Results,Groups,Bounds,Metrics}{,_country,_study}.csv
      lSource.rds
      analytics{Input,Summary}.rds
      reporting{Results,Groups,Bounds,Metrics}{,_country,_study}.rds

## Snapshot

`snapshot.json` uses the generic "refresh" snapshot format (version 1),
written by
[`write_snapshot_json()`](https://gilead-public.github.io/gsm.datasim/dev/reference/write_snapshot_json.md):
`refresh_snapshot`, `key` (the release tag), `created`, `objects` (the
15 `.rds` files with their sha256), and `packages` (installed `version`
and `remote_sha` of each pipeline package, including gsm.core).
gsm-specific details live under `metadata`: `channel`, `fingerprint`,
`simulation`, `built_by`, `workflow_run`, `r_version`, and `size`.

`fingerprint` is a SHA-256 hash over the per-file checksums of the
bundle files (excluding `snapshot.json`), so two builds with identical
data share a fingerprint regardless of build time or origin.

## Examples

``` r
if (FALSE) { # \dontrun{
build_testdata_bundle("bundle")
} # }
```
