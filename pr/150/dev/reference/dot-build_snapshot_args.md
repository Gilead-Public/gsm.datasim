# Build the snapshot.json contents

Collects the objects, package versions, and gsm-specific metadata for
[`gsm.core::write_snapshot_json()`](https://gilead-public.github.io/gsm.core/reference/write_snapshot_json.html).
The fingerprint is
[`gsm.core::fresh_fingerprint()`](https://gilead-public.github.io/gsm.core/reference/fresh_fingerprint.html)
of the RDS objects' digests, so identical data yields an identical
fingerprint (and `key`) regardless of build metadata, R version, or CSV
contents.

## Usage

``` r
.build_snapshot_args(
  output_dir,
  config,
  allow_local,
  packages = .check_package_versions(allow_local),
  objects = NULL
)
```

## Arguments

- output_dir:

  Bundle directory.

- config:

  A validated config from
  [`read_testdata_config()`](https://gilead-public.github.io/gsm.datasim/dev/reference/read_testdata_config.md).

- allow_local:

  Allow packages without a `RemoteSha`.

- packages:

  Package versions from
  [`.check_package_versions()`](https://gilead-public.github.io/gsm.datasim/dev/reference/dot-check_package_versions.md).

- objects:

  Named list of the bundle's R objects. Read from the RDS files in
  `output_dir` when `NULL`.

## Value

List of arguments for
[`gsm.core::write_snapshot_json()`](https://gilead-public.github.io/gsm.core/reference/write_snapshot_json.html).
