# Build the snapshot.json contents

Collects the release key, objects, package versions, and gsm-specific
metadata for
[`write_snapshot_json()`](https://gilead-public.github.io/gsm.datasim/dev/reference/write_snapshot_json.md).
The fingerprint is a sha256 over the per-file checksums of every bundle
file except `snapshot.json`, so identical data yields an identical
fingerprint regardless of build metadata.

## Usage

``` r
.build_snapshot_args(
  output_dir,
  config,
  channel,
  key,
  allow_local,
  packages = .check_package_versions(allow_local)
)
```

## Arguments

- output_dir:

  Bundle directory.

- config:

  A validated config from
  [`read_testdata_config()`](https://gilead-public.github.io/gsm.datasim/dev/reference/read_testdata_config.md).

- channel:

  Release channel.

- key:

  Release tag, or `NULL` to generate one.

- allow_local:

  Allow packages without a `RemoteSha`.

- packages:

  Package versions from
  [`.check_package_versions()`](https://gilead-public.github.io/gsm.datasim/dev/reference/dot-check_package_versions.md).

## Value

List of arguments for
[`write_snapshot_json()`](https://gilead-public.github.io/gsm.datasim/dev/reference/write_snapshot_json.md).
