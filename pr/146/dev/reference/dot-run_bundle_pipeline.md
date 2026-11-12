# Run the bundle pipeline for one snapshot

Ingests raw data, then runs mapping, metric, and reporting workflows and
stamps `SnapshotDate` on reporting Results and Bounds.

## Usage

``` r
.run_bundle_pipeline(raw, workflows, snapshot_date)
```

## Arguments

- raw:

  A raw-data snapshot.

- workflows:

  Output of
  [`.bundle_workflows()`](https://gilead-public.github.io/gsm.datasim/dev/reference/dot-bundle_workflows.md).

- snapshot_date:

  Snapshot `Date`.

## Value

List with `mapped`, `analyzed`, and `reporting`.
