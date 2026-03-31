# Generate a Single Snapshot of Domain Data

Iterates over all domains in `combined_specs` using the three-tier
fallback (registry → legacy → type-based). Supports cumulative
generation via `previous_data`.

## Usage

``` r
.generate_single_snapshot(
  combined_specs,
  domain_n,
  registry,
  start_date,
  end_date,
  snapshot_idx,
  snapshot_count,
  snapshot_width,
  study_id,
  previous_data
)
```
