# Generate study data across multiple snapshots

Generate study data across multiple snapshots

Generate study data across multiple snapshots

## Usage

``` r
generate_study_snapshots(
  study_id,
  participants,
  sites,
  snapshots,
  interval,
  mappings,
  base_date = NULL,
  outlier_intensity = 1,
  verbose = FALSE
)

generate_study_snapshots(
  study_id,
  participants,
  sites,
  snapshots,
  interval,
  mappings,
  base_date = NULL,
  outlier_intensity = 1,
  verbose = FALSE
)
```

## Arguments

- study_id:

  Study identifier

- participants:

  Number of participants

- sites:

  Number of sites

- snapshots:

  Number of snapshots

- interval:

  Time interval between snapshots

- mappings:

  Vector of mapping names to use

- base_date:

  Base date for snapshot generation (defaults to "2012-01-31" if NULL)

- outlier_intensity:

  Global multiplier for outlier-like values in domain generators.

- verbose:

  Whether to print progress/output messages

## Value

List of raw data for each snapshot

List of raw data for each snapshot

## Examples

``` r
if (FALSE) { # \dontrun{
mappings <- ensure_core_mappings(c("AE", "LB"))
snapshots <- generate_study_snapshots(
  study_id = "STUDY-001",
  participants = 100, sites = 10, snapshots = 3,
  interval = "1 month", mappings = mappings
)
length(snapshots)
} # }
```
