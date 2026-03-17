# Generate study data across multiple snapshots

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

- verbose:

  Whether to print progress/output messages

## Value

List of raw data for each snapshot
