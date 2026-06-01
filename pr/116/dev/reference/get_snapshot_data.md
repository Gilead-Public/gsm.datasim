# Get Data for Specific Snapshot

Retrieves data for a specific snapshot from longitudinal study.

## Usage

``` r
get_snapshot_data(study, snapshot)
```

## Arguments

- study:

  Longitudinal study data structure

- snapshot:

  Snapshot number (1-based)

## Value

Data for the specified snapshot

## Examples

``` r
if (FALSE) { # \dontrun{
study <- create_longitudinal_study("STUDY-001", participants = 50, sites = 5, snapshots = 3)
snap1 <- get_snapshot_data(study, snapshot = 1)
names(snap1)
} # }
```
