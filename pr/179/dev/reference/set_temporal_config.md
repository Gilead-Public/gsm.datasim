# Set Temporal Configuration

Updates temporal configuration settings in a study config.

## Usage

``` r
set_temporal_config(
  config,
  start_date = NULL,
  snapshot_count = NULL,
  snapshot_width = NULL,
  end_date = NULL
)
```

## Arguments

- config:

  Study configuration list

- start_date:

  Study start date

- snapshot_count:

  Number of snapshots

- snapshot_width:

  Time between snapshots

- end_date:

  Study end date

## Value

Updated study configuration

## Examples

``` r
config <- create_study_config("TRIAL001")
config <- set_temporal_config(config, start_date = "2023-06-01", snapshot_count = 6)
config$temporal_config$snapshot_count
#> [1] 6
```
