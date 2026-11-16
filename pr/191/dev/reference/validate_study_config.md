# Validate Study Configuration

Validates study configuration parameters.

## Usage

``` r
validate_study_config(config)
```

## Arguments

- config:

  Study configuration list

## Value

TRUE if valid, stops with error if invalid

## Examples

``` r
config <- create_study_config("TRIAL001", participant_count = 100, site_count = 10)
config <- set_temporal_config(config, snapshot_count = 3)
validate_study_config(config)
#> [1] TRUE
```
