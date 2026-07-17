# Generate analytics layers from raw data

Convenience wrapper that executes the analytics pipeline and returns the
raw analytics results.

## Usage

``` r
generate_analytics_layers(raw_data, config, verbose = FALSE)
```

## Arguments

- raw_data:

  Raw study data (single or multi-snapshot list)

- config:

  Study configuration object

- verbose:

  Whether to print progress/output messages

## Value

Raw analytics pipeline results

## Examples

``` r
if (FALSE) { # \dontrun{
config <- create_standard_study_config("STUDY001", participant_count = 50, site_count = 5)
config <- set_temporal_config(config, snapshot_count = 1)
raw_data <- generate_study_data(config)
analytics <- generate_analytics_layers(raw_data, config, verbose = TRUE)
} # }
```
