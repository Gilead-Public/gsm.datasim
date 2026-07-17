# Generate reporting layers from analytics results

Convenience wrapper that runs the gsm.reporting pipeline and returns the
raw reporting results per snapshot.

## Usage

``` r
generate_reporting_layers(analytics_results, config, verbose = FALSE)
```

## Arguments

- analytics_results:

  Output from `execute_analytics_pipeline` or
  `generate_analytics_layers`

- config:

  Study configuration object

- verbose:

  Whether to print progress/output messages

## Value

Named list of reporting results per snapshot

## Examples

``` r
if (FALSE) { # \dontrun{
config <- create_standard_study_config("STUDY001", participant_count = 50, site_count = 5)
config <- set_temporal_config(config, snapshot_count = 1)
raw_data <- generate_study_data(config)
analytics <- generate_analytics_layers(raw_data, config)
reporting <- generate_reporting_layers(analytics, config, verbose = TRUE)
} # }
```
