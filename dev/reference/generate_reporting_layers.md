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
