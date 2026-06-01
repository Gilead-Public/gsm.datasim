# Execute the reporting pipeline using gsm.reporting workflows

Runs the gsm.reporting workflow layer (workflow/3_reporting) for each
snapshot using the mapped data, analytics results, and workflow list
from the analytics pipeline output. Returns a named list of reporting
results per snapshot.

## Usage

``` r
execute_reporting_pipeline(analytics_results, config)
```

## Arguments

- analytics_results:

  Output from `execute_analytics_pipeline`

- config:

  Study configuration object

## Value

Named list of reporting results per snapshot
