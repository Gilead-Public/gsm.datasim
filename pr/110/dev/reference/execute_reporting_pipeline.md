# Execute the reporting pipeline using gsm.reporting workflows

Runs the gsm.reporting workflow layer (workflow/3_reporting) for each
snapshot using the mapped data, analytics results, and workflow list
from the analytics pipeline output. Returns a named list of reporting
results per snapshot.

Runs the gsm.reporting workflow layer (workflow/3_reporting) for each
snapshot using the mapped data, analytics results, and workflow list
from the analytics pipeline output. Returns a named list of reporting
results per snapshot.

## Usage

``` r
execute_reporting_pipeline(analytics_results, config)

execute_reporting_pipeline(analytics_results, config)
```

## Arguments

- analytics_results:

  Output from `execute_analytics_pipeline`

- config:

  Study configuration object

## Value

Named list of reporting results per snapshot

Named list of reporting results per snapshot

## Examples

``` r
if (FALSE) { # \dontrun{
config <- create_standard_study_config("STUDY001", participant_count = 50, site_count = 5)
config <- set_temporal_config(config, snapshot_count = 1)
raw_data <- generate_study_data(config)
analytics <- generate_analytics_layers(raw_data, config)
reporting <- execute_reporting_pipeline(analytics, config)
} # }
```
