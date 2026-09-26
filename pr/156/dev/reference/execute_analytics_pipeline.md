# Execute analytics pipeline

Execute analytics pipeline

## Usage

``` r
execute_analytics_pipeline(raw_data, config)
```

## Arguments

- raw_data:

  Raw study data

- config:

  Study configuration object

## Value

Analytics pipeline results

## Examples

``` r
if (FALSE) { # \dontrun{
config <- create_standard_study_config("STUDY001", participant_count = 50, site_count = 5)
config <- set_temporal_config(config, snapshot_count = 1)
raw_data <- generate_study_data(config)
results <- execute_analytics_pipeline(raw_data, config)
} # }
```
