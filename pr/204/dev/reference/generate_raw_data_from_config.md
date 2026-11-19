# Generate raw data from a study config

Convenience wrapper for generating snapshot raw data directly from a
configured study object.

## Usage

``` r
generate_raw_data_from_config(config, verbose = FALSE)
```

## Arguments

- config:

  Study configuration object with enabled datasets.

- verbose:

  Whether to print progress/output messages.

## Value

List of raw data for enabled datasets.

## Examples

``` r
if (FALSE) { # \dontrun{
config <- create_standard_study_config("STUDY001", participant_count = 50, site_count = 5)
config <- set_temporal_config(config, snapshot_count = 2)
raw <- generate_raw_data_from_config(config)
names(raw)  # snapshot date keys
} # }
```
