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
