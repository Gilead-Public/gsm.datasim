# Remove Dataset Configuration

Removes a dataset configuration from the study config.

## Usage

``` r
remove_dataset_config(config, dataset_type)
```

## Arguments

- config:

  Study configuration list

- dataset_type:

  Type of dataset to remove

## Value

Updated study configuration

## Examples

``` r
config <- create_study_config("TRIAL001")
config <- add_dataset_config(config, "Raw_AE", enabled = TRUE)
config <- remove_dataset_config(config, "Raw_AE")
"Raw_AE" %in% names(config$dataset_configs)
#> [1] FALSE
```
