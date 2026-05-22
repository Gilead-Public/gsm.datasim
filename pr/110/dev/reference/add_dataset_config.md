# Add Dataset Configuration

Adds a dataset configuration to the study config.

## Usage

``` r
add_dataset_config(
  config,
  dataset_type,
  enabled = TRUE,
  count_formula = NULL,
  growth_pattern = "linear",
  dependencies = character(0),
  custom_args = list()
)
```

## Arguments

- config:

  Study configuration list

- dataset_type:

  Type of dataset (e.g., "Raw_AE")

- enabled:

  Whether the dataset should be generated

- count_formula:

  Formula for calculating record count

- growth_pattern:

  How the dataset grows over time

- dependencies:

  Dependencies on other datasets

- custom_args:

  Additional arguments for the dataset generator

## Value

Updated study configuration
