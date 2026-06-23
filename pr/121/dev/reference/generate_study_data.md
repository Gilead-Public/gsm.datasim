# Generate study data with configuration

Generate study data with configuration

## Usage

``` r
generate_study_data(
  config,
  workflow_path = "workflow/1_mappings",
  mappings = NULL,
  package = "gsm.mapping",
  verbose = FALSE
)
```

## Arguments

- config:

  Study configuration object

- workflow_path:

  Path to workflow mappings (not used in new approach)

- mappings:

  Optional mappings list (not used in new approach)

- package:

  Package containing workflows (not used in new approach)

- verbose:

  Whether to print progress/output messages

## Value

List of generated study data
