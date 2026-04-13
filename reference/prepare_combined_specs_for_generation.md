# Prepare combined specs for generation

Removes mapped outputs from `combined_specs`, ensures required core raw
datasets are ordered first, and optionally filters to `desired_specs`.

## Usage

``` r
prepare_combined_specs_for_generation(combined_specs, desired_specs = NULL)
```

## Arguments

- combined_specs:

  A named list of dataset specifications.

- desired_specs:

  Optional character vector of dataset names to keep.

## Value

A reordered (and optionally filtered) named list of dataset specs.

## Examples

``` r
specs <- list(
  Raw_AE = list(aest_dt = list(required = TRUE)),
  Mapped_AE = list(),
  Raw_SUBJ = list(subjid = list(required = TRUE))
)

prepared <- prepare_combined_specs_for_generation(specs)
names(prepared)
#> [1] "Raw_SUBJ"  "Raw_SV"    "Raw_VISIT" "Raw_AE"   
```
