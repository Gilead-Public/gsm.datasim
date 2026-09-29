# Generate raw data for endpoints study (multi-package)

Generate raw data for endpoints study (multi-package)

## Usage

``` r
generate_raw_data_for_endpoints(config, domain_package_df)
```

## Arguments

- config:

  Study configuration object with enabled datasets

- domain_package_df:

  Data frame mapping domains to packages

## Value

List of raw data for enabled datasets

## Examples

``` r
if (FALSE) { # \dontrun{
config <- create_standard_study_config("STUDY001", participant_count = 50, site_count = 5)
domain_pkg_df <- data.frame(
  domain  = c("AE", "LB"),
  package = c("gsm.mapping", "gsm.mapping"),
  stringsAsFactors = FALSE
)
raw <- generate_raw_data_for_endpoints(config, domain_pkg_df)
} # }
```
