# Example of config interface usage

Example of config interface usage

## Examples

``` r
if (FALSE) { # \dontrun{
# Simple study with standard datasets using config approach
config <- create_study_config("ONCOLOGY001", participant_count = 200, site_count = 15) %>%
  set_temporal_config(start_date = "2023-01-01", snapshot_count = 12, snapshot_width = "months") %>%
  add_dataset_config("Raw_AE", enabled = TRUE, 
                    count_formula = function(config, snapshot_idx = 1) {
                      base_count <- config$study_params$participant_count * 2.5
                      factor <- snapshot_idx / config$temporal_config$snapshot_count
                      round(base_count * factor)
                    }) %>%
  add_dataset_config("Raw_SV", enabled = TRUE) %>%
  add_dataset_config("Raw_VISIT", enabled = TRUE)
study_data <- generate_study_data(config)

# Using convenience function for standard datasets
config <- create_standard_study_config("TRIAL002", participant_count = 100, site_count = 10,
                                       adverse_events = TRUE, lab_data = TRUE)

# Custom dataset configuration
config <- create_study_config("CUSTOM001", participant_count = 300, site_count = 20) %>%
  add_dataset_config("Raw_Biomarker", enabled = TRUE,
                    count_formula = function(config) config$study_params$participant_count * 5,
                    dependencies = "Raw_SUBJ")
study_data <- generate_study_data(config)
} # }
```
