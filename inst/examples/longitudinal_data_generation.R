# Example: Longitudinal Data Generation - Issue #95 Solution
# This script demonstrates the intuitive, well-structured solution for longitudinal data generation

library(gsm.datasim)


# This single function call replaces 50+ lines of manual workflow orchestration

# Standard study (core mappings)
study <- quick_longitudinal_study(
  study_name = "Oncology Phase III Trial",
  participants = 1000,
  sites = 150,
  months_duration = 3,
  study_type = "standard",
  outlier_intensity = 1
)

# Increase outlier prevalence to trigger more analytics flags
high_outlier_study <- quick_longitudinal_study(
  study_name = "Oncology Phase III Trial - High Outliers",
  participants = 1000,
  sites = 150,
  months_duration = 3,
  study_type = "standard",
  outlier_intensity = 2.5
)

# Endpoints study (endpoint mappings)
# endpoints_study <- quick_longitudinal_study(
#   study_name = "Cardio Endpoints Trial",
#   study_type = "endpoints"
# )

# Explore the generated study
summarize_longitudinal_study(study)

# Access analytics results intuitively (if pipeline was successful)
if (!is.null(study$analytics)) {
  latest_snapshot <- tail(names(study$analytics), 1)
  latest_analytics <- study$analytics[[latest_snapshot]]
  cat("Latest analytics snapshot:", latest_snapshot, "\n")
  
  # Count metrics from raw analytics results
  if (!is.null(latest_analytics) && "results" %in% names(latest_analytics)) {
    results <- latest_analytics$results
    analysis_results <- results[grep("^Analysis_", names(results), ignore.case = TRUE)]
    legacy_results <- results[grep("^(site|country|study)", names(results), ignore.case = TRUE)]
    total_metrics <- length(analysis_results) + length(legacy_results)
    cat("Total metrics available:", total_metrics, "metrics\n")
    
    if (length(analysis_results) > 0) {
      cat("Analysis results:", length(analysis_results), "metrics\n")
    }
    if (length(legacy_results) > 0) {
      cat("Legacy results:", length(legacy_results), "metrics\n")
    }
  } else {
    cat("No analytics results available\n")
  }
}


# Use mapping names directly for domains

cardio_study <- create_longitudinal_study(
  study_id = "CARDIO-001",
  participants = 500,
  sites = 25,
  snapshots = 6,
  interval = "2 months",
  domains = c("AE", "LB", "VISIT", "QUERY"),
  run_analytics = FALSE,  # Just raw data for this example
  outlier_intensity = 2
)

summarize_longitudinal_study(cardio_study)

# Access specific domain data across snapshots
ae_timeline <- get_domain_timeline(cardio_study, "AE")
cat("Adverse events generated across", length(ae_timeline), "snapshots\n")

# Access specific snapshot data
snapshot_1_data <- get_snapshot_data(cardio_study, 1)
cat("Datasets in snapshot 1:", paste(names(snapshot_1_data), collapse = ", "), "\n")


# Direct configuration approach with pipes

# Note: The following complex study configuration demonstrates the config approach
complex_study <- create_study_config("COMPLEX-TRIAL-001", participant_count = 300, site_count = 20) %>%
  set_temporal_config(start_date = "2023-01-01", snapshot_count = 8, snapshot_width = "6 weeks") %>%
  add_dataset_config("Raw_AE", enabled = TRUE, 
                    count_formula = function(config, snapshot_idx = 1) {
                      base_count <- config$study_params$participant_count * 2.5
                      factor <- snapshot_idx / config$temporal_config$snapshot_count
                      round(base_count * factor)
                    }) %>%
  add_dataset_config("Raw_VISIT", enabled = TRUE) %>%
  add_dataset_config("Raw_LB", enabled = TRUE) %>%
  add_dataset_config("Raw_QUERY", enabled = TRUE) %>%
  add_dataset_config("Raw_DATACHG", enabled = TRUE)

# Preview configuration before generation
cat("Study Configuration:", complex_study$study_params$study_id, "\n")
cat("Participants:", complex_study$study_params$participant_count, "Sites:", complex_study$study_params$site_count, "\n")
cat("Enabled datasets:", paste(names(complex_study$dataset_configs), collapse = ", "), "\n")

# Generate the data (commented out to avoid long execution)
# complex_results <- generate_study_data(complex_study)


# For users who need full control over the configuration

config <- create_study_config(study_id = "ADVANCED-001", participant_count = 200, site_count = 15) %>%
  set_temporal_config(start_date = "2024-01-01", snapshot_count = 10, snapshot_width = "months") %>%
  add_dataset_config("Raw_AE", enabled = TRUE) %>%
  add_dataset_config("Raw_LB", enabled = TRUE)

# Validate configuration
validate_study_config(config)
cat("Advanced configuration created and validated successfully\n")

# The key improvement: Clean abstraction that hides complexity while providing flexibility
