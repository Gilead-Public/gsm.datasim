#' Create Study Configuration
#'
#' Creates a study configuration list with study parameters, temporal configuration,
#' and dataset specifications for clinical trial data generation.
#'
#' @param study_id Study identifier
#' @param participant_count Number of participants
#' @param site_count Number of sites
#' @param analytics_package Analytics package to use
#' @param analytics_workflows Specific workflows to run
#' @param reporting_package Reporting package to use (default: \code{"gsm.reporting"})
#' @param reporting_workflows Specific reporting workflows to run (default: all)
#' @param outlier_intensity Global multiplier for outlier-like values in domain generators.
#'   Use \code{1} for current baseline, values \code{>1} to increase outlier prevalence.
#'
#' @return A list containing study configuration
#' @examples
#' config <- create_study_config("STUDY001", participant_count = 200, site_count = 15)
#' config$study_params$study_id
#' config$temporal_config$snapshot_count
#' @export
create_study_config <- function(study_id = "STUDY001", participant_count = 100, site_count = 10,
                                analytics_package = NULL, analytics_workflows = NULL,
                                reporting_package = NULL, reporting_workflows = NULL,
                                outlier_intensity = 1) {
  config <- list(
    study_params = list(
      study_id = study_id,
      participant_count = participant_count,
      site_count = site_count,
      analytics_package = analytics_package,
      analytics_workflows = analytics_workflows,
      reporting_package = reporting_package,
      reporting_workflows = reporting_workflows,
      outlier_intensity = outlier_intensity
    ),
    temporal_config = list(
      start_date = as.Date("2023-01-01"),
      snapshot_count = 5,
      snapshot_width = "months",
      end_date = NULL
    ),
    dataset_configs = list()
  )

  # Add default required datasets
  config <- add_dataset_config(config, "Raw_STUDY", enabled = TRUE, count_formula = 1)
  config <- add_dataset_config(config, "Raw_SITE", enabled = TRUE, count_formula = function(config) config$study_params$site_count)
  config <- add_dataset_config(config, "Raw_SUBJ", enabled = TRUE, count_formula = function(config) config$study_params$participant_count)
  config <- add_dataset_config(config, "Raw_ENROLL", enabled = TRUE, count_formula = function(config) config$study_params$participant_count)

  class(config) <- c("study_config", "list")
  return(config)
}

#' Set Outlier Configuration
#'
#' Updates outlier generation intensity in a study config.
#'
#' @param config Study configuration list
#' @param intensity Global outlier intensity multiplier. \code{1} keeps current
#'   behavior, values \code{>1} increase outlier prevalence.
#'
#' @return Updated study configuration
#' @examples
#' config <- create_study_config("TRIAL001")
#' config <- set_outlier_config(config, intensity = 2)
#' config$study_params$outlier_intensity
#' @export
set_outlier_config <- function(config, intensity = 1) {
  config$study_params$outlier_intensity <- intensity
  return(config)
}

#' Set Temporal Configuration
#'
#' Updates temporal configuration settings in a study config.
#'
#' @param config Study configuration list
#' @param start_date Study start date
#' @param snapshot_count Number of snapshots
#' @param snapshot_width Time between snapshots
#' @param end_date Study end date
#'
#' @return Updated study configuration
#' @examples
#' config <- create_study_config("TRIAL001")
#' config <- set_temporal_config(config, start_date = "2023-06-01", snapshot_count = 6)
#' config$temporal_config$snapshot_count
#' @export
set_temporal_config <- function(config, start_date = NULL, snapshot_count = NULL,
                                snapshot_width = NULL, end_date = NULL) {
  if (!is.null(start_date)) config$temporal_config$start_date <- as.Date(start_date)
  if (!is.null(snapshot_count)) config$temporal_config$snapshot_count <- snapshot_count
  if (!is.null(snapshot_width)) config$temporal_config$snapshot_width <- snapshot_width
  if (!is.null(end_date)) {
    tryCatch(
      {
        config$temporal_config$end_date <- as.Date(end_date)
      },
      error = function(e) {
        # Skip setting end_date if conversion fails
        NULL
      }
    )
  }

  return(config)
}

#' Add Dataset Configuration
#'
#' Adds a dataset configuration to the study config.
#'
#' @param config Study configuration list
#' @param dataset_type Type of dataset (e.g., "Raw_AE")
#' @param enabled Whether the dataset should be generated
#' @param count_formula Formula for calculating record count
#' @param growth_pattern How the dataset grows over time
#' @param dependencies Dependencies on other datasets
#' @param custom_args Additional arguments for the dataset generator
#'
#' @return Updated study configuration
#' @examples
#' config <- create_study_config("TRIAL001")
#' config <- add_dataset_config(config, "Raw_AE", enabled = TRUE)
#' "Raw_AE" %in% names(config$dataset_configs)
#' @export
add_dataset_config <- function(config, dataset_type, enabled = TRUE, count_formula = NULL,
                               growth_pattern = "linear", dependencies = character(0),
                               custom_args = list()) {
  config$dataset_configs[[dataset_type]] <- list(
    enabled = enabled,
    count_formula = count_formula,
    growth_pattern = growth_pattern,
    dependencies = dependencies,
    custom_args = custom_args
  )

  return(config)
}

#' Remove Dataset Configuration
#'
#' Removes a dataset configuration from the study config.
#'
#' @param config Study configuration list
#' @param dataset_type Type of dataset to remove
#'
#' @return Updated study configuration
#' @examples
#' config <- create_study_config("TRIAL001")
#' config <- add_dataset_config(config, "Raw_AE", enabled = TRUE)
#' config <- remove_dataset_config(config, "Raw_AE")
#' "Raw_AE" %in% names(config$dataset_configs)
#' @export
remove_dataset_config <- function(config, dataset_type) {
  config$dataset_configs[[dataset_type]] <- NULL
  return(config)
}

#' Validate Study Configuration
#'
#' Validates study configuration parameters.
#'
#' @param config Study configuration list
#'
#' @return TRUE if valid, stops with error if invalid
#' @examples
#' config <- create_study_config("TRIAL001", participant_count = 100, site_count = 10)
#' config <- set_temporal_config(config, snapshot_count = 3)
#' validate_study_config(config)
#' @export
validate_study_config <- function(config) {
  # Basic checks only
  if (config$temporal_config$snapshot_count < 1) {
    stop("snapshot_count must be at least 1")
  }

  if (config$study_params$participant_count < 1) {
    stop("participant_count must be at least 1")
  }

  if (config$study_params$site_count < 1) {
    stop("site_count must be at least 1")
  }

  if (!is.null(config$study_params$outlier_intensity) &&
    (!is.numeric(config$study_params$outlier_intensity) ||
      length(config$study_params$outlier_intensity) != 1 ||
      is.na(config$study_params$outlier_intensity) ||
      config$study_params$outlier_intensity < 0)) {
    stop("outlier_intensity must be a single non-negative numeric value")
  }

  return(TRUE)
}

#' Create Study Configuration for Standard Datasets
#'
#' Convenience function to create a study configuration with standard clinical datasets.
#'
#' @param study_id Study identifier
#' @param participant_count Number of participants
#' @param site_count Number of sites
#' @param analytics_package Analytics package to use
#' @param analytics_workflows Specific workflows to run
#' @param study Include study metadata (Raw_STUDY)
#' @param subjects Include subject demographics (Raw_SUBJ)
#' @param sites_data Include site information (Raw_SITE)
#' @param adverse_events Include adverse event data
#' @param protocol_deviations Include protocol deviation data
#' @param lab_data Include laboratory data
#' @param subject_visits Include subject visit data (Raw_VISIT)
#' @param visit_schedule Include visit schedule data (Raw_VISIT)
#' @param enrollment Include enrollment data
#' @param data_changes Include data change tracking (Raw_DATACHG)
#' @param data_entry Include data entry tracking (Raw_DATAENT)
#' @param queries Include query data (Raw_QUERY)
#' @param pharmacokinetics Include pharmacokinetics data
#' @param study_drug_completion Include study drug completion (Raw_SDRGCOMP)
#' @param study_completion Include overall study completion (Raw_STUDCOMP)
#' @param inclusion_exclusion Include inclusion/exclusion criteria (Raw_IE)
#' @param exclusions Include exclusion tracking (Raw_EXCLUSION)
#' @param country Include country mapping
#' @param death Include death events (Raw_Death)
#' @param randomization Include randomization data (Raw_Randomization)
#' @param overall_response Include overall response data (Raw_OverallResponse)
#' @param outlier_intensity Global multiplier for outlier-like values in domain generators.
#'
#' @return Study configuration with standard datasets
#' @examples
#' # All default datasets
#' config <- create_standard_study_config("TRIAL001", participant_count = 100, site_count = 10)
#' names(config$dataset_configs)
#'
#' # Select a subset of domains
#' config <- create_standard_study_config(
#'   "TRIAL002",
#'   participant_count = 50,
#'   adverse_events = TRUE, lab_data = TRUE,
#'   pharmacokinetics = FALSE, overall_response = FALSE
#' )
#' @export
create_standard_study_config <- function(study_id = "STUDY001", participant_count = 100, site_count = 10,
                                         analytics_package = NULL, analytics_workflows = NULL,
                                         study = TRUE, subjects = TRUE, sites_data = TRUE,
                                         adverse_events = TRUE, protocol_deviations = TRUE,
                                         lab_data = TRUE, subject_visits = TRUE,
                                         visit_schedule = TRUE, enrollment = TRUE,
                                         data_changes = TRUE, data_entry = TRUE,
                                         queries = TRUE, pharmacokinetics = TRUE,
                                         study_drug_completion = TRUE, study_completion = TRUE,
                                         inclusion_exclusion = TRUE, exclusions = TRUE,
                                         country = TRUE,
                                         death = TRUE, randomization = TRUE, overall_response = TRUE,
                                         outlier_intensity = 1) {
  config <- create_study_config(
    study_id = study_id,
    participant_count = participant_count,
    site_count = site_count,
    analytics_package = analytics_package,
    analytics_workflows = analytics_workflows,
    outlier_intensity = outlier_intensity
  )

  # Core datasets (override automatic inclusion if user wants to disable)
  if (!study) config <- remove_dataset_config(config, "Raw_STUDY")
  if (!subjects) config <- remove_dataset_config(config, "Raw_SUBJ")
  if (!sites_data) config <- remove_dataset_config(config, "Raw_SITE")

  if (adverse_events) config <- add_dataset_config(config, "Raw_AE", enabled = TRUE)
  if (protocol_deviations) config <- add_dataset_config(config, "Raw_PD", enabled = TRUE)
  if (lab_data) config <- add_dataset_config(config, "Raw_LB", enabled = TRUE)
  if (subject_visits || visit_schedule) config <- add_dataset_config(config, "Raw_VISIT", enabled = TRUE)
  if (enrollment) config <- add_dataset_config(config, "Raw_ENROLL", enabled = TRUE)
  if (data_changes) config <- add_dataset_config(config, "Raw_DATACHG", enabled = TRUE)
  if (data_entry) config <- add_dataset_config(config, "Raw_DATAENT", enabled = TRUE)
  if (queries) config <- add_dataset_config(config, "Raw_QUERY", enabled = TRUE)
  if (pharmacokinetics) config <- add_dataset_config(config, "Raw_PK", enabled = TRUE)
  if (study_drug_completion) config <- add_dataset_config(config, "Raw_SDRGCOMP", enabled = TRUE)
  if (study_completion) config <- add_dataset_config(config, "Raw_STUDCOMP", enabled = TRUE)
  if (inclusion_exclusion) config <- add_dataset_config(config, "Raw_IE", enabled = TRUE)
  if (exclusions) config <- add_dataset_config(config, "Raw_EXCLUSION", enabled = TRUE)
  if (country) config <- add_dataset_config(config, "Raw_COUNTRY", enabled = TRUE)
  if (death) config <- add_dataset_config(config, "Raw_Death", enabled = TRUE)
  if (randomization) config <- add_dataset_config(config, "Raw_Randomization", enabled = TRUE)
  if (overall_response) config <- add_dataset_config(config, "Raw_OverallResponse", enabled = TRUE)

  return(config)
}

#' Example of config interface usage
#'
#' @name study_config_examples
#' @examples
#' \dontrun{
#' # Simple study with standard datasets using config approach
#' config <- create_study_config("ONCOLOGY001", participant_count = 200, site_count = 15) %>%
#'   set_temporal_config(start_date = "2023-01-01", snapshot_count = 12, snapshot_width = "months") %>%
#'   add_dataset_config("Raw_AE",
#'     enabled = TRUE,
#'     count_formula = function(config, snapshot_idx = 1) {
#'       base_count <- config$study_params$participant_count * 2.5
#'       factor <- snapshot_idx / config$temporal_config$snapshot_count
#'       round(base_count * factor)
#'     }
#'   ) %>%
#'   add_dataset_config("Raw_VISIT", enabled = TRUE)
#' study_data <- generate_study_data(config)
#'
#' # Using convenience function for standard datasets
#' config <- create_standard_study_config("TRIAL002",
#'   participant_count = 100, site_count = 10,
#'   adverse_events = TRUE, lab_data = TRUE
#' )
#'
#' # Custom dataset configuration
#' config <- create_study_config("CUSTOM001", participant_count = 300, site_count = 20) %>%
#'   add_dataset_config("Raw_Biomarker",
#'     enabled = TRUE,
#'     count_formula = function(config) config$study_params$participant_count * 5,
#'     dependencies = "Raw_SUBJ"
#'   )
#' study_data <- generate_study_data(config)
#' }
NULL


#' Create Longitudinal Study Data Structure
#'
#' Creates a longitudinal study data structure that encapsulates study data
#' and provides intuitive access methods for different analysis perspectives.
#'
#' @param study_id Study identifier
#' @param raw_data Raw study data snapshots
#' @param config Configuration parameters
#'
#' @return A longitudinal study data structure
#' @examples
#' config <- list(participants = 50, sites = 5, snapshots = 2, interval = "1 month",
#'                domains = c("AE", "LB"))
#' study <- create_longitudinal_study_data("MY-STUDY", raw_data = list(), config = config)
#' study$study_id
#' @export
create_longitudinal_study_data <- function(study_id, raw_data, config) {
  structure(
    list(
      study_id = study_id,
      raw_data = raw_data,
      config = config,
      analytics = NULL
    ),
    class = c("longitudinal_study", "list")
  )
}

#' Get Summary of Longitudinal Study
#'
#' Display comprehensive study summary.
#'
#' @param study Longitudinal study data structure
#' @param verbose Whether to print summary output
#'
#' @return Invisibly returns the study structure
#' @examples
#' \dontrun{
#' study <- create_longitudinal_study("STUDY-001", participants = 50, sites = 5, snapshots = 2)
#' summarize_longitudinal_study(study)
#' }
#' @export
summarize_longitudinal_study <- function(study, verbose = TRUE) {
  if (isTRUE(verbose)) {
    cat("Longitudinal Study Summary\n")
    cat("=========================\n")
    cat(sprintf("Study ID: %s\n", study$study_id))
    cat(sprintf("Participants: %d\n", study$config$participants))
    cat(sprintf("Sites: %d\n", study$config$sites))
    snapshot_count <- if (!is.null(study$config$snapshots)) study$config$snapshots else length(study$raw_data)
    cat(sprintf("Snapshots: %d\n", snapshot_count))
    cat(sprintf("Interval: %s\n", study$config$interval))
    cat(sprintf("Domains: %s\n", paste(study$config$domains, collapse = ", ")))
    cat(sprintf("Analytics Available: %s\n", !is.null(study$analytics)))
  }

  if (isTRUE(verbose) && length(study$raw_data) > 0) {
    cat(sprintf("\nData Snapshots: %d\n", length(study$raw_data)))
    cat("Available datasets per snapshot:\n")
    for (i in seq_along(study$raw_data)[1:min(3, length(study$raw_data))]) {
      # Extract date from the names of the raw_data list
      snapshot_date <- names(study$raw_data)[i]
      if (is.null(snapshot_date) || snapshot_date == "") {
        snapshot_date <- "Unknown"
      }
      cat(sprintf("  Snapshot %d: %s\n", i, snapshot_date))
    }
    if (length(study$raw_data) > 3) {
      cat("  ...\n")
    }
  }

  invisible(study)
}

#' Run Analytics Pipeline on Longitudinal Study
#'
#' Execute full analytics pipeline on study data.
#'
#' @param study Longitudinal study data structure
#'
#' @return Updated study structure with analytics results
#' @examples
#' \dontrun{
#' study <- create_longitudinal_study(
#'   "STUDY-001", participants = 50, sites = 5, snapshots = 2,
#'   analytics_package = "gsm.kri"
#' )
#' study <- run_longitudinal_analytics(study)
#' names(study$analytics)
#' }
#' @param verbose Whether to print progress output
#' @export
run_longitudinal_analytics <- function(study, verbose = FALSE) {
  verbose <- if (!is.null(study$config$verbose)) isTRUE(study$config$verbose) else verbose
  study$analytics <- generate_analytics_layers(
    raw_data = study$raw_data,
    config = study$config,
    verbose = verbose
  )

  return(study)
}

#' Run Reporting Pipeline on Longitudinal Study
#'
#' Execute the gsm.reporting pipeline on study analytics results, producing a
#' \code{reporting} list of data frames on the study object (one entry per snapshot).
#' The study must already have analytics results (run \code{run_longitudinal_analytics}
#' first, or call \code{create_longitudinal_study} with \code{run_analytics = TRUE}).
#'
#' @param study Longitudinal study data structure
#' @param verbose Whether to print progress output
#' @return Updated study structure with \code{study$reporting} populated
#' @examples
#' \dontrun{
#' study <- create_longitudinal_study(
#'   "STUDY-001", participants = 50, sites = 5, snapshots = 2,
#'   run_analytics = TRUE, analytics_package = "gsm.kri"
#' )
#' study <- run_longitudinal_reporting(study)
#' names(study$reporting)
#' }
#' @export
run_longitudinal_reporting <- function(study, verbose = FALSE) {
  verbose <- if (!is.null(study$config$verbose)) isTRUE(study$config$verbose) else verbose

  if (is.null(study$analytics)) {
    stop("No analytics results found on study object. Run run_longitudinal_analytics() first.")
  }

  study$reporting <- generate_reporting_layers(
    analytics_results = study$analytics,
    config = study$config,
    verbose = verbose
  )

  return(study)
}

#' Get Data for Specific Snapshot
#'
#' Retrieves data for a specific snapshot from longitudinal study.
#'
#' @param study Longitudinal study data structure
#' @param snapshot Snapshot number (1-based)
#'
#' @return Data for the specified snapshot
#' @examples
#' \dontrun{
#' study <- create_longitudinal_study("STUDY-001", participants = 50, sites = 5, snapshots = 3)
#' snap1 <- get_snapshot_data(study, snapshot = 1)
#' names(snap1)
#' }
#' @export
get_snapshot_data <- function(study, snapshot) {
  if (snapshot < 1 || snapshot > length(study$raw_data)) {
    stop(sprintf("Snapshot %d not available. Study has %d snapshots.", snapshot, length(study$raw_data)))
  }
  return(study$raw_data[[snapshot]])
}

#' Get Domain Timeline Data
#'
#' Get specific domain data across all snapshots.
#'
#' @param study Longitudinal study data structure
#' @param domain_name Domain mapping name (e.g., "AE", "LB")
#'
#' @return Timeline data for the specified domain
#' @examples
#' \dontrun{
#' study <- create_longitudinal_study("STUDY-001", participants = 50, sites = 5,
#'                                     snapshots = 3, domains = c("AE", "LB"))
#' ae_timeline <- get_domain_timeline(study, "AE")
#' length(ae_timeline) # one entry per snapshot
#' }
#' @export
get_domain_timeline <- function(study, domain_name) {
  raw_name <- paste0("Raw_", domain_name)

  timeline_data <- list()
  for (i in seq_along(study$raw_data)) {
    if (raw_name %in% names(study$raw_data[[i]])) {
      timeline_data[[paste0("snapshot_", i)]] <- study$raw_data[[i]][[raw_name]]
    }
  }

  return(timeline_data)
}

#' Get Available Domain Names
#'
#' `r lifecycle::badge("experimental")`
#'
#' Return list of available domain names across all snapshots.
#'
#' @param study Longitudinal study data structure
#'
#' @return Character vector of domain names
#' @examples
#' \dontrun{
#' study <- create_longitudinal_study("STUDY-001", participants = 50, sites = 5, snapshots = 2)
#' get_available_domains(study)
#' }
#' @export
get_available_domains <- function(study) {
  if (length(study$raw_data) > 0) {
    return(unique(unlist(lapply(study$raw_data, names))))
  }
  return(character(0))
}
