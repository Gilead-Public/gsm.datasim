#' Generate raw data for endpoints study (multi-package)
#'
#' @param config Study configuration object with enabled datasets
#' @param domain_package_df Data frame mapping domains to packages
#' @return List of raw data for enabled datasets
#' @export
generate_raw_data_for_endpoints <- function(config, domain_package_df) {
  generate_snapshots_from_config(
    config = config,
    domain_package_df = domain_package_df,
    default_package = "gsm.mapping",
    workflow_path = "workflow/1_mappings",
    verbose = FALSE
  )
}

# WP1 core helper: resolve enabled mapping names from study config
get_enabled_mapping_names <- function(config) {
  enabled_datasets <- names(config$dataset_configs)[
    sapply(config$dataset_configs, function(x) x$enabled)
  ]
  gsub("^Raw_", "", enabled_datasets)
}

# WP1 core helper: resolve domain->package data frame for enabled mappings
resolve_domain_package_df <- function(config, domain_package_df = NULL, default_package = "gsm.mapping") {
  mapping_names <- get_enabled_mapping_names(config)

  if (is.null(domain_package_df)) {
    return(data.frame(
      domain = mapping_names,
      package = rep(default_package, length(mapping_names)),
      stringsAsFactors = FALSE
    ))
  }

  resolved <- domain_package_df[match(mapping_names, domain_package_df$domain), , drop = FALSE]
  if (nrow(resolved) == 0) {
    resolved <- data.frame(domain = character(0), package = character(0), stringsAsFactors = FALSE)
  }

  missing_idx <- is.na(resolved$package)
  if (any(missing_idx)) {
    resolved$domain[missing_idx] <- mapping_names[missing_idx]
    resolved$package[missing_idx] <- default_package
  }

  resolved <- resolved[!duplicated(resolved$domain), , drop = FALSE]
  resolved
}

# WP1 core helper: generate snapshots from config (single- and multi-package)
generate_snapshots_from_config <- function(config,
                                           domain_package_df = NULL,
                                           default_package = "gsm.mapping",
                                           workflow_path = "workflow/1_mappings",
                                           verbose = FALSE) {
  outlier_intensity <- if (!is.null(config$study_params$outlier_intensity)) {
    config$study_params$outlier_intensity
  } else {
    1
  }
  old_outlier_intensity <- getOption("gsm.datasim.outlier_intensity")
  options(gsm.datasim.outlier_intensity = outlier_intensity)
  on.exit(options(gsm.datasim.outlier_intensity = old_outlier_intensity), add = TRUE)

  domain_pkgs <- resolve_domain_package_df(config, domain_package_df, default_package)
  if (nrow(domain_pkgs) == 0) {
    return(list())
  }

  pkgs <- unique(domain_pkgs$package)
  raw_by_package <- list()

  for (pkg in pkgs) {
    domains_in_pkg <- domain_pkgs$domain[domain_pkgs$package == pkg]
    if (length(domains_in_pkg) == 0) next

    if (isTRUE(verbose)) {
      cat("Generating raw data for package", pkg, "with", length(domains_in_pkg), "domains...\n")
    }

    combined_specs <- load_specs(
      workflow_path = workflow_path,
      mappings = domains_in_pkg,
      package = pkg
    )
    prepared_specs <- prepare_combined_specs_for_generation(combined_specs)

    raw_by_package[[pkg]] <- generate_snapshots_from_combined_specs(
      SnapshotCount = config$temporal_config$snapshot_count,
      SnapshotWidth = config$temporal_config$snapshot_width,
      ParticipantCount = config$study_params$participant_count,
      SiteCount = config$study_params$site_count,
      StudyID = config$study_params$study_id,
      combined_specs = prepared_specs,
      mappings = domains_in_pkg,
      strStartDate = as.character(config$temporal_config$start_date)
    )
  }

  if (length(raw_by_package) == 0) {
    return(list())
  }

  if (length(raw_by_package) == 1) {
    return(raw_by_package[[1]])
  }

  first_pkg <- names(raw_by_package)[1]
  n_snapshots <- length(raw_by_package[[first_pkg]])
  all_raw_data <- vector("list", n_snapshots)

  for (i in seq_len(n_snapshots)) {
    merged_snapshot <- list()
    for (pkg in names(raw_by_package)) {
      snapshot_i <- raw_by_package[[pkg]][[i]]
      merged_snapshot <- c(merged_snapshot, snapshot_i)
    }
    all_raw_data[[i]] <- merged_snapshot
  }

  names(all_raw_data) <- names(raw_by_package[[first_pkg]])
  all_raw_data
}
#' Study Utility Functions
#'
#' This file contains utility functions for study validation, data generation,
#' and analytics pipeline execution.

#' Validate study input parameters
#'
#' @param participants Number of participants
#' @param sites Number of sites
#' @param snapshots Number of snapshots
#' @param domains Domains to include
#' @export
validate_study_inputs <- function(participants, sites, snapshots, domains) {
  if (participants <= 0) stop("Participants must be positive")
  if (sites <= 0) stop("Sites must be positive")
  if (snapshots <= 0) stop("Snapshots must be positive")
  if (length(domains) == 0) stop("At least one domain must be specified")
}

#' Ensure core mappings are included
#'
#' @param domains Vector of domain names
#' @return Vector with required core mappings added
#' @export
ensure_core_mappings <- function(domains) {
  # Required core mappings for any study
  required_mappings <- c("STUDY", "SITE", "SUBJ", "ENROLL")

  # Convert domains to mapping names (add Raw_ prefix)
  mapping_names <- paste0("Raw_", domains)

  # Add required mappings
  all_mappings <- unique(c(paste0("Raw_", required_mappings), mapping_names))

  return(all_mappings)
}

#' Generate study data across multiple snapshots
#'
#' @param study_id Study identifier
#' @param participants Number of participants
#' @param sites Number of sites
#' @param snapshots Number of snapshots
#' @param interval Time interval between snapshots
#' @param mappings Vector of mapping names to use
#' @param base_date Base date for snapshot generation (defaults to "2012-01-31" if NULL)
#' @param outlier_intensity Global multiplier for outlier-like values in domain generators.
#' @param verbose Whether to print progress/output messages
#' @return List of raw data for each snapshot
#' @export
generate_study_snapshots <- function(study_id, participants, sites, snapshots, interval, mappings,
                                     base_date = NULL, outlier_intensity = 1, verbose = FALSE) {
  snapshot_width <- parse_interval_to_snapshot_width(interval)

  # Calculate start dates for each snapshot
  if (is.null(base_date)) {
    base_date <- as.Date("2012-01-31")
  } else {
    base_date <- as.Date(base_date)
  }
  if (snapshot_width == "months") {
    base_month_start <- as.Date(format(base_date, "%Y-%m-01"))
    month_starts <- seq(base_month_start, length.out = snapshots, by = "month")
    start_dates <- lubridate::ceiling_date(month_starts, "month") - 1
  } else {
    start_dates <- seq(base_date, length.out = snapshots, by = snapshot_width)
  }

  # Ramp up participants and sites gradually across snapshots, mimicking real
  # enrollment patterns (the same approach used in generate_rawdata_for_single_study).
  subject_counts <- count_gen(participants, snapshots)
  site_counts    <- count_gen(sites, snapshots)

  raw_data_list <- list()

  for (i in 1:snapshots) {
    if (isTRUE(verbose)) cat("Generating snapshot", i, "of", snapshots,
                              "(", subject_counts[i], "participants,",
                              site_counts[i], "sites)\n")

    config <- create_study_config(
      study_id = study_id,
      participant_count = subject_counts[i],
      site_count        = site_counts[i],
      outlier_intensity = outlier_intensity
    )

    # Set temporal configuration with the specific start date for this snapshot
    config <- set_temporal_config(
      config,
      start_date = start_dates[i],
      snapshot_count = 1,
      snapshot_width = snapshot_width
    )

    # Add enabled datasets based on mappings
    for (mapping in mappings) {
      config <- add_dataset_config(config, mapping, enabled = TRUE)
    }
    config$verbose <- verbose

    raw_data <- generate_study_data(config, verbose = verbose)
    # Unwrap single-snapshot lists to avoid nested date keys
    if (is.list(raw_data) && length(raw_data) == 1 && is.list(raw_data[[1]])) {
      raw_data <- raw_data[[1]]
    }
    raw_data_list[[i]] <- raw_data
  }

  # Add the calculated dates as names to the list
  names(raw_data_list) <- as.character(start_dates)

  return(raw_data_list)
}

#' Parse interval string to snapshot width
#'
#' @param interval Interval string (e.g., "1 month", "2 weeks")
#' @return Snapshot width for temporal configuration
#' @export
parse_interval_to_snapshot_width <- function(interval) {
  if (grepl("month", interval, ignore.case = TRUE)) {
    return("months")
  } else if (grepl("week", interval, ignore.case = TRUE)) {
    return("weeks")
  } else if (grepl("day", interval, ignore.case = TRUE)) {
    return("days")
  } else {
    return("months")  # default
  }
}

#' Execute analytics pipeline
#'
#' @param raw_data Raw study data
#' @param config Study configuration object
#' @return Analytics pipeline results
#' @export
execute_analytics_pipeline <- function(raw_data, config) {
  verbose <- if (!is.null(config$verbose)) isTRUE(config$verbose) else FALSE
  vcat <- function(...) {
    if (isTRUE(verbose)) cat(...)
  }

  tryCatch({
    # Check if gsm.core is available
    if (!requireNamespace("gsm.core", quietly = TRUE)) {
      if (isTRUE(verbose)) message("gsm.core package not available. Skipping analytics pipeline.")
      return(NULL)
    }

    vcat("Running analytics pipeline on", length(raw_data), "snapshots...\n")

    # Determine workflow configuration once
    if (!is.null(config$study_params$analytics_package) && !is.null(config$study_params$analytics_workflows)) {
      lWorkflow <- gsm.core::MakeWorkflowList(
        strPackage = config$study_params$analytics_package,
        strNames = config$study_params$analytics_workflows
      )
    } else if (!is.null(config$study_params$analytics_package)) {
      lWorkflow <- gsm.core::MakeWorkflowList(
        strPackage = config$study_params$analytics_package
      )
    } else {
      lWorkflow <- gsm.core::MakeWorkflowList(
        strPackage = "gsm.kri"
      )
    }

    snapshot_names <- names(raw_data)
    if (is.null(snapshot_names) || any(snapshot_names == "")) {
      snapshot_names <- paste0("snapshot_", seq_along(raw_data))
    }
    snapshot_results <- stats::setNames(vector("list", length(raw_data)), snapshot_names)

    for (i in seq_along(raw_data)) {
      snapshot_name <- snapshot_names[[i]]
      snapshot_data <- raw_data[[i]]

      vcat("\nProcessing snapshot ", i, "/", length(raw_data), " (", snapshot_name, ")...\n", sep = "")
      vcat("Available datasets:", paste(names(snapshot_data), collapse = ", "), "\n")

      required_datasets <- c("Raw_SITE", "Raw_SUBJ")
      available_datasets <- intersect(required_datasets, names(snapshot_data))
      if (length(available_datasets) < length(required_datasets)) {
        warning("Skipping snapshot ", snapshot_name, ": missing required datasets (", paste(required_datasets, collapse = ", "), ")")
        snapshot_results[[snapshot_name]] <- NULL
        next
      }

      # Load all mapping workflows from gsm.mapping (no strNames filter, matching the
      # run_gsm_workflows.R pattern). Run each individually so that a workflow whose
      # source data is absent fails silently rather than blocking the rest.

      # Workflows backed by a Raw_* dataset in this snapshot
      available_raw_names <- stringr::str_replace(names(snapshot_data), "^Raw_", "")

      # Derived mappings that have no Raw_* source but depend on prior mapped outputs
      derived_mapping_names <- c("COUNTRY") 
      
      # Add EXCLUSION if IE, ENROLL, and PD are all included in the raw data
      if (all(c("IE", "ENROLL", "PD") %in% available_raw_names)) {
        derived_mapping_names <- c(derived_mapping_names, "EXCLUSION")
      }

      workflows_to_run <- unique(c(available_raw_names, derived_mapping_names))

      all_mappings_wf <- gsm.core::MakeWorkflowList(
        strNames   = workflows_to_run,
        strPath    = "workflow/1_mappings",
        strPackage = "gsm.mapping"
      )

      # Ingest only from raw-data-backed workflows (derived ones have no Raw_* spec)
      raw_backed_names <- intersect(names(all_mappings_wf), available_raw_names)
      raw_mappings_wf  <- all_mappings_wf[raw_backed_names]
      mappings_spec    <- CombineSpecs(raw_mappings_wf)
      lRaw             <- Ingest(snapshot_data, mappings_spec)

      # Run each workflow in order, accumulating results so derived mappings
      # (e.g. COUNTRY) can see the output of earlier ones (e.g. Mapped_SITE)
      mapped_data <- list()
      for (mwf_name in names(all_mappings_wf)) {
        single_mwf <- all_mappings_wf[mwf_name]
        result <- tryCatch(
          gsm.core::RunWorkflows(lWorkflow = single_mwf, lData = c(lRaw, mapped_data)),
          error = function(e) NULL
        )
        if (!is.null(result)) {
          mapped_data <- c(mapped_data, result)
        }
      }

      lResults <- list()
      for (wf_name in names(lWorkflow)) {
        single_wf <- lWorkflow[wf_name]
        wf_result <- tryCatch(
          gsm.core::RunWorkflows(
            lWorkflow = single_wf,
            lData = mapped_data
          ),
          error = function(e) {
            warning("Skipping workflow ", wf_name, " for snapshot ", snapshot_name, ": ", e$message)
            return(NULL)
          }
        )
        if (!is.null(wf_result)) {
          lResults <- c(lResults, wf_result)
        }
      }

      analytics_summary <- list(
        snapshot = snapshot_name,
        total_participants = nrow(snapshot_data$Raw_SUBJ %||% data.frame()),
        total_sites = nrow(snapshot_data$Raw_SITE %||% data.frame()),
        domains_available = names(snapshot_data),
        snapshots_processed = 1,
        workflows_executed = names(lResults),
        kri_results = length(lResults)
      )

      snapshot_results[[snapshot_name]] <- list(
        summary = analytics_summary,
        results = lResults,
        mapped = mapped_data,
        lWorkflow = lWorkflow,
        data = snapshot_data
      )
    }

    processed_count <- sum(!vapply(snapshot_results, is.null, logical(1)))
    vcat("\nAnalytics pipeline completed for ", processed_count, " of ", length(snapshot_results), " snapshots.\n", sep = "")

    if (processed_count == 0) {
      return(NULL)
    }

    return(snapshot_results)

  }, error = function(e) {
    warning("GSM analytics pipeline failed: ", e$message)
    vcat("Analytics pipeline skipped due to error.\n")
    return(NULL)
  })
}

organize_analytics_results <- function(pipeline_results, verbose = FALSE) {
  vcat <- function(...) if (isTRUE(verbose)) cat(...)

  organize_metric <- function(metric_name, metric_payload) {
    metric_id <- sub("^Analysis_", "", metric_name)

    data_frames <- lapply(metric_payload, function(tbl) {
      if (!is.data.frame(tbl)) return(tbl)
      if (!("Metric_ID" %in% names(tbl))) {
        tbl$Metric_ID <- metric_id
      }
      tbl
    })

    list(
      metric_id = metric_id,
      data_frames = data_frames
    )
  }

  organize_snapshot <- function(snapshot_results) {
    if (is.null(snapshot_results$results) || !is.list(snapshot_results$results)) {
      return(list())
    }

    metric_names <- names(snapshot_results$results)
    analysis_metric_names <- metric_names[startsWith(metric_names, "Analysis_")]

    out <- list()
    for (metric_name in analysis_metric_names) {
      metric_payload <- snapshot_results$results[[metric_name]]
      if (!is.list(metric_payload)) next
      metric_id <- sub("^Analysis_", "", metric_name)
      out[[metric_id]] <- organize_metric(metric_name, metric_payload)
    }

    out
  }

  if (is.null(pipeline_results) || !is.list(pipeline_results)) {
    return(list())
  }

  if ("results" %in% names(pipeline_results)) {
    vcat("Organizing analytics for single snapshot...\n")
    return(organize_snapshot(pipeline_results))
  }

  snapshot_names <- names(pipeline_results)
  if (is.null(snapshot_names) || any(snapshot_names == "")) {
    snapshot_names <- as.character(seq_along(pipeline_results))
  }

  vcat("Organizing analytics for ", length(snapshot_names), " snapshots...\n", sep = "")

  stats::setNames(
    lapply(snapshot_names, function(snapshot_name) {
      organize_snapshot(pipeline_results[[snapshot_name]])
    }),
    snapshot_names
  )
}

#' Generate analytics layers from raw data
#'
#' Convenience wrapper that executes the analytics pipeline and returns
#' the raw analytics results.
#'
#' @param raw_data Raw study data (single or multi-snapshot list)
#' @param config Study configuration object
#' @param verbose Whether to print progress/output messages
#' @return Raw analytics pipeline results
#' @export
generate_analytics_layers <- function(raw_data, config, verbose = FALSE) {
  config$verbose <- verbose
  execute_analytics_pipeline(raw_data, config)
}

#' Execute the reporting pipeline using gsm.reporting workflows
#'
#' Runs the gsm.reporting workflow layer (workflow/3_reporting) for each snapshot
#' using the mapped data, analytics results, and workflow list from the analytics
#' pipeline output. Returns a named list of reporting results per snapshot.
#'
#' @param analytics_results Output from \code{execute_analytics_pipeline}
#' @param config Study configuration object
#' @return Named list of reporting results per snapshot
#' @export
execute_reporting_pipeline <- function(analytics_results, config) {
  verbose <- if (!is.null(config$verbose)) isTRUE(config$verbose) else FALSE
  vcat <- function(...) if (isTRUE(verbose)) cat(...)

  tryCatch({
    if (!requireNamespace("gsm.reporting", quietly = TRUE)) {
      if (isTRUE(verbose)) message("gsm.reporting package not available. Skipping reporting pipeline.")
      return(NULL)
    }
    if (!requireNamespace("gsm.core", quietly = TRUE)) {
      if (isTRUE(verbose)) message("gsm.core package not available. Skipping reporting pipeline.")
      return(NULL)
    }
    if (is.null(analytics_results)) {
      if (isTRUE(verbose)) message("No analytics results provided. Skipping reporting pipeline.")
      return(NULL)
    }

    reporting_package  <- config$study_params$reporting_package  %||% "gsm.reporting"
    reporting_workflows <- config$study_params$reporting_workflows

    if (!is.null(reporting_workflows)) {
      reporting_wf <- gsm.core::MakeWorkflowList(
        strPackage = reporting_package,
        strNames   = reporting_workflows,
        strPath    = "workflow/3_reporting"
      )
    } else {
      reporting_wf <- gsm.core::MakeWorkflowList(
        strPackage = reporting_package,
        strPath    = "workflow/3_reporting"
      )
    }

    snapshot_names   <- names(analytics_results)
    snapshot_results <- stats::setNames(vector("list", length(analytics_results)), snapshot_names)

    for (snapshot_name in snapshot_names) {
      snap <- analytics_results[[snapshot_name]]
      if (is.null(snap)) {
        snapshot_results[[snapshot_name]] <- NULL
        next
      }

      mapped     <- snap$mapped
      lAnalyzed  <- snap$results
      lWorkflow  <- snap$lWorkflow

      if (is.null(mapped) || is.null(lAnalyzed) || is.null(lWorkflow)) {
        warning("Skipping reporting for snapshot ", snapshot_name,
                ": analytics pipeline output is missing mapped data, results, or workflow list.")
        snapshot_results[[snapshot_name]] <- NULL
        next
      }

      vcat("Running reporting pipeline for snapshot: ", snapshot_name, "\n", sep = "")

      lReporting <- tryCatch(
        gsm.core::RunWorkflows(
          lWorkflow = reporting_wf,
          lData     = c(mapped, list(lAnalyzed = lAnalyzed, lWorkflows = lWorkflow))
        ),
        error = function(e) {
          warning("Reporting pipeline failed for snapshot ", snapshot_name, ": ", e$message)
          NULL
        }
      )

      snapshot_results[[snapshot_name]] <- lReporting
    }

    processed_count <- sum(!vapply(snapshot_results, is.null, logical(1)))
    vcat("Reporting pipeline completed for ", processed_count, " of ",
         length(snapshot_results), " snapshots.\n", sep = "")

    if (processed_count == 0) return(NULL)
    return(snapshot_results)

  }, error = function(e) {
    warning("GSM reporting pipeline failed: ", e$message)
    return(NULL)
  })
}

#' Generate reporting layers from analytics results
#'
#' Convenience wrapper that runs the gsm.reporting pipeline and returns
#' the raw reporting results per snapshot.
#'
#' @param analytics_results Output from \code{execute_analytics_pipeline} or
#'   \code{generate_analytics_layers}
#' @param config Study configuration object
#' @param verbose Whether to print progress/output messages
#' @return Named list of reporting results per snapshot
#' @export
generate_reporting_layers <- function(analytics_results, config, verbose = FALSE) {
  config$verbose <- verbose
  execute_reporting_pipeline(analytics_results, config)
}

#' Generate raw data from a study config
#'
#' Convenience wrapper for generating snapshot raw data directly from a
#' configured study object.
#'
#' @param config Study configuration object with enabled datasets.
#' @param verbose Whether to print progress/output messages.
#' @return List of raw data for enabled datasets.
#' @export
generate_raw_data_from_config <- function(config, verbose = FALSE) {
  generate_snapshots_from_config(
    config = config,
    domain_package_df = NULL,
    default_package = "gsm.mapping",
    workflow_path = "workflow/1_mappings",
    verbose = verbose
  )
}

#' Generate study data with configuration
#'
#' @param config Study configuration object
#' @param workflow_path Path to workflow mappings (not used in new approach)
#' @param mappings Optional mappings list (not used in new approach)
#' @param package Package containing workflows (not used in new approach)
#' @param verbose Whether to print progress/output messages
#' @return List of generated study data
#' @export
generate_study_data <- function(config, workflow_path = "workflow/1_mappings",
                                           mappings = NULL, package = "gsm.mapping",
                                           verbose = FALSE) {

  # Use the new helper function instead of the old generate_rawdata_for_single_study
  raw_data <- generate_raw_data_from_config(config, verbose = verbose)

  return(raw_data)
}
