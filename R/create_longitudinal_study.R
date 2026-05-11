#' High-Level Study Creation API
#'
#' This file contains convenience functions for creating studies with minimal configuration.
#' These functions provide simplified interfaces for common study types.

#' Create longitudinal study data
#'
#' Creates a complete longitudinal study with multiple snapshots
#' @param study_id Study identifier
#' @param participants Number of participants
#' @param sites Number of sites
#' @param snapshots Number of snapshots
#' @param interval Time between snapshots (e.g., "1 month", "2 weeks")
#' @param domains Clinical domains to include
#' @param run_analytics Whether to run the analytics pipeline (default FALSE)
#' @param analytics_package Package containing workflows (optional)
#' @param analytics_workflows Specific workflows to run (optional)
#' @param run_reporting Whether to run the reporting pipeline after analytics (default FALSE)
#' @param outlier_intensity Global multiplier for outlier-like values in domain generators.
#' @param verbose Whether to print progress/output messages
#' @return LongitudinalStudy object with generated data
#' @export
create_longitudinal_study <- function(study_id = "STUDY-001",
                                     participants = 100,
                                     sites = 10,
                                     snapshots = 5,
                                     interval = "1 month",
                                     domains = c("AE", "LB", "VISIT"),
                                     run_analytics = FALSE,
                                     analytics_package = NULL,
                                     analytics_workflows = NULL,
                                     run_reporting = FALSE,
                                     outlier_intensity = 1,
                                     verbose = FALSE) {

  # Validate inputs
  validate_study_inputs(participants, sites, snapshots, domains)

  # Ensure required mappings are included
  mappings <- ensure_core_mappings(domains)

  # Generate the raw data for all snapshots
  raw_data <- generate_study_snapshots(
    study_id,
    participants,
    sites,
    snapshots,
    interval,
    mappings,
    outlier_intensity = outlier_intensity,
    verbose = verbose
  )

  # Create study object with proper config
  config <- list(
    participants = participants,
    sites = sites,
    snapshots = snapshots,
    interval = interval,
    domains = domains, # domains should be without Raw_
    study_type = "standard",  # default for this function
    analytics_package = analytics_package,
    analytics_workflows = analytics_workflows,
    verbose = verbose
  )

  study <- create_longitudinal_study_data(
    study_id = study_id,
    raw_data = raw_data,
    config = config
  )

  # Run analytics pipeline if requested
  if (run_analytics) {
    if (isTRUE(verbose)) cat("Running analytics pipeline...\n")

    # Create configuration for analytics
    analytics_config <- create_study_config(
      study_id = study_id,
      participant_count = participants,
      site_count = sites,
      analytics_package = analytics_package,
      analytics_workflows = analytics_workflows,
      outlier_intensity = outlier_intensity
    )
    analytics_config$domains <- domains
    analytics_config$verbose <- verbose

    study$analytics <- generate_analytics_layers(
      raw_data = raw_data,
      config = analytics_config,
      verbose = verbose
    )

    if (isTRUE(run_reporting)) {
      if (isTRUE(verbose)) cat("Running reporting pipeline...\n")
      study$reporting <- generate_reporting_layers(
        analytics_results = study$analytics,
        config = analytics_config,
        verbose = verbose
      )
    }
  }

  return(study)
}

#' Quick longitudinal study creation
#'
#' Creates a complete longitudinal study with sensible defaults and runs analytics
#' @param study_name Name of the study
#' @param participants Number of participants (default 1000)
#' @param sites Number of sites (default 150)
#' @param months_duration Duration in months (default 24)
#' @param study_type Type of study - "standard" or "endpoints"
#' @param include_pipeline Whether to run both the analytics and reporting pipelines (default FALSE)
#' @param outlier_intensity Global multiplier for outlier-like values in domain generators.
#' @param verbose Whether to print progress/output messages
#' @return LongitudinalStudy object with complete data and analytics
#' @export
quick_longitudinal_study <- function(study_name = "GS-US-000-0001",
                                    participants = 1000,
                                    sites = 150,
                                    months_duration = 24,
                                    study_type = "standard",
                                    include_pipeline = FALSE,
                                    outlier_intensity = 1,
                                    verbose = FALSE) {

  if (isTRUE(verbose)) {
    cat("Creating", study_type, "longitudinal study:", study_name, "\n")
    cat("Parameters:", participants, "participants,", sites, "sites,", months_duration, "months\n")
  }

  # Create study ID from name
  study_id <- gsub("[^A-Za-z0-9]", "-", toupper(study_name))

  # Determine domains based on study type

  if (study_type == "standard") {
    # Only include standard domains, no endpoints-only domains
    domains <- c("AE", "LB", "VISIT", "PD", "PK", "QUERY", "DATACHG", "DATAENT", "STUDCOMP", "SDRGCOMP", "IE", "EXCLUSION", "Death", "Randomization", "OverallResponse")
    pkg <- "gsm.kri"
    study <- create_longitudinal_study(
      study_id = study_id,
      participants = participants,
      sites = sites,
    snapshots = months_duration,
      interval = "1 month",
      domains = domains,
      run_analytics = include_pipeline,
      analytics_package = pkg,
      run_reporting = include_pipeline,
      outlier_intensity = outlier_intensity,
      verbose = verbose
    )
  } else if (study_type == "endpoints") {
    # Endpoints: use multi-package logic
    domain_pkg_df <- get_endpoints_domains()
    domains <- domain_pkg_df$domain
    # Build config
    config <- create_study_config(
      study_id = study_id,
      participant_count = participants,
      site_count = sites,
      outlier_intensity = outlier_intensity
    )
    # Add all endpoint/mapping domains
    for (d in domains) {
      config <- add_dataset_config(config, paste0("Raw_", d), enabled = TRUE)
    }
    # Set temporal config
    config <- set_temporal_config(config, snapshot_count = months_duration, snapshot_width = "months")
    # Generate raw data using new helper
    raw_data <- generate_raw_data_for_endpoints(config, domain_pkg_df)
    # Create study object
    study <- create_longitudinal_study_data(
      study_id = study_id,
      raw_data = raw_data,
      config = list(
        participants = participants,
        sites = sites,
        snapshots = months_duration,
        interval = "1 month",
        domains = domains,
        study_type = "endpoints",
        analytics_package = "gsm.endpoints",
        verbose = verbose
      )
    )
    # Run analytics and reporting if requested
    if (include_pipeline) {
      if (isTRUE(verbose)) cat("Running analytics pipeline...\n")
      analytics_config <- create_study_config(
        study_id = study_id,
        participant_count = participants,
        site_count = sites,
        analytics_package = "gsm.endpoints",
        outlier_intensity = outlier_intensity
      )
      analytics_config$domains <- domains
      analytics_config$verbose <- verbose
      study$analytics <- generate_analytics_layers(
        raw_data = raw_data,
        config = analytics_config,
        verbose = verbose
      )
      if (isTRUE(verbose)) cat("Running reporting pipeline...\n")
      study$reporting <- generate_reporting_layers(
        analytics_results = study$analytics,
        config = analytics_config,
        verbose = verbose
      )
    }
  } else {
    stop("study_type must be 'standard' or 'endpoints'")
  }

  if (isTRUE(verbose)) {
    cat("Study creation completed successfully!\n")
    cat("Generated", length(study$raw_data), "snapshots with", length(study$config$domains), "domains\n")
  }

  if (isTRUE(verbose) && !is.null(study$analytics)) {
    total_metrics <- 0
    analytics_snapshots <- study$analytics
    if (is.list(analytics_snapshots) && length(analytics_snapshots) > 0) {
      # Count metrics directly from raw analytics results
      if ("results" %in% names(analytics_snapshots)) {
        # Single snapshot
        results <- analytics_snapshots$results
        total_metrics <- length(results[grep("^(Analysis_|site|country|study)", names(results), ignore.case = TRUE)])
      } else {
        # Multiple snapshots
        for (snapshot_name in names(analytics_snapshots)) {
          snapshot_analytics <- analytics_snapshots[[snapshot_name]]
          if (!is.null(snapshot_analytics) && "results" %in% names(snapshot_analytics)) {
            results <- snapshot_analytics$results
            snapshot_metrics <- length(results[grep("^(Analysis_|site|country|study)", names(results), ignore.case = TRUE)])
            total_metrics <- total_metrics + snapshot_metrics
          }
        }
      }
    }
    cat("Analytics pipeline completed with results for",
        total_metrics, "metrics\n")
  }

  if (isTRUE(verbose) && !is.null(study$reporting)) {
    reporting_count <- sum(!vapply(study$reporting, is.null, logical(1)))
    cat("Reporting pipeline completed for", reporting_count, "of", length(study$reporting), "snapshots\n")
  }

  return(study)
}

#' Create Multiple Longitudinal Studies
#'
#' Creates multiple longitudinal studies simultaneously, allowing for efficient
#' batch generation of study data with shared or per-study configuration.
#'
#' @param study_names Character vector of study names/identifiers
#' @param participants Number of participants per study (default 100). Can be a single value
#'   applied to all studies or a vector of values per study.
#' @param sites Number of sites per study (default 10). Can be a single value 
#'   applied to all studies or a vector of values per study.
#' @param snapshots Number of snapshots per study (default 5). Can be a single value
#'   applied to all studies or a vector of values per study.
#' @param interval Time between snapshots (default "1 month"). Can be a single value
#'   applied to all studies or a vector of values per study.
#' @param domains Clinical domains to include (default c("AE", "LB", "VISIT")).
#'   Applied to all studies unless overridden in study_configs.
#' @param run_analytics Whether to run the analytics pipeline (default FALSE)
#' @param analytics_package Package containing workflows (optional)
#' @param analytics_workflows Specific workflows to run (optional)
#' @param run_reporting Whether to run the reporting pipeline after analytics (default FALSE)
#' @param outlier_intensity Global multiplier for outlier-like values (default 1).
#'   Can be a single value applied to all studies or a vector of values per study.
#' @param study_configs Optional named list of per-study configurations to override defaults.
#'   Names should match study_names. Each element can contain any of the parameters
#'   above to override the global settings for that specific study.
#' @param parallel Whether to generate studies in parallel (default FALSE).
#'   Requires parallel processing setup if TRUE.
#' @param export_studies Whether to automatically export all studies to disk (default FALSE)
#' @param export_dir Directory to export studies to if export_studies = TRUE
#' @param verbose Whether to print progress/output messages (default FALSE)
#'
#' @return Named list of LongitudinalStudy objects, with names corresponding to study_names
#' @export
#'
#' @examples
#' \dontrun{
#' # Create three studies with shared configuration
#' studies <- create_multiple_longitudinal_studies(
#'   study_names = c("STUDY-001", "STUDY-002", "STUDY-003"),
#'   participants = 150,
#'   sites = 12,
#'   snapshots = 6,
#'   domains = c("AE", "LB", "VISIT", "PD"),
#'   verbose = TRUE
#' )
#'
#' # Create studies with per-study configuration
#' studies <- create_multiple_longitudinal_studies(
#'   study_names = c("PHASE2-001", "PHASE3-001"),
#'   participants = c(100, 500),
#'   sites = c(8, 25),
#'   snapshots = c(4, 12),
#'   study_configs = list(
#'     "PHASE2-001" = list(domains = c("AE", "LB")),
#'     "PHASE3-001" = list(domains = c("AE", "LB", "VISIT", "PD", "PK"))
#'   ),
#'   verbose = TRUE
#' )
#' }
create_multiple_longitudinal_studies <- function(study_names,
                                                participants = 100,
                                                sites = 10,
                                                snapshots = 5,
                                                interval = "1 month",
                                                domains = c("AE", "LB", "VISIT"),
                                                run_analytics = FALSE,
                                                analytics_package = NULL,
                                                analytics_workflows = NULL,
                                                run_reporting = FALSE,
                                                outlier_intensity = 1,
                                                study_configs = NULL,
                                                parallel = FALSE,
                                                export_studies = FALSE,
                                                export_dir = ".",
                                                verbose = FALSE) {

  # Validate inputs
  if (length(study_names) == 0) {
    stop("study_names must contain at least one study name")
  }
  
  if (any(duplicated(study_names))) {
    stop("study_names contains duplicate values")
  }

  # Helper function to prepare parameters for each study
  prepare_study_params <- function(study_name, index) {
    # Start with global defaults
    params <- list(
      study_id = study_name,
      participants = if (length(participants) == 1) participants else participants[index],
      sites = if (length(sites) == 1) sites else sites[index],
      snapshots = if (length(snapshots) == 1) snapshots else snapshots[index],
      interval = if (length(interval) == 1) interval else interval[index],
      domains = domains,
      run_analytics = run_analytics,
      analytics_package = analytics_package,
      analytics_workflows = analytics_workflows,
      run_reporting = run_reporting,
      outlier_intensity = if (length(outlier_intensity) == 1) outlier_intensity else outlier_intensity[index],
      verbose = verbose
    )
    
    # Override with study-specific configs if provided
    if (!is.null(study_configs) && study_name %in% names(study_configs)) {
      study_config <- study_configs[[study_name]]
      params <- modifyList(params, study_config)
    }
    
    return(params)
  }

  # Create studies
  if (isTRUE(verbose)) {
    cat("Creating", length(study_names), "longitudinal studies...\n")
    cat("Study names:", paste(study_names, collapse = ", "), "\n")
  }

  if (parallel) {
    # Check if parallel backend is available
    if (!requireNamespace("parallel", quietly = TRUE)) {
      warning("parallel package not available, proceeding sequentially")
      parallel <- FALSE
    }
  }

  # Generate studies
  if (parallel && requireNamespace("parallel", quietly = TRUE)) {
    if (isTRUE(verbose)) cat("Generating studies in parallel...\n")
    
    # Set up cluster
    cl <- parallel::makeCluster(min(parallel::detectCores() - 1, length(study_names)))
    
    # Export necessary objects to cluster
    parallel::clusterEvalQ(cl, library(gsm.datasim))
    
    # Generate studies in parallel
    studies <- tryCatch({
      parallel::clusterMap(cl, function(name, idx) {
        params <- prepare_study_params(name, idx)
        do.call(create_longitudinal_study, params)
      }, study_names, seq_along(study_names), SIMPLIFY = FALSE)
    }, finally = {
      parallel::stopCluster(cl)
    })
    
    names(studies) <- study_names
    
  } else {
    # Sequential generation
    if (isTRUE(verbose)) cat("Generating studies sequentially...\n")
    
    studies <- vector("list", length(study_names))
    names(studies) <- study_names
    
    for (i in seq_along(study_names)) {
      study_name <- study_names[i]
      
      if (isTRUE(verbose)) {
        cat(sprintf("Creating study %d/%d: %s\n", i, length(study_names), study_name))
      }
      
      params <- prepare_study_params(study_name, i)
      studies[[study_name]] <- do.call(create_longitudinal_study, params)
      
      if (isTRUE(verbose)) {
        cat(sprintf("  - Study %s completed successfully\n", study_name))
      }
    }
  }

  # Export studies if requested
  if (export_studies) {
    if (isTRUE(verbose)) cat("Exporting", length(studies), "studies to disk...\n")
    
    for (study_name in names(studies)) {
      if (isTRUE(verbose)) cat("  - Exporting", study_name, "...\n")
      
      export_study_data(
        study = studies[[study_name]],
        output_dir = export_dir,
        study_folder = study_name,
        overwrite = FALSE,
        verbose = verbose
      )
    }
    
    if (isTRUE(verbose)) cat("All studies exported to:", file.path(export_dir), "\n")
  }

  # Summary
  if (isTRUE(verbose)) {
    cat("\n=== Study Generation Summary ===\n")
    cat("Total studies created:", length(studies), "\n")
    
    for (study_name in names(studies)) {
      study <- studies[[study_name]]
      cat(sprintf("  %s: %d participants, %d sites, %d snapshots, %d domains\n",
                  study_name,
                  study$config$participants %||% "unknown",
                  study$config$sites %||% "unknown", 
                  length(study$raw_data),
                  length(study$config$domains %||% c())))
    }
    
    if (run_analytics) {
      analytics_completed <- sum(sapply(studies, function(s) !is.null(s$analytics)))
      cat("Analytics completed for", analytics_completed, "studies\n")
    }
    
    if (run_reporting) {
      reporting_completed <- sum(sapply(studies, function(s) !is.null(s$reporting)))
      cat("Reporting completed for", reporting_completed, "studies\n")
    }
  }

  # Set class for the collection
  class(studies) <- c("multiple_longitudinal_studies", "list")
  
  return(studies)
}

#' Print method for multiple longitudinal studies
#' @param x A multiple_longitudinal_studies object
#' @param ... Additional arguments (unused)
#' @method print multiple_longitudinal_studies
#' @export
print.multiple_longitudinal_studies <- function(x, ...) {
  cat("Multiple Longitudinal Studies Collection\n")
  cat("=======================================\n")
  cat("Number of studies:", length(x), "\n\n")
  
  for (study_name in names(x)) {
    study <- x[[study_name]]
    cat("Study:", study_name, "\n")
    cat("  - Participants:", study$config$participants %||% "unknown", "\n")
    cat("  - Sites:", study$config$sites %||% "unknown", "\n")
    cat("  - Snapshots:", length(study$raw_data), "\n")
    cat("  - Domains:", length(study$config$domains %||% c()), 
        paste0("(", paste(study$config$domains %||% c(), collapse = ", "), ")"), "\n")
    cat("  - Analytics:", if (!is.null(study$analytics)) "Yes" else "No", "\n")
    cat("  - Reporting:", if (!is.null(study$reporting)) "Yes" else "No", "\n")
    cat("\n")
  }
}

#' Export multiple longitudinal studies to disk
#'
#' Convenience function to export all studies in a multiple_longitudinal_studies object
#'
#' @param studies A multiple_longitudinal_studies object
#' @param output_dir Root directory for export (default ".")
#' @param overwrite Whether to overwrite existing files (default FALSE)
#' @param save_rds Whether to save RDS files alongside CSVs (default FALSE)
#' @param verbose Whether to print progress messages (default FALSE)
#' @return Invisible list of study export paths
#' @export
export_multiple_studies <- function(studies,
                                   output_dir = ".",
                                   overwrite = FALSE,
                                   save_rds = FALSE,
                                   verbose = FALSE) {
  
  if (!inherits(studies, "multiple_longitudinal_studies")) {
    stop("studies must be a multiple_longitudinal_studies object")
  }
  
  if (isTRUE(verbose)) {
    cat("Exporting", length(studies), "studies to disk...\n")
  }
  
  export_paths <- vector("list", length(studies))
  names(export_paths) <- names(studies)
  
  for (study_name in names(studies)) {
    if (isTRUE(verbose)) cat("  - Exporting", study_name, "...\n")
    
    export_path <- export_study_data(
      study = studies[[study_name]],
      output_dir = output_dir,
      study_folder = study_name,
      overwrite = overwrite,
      save_rds = save_rds,
      verbose = verbose
    )
    
    export_paths[[study_name]] <- export_path
  }
  
  if (isTRUE(verbose)) {
    cat("All", length(studies), "studies exported to:", normalizePath(output_dir), "\n")
  }
  
  invisible(export_paths)
}

#' Summary method for multiple longitudinal studies
#' @param object A multiple_longitudinal_studies object
#' @param ... Additional arguments (unused)
#' @method summary multiple_longitudinal_studies
#' @export
summary.multiple_longitudinal_studies <- function(object, ...) {
  
  n_studies <- length(object)
  
  # Aggregate statistics
  total_participants <- sum(sapply(object, function(s) s$config$participants %||% 0))
  total_sites <- sum(sapply(object, function(s) s$config$sites %||% 0))
  total_snapshots <- sum(sapply(object, function(s) length(s$raw_data)))
  
  analytics_count <- sum(sapply(object, function(s) !is.null(s$analytics)))
  reporting_count <- sum(sapply(object, function(s) !is.null(s$reporting)))
  
  # Domain coverage
  all_domains <- unique(unlist(lapply(object, function(s) s$config$domains)))
  
  result <- list(
    n_studies = n_studies,
    study_names = names(object),
    total_participants = total_participants,
    total_sites = total_sites,
    total_snapshots = total_snapshots,
    analytics_completed = analytics_count,
    reporting_completed = reporting_count,
    unique_domains = all_domains,
    study_details = lapply(object, function(s) {
      list(
        participants = s$config$participants,
        sites = s$config$sites,
        snapshots = length(s$raw_data),
        domains = s$config$domains,
        has_analytics = !is.null(s$analytics),
        has_reporting = !is.null(s$reporting)
      )
    })
  )
  
  class(result) <- "summary.multiple_longitudinal_studies"
  return(result)
}

#' Print method for summary of multiple longitudinal studies
#' @param x A summary.multiple_longitudinal_studies object
#' @param ... Additional arguments (unused)
#' @method print summary.multiple_longitudinal_studies
#' @export
print.summary.multiple_longitudinal_studies <- function(x, ...) {
  cat("Summary: Multiple Longitudinal Studies\n")
  cat("=====================================\n\n")
  
  cat("Collection Overview:\n")
  cat("  - Number of studies:", x$n_studies, "\n")
  cat("  - Total participants:", x$total_participants, "\n")
  cat("  - Total sites:", x$total_sites, "\n")
  cat("  - Total snapshots:", x$total_snapshots, "\n")
  cat("  - Analytics completed:", x$analytics_completed, "of", x$n_studies, "studies\n")
  cat("  - Reporting completed:", x$reporting_completed, "of", x$n_studies, "studies\n")
  cat("  - Unique domains:", length(x$unique_domains), 
      paste0("(", paste(x$unique_domains, collapse = ", "), ")"), "\n\n")
  
  cat("Individual Study Details:\n")
  for (study_name in x$study_names) {
    details <- x$study_details[[study_name]]
    cat("  ", study_name, ":\n")
    cat("    - Participants:", details$participants, "| Sites:", details$sites, "| Snapshots:", details$snapshots, "\n")
    cat("    - Domains:", paste(details$domains, collapse = ", "), "\n")
    cat("    - Analytics:", if (details$has_analytics) "Yes" else "No", 
        "| Reporting:", if (details$has_reporting) "Yes" else "No", "\n")
  }
}

#' Create a portfolio of studies from a base config and per-study variants
#'
#' A convenience wrapper around [create_multiple_longitudinal_studies()] that lets
#' you define a shared base configuration and a single named list of per-study
#' overrides (variants).  Only the fields that differ between studies need to
#' appear in each variant entry -- everything else falls back to the base.
#'
#' Vectorised parameters (`participants`, `sites`, `snapshots`,
#' `outlier_intensity`, `interval`) are automatically extracted from the variant
#' list so you never have to maintain a parallel vector alongside `study_configs`.
#'
#' @param variants Named list of per-study override lists.  Names become the
#'   study identifiers.  Each element may contain any combination of:
#'   `participants`, `sites`, `snapshots`, `interval`, `domains`,
#'   `outlier_intensity`, `run_analytics`, `run_reporting`, or any other
#'   argument accepted by [create_longitudinal_study()].
#' @param participants Default participant count for studies that do not specify
#'   their own (default 100).
#' @param sites Default site count (default 10).
#' @param snapshots Default number of snapshots (default 6).
#' @param interval Default snapshot interval (default `"1 month"`).
#' @param domains Default domain vector (default `c("AE", "LB", "VISIT")`).
#' @param ... Additional arguments forwarded verbatim to
#'   [create_multiple_longitudinal_studies()] (e.g. `run_analytics`,
#'   `run_reporting`, `parallel`, `verbose`).
#'
#' @return A `multiple_longitudinal_studies` object (named list of
#'   `longitudinal_study` objects).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Define shared defaults; each variant only specifies what changes
#' studies <- study_portfolio(
#'   variants = list(
#'     "PHASE2-SMALL" = list(participants = 80,  sites = 8,  snapshots = 4,
#'                           domains = c("AE", "LB")),
#'     "PHASE3-LARGE" = list(participants = 400, sites = 25, snapshots = 12,
#'                           domains = c("AE", "LB", "VISIT", "PD")),
#'     "SAFETY-RUN"   = list(participants = 50,  sites = 3,  snapshots = 8,
#'                           outlier_intensity = 2)
#'   ),
#'   # Shared defaults (used when a variant does not override)
#'   participants = 100,
#'   sites        = 10,
#'   snapshots    = 6,
#'   interval     = "1 month",
#'   domains      = c("AE", "LB", "VISIT"),
#'   run_analytics = FALSE,
#'   verbose       = TRUE
#' )
#'
#' names(studies)                         # "PHASE2-SMALL" "PHASE3-LARGE" "SAFETY-RUN"
#' studies[["PHASE3-LARGE"]]$config       # inspect per-study config
#' }
study_portfolio <- function(variants,
                            participants = 100,
                            sites = 10,
                            snapshots = 6,
                            interval = "1 month",
                            domains = c("AE", "LB", "VISIT"),
                            ...) {
  if (!is.list(variants) || is.null(names(variants)) || any(names(variants) == "")) {
    stop("`variants` must be a fully named list (each element name becomes a study ID).")
  }

  study_names <- names(variants)

  # Scalar params that create_multiple_longitudinal_studies can accept as vectors
  scalar_params <- c("participants", "sites", "snapshots", "interval", "outlier_intensity")

  extract_vec <- function(key, default) {
    vapply(variants, function(v) {
      val <- v[[key]]
      if (!is.null(val)) val else default
    }, FUN.VALUE = vector(typeof(default), 1L))
  }

  participants_vec      <- extract_vec("participants",      participants)
  sites_vec             <- extract_vec("sites",             sites)
  snapshots_vec         <- extract_vec("snapshots",         snapshots)
  interval_vec          <- extract_vec("interval",          interval)
  outlier_intensity_vec <- extract_vec("outlier_intensity", 1)

  # Remaining per-study overrides (non-scalar fields like domains, run_analytics, etc.)
  study_configs <- lapply(variants, function(v) {
    extra <- v[setdiff(names(v), scalar_params)]
    if (length(extra) == 0) NULL else extra
  })
  study_configs <- Filter(Negate(is.null), study_configs)

  create_multiple_longitudinal_studies(
    study_names       = study_names,
    participants      = participants_vec,
    sites             = sites_vec,
    snapshots         = snapshots_vec,
    interval          = interval_vec,
    domains           = domains,
    outlier_intensity = outlier_intensity_vec,
    study_configs     = if (length(study_configs) > 0) study_configs else NULL,
    ...
  )
}
