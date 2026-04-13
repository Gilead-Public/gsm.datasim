.gsm_datasim_runtime_state <- new.env(parent = emptyenv())
.gsm_datasim_runtime_state$legacy_mode_warned <- FALSE

#' Generate Raw Data for Study Snapshots
#'
#' This function generates raw data for study snapshots based on provided participant count,
#' site count, study ID, and snapshot count. If any of these parameters are `NULL`, it loads
#' a template CSV file to iterate over multiple study configurations. The generated raw data
#' is saved as an RDS file.
#'
#' @param ParticipantCount An integer specifying the number of participants for the study.
#' If `NULL`, the function will use values from the template file.
#' @param SiteCount An integer specifying the number of sites for the study. If `NULL`, the function
#' will use values from the template file.
#' @param StudyID A string specifying the study identifier. If `NULL`, the function will use values
#' from the template file.
#' @param SnapshotCount An integer specifying the number of snapshots for the study. If `NULL`,
#' the function will use values from the template file.
#' @param SnapshotWidth A character specifying the frequency of snapshots, defaults to "months".
#' Accepts "days", "weeks", "months" and "years". User can also place a number and unit such as "3 months".
#' @param template_path A string specifying the path to the template CSV file. Default is
#' `"~/gsm.datasim/inst/template.csv"`.
#' @param workflow_path A string specifying the path to the workflow mappings. Default is
#' `"workflow/1_mappings"`.
#' @param generate_reports A boolean, specifying whether or not to produce reports upon execution. Default is FALSE.
#' @param mappings A string specifying the names of the workflows to run.
#' @param package A string specifying the package in which the workflows used in `MakeWorkflowList()` are located. Default is "gsm".
#' @param strStartDate A string to denote when the first snapshot of simulated data occurs
#' @param save A boolean, specifying whether or not this should be saved out as an RDS
#' @param generation_mode Generation backend to use: "core" (default) or "legacy".
#'
#' @return A list of raw data generated for each study snapshot, saved as an RDS file in `"data-raw/raw_data.RDS"`.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item If `ParticipantCount`, `SiteCount`, `StudyID`, or `SnapshotCount` is `NULL`, the function reads
#'   the `template.csv` file to get the necessary parameters for multiple studies.
#'   \item It generates raw data for study snapshots based on either provided parameters or the template file.
#'   \item The generated data is saved as an RDS file and returned as a list.
#' }
#'
#' @examples
#' \dontrun{
#' # Generate raw data using specified parameters
#' data <- raw_data_generator(
#'   ParticipantCount = 100,
#'   SiteCount = 10,
#'   StudyID = "Study01",
#'   SnapshotCount = 5,
#'   SnapshotWidth = "months"
#' )
#'
#' # Generate raw data using a template file
#' data <- raw_data_generator()
#' }
#'
#' @export
raw_data_generator <- function(
  ParticipantCount = NULL,
  SiteCount = NULL,
  StudyID = NULL,
  SnapshotCount = NULL,
  SnapshotWidth = NULL,
  template_path = system.file("template.csv", package = "gsm.datasim"),
  workflow_path = "workflow/1_mappings",
  generate_reports = FALSE,
  mappings = NULL,
  package = "gsm.mapping",
  strStartDate = "2012-01-01",
  save = FALSE,
  generation_mode = c("core", "legacy")
) {
  generation_mode <- match.arg(generation_mode)

  if (identical(generation_mode, "legacy") && !isTRUE(.gsm_datasim_runtime_state$legacy_mode_warned)) {
    warning(
      "generation_mode = 'legacy' is maintained for compatibility. ",
      "Prefer generation_mode = 'core' for the refactored study-builder path.",
      call. = FALSE
    )
    .gsm_datasim_runtime_state$legacy_mode_warned <- TRUE
  }

  build_config_and_generate <- function(participant_count, site_count, study_id, snapshot_count, snapshot_width) {
    config <- create_study_config(
      study_id = study_id,
      participant_count = participant_count,
      site_count = site_count
    )

    config <- set_temporal_config(
      config,
      start_date = strStartDate,
      snapshot_count = snapshot_count,
      snapshot_width = snapshot_width
    )

    mapping_names <- mappings
    if (is.null(mapping_names) || length(mapping_names) == 0) {
      wf_all <- gsm.core::MakeWorkflowList(
        strPath = workflow_path,
        strPackage = package
      )
      mapping_names <- names(wf_all)
    }

    if (!is.null(mapping_names) && length(mapping_names) > 0) {
      for (mapping_name in mapping_names) {
        config <- add_dataset_config(config, paste0("Raw_", mapping_name), enabled = TRUE)
      }
    }

    generate_snapshots_from_config(
      config = config,
      domain_package_df = NULL,
      default_package = package,
      workflow_path = workflow_path,
      verbose = FALSE
    )
  }

  build_legacy_and_generate <- function(participant_count, site_count, study_id, snapshot_count, snapshot_width) {
    generate_rawdata_for_single_study(
      SnapshotCount = snapshot_count,
      SnapshotWidth = snapshot_width,
      ParticipantCount = participant_count,
      SiteCount = site_count,
      StudyID = study_id,
      workflow_path = workflow_path,
      mappings = mappings,
      package = package,
      strStartDate = strStartDate
    )
  }

  generate_for_study <- if (identical(generation_mode, "legacy")) {
    build_legacy_and_generate
  } else {
    build_config_and_generate
  }

  # Initialize the list to store raw data
  raw_data_list <- list()

  # Check if any of the key parameters are NULL
  if (any(is.null(c(ParticipantCount, SiteCount, StudyID, SnapshotCount, SnapshotWidth)))) {
    # Read the template CSV file
    template <- read.csv(template_path, stringsAsFactors = FALSE)

    # Generate raw data for each study configuration in the template
    raw_data_list <- lapply(seq_len(nrow(template)), function(i) {
      curr_vars <- template[i, ]
      logger::log_info(glue::glue("Adding {curr_vars$StudyID}..."))
      tictoc::tic()
      res <- generate_for_study(
        participant_count = curr_vars$ParticipantCount,
        site_count = curr_vars$SiteCount,
        study_id = curr_vars$StudyID,
        snapshot_count = curr_vars$SnapshotCount,
        snapshot_width = curr_vars$SnapshotWidth
      )

      logger::log_info(glue::glue("Added {curr_vars$StudyID} successfully"))
      tictoc::toc()

      res
    })

    # Assign study IDs as names to the list elements
    names(raw_data_list) <- template$StudyID
  } else {
    # Generate raw data for the single study configuration provided
    raw_data_list[[StudyID]] <- generate_for_study(
      participant_count = ParticipantCount,
      site_count = SiteCount,
      study_id = StudyID,
      snapshot_count = SnapshotCount,
      snapshot_width = SnapshotWidth
    )
  }

  if (generate_reports) {
    raw_data_list <- lapply(raw_data_list, function(study_data) {
      create_snapshot_reports(study_data)
    })
  }

  # Save the raw data list to an RDS file
  if (save) {
    save_data_on_disk(raw_data_list)

    logger::log_info(glue::glue("Dataset saved successfully!"))
  }

  return(raw_data_list)
}
