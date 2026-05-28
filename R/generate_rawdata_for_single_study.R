#' Generate Snapshots for Single Study Data
#'
#' This function generates a list of study data snapshots based on the provided specifications,
#' including participant count, site count, study ID, and the number of snapshots to be generated.
#' The data is simulated over a series of months, with key variables such as subject enrollment,
#' adverse events, and time on study being populated according to the study's specifications.
#'
#' @param SnapshotCount An integer specifying the number of snapshots to generate.
#' @param SnapshotWidth A character specifying the frequency of snapshots, defaults to "months".
#' Accepts "days", "weeks", "months" and "years". User can also place a number and unit such as "3 months".
#' @param ParticipantCount An integer specifying the number of participants in the study.
#' @param SiteCount An integer specifying the number of sites for the study.
#' @param StudyID A string specifying the study identifier.
#' @param workflow_path A string specifying the path to the workflow mappings.
#' @param mappings A string specifying the names of the workflows to run.
#' @param package A string specifying the package in which the workflows used in `MakeWorkflowList()` are located.
#' @param desired_specs A list of specifications of the data types that should be included.
#'
#' @return A list of data snapshots, where each element contains simulated data for a particular snapshot
#' period (typically a month), with variables populated according to the provided specifications.
#'
#' @export
#' @details
#' The function generates snapshots over a sequence of months, starting from `"2012-01-01"`. For each snapshot:
#' \enumerate{
#'   \item The number of adverse events (`ae_num`) is simulated.
#'   \item The number of participants screened is determined.
#'   \item Key variables (such as subject ID, enrollment date, time on study, etc.) are populated for each data type
#'   specified in `combined_specs`.
#' }
#'
#' @examples
#' snapshots <- generate_rawdata_for_single_study(
#'   SnapshotCount = 3,
#'   SnapshotWidth = "months",
#'   ParticipantCount = 50,
#'   SiteCount = 5,
#'   StudyID = "ABC",
#'   workflow_path = "workflow/1_mappings",
#'   mappings = "AE",
#'   package = "gsm.mapping",
#'   desired_specs = NULL
#' )
#'
generate_rawdata_for_single_study <- function(SnapshotCount,
  SnapshotWidth,
  ParticipantCount,
  SiteCount,
  StudyID,
  workflow_path,
  mappings,
  package,
  desired_specs = NULL) {
  # Generate start and end dates for snapshots
  start_dates <- seq(as.Date("2012-01-01"), length.out = SnapshotCount, by = SnapshotWidth)
  end_dates <- seq(as.Date("2012-02-01"), length.out = SnapshotCount, by = SnapshotWidth) - 1

  # Load workflow mappings and combine specifications
  combined_specs <- load_specs(workflow_path, mappings, package)
  combined_specs <- purrr::list_modify(
    combined_specs,
    !!!rlang::set_names(
      rep(list(rlang::zap()), sum(startsWith(names(combined_specs), "Mapped_"))),
      names(combined_specs)[startsWith(names(combined_specs), "Mapped_")]
    )
  )

  # Specify the desired first few elements in order
  desired_order <- c("Raw_STUDY", "Raw_SITE", "Raw_SUBJ", "Raw_ENROLL", "Raw_SV", "Raw_VISIT", "Raw_STUDCOMP")
  if (!("Raw_SV" %in% names(combined_specs))) {
    combined_specs$Raw_SV <- list(
      subjid = list(required = TRUE),
      foldername = list(required = TRUE),
      instancename = list(required = TRUE),
      visit_dt = list(required = TRUE)
    )
  }
  if (!("Raw_VS" %in% names(combined_specs))) {
    combined_specs$Raw_VS <- list(
      subjid = list(required = TRUE),
      studyid = list(required = TRUE),
      visnam = list(required = TRUE),
      vs_dt = list(required = TRUE),
      vsperf_std = list(required = TRUE),
      weight = list(required = TRUE),
      sysbp = list(required = TRUE),
      diabp = list(required = TRUE)
    )
  }
  if (!("Visit" %in% names(combined_specs))) {
    combined_specs$Raw_VISIT <- list(
      subjid = list(required = TRUE),
      visit = list(
        required = TRUE,
        source_col = "foldername"
      ),
      visit_date = list(
        required = TRUE,
        source_col = "visit_dt"
      ),
      studyid = list(required = TRUE),
      invid = list(required = TRUE)
    )
  }
  desired_order <- desired_order[desired_order %in% names(combined_specs)]

  # Rearrange the elements
  combined_specs <- combined_specs[c(desired_order, setdiff(names(combined_specs), desired_order))]

  if (!is.null(desired_specs)) {
    combined_specs <- combined_specs[desired_specs]
  }

  subject_count <- count_gen(ParticipantCount, SnapshotCount)
  site_count <- count_gen(SiteCount, SnapshotCount)
  if (SnapshotCount > 1) {
    enrollment_count <- enrollment_count_gen(subject_count)
  }
  enrollment_count <- subject_count

  ae_count <- subject_count * 3
  pd_count <- subject_count * 3
  sdrgcomp_count <- ceiling(subject_count / 2)
  studcomp_count <- ceiling(subject_count / 10)
  consents_count <- ceiling(subject_count / 75)
  death_count <- ceiling(subject_count / 85)
  anticancer_count <- ceiling(subject_count / 10)


  # print(subject_count)
  # print(site_count)
  # print(enrollment_count)
  # print("--------------")

  snapshots <- list()

  # Generate snapshots using lapply
  for (snapshot_idx in seq_len(SnapshotCount)) {
    # Initialize list to store data types
    logger::log_info(glue::glue(" -- Adding snapshot {snapshot_idx}..."))
    data <- list()

    if (snapshot_idx == 1) {
      previous_data <- list()
      data$Raw_STUDY <- as.data.frame(Raw_STUDY(data, previous_data, combined_specs,
        StudyID = StudyID,
        SiteCount = SiteCount,
        ParticipantCount = ParticipantCount,
        MinDate = start_dates[snapshot_idx],
        MaxDate = end_dates[snapshot_idx],
        GlobalMaxDate = max(end_dates)
      ))
      data$raw_gilda_study_data <- as.data.frame(raw_gilda_study_data(data, previous_data, combined_specs,
        StudyID = StudyID,
        SiteCount = SiteCount,
        ParticipantCount = ParticipantCount,
        MinDate = start_dates[snapshot_idx],
        MaxDate = end_dates[snapshot_idx],
        GlobalMaxDate = max(end_dates)
      ))
    } else {
      data$Raw_STUDY <- snapshots[[1]]$Raw_STUDY
      data$Raw_STUDY$act_fpfv <- act_fpfv(
        start_dates[snapshot_idx],
        end_dates[snapshot_idx],
        data$Raw_STUDY$act_fpfv
      )
      data$raw_gilda_study_data <- snapshots[[1]]$raw_gilda_study_data
      previous_data <- snapshots[[snapshot_idx - 1]]
    }

    # Loop over each raw data type specified in combined_specs
    for (data_type in names(combined_specs)) {
      if (data_type %in% c("Raw_STUDY", "raw_gilda_study_data")) next

      logger::log_info(glue::glue(" ---- Adding dataset {data_type}..."))

      # Determine the number of records 'n' based on data_type
      n <- dplyr::case_when(
        data_type == "Raw_AE" ~ ae_count[snapshot_idx],
        data_type == "Raw_ENROLL" ~ unlist(enrollment_count[snapshot_idx]),
        data_type == "Raw_SITE" ~ site_count[snapshot_idx],
        data_type == "Raw_PD" ~ pd_count[snapshot_idx],
        data_type == "Raw_SUBJ" ~ subject_count[snapshot_idx],
        data_type == "Raw_SDRGCOMP" ~ sdrgcomp_count[snapshot_idx],
        data_type == "Raw_STUDCOMP" ~ studcomp_count[snapshot_idx],
        data_type == "Raw_Consents" ~ consents_count[snapshot_idx],
        data_type == "Raw_Death" ~ death_count[snapshot_idx],
        data_type == "Raw_AntiCancer" ~ anticancer_count[snapshot_idx],
        data_type == "Raw_IE" ~ unlist(enrollment_count[snapshot_idx]),
        TRUE ~ subject_count[snapshot_idx]
      )
      generator_func <- data_type
      # Determine arguments based on variable name
      args <- switch(data_type,
        Raw_SITE = list(data, previous_data, combined_specs, n_sites = n, startDate = start_dates[snapshot_idx], split_vars = list("Country_State_City")),
        Raw_SUBJ = list(data, previous_data, combined_specs,
          n_subj = n, startDate = start_dates[snapshot_idx],
          endDate = end_dates[snapshot_idx], split_vars = list(
            "subject_site_synq",
            "subjid_subject_nsv",
            "enrollyn_enrolldt_timeonstudy_firstparticipantdate_firstdosedate_timeontreatment"
          )
        ),
        Raw_ENROLL = list(data, previous_data, combined_specs, n_enroll = n, startDate = start_dates[snapshot_idx], split_vars = list("subject_to_enrollment")),
        Raw_IE = list(data, previous_data, combined_specs, n_IE = n, split_vars = list("subject_to_ie", "tiver_ietestcd_ietest_ieorres_iecat")),
        Raw_SV = list(data, previous_data, combined_specs,
          n = n, startDate = start_dates[snapshot_idx], split_vars = list("subjid_repeated"),
          SnapshotWidth = SnapshotWidth
        ),
        Raw_STUDCOMP = list(data, previous_data, combined_specs, n = n, startDate = start_dates[snapshot_idx], split_vars = list("subjid_invid_unique")),
        Raw_LB = list(data, previous_data, combined_specs, n = n, startDate = start_dates[snapshot_idx], split_vars = list("subj_visit_repeated")),
        Raw_VS = list(data, previous_data, combined_specs, n = n, startDate = start_dates[snapshot_idx], split_vars = list("subj_visit_repeated")),
        Raw_DATACHG = list(data, previous_data, combined_specs, n = n, startDate = start_dates[snapshot_idx], split_vars = list("subject_nsv_visit_repeated")),
        Raw_DATAENT = list(data, previous_data, combined_specs, n = n, startDate = start_dates[snapshot_idx], split_vars = list("subject_nsv_visit_repeated")),
        Raw_QUERY = list(data, previous_data, combined_specs, n = n, startDate = start_dates[snapshot_idx], split_vars = list("subject_nsv_visit_repeated")),
        Raw_AE = list(data, previous_data, combined_specs,
          n = n, startDate = start_dates[snapshot_idx],
          endDate = end_dates[snapshot_idx], split_vars = list("aest_dt_aeen_dt")
        ),
        Raw_AntiCancer = list(data, previous_data, combined_specs, n = n, startDate = start_dates[snapshot_idx]),
        Raw_Baseline = list(data, previous_data, combined_specs, n = n, startDate = start_dates[snapshot_idx]),
        Raw_Consents = list(data, previous_data, combined_specs, n = n, startDate = start_dates[snapshot_idx]),
        Raw_Death = list(data, previous_data, combined_specs, n = n, startDate = start_dates[snapshot_idx]),
        Raw_VISIT = list(data, previous_data, combined_specs,
          n = n,
          startDate = start_dates[snapshot_idx],
          SnapshotCount = SnapshotCount,
          SnapshotWidth = SnapshotWidth,
          split_vars = list("subjid_invid")
        ),
        Raw_Randomization = list(data, previous_data, combined_specs,
          n = n,
          startDate = start_dates[snapshot_idx],
          split_vars = list("subjid_invid_country")
        ),
        Raw_OverallResponse = list(data, previous_data, combined_specs,
          n = n,
          split_vars = list("subjid_rs_dt")
        ),
        Raw_PK = list(data, previous_data, combined_specs, n = n, startDate = start_dates[snapshot_idx], split_vars = list("subjid_visit_pkdat")),
        Raw_IE = list(data, previous_data, combined_specs, n = n, split_vars = list("tiver_ietestcd_ietest_ieorres_iecat")),
        list(data, previous_data, combined_specs, n = n, startDate = start_dates[snapshot_idx]) # Default case
      )


      variable_data <- do.call(generator_func, args)
      # Combine variables into a data frame
      data[[data_type]] <- as.data.frame(variable_data)
      logger::log_info(glue::glue(" ---- Dataset {data_type} added successfully"))
    }

    if (nrow(data$Raw_ENROLL) > 0) {
      to_subj <- data$Raw_ENROLL %>%
        dplyr::select(subjid, enrollyn)

      data$Raw_SUBJ <- data$Raw_SUBJ %>%
        dplyr::rows_upsert(to_subj, by = "subjid") %>%
        dplyr::mutate(
          enrolldt = dplyr::if_else(enrollyn == "N", as.Date(NA), enrolldt),
          timeonstudy = dplyr::if_else(enrollyn == "N", NA, timeonstudy)
        )
    }
    if (!"gilda_STUDY" %in% mappings) {
      data$raw_gilda_study_data <- NULL
    }
    if ("Raw_IE" %in% names(data)) {
      unenrolled <- data$Raw_SUBJ %>%
        filter(enrollyn == "N") %>%
        pull(subjid)
      data$Raw_IE <- data$Raw_IE %>%
        slice_sample(n = round(ParticipantCount / 3)) %>%
        filter(!(subjid %in% unenrolled))
    }
    if ("Raw_Randomization" %in% names(data)) {
      data$Raw_Randomization <- data$Raw_Randomization %>%
        group_by(subjid) %>%
        filter(rgmn_dt == min(rgmn_dt, na.rm = TRUE)) %>%
        ungroup()
    }
    snapshots[[snapshot_idx]] <- data
    logger::log_info(glue::glue(" -- Snapshot {snapshot_idx} added successfully"))
  }

  # Assign snapshot end dates as names
  names(snapshots) <- as.character(end_dates)
  return(snapshots)
}
