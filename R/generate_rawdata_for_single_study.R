#' Filter visit rows based on subject status across domains
#'
#' Applies conditional visit rules:
#' - Screening: all subjects
#' - VISIT 1-5: only enrolled subjects (enrollyn == "Y")
#' - End of Treatment: only subjects who discontinued drug (sdrgyn == "N")
#' - Follow-up: only subjects who completed the study (compyn == "Y")
#'
#' @param data Named list of data frames (must contain Raw_VISIT and Raw_SUBJ;
#'   optionally Raw_SDRGCOMP and Raw_STUDCOMP).
#' @return Filtered Raw_VISIT data frame.
#' @keywords internal
filter_visits_by_status <- function(data) {
  treatment_visits <- paste0("VISIT ", 1:5)

  # Screening: all subjects (no filter needed)

  # VISIT 1-5: only enrolled subjects
  enrolled_subjs <- data$Raw_SUBJ %>%
    dplyr::filter(enrollyn == "Y") %>%
    dplyr::pull(subjid)

  # End of Treatment: only subjects who discontinued drug (sdrgyn == "N")
  eot_subjs <- character(0)
  if ("Raw_SDRGCOMP" %in% names(data)) {
    eot_subjs <- data$Raw_SDRGCOMP %>%
      dplyr::distinct(subjid, .keep_all = TRUE) %>%
      dplyr::filter(sdrgyn == "N") %>%
      dplyr::pull(subjid)
  }

  # Follow-up: only subjects who completed the study (compyn == "Y")
  followup_subjs <- character(0)
  if ("Raw_STUDCOMP" %in% names(data)) {
    followup_subjs <- data$Raw_STUDCOMP %>%
      dplyr::distinct(subjid, .keep_all = TRUE) %>%
      dplyr::filter(compyn == "Y") %>%
      dplyr::pull(subjid)
  }

  data$Raw_VISIT %>%
    dplyr::filter(
      (foldername == "Screening") |
        (foldername %in% treatment_visits & subjid %in% enrolled_subjs) |
        (foldername == "End of Treatment" & subjid %in% eot_subjs) |
        (foldername == "Follow-up" & subjid %in% followup_subjs)
    )
}

#' Prepare combined specs for generation
#'
#' Removes mapped outputs from `combined_specs`, ensures required core raw
#' datasets are ordered first, and optionally filters to `desired_specs`.
#'
#' @param combined_specs A named list of dataset specifications.
#' @param desired_specs Optional character vector of dataset names to keep.
#'
#' @return A reordered (and optionally filtered) named list of dataset specs.
#' @export
#'
#' @examples
#' specs <- list(
#'   Raw_AE = list(aest_dt = list(required = TRUE)),
#'   Mapped_AE = list(),
#'   Raw_SUBJ = list(subjid = list(required = TRUE))
#' )
#'
#' prepared <- prepare_combined_specs_for_generation(specs)
#' names(prepared)
#'
prepare_combined_specs_for_generation <- function(combined_specs, desired_specs = NULL) {
  combined_specs <- purrr::list_modify(
    combined_specs,
    !!!rlang::set_names(
      rep(list(rlang::zap()), sum(startsWith(names(combined_specs), "Mapped_"))),
      names(combined_specs)[startsWith(names(combined_specs), "Mapped_")]
    )
  )

  # Specify the desired first few elements in order
  desired_order <- c("Raw_STUDY", "Raw_SITE", "Raw_SUBJ", "Raw_ENROLL", "Raw_VISIT", "Raw_STUDCOMP", "Raw_SDRGCOMP")
  if (!("Raw_VISIT" %in% names(combined_specs))) {
    combined_specs$Raw_VISIT <- list(
      subjid = list(required = TRUE),
      foldername = list(required = TRUE),
      instancename = list(required = TRUE),
      visit_dt = list(required = TRUE),
      studyid = list(required = TRUE)
    )
  }
  desired_order <- desired_order[desired_order %in% names(combined_specs)]

  # Rearrange the elements
  combined_specs <- combined_specs[c(desired_order, setdiff(names(combined_specs), desired_order))]

  if (!is.null(desired_specs)) {
    combined_specs <- combined_specs[desired_specs]
  }

  combined_specs
}

generate_snapshots_from_combined_specs <- function(SnapshotCount,
                                                   SnapshotWidth,
                                                   ParticipantCount,
                                                   SiteCount,
                                                   StudyID,
                                                   combined_specs,
                                                   mappings,
                                                   strStartDate = "2012-01-01") {
  # Generate start and end dates for snapshots
  start_dates <- seq(as.Date(strStartDate), length.out = SnapshotCount, by = SnapshotWidth)
  end_dates <- start_dates + 28

  subject_count <- count_gen(ParticipantCount, SnapshotCount)
  site_count <- count_gen(SiteCount, SnapshotCount)
  if (SnapshotCount > 1) {
    enrollment_count <- enrollment_count_gen(subject_count)
  }
  enrollment_count <- subject_count

  ae_count <- subject_count * 3
  pd_count <- subject_count * 3
  sdrgcomp_count <- ceiling(subject_count / 10)
  studcomp_count <- ceiling(subject_count / 10)
  consents_count <- ceiling(subject_count / 75)
  death_count <- ceiling(subject_count / 85)
  anticancer_count <- ceiling(subject_count / 75)

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

      registry_context <- list(
        data = data,
        previous_data = previous_data,
        combined_specs = combined_specs,
        n = n,
        start_date = start_dates[snapshot_idx],
        end_date = end_dates[snapshot_idx],
        snapshot_idx = snapshot_idx,
        snapshot_count = SnapshotCount,
        snapshot_width = SnapshotWidth,
        study_id = StudyID
      )

      migrated_data <- generate_domain_from_registry(
        data_type = data_type,
        context = registry_context
      )

      if (!is.null(migrated_data)) {
        data[[data_type]] <- migrated_data
        logger::log_info(glue::glue(" ---- Dataset {data_type} added successfully"))
        next
      }

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
        Raw_STUDCOMP = list(data, previous_data, combined_specs, n = n, startDate = start_dates[snapshot_idx], split_vars = list("subjid_invid_unique")),
        Raw_LB = list(data, previous_data, combined_specs, n = n, startDate = start_dates[snapshot_idx], split_vars = list("subj_visit_repeated")),
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
          SnapshotWidth = SnapshotWidth,
          split_vars = list("subjid_repeated")
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
    if (!("gilda_STUDY" %in% mappings)) {
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
    if ("Raw_VISIT" %in% names(data)) {
      data$Raw_VISIT <- filter_visits_by_status(data)
    }
    snapshots[[snapshot_idx]] <- data
    logger::log_info(glue::glue(" -- Snapshot {snapshot_idx} added successfully"))
  }

  # Assign snapshot end dates as names
  names(snapshots) <- as.character(end_dates)
  snapshots
}

generate_rawdata_for_single_study <- function(SnapshotCount,
  SnapshotWidth,
  ParticipantCount,
  SiteCount,
  StudyID,
  workflow_path,
  mappings,
  package,
  strStartDate = "2012-01-01",
  desired_specs = NULL) {
  combined_specs <- load_specs(workflow_path, mappings, package)
  prepared_specs <- prepare_combined_specs_for_generation(
    combined_specs = combined_specs,
    desired_specs = desired_specs
  )

  generate_snapshots_from_combined_specs(
    SnapshotCount = SnapshotCount,
    SnapshotWidth = SnapshotWidth,
    ParticipantCount = ParticipantCount,
    SiteCount = SiteCount,
    StudyID = StudyID,
    combined_specs = prepared_specs,
    mappings = mappings,
    strStartDate = strStartDate
  )
}
