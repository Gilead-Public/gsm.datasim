#' Generate Raw Gilda Study Data
#'
#' Generate Raw Gilda Study Data based on `gilda_STUDY.yaml` from `monitoring.visit.app`.
#' Includes information about a clinical study, target enrollment/recruitment numbers and
#' actual enrollment.
#'
#' @param data List of data frames pertaining to a simulated study which is passed through in a list of snapshots
#' @param previous_data List of data frames pertaining to prior snpashot with relation to `data`
#' @param spec  A list representing the combined specifications from `CombineSpecs` across the desired domains for a simulation.
#' @param ... Additional arguments to be passed.
#'
#' @returns a data.frame pertaining to the raw dataset plugged into `gilda_STUDY.yaml`
#' @family internal
#' @keywords internal
#' @noRd

raw_gilda_study_data <- function(data, previous_data, spec, ...) {
  inps <- list(...)

  curr_spec <- spec$raw_gilda_study_data

  if ("raw_gilda_study_data" %in% names(previous_data)) {
    dataset <- previous_data$raw_gilda_study_data
  } else {
    dataset <- NULL
  }

  args <- list(
    protocol = list(inps$StudyID),
    num_plan_subj = list(inps$ParticipantCount),
    num_plan_site = list(inps$SiteCount),
    act_lplv = list(inps$MinDate, inps$MaxDate, dataset$act_lplv),
    act_fpfv = list(inps$MinDate, inps$MaxDate, dataset$act_fpfv),
    est_fpfv = list(inps$GlobalMaxDate - 30, inps$GlobalMaxDate, dataset$est_fpfv),
    est_lplv = list(inps$GlobalMaxDate + 30, inps$GlobalMaxDate + 120, dataset$est_lplv),
    phase = list(1, external_phase = c("P1", "P2", "P3", "P4")),
    default = list(1)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$raw_gilda_study_data, ...)
  return(res)
}

protocol <- function(studyid) {
  return(studyid)
}

act_lplv <- function(date_min, date_lim, prev_data, ...) {
  generate_random_fpfv(date_min, date_lim, FALSE, prev_data)
}

