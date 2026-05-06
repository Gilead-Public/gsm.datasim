#' Generate Raw Visit Data
#'
#' Generate Raw Visit Data based on `Visit.yaml` from `gsm.mapping`.
#'
#' @inheritParams Raw_STUDY
#' @param startDate The beginning of dates to which the subjects' Visits are
#' @returns a data.frame pertaining to the raw dataset plugged into `Visit.yaml`
#' @family internal
#' @keywords internal
#' @noRd
Raw_VISIT <- function(data, previous_data, spec, startDate, SnapshotCount, SnapshotWidth, ...) {
  inps <- list(...)
  if ("Raw_VISIT" %in% names(previous_data)) {
    dataset <- previous_data$Raw_VISIT
    previous_row_num <- length(unique(dataset$subjid))
  } else {
    dataset <- NULL
    previous_row_num <- 0
  }

  n <- inps$n - previous_row_num
  if (n == 0) {
    return(dataset)
  }

  possible_Visits <- data.frame(
    visit = c("Screening", paste0("VISIT ", 1:SnapshotCount), "Follow-up")
  )

  curr_spec <- spec$Raw_VISIT


  if (!("subjid" %in% names(curr_spec))) {
    curr_spec$subjid <- list(required = TRUE)
  }

  if (!("visit_date" %in% names(curr_spec))) {
    curr_spec$visit_date <- list(required = TRUE)
  }

  if (!("visit" %in% names(curr_spec))) {
    curr_spec$visit <- list(required = TRUE)
  }

  # if (!("invid" %in% names(curr_spec))) {
  #   curr_spec$invid <- list(required = TRUE)
  # }

  if (all(c("subjid", "invid") %in% names(curr_spec))) {
    curr_spec$subjid_invid <- list(required = TRUE)
    curr_spec$subjid <- NULL
    curr_spec$invid <- NULL
  }


  # subjs <- subjid(n, external_subjid = data$Raw_SUBJ$subjid, replace = TRUE)
  args <- list(
    subjid_invid = list(nrow(possible_Visits), data$Raw_SUBJ),
    visit_date = list(n, startDate, possible_Visits, SnapshotWidth = SnapshotWidth),
    studyid = list(n, data$Raw_STUDY$protocol_number[[1]]),
    # invid = list(n = length(subjs)),
    default = list(n, possible_Visits)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_VISIT, ...)
  return(res)
}

subjid_invid <- function(n, Raw_SUBJ_data, replace = TRUE, ...) {
  res <- Raw_SUBJ_data[
    sample(nrow(Raw_SUBJ_data), n, replace = replace),
    c("subjid", "invid")
  ]
  return(list(
    subjid = res$subjid,
    invid = res$invid
  ))
}

# visit_date <- function(n, start_date, possible_Visits, SnapshotWidth,...) {
#   rep(generate_consecutive_random_dates(nrow(possible_Visits), start_date, period_to_days(SnapshotWidth)), n)
# }

visit_dt <- function(n, start_date, possible_Visits, SnapshotWidth, ...) {
  rep(generate_consecutive_random_dates(nrow(possible_Visits), start_date, period_to_days(SnapshotWidth)), n)
}

visit <- function(n, possible_Visits, ...) {
  rep(possible_Visits$visit, n)
}
