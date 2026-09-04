#' Generate Raw STUDCOMP Data
#'
#' Generate Raw STUDCOMP based on `STUDCOMP.yaml` from `gsm.mapping`.
#'
#' @inheritParams Raw_STUDY
#' @returns a data.frame pertaining to the raw dataset plugged into `STUDCOMP.yaml`
#' @family internal
#' @keywords internal
#' @noRd

Raw_STUDCOMP <- function(data, previous_data, spec, startDate, ...) {
  # Function body for Raw_SDRGCOMP
  inps <- list(...)

  curr_spec <- spec$Raw_STUDCOMP

  if ("Raw_STUDCOMP" %in% names(previous_data)) {
    dataset <- previous_data$Raw_STUDCOMP
    previous_row_num <- nrow(dataset)
  } else {
    dataset <- NULL
    previous_row_num <- 0
  }

  n <- inps$n - previous_row_num
  if (n == 0) {
    return(dataset)
  }

  if (all(c("subjid", "invid") %in% names(curr_spec))) {
    curr_spec$subjid_invid_unique <- list(required = TRUE)
    curr_spec$subjid <- NULL
    curr_spec$invid <- NULL
  }

  args <- list(
    subjid_invid_unique = list(n, data$Raw_SUBJ, previous_data$Raw_STUDCOMP, replace = FALSE),
    studyid = list(n, data$Raw_STUDY$protocol_number[[1]]),
    default = list(n, startDate)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_STUDCOMP, ...)

  return(res)
}

Raw_StudyCompletion <- function(data, previous_data, spec, startDate = Sys.Date(), ...) {
  # Function body for Raw_StudyCompletion
  inps <- list(...)

  curr_spec <- spec$Raw_StudyCompletion

  if ("Raw_StudyCompletion" %in% names(previous_data)) {
    dataset <- previous_data$Raw_StudyCompletion
    previous_row_num <- nrow(dataset)
  } else {
    dataset <- NULL
    previous_row_num <- 0
  }

  n <- inps$n - previous_row_num
  if (n == 0) {
    return(dataset)
  }

  args <- list(
    subjid = list(n, data$Raw_SUBJ$subjid, replace = FALSE),
    default = list(n, startDate)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_StudyCompletion, ...)

  return(res)
}

subjid_invid_unique <- function(n, Raw_SUBJ_data, previous_STUDCOMP_data, replace = TRUE, ...) {
  eligible_subj_data <- Raw_SUBJ_data[
    !(Raw_SUBJ_data$subjid %in% previous_STUDCOMP_data$subjid),
    c("subjid", "invid")
  ]
  res <- eligible_subj_data[
    sample(nrow(eligible_subj_data), n, replace = replace),
    c("subjid", "invid")
  ]
  return(list(
    subjid = res$subjid,
    invid = res$invid
  ))
}

compyn <- function(n, ...) {
  # Function body for compyn
  sample(c(NA, "N", "Y"),
    size = n,
    prob = c(0.7, 0.1, 0.2),
    replace = TRUE
  )
}

compreas <- function(n, ...) {
  sample(c("", "Lost to Follow-Up", "Death", "Withdrew Consent"),
    size = n,
    prob = c(0.85, 0.05, 0.05, 0.05),
    replace = TRUE
  )
}

completion_date <- function(n, ...) {
  rep(as.Date(Sys.Date()), n)
}
