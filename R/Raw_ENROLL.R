#' Generate Raw ENROLL Data
#'
#' Generate Raw ENROLL based on `ENROLL.yaml` from `gsm.mapping`.
#'
#' @inheritParams Raw_STUDY
#' @returns a data.frame pertaining to the raw dataset plugged into `ENROLL.yaml`
#' @family internal
#' @keywords internal
#' @noRd
Raw_ENROLL <- function(data, previous_data, spec, startDate, ...) {
  inps <- list(...)

  curr_spec <- spec$Raw_ENROLL

  if ("Raw_ENROLL" %in% names(previous_data)) {
    dataset <- previous_data$Raw_ENROLL
    previous_row_num <- nrow(dataset)
  } else {
    dataset <- NULL
    previous_row_num <- 0
  }

  n <- inps$n_enroll - previous_row_num
  if (n == 0) {
    return(dataset)
  }

  if (all(c("invid", "country", "subjid", "subjectid", "enrollyn") %in% names(curr_spec))) {
    curr_spec$subject_to_enrollment <- list(required = TRUE)
    curr_spec$invid <- NULL
    curr_spec$country <- NULL
    curr_spec$subjid <- NULL
    curr_spec$subjectid <- NULL
    curr_spec$enrollyn <- NULL
  }

  args <- list(
    studyid = list(n, data$Raw_STUDY$protocol_number[[1]]),
    subject_to_enrollment = list(n, data, previous_data$Raw_ENROLL$subjid),
    default = list(n, startDate)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_ENROLL, ...)

  return(res)
}

subjectid <- function(n, ...) {
  # Function body for subjectid
  paste0("S", 1:n)
}

subject_to_enrollment <- function(n, data, previous_data, ...) {
  if (length(previous_data) != 0) {
    data_pool <- data$Raw_SUBJ[!(data$Raw_SUBJ$subjid %in% previous_data), ]
  } else {
    data_pool <- data$Raw_SUBJ
  }

  sample_subset <- sample(min(nrow(data_pool), n), n, replace = FALSE)
  res <- data_pool[
    sample_subset,
    c("subjid", "invid", "country", "enrollyn")
  ] %>%
    dplyr::mutate(subjectid = paste0("XX-", subjid))

  return(res)
}
