#' Generate Raw PD Data
#'
#' Generate Raw PD based on `PD.yaml` from `gsm.mapping`.
#'
#' @inheritParams Raw_STUDY
#' @returns a data.frame pertaining to the raw dataset plugged into `PD.yaml`
#' @family internal
#' @keywords internal
#' @noRd

Raw_PD <- function(data, previous_data, spec, startDate, ...) {
  inps <- list(...)

  curr_spec <- spec$Raw_PD

  if ("Raw_PD" %in% names(previous_data)) {
    dataset <- previous_data$Raw_PD
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
    studyid = list(n, data$Raw_STUDY$protocol_number[[1]]),
    subjid = list(n, external_subjid = data$Raw_SUBJ$subjid),
    dvdecod = list(n, data$Raw_SUBJ),
    default = list(n, startDate)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_PD, ...)

  return(res)
}

deemedimportant <- function(n, ...) {
  # Function body for deemedimportant
  sample(c("Yes", "No"), n, replace = TRUE)
}

dvdecod <- function(n, Raw_SUBJ_data = NULL, ...) {
  row_keys <- if (is.data.frame(Raw_SUBJ_data) && "subjid" %in% names(Raw_SUBJ_data)) {
    sample(Raw_SUBJ_data$subjid, n, replace = TRUE)
  } else {
    NULL
  }

  sample_categorical_with_hotspots(
    values = c("Informed Consent", "Missing Data", "Study Procedures", "Inclusion Criteria", "Exclusion Criteria"),
    n = n,
    base_prob = c(0.3, 0.3, 0.3, 0.05, 0.05),
    outlier_idx = 4:5,
    row_keys = row_keys,
    key_map = Raw_SUBJ_data,
    key_col = "subjid",
    site_col = "invid"
  )
}

dvterm <- function(n, ...) {
  # Function body for deemedimportant
  sample(c("Inclusion/Exclusion description"), n, replace = TRUE)
}

category <- function(n, ...) {
  sample(c("cat 1", "cat 2"), n, replace = TRUE)
}
