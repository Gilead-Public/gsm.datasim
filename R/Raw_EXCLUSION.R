#' Generate Raw EXCLUSION Data
#'
#' Generate Raw EXCLUSION data for study exclusion criteria records.
#' Follows the same delta-accumulation pattern as other domain generators.
#'
#' @inheritParams Raw_STUDY
#' @returns a data.frame pertaining to the raw dataset for exclusion criteria
#' @family internal
#' @keywords internal
#' @noRd
Raw_EXCLUSION <- function(data, previous_data, spec, ...) {
  inps <- list(...)

  curr_spec <- spec$Raw_EXCLUSION

  if ("Raw_EXCLUSION" %in% names(previous_data)) {
    dataset <- previous_data$Raw_EXCLUSION
    previous_row_num <- nrow(dataset)
  } else {
    dataset <- NULL
    previous_row_num <- 0
  }

  n <- inps$n_EXCLUSION - previous_row_num
  if (n == 0) {
    return(dataset)
  }

  args <- list(
    subjid = list(n, external_subjid = data$Raw_SUBJ$subjid, replace = FALSE),
    studyid = list(n, data$Raw_STUDY$protocol_number[[1]]),
    excl_reason = list(n),
    default = list(n)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_EXCLUSION, ...)

  return(res)
}

excl_reason <- function(n, ...) {
  sample(
    c(
      "Screen Failure",
      "Withdrew Consent",
      "Protocol Deviation",
      "Lost to Follow-Up",
      "Physician Decision"
    ),
    size = n,
    prob = c(0.35, 0.20, 0.20, 0.15, 0.10),
    replace = TRUE
  )
}
