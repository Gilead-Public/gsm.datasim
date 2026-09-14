#' Generate Raw SDRGCOMP Data
#'
#' Generate Raw SDRGCOMP based on `SDRGCOMP.yaml` from `gsm.mapping`.
#'
#' @inheritParams Raw_STUDY
#' @returns a data.frame pertaining to the raw dataset plugged into `SDRGCOMP.yaml`
#' @family internal
#' @keywords internal
#' @noRd

Raw_SDRGCOMP <- function(data, previous_data, spec, startDate, ...) {
  # Function body for Raw_SDRGCOMP
  inps <- list(...)

  curr_spec <- spec$Raw_SDRGCOMP

  if ("Raw_SDRGCOMP" %in% names(previous_data)) {
    dataset <- previous_data$Raw_SDRGCOMP
    previous_row_num <- nrow(dataset)
  } else {
    dataset <- NULL
    previous_row_num <- 0
  }

  n <- inps$n - previous_row_num
  if (n == 0) {
    return(dataset)
  }

  existing_subjs <- if (is.null(dataset)) unique(character(0)) else unique(dataset$subjid)
  available_subjs <- setdiff(unique(data$Raw_VISIT$subjid), existing_subjs)

  args <- list(
    subjid = list(n, available_subjs, replace = FALSE),
    studyid = list(n, data$Raw_STUDY$protocol_number[[1]]),
    default = list(n, startDate)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_SDRGCOMP, ...)

  return(res)
}

sdrgyn <- function(n, ...) {
  # Function body for sdrgyn
  sample(c("Y", "N"),
    prob = c(0.70, 0.30),
    n,
    replace = TRUE
  )
}

#' Subjects that are enrolled but never dosed (IP non-starters)
#'
#' The single definition of "enrolled but never dosed", used to seed the
#' simulated `drv_ip_nonstarter_status`.
#'
#' @param raw_subj a `Raw_SUBJ` data.frame carrying `subjid`, `enrollyn`,
#'   `firstdosedate`.
#' @returns a character vector of non-starter `subjid`s.
#' @family internal
#' @keywords internal
#' @noRd
nonstarter_subjids <- function(raw_subj) {
  if (is.null(raw_subj) ||
    !all(c("subjid", "enrollyn", "firstdosedate") %in% names(raw_subj))) {
    return(character(0))
  }
  as.character(raw_subj$subjid[raw_subj$enrollyn %in% "Y" & is.na(raw_subj$firstdosedate)])
}
