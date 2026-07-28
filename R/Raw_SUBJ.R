#' Generate Raw SUBJ Data
#'
#' Generate Raw SUBJ Data based on `SUBJ.yaml` from `gsm.mapping`.
#'
#' @inheritParams Raw_STUDY
#' @param startDate The beginning of dates to which the subjects can be enrolled/started
#' @param endDate The end of dates to which the subjects completed/leave study.
#'
#' @returns a data.frame pertaining to the raw dataset plugged into `SUBJ.yaml`
#' @family internal
#' @keywords internal
#' @noRd
Raw_SUBJ <- function(data, previous_data, spec, startDate, endDate, ...) {
  inps <- list(...)

  curr_spec <- spec$Raw_SUBJ

  if ("Raw_SUBJ" %in% names(previous_data)) {
    dataset <- previous_data$Raw_SUBJ
    previous_row_num <- nrow(dataset)
  } else {
    dataset <- NULL
    previous_row_num <- 0
  }

  n <- inps$n_subj - previous_row_num
  if (n == 0) {
    return(dataset)
  }

  if (!("enrolldt" %in% names(curr_spec))) {
    curr_spec$enrolldt <- list(required = TRUE)
  }

  if (all(c("invid", "country") %in% names(curr_spec))) {
    curr_spec$subject_site_synq <- list(required = TRUE)
    curr_spec$invid <- NULL
    curr_spec$country <- NULL
  }

  if (all(c("subjid", "subject_nsv") %in% names(curr_spec))) {
    curr_spec$subjid_subject_nsv <- list(required = TRUE)
    curr_spec$subjid <- NULL
    curr_spec$subject_nsv <- NULL
  }

  if (all(c("enrollyn", "enrolldt", "timeonstudy", "firstparticipantdate", "firstdosedate", "timeontreatment") %in% names(curr_spec))) {
    curr_spec$enrollyn_enrolldt_timeonstudy_firstparticipantdate_firstdosedate_timeontreatment <- list(required = TRUE)
    curr_spec$enrolldt <- NULL
    curr_spec$timeonstudy <- NULL
    curr_spec$enrollyn <- NULL

    curr_spec$firstparticipantdate <- NULL
    curr_spec$firstdosedate <- NULL
    curr_spec$timeontreatment <- NULL
  }

  args <- list(
    studyid = list(n, data$Raw_STUDY$protocol_number[[1]]),
    subjid_subject_nsv = list(n, previous_data$Raw_SUBJ$subjid),
    subject_site_synq = list(n, data$Raw_SITE),
    enrollyn_enrolldt_timeonstudy_firstparticipantdate_firstdosedate_timeontreatment = list(n, startDate, endDate),
    default = list(n, startDate)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_SUBJ, ...)

  # Recalculate for all data
  res$timeonstudy <- timeonstudy(n, res$enrolldt, endDate)

  return(res)
}

subjid <- function(n, external_subjid = NULL, replace = TRUE, previous_subjid = NULL, ...) {
  args <- list(...)

  if (!is.null(external_subjid)) {
    return(sample(external_subjid, n, replace = replace))
  }

  # Generate all possible 3-digit numbers as strings with leading zeros
  possible_numbers <- sprintf("%03d", 0:99999)

  # Create all possible strings starting with "0X" and ending with the 3-digit numbers
  possible_strings <- paste0("S", possible_numbers)

  # Exclude the old strings to avoid duplication
  new_strings_available <- setdiff(possible_strings, previous_subjid)

  # Check if there are enough unique strings to generate
  if (length(new_strings_available) < n) {
    stop("Not enough unique strings available to generate ", n, " new strings.")
  }
  res <- sample(new_strings_available, n, replace = FALSE)

  # Randomly sample 'n' unique strings from the available strings
  return(res)
}

subjid_subject_nsv <- function(n, dataset, ...) {
  subjid_dat <- subjid(n, previous_subjid = dataset, ...)
  subject_nsv_dat <- subject_nsv(n, subjid_dat, ...)
  return(list(
    subjid = subjid_dat,
    subject_nsv = subject_nsv_dat
  ))
}

subject_site_synq <- function(n, Raw_SITE_data, ...) {
  Raw_SITE_data[
    sample(nrow(Raw_SITE_data), n, replace = TRUE),
    c("pi_number", "country")
  ] %>%
    dplyr::rename("invid" = "pi_number")
}

enrollyn <- function(n, ...) {
  # Function body for enrollyn
  # if (isSubjDataset) {
  #   return("Y")
  # } else {
  #   return("N")
  # }
  sample(c("Y", "N"),
    prob = c(0.75, 0.25),
    n,
    replace = TRUE
  )
}

subject_nsv <- function(n, subjid, subject_nsv = NULL, replace = TRUE, ...) {
  # Function body for subject_nsv
  if (!is.null(subject_nsv)) {
    return(sample(subject_nsv, n, replace = replace))
  }
  return(paste0(subjid, "-XXXX"))
}

enrolldt <- function(n, startDate, endDate, enrollyn_dat, ...) {
  full_sample <- sample(seq(as.Date(startDate), as.Date(endDate), by = "day"), n, replace = TRUE)
  full_sample[enrollyn_dat == "N"] <- NA
  return(full_sample)
}


timeonstudy <- function(n, enrolldt, endDate, ...) {
  # Function body for timeonstudy
  as.numeric(as.Date(endDate) - as.Date(enrolldt)) %>% as.integer()
}

agerep <- function(n, ...) {
  sample(18:55, n, replace = T)
}

sex <- function(n, ...) {
  sample(c("M", "F"), n, replace = T)
}
race <- function(n, ...) {
  sample(c("White", "Asian", "Black", "Other"), n, replace = T)
}
enrollyn_enrolldt_timeonstudy_firstparticipantdate_firstdosedate_timeontreatment <- function(n, startDate, endDate, ...) {
  enrollyn_dat <- enrollyn(n, ...)
  enrolldt_dat <- enrolldt(n, startDate, endDate, enrollyn_dat, ...)
  timeonstudy_dat <- timeonstudy(n, enrolldt_dat, endDate, ...)

  firstparticipantdate_dat <- enrolldt_dat
  firstdosedate_dat <- enrolldt_dat
  timeontreatment_dat <- timeonstudy_dat
  return(list(
    enrollyn = enrollyn_dat,
    enrolldt = enrolldt_dat,
    timeonstudy = timeonstudy_dat,
    firstparticipantdate = firstparticipantdate_dat,
    firstdosedate = firstdosedate_dat,
    timeontreatment = timeontreatment_dat
  ))
}
