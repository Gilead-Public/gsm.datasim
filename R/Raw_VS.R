#' Generate Raw VS (Vital Signs) Data
#'
#' Generate Raw VS based on `VS.yaml` from `gsm.mapping`.
#' Wide format: one row per subject × visit with columns for all 8 vitals measures:
#' weight, height, bmi, sysbp, diabp, pulse, temp, resp.
#'
#' @inheritParams Raw_STUDY
#' @returns a data.frame pertaining to the raw dataset plugged into `VS.yaml`
#' @family internal
#' @keywords internal
#' @noRd

Raw_VS <- function(data, previous_data, spec, startDate, ...) {
  inps <- list(...)

  curr_spec <- spec$Raw_VS

  if ("Raw_VS" %in% names(previous_data)) {
    dataset <- previous_data$Raw_VS
    previous_row_num <- length(unique(dataset$subjid))
  } else {
    dataset <- NULL
    previous_row_num <- 0
  }

  n <- inps$n - previous_row_num
  if (n == 0) {
    return(dataset)
  }

  # Ensure required fields are in spec
  if (!("visnam" %in% names(curr_spec))) {
    curr_spec$visnam <- list(required = TRUE)
  }

  if (all(c("subjid", "visnam") %in% names(curr_spec))) {
    curr_spec$subj_visit_repeated <- list(required = TRUE)
    curr_spec$subjid <- NULL
    curr_spec$visnam <- NULL
  }

  subjs <- subjid(n, external_subjid = data$Raw_SUBJ$subjid, replace = FALSE)
  subj_visits <- data$Raw_SV %>%
    dplyr::filter(subjid %in% subjs) %>%
    dplyr::select(subjid, instancename)

  all_n <- nrow(subj_visits)

  args <- list(
    subj_visit_repeated = list(1, subj_visits),
    studyid = list(all_n, data$Raw_STUDY$protocol_number[[1]]),
    vs_dt = list(all_n, startDate),
    vsperf_std = list(all_n),
    weight = list(all_n, subj_visits$subjid),
    height = list(all_n, subj_visits$subjid),
    bmi = list(all_n, subj_visits$subjid),
    sysbp = list(all_n, subj_visits$subjid),
    diabp = list(all_n, subj_visits$subjid),
    pulse = list(all_n, subj_visits$subjid),
    temp = list(all_n, subj_visits$subjid),
    resp = list(all_n, subj_visits$subjid),
    default = list(all_n, subj_visits)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_VS, ...)

  return(res)
}


# Note: uses the existing `subj_visit_repeated` function from Raw_LB.R
# with n=1 (one row per subject-visit, no test repeat factor)


vs_dt <- generic_date


vsperf_std <- function(n, ...) {
  # ~95% performed, ~5% not performed
  sample(c("Y", "N"), n, prob = c(0.95, 0.05), replace = TRUE)
}


weight <- function(n, subjects, ...) {
  # Generate realistic weights with ~10% duplicates per subject
  .generate_vital_with_duplicates(n, subjects, mean = 75, sd = 10, digits = 1)
}


sysbp <- function(n, subjects, ...) {
  # Generate systolic BP with ~10% duplicates per subject
  .generate_vital_with_duplicates(n, subjects, mean = 125, sd = 15, digits = 0)
}


diabp <- function(n, subjects, ...) {
  # Generate diastolic BP with ~10% duplicates per subject
  .generate_vital_with_duplicates(n, subjects, mean = 80, sd = 10, digits = 0)
}


height <- function(n, subjects, ...) {
  # Generate height (cm) with ~10% duplicates per subject
  .generate_vital_with_duplicates(n, subjects, mean = 170, sd = 10, digits = 1)
}


bmi <- function(n, subjects, ...) {
  # Generate BMI with ~10% duplicates per subject
  .generate_vital_with_duplicates(n, subjects, mean = 25, sd = 4, digits = 1)
}


pulse <- function(n, subjects, ...) {
  # Generate pulse/heart rate with ~10% duplicates per subject
  .generate_vital_with_duplicates(n, subjects, mean = 72, sd = 12, digits = 0)
}


temp <- function(n, subjects, ...) {
  # Generate temperature (°C) with ~10% duplicates per subject
  .generate_vital_with_duplicates(n, subjects, mean = 36.8, sd = 0.4, digits = 1)
}


resp <- function(n, subjects, ...) {
  # Generate respiratory rate with ~10% duplicates per subject
  .generate_vital_with_duplicates(n, subjects, mean = 16, sd = 3, digits = 0)
}


#' Generate vital sign values with intentional duplicate injection
#'
#' For each subject, generates values from a normal distribution, then replaces
#' ~dDuplicateRate of subsequent values with a copy of a previous value.
#'
#' @param n Total number of values to generate
#' @param subjects Character vector of subject IDs (length n, with repeats)
#' @param mean Mean of normal distribution
#' @param sd Standard deviation
#' @param digits Number of decimal places to round
#' @param dDuplicateRate Proportion of subsequent records to make duplicates
#' @returns Numeric vector of length n
#' @keywords internal
#' @noRd
.generate_vital_with_duplicates <- function(n, subjects, mean, sd, digits, dDuplicateRate = 0.10) {
  # Generate all values first

  values <- round(stats::rnorm(n, mean = mean, sd = sd), digits = digits)

  # Inject duplicates per subject
  unique_subjs <- unique(subjects)
  for (subj in unique_subjs) {
    idx <- which(subjects == subj)
    if (length(idx) <= 1) next

    # For subsequent records (not the first), randomly duplicate
    subsequent_idx <- idx[-1]
    n_dups <- max(1, round(length(subsequent_idx) * dDuplicateRate))
    dup_positions <- sample(subsequent_idx, size = min(n_dups, length(subsequent_idx)))

    for (pos in dup_positions) {
      # Copy a previous value for this subject
      prior_idx <- idx[idx < pos]
      values[pos] <- values[sample(prior_idx, 1)]
    }
  }

  return(values)
}
