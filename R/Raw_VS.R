#' Column generators for Raw VS (Vital Signs) Data
#'
#' Generate Raw VS based on `VS.yaml` from `gsm.mapping`.
#' Wide format: one row per subject × visit with columns for all 8 vitals measures:
#' weight, height, bmi, sysbp, diabp, pulse, temp, resp.
#'
#' Domain generation itself is registered in `domain_registry.R` (`Raw_VS`
#' entry); the functions below are the per-column generators dispatched by
#' `add_new_var_data()` based on `spec$Raw_VS` column names.
#'
#' @family internal
#' @keywords internal
#' @noRd


# Note: parallels `subj_visit_repeated()` in Raw_LB.R (n=1, one row per
# subject-visit, no test repeat factor), but retains the `instancename`
# column name per the VS.yaml spec (Raw_LB uses `visnam`). Named distinctly
# from Raw_LB's `subj_visit_repeated()` to avoid colliding in the package
# namespace (generator functions are dispatched by bare name via `do.call()`).
vs_subj_visit_repeated <- function(n, data, ...) {
  res <- repeat_rows(n, data)
  return(list(
    subjid = res$subjid,
    instancename = res$instancename
  ))
}

vs_invid_repeated <- function(n, invids, ...) {
  return(list(
    invid = repeat_rows(n, invids)
  ))
}


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
