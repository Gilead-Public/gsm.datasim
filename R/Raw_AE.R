#' Generate Raw AE Data
#'
#' Generate Raw AE Data based on `AE.yaml` from `gsm.mapping`.
#'
#' @inheritParams Raw_STUDY
#' @param startDate The beginning of dates to which the AEs can occur.
#' @param endDate The end of dates to which the AEs can occur.
#'
#' @returns a data.frame pertaining to the raw dataset plugged into `AE.yaml`
#' @family internal
#' @keywords internal
#' @noRd
Raw_AE <- function(data, previous_data, spec, startDate, endDate, ...) {
  inps <- list(...)

  curr_spec <- spec$Raw_AE

  if ("Raw_AE" %in% names(previous_data)) {
    dataset <- previous_data$Raw_AE
    previous_row_num <- nrow(dataset)
  } else {
    dataset <- NULL
    previous_row_num <- 0
  }

  n <- inps$n - previous_row_num
  if (n == 0) {
    return(dataset)
  }

  if (all(c("aeser", "aest_dt", "aeen_dt", "mdrpt_nsv", "mdrsoc_nsv", "aetoxgr") %in% names(curr_spec))) {
    curr_spec$aeser <- list(required = TRUE)
    curr_spec$aest_dt_aeen_dt <- list(required = TRUE)
    curr_spec$aest_dt <- NULL
    curr_spec$aeen_dt <- NULL
    curr_spec$mdrpt_nsv <- list(required = TRUE)
    curr_spec$mdrsoc_nsv <- list(required = TRUE)
    curr_spec$aetoxgr <- list(required = TRUE)
  }

  args <- list(
    subjid = list(n, external_subjid = data$Raw_SUBJ$subjid),
    aest_dt_aeen_dt = list(n, startDate, endDate),
    studyid = list(n, data$Raw_STUDY$protocol_number[[1]]),
    default = list(n, startDate)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_AE, ...)
  res <- simulate_ae_grading(
    res,
    new_rows = seq_len(n) + previous_row_num,
    Raw_SUBJ_data = data$Raw_SUBJ,
    studyid = data$Raw_STUDY$protocol_number[[1]],
    spec = spec$Raw_AE
  )

  return(res)
}

aeser <- function(n, ..., Raw_SUBJ_data = NULL, row_keys = NULL) {
  # Favor serious AEs in hotspot sites so z-score metrics can separate entities.
  # `row_keys` must be each record's own subjid; otherwise the hotspot site
  # assignment is unrelated to the site the record maps to.
  sample_categorical_with_hotspots(
    values = c("Y", "N"),
    n = n,
    base_prob = c(0.15, 0.85),
    outlier_idx = 1,
    row_keys = row_keys,
    key_map = Raw_SUBJ_data,
    key_col = "subjid",
    site_col = "invid"
  )
}

aeongo <- function(n, ...) {
  # Function body for aeser
  sample(c("Y", "N"), n, replace = TRUE)
}

aerel <- function(n, ...) {
  # Function body for aeser
  sample(c("Y", "N"), n, replace = TRUE)
}

aest_dt <- function(n, startDate, endDate, ...) {
  sample(seq(as.Date(startDate), as.Date(endDate), by = "day"), n, replace = TRUE)
}
aeen_dt <- function(n, aestartDate, ...) {
  as.Date(aestartDate) + sample(1:3, n, replace = TRUE)
}
mdrpt_nsv <- function(n, ...) {
  ae_term_catalog()$mdrpt_nsv[sample_ae_term_idx(n)]
}
mdrsoc_nsv <- function(n, ...) {
  ae_term_catalog()$mdrsoc_nsv[sample_ae_term_idx(n)]
}
aetoxgr <- function(n, ..., mdrpt_nsv = NULL, site_shift = 0) {
  if (is.null(mdrpt_nsv)) {
    mdrpt_nsv <- ae_term_catalog()$mdrpt_nsv[sample_ae_term_idx(n)]
  }
  draw_ae_grades(mdrpt_nsv, site_shift = site_shift)
}

aest_dt_aeen_dt <- function(n, startDate, endDate, ...) {
  aest_dat <- aest_dt(n, startDate, endDate, ...)
  aeen_dat <- aeen_dt(n, aest_dat, ...)
  return(list(
    aest_dt = aest_dat,
    aeen_dt = aeen_dat
  ))
}
