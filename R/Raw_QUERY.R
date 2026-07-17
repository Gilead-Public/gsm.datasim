#' Generate Raw QUERY Data
#'
#' Generate Raw QUERY based on `QUERY.yaml` from `gsm.mapping`.
#'
#' @inheritParams Raw_STUDY
#' @returns a data.frame pertaining to the raw dataset plugged into `QUERY.yaml`
#' @family internal
#' @keywords internal
#' @noRd

Raw_QUERY <- function(data, previous_data, spec, startDate, ...) {
  inps <- list(...)

  curr_spec <- spec$Raw_QUERY

  if ("Raw_QUERY" %in% names(previous_data)) {
    dataset <- previous_data$Raw_QUERY
    previous_row_num <- length(unique(dataset$subjid))
  } else {
    dataset <- NULL
    previous_row_num <- 0
  }

  n <- inps$n - previous_row_num
  if (n == 0) {
    return(dataset)
  }

  # Function body for Raw_QUERY
  if (!("visnam" %in% names(curr_spec))) {
    curr_spec$visnam <- list(required = TRUE)
  }

  entries_per_subj_visit <- 2

  if (all(c("subject_nsv", "visnam") %in% names(curr_spec))) {
    curr_spec$subject_nsv_visit_repeated <- list(required = TRUE)
    curr_spec$subject_nsv <- NULL
    curr_spec$visnam <- NULL
  }

  subject_nsvs <- subject_nsv(n, data$Raw_SUBJ$subjid,
    subject_nsv = data$Raw_SUBJ$subject_nsv, replace = FALSE
  )

  subject_nsv_visits <- data$Raw_VISIT %>%
    dplyr::left_join((data$Raw_SUBJ %>% dplyr::select(subjid, subject_nsv)), by = dplyr::join_by(subjid)) %>%
    dplyr::filter(subject_nsv %in% subject_nsvs) %>%
    dplyr::select(subject_nsv, instancename)

  all_n <- nrow(subject_nsv_visits) * entries_per_subj_visit

  args <- list(
    subject_nsv_visit_repeated = list(entries_per_subj_visit, subject_nsv_visits),
    studyid = list(all_n, data$Raw_STUDY$protocol_number[[1]]),
    queryage = list(all_n, subject_nsv_visits, data$Raw_SUBJ),
    default = list(all_n, startDate)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_QUERY, ...)

  return(res)
}

querystatus <- function(n, ...) {
  # Function body for querystatus
  base_prob <- c(0.02, 0.96, 0.02)
  sample(c("Answered", "Closed", "Open"),
    prob = scale_outlier_probabilities(base_prob, outlier_idx = 3),
    n,
    replace = TRUE
  )
}

queryage <- function(n, subject_nsv_visits = NULL, Raw_SUBJ_data = NULL, ...) {
  # z-score style long-tail query ages to better support z-score based flags.
  vals <- generate_zscore_outlier_values(
    n = n,
    mean = 45,
    sd = 22,
    min_value = 1,
    max_value = 359,
    one_sided = TRUE,
    integer = TRUE
  )

  if (!is.null(subject_nsv_visits) && "subject_nsv" %in% names(subject_nsv_visits)) {
    vals <- inject_site_hotspot_outliers(
      values = vals,
      row_keys = subject_nsv_visits$subject_nsv,
      key_map = Raw_SUBJ_data,
      key_col = "subject_nsv",
      site_col = "invid",
      min_z = 3
    )
    vals <- as.integer(pmin(359, pmax(1, round(vals))))
  }

  vals
}
