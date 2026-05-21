#' Generate Raw DATAENT Data
#'
#' Generate Raw DATAENT based on `DATAENT.yaml` from `gsm.mapping`.
#'
#' @inheritParams Raw_STUDY
#' @returns a data.frame pertaining to the raw dataset plugged into `DATAENT.yaml`
#' @family internal
#' @keywords internal
#' @noRd

Raw_DATAENT <- function(data, previous_data, spec, startDate, ...) {
  inps <- list(...)

  curr_spec <- spec$Raw_DATAENT

  if ("Raw_DATAENT" %in% names(previous_data)) {
    dataset <- previous_data$Raw_DATAENT
    previous_row_num <- length(unique(dataset$subject_nsv))
  } else {
    dataset <- NULL
    previous_row_num <- 0
  }

  n <- inps$n - previous_row_num
  if (n == 0) {
    return(dataset)
  }

  if (!("visnam" %in% names(curr_spec))) {
    curr_spec$visnam <- list(required = TRUE)
  }

  if (!("form" %in% names(curr_spec))) {
    curr_spec$form <- list(required = TRUE)
  }

  form <- paste0("form", 1:8)
  forms <- data.frame(
    form = form
  )

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

  all_n <- nrow(subject_nsv_visits) * nrow(forms)

  args <- list(
    subject_nsv_visit_repeated = list(nrow(forms), subject_nsv_visits),
    visit_date = list(all_n, startDate),
    studyid = list(all_n, data$Raw_STUDY$protocol_number[[1]]),
    data_entry_lag = list(all_n, subject_nsv_visits, data$Raw_SUBJ),
    default = list(all_n, subject_nsv_visits, forms, startDate)
  )


  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_DATAENT, ...)

  return(res)
}

data_entry_lag <- function(n, subject_nsv_visits = NULL, Raw_SUBJ_data = NULL, ...) {
  # z-score style long-tail lags so flagging can key off distance from the mean.
  vals <- generate_zscore_outlier_values(
    n = n,
    mean = 4,
    sd = 2.2,
    min_value = 0,
    max_value = 30,
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
    vals <- as.integer(pmin(30, pmax(0, round(vals))))
  }

  vals
}
