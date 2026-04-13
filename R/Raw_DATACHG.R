#' Generate Raw DATACHG Data
#'
#' Generate Raw DATACHG based on `DATACHG.yaml` from `gsm.mapping`.
#'
#' @inheritParams Raw_STUDY
#' @returns a data.frame pertaining to the raw dataset plugged into `DATACHG.yaml`
#' @family internal
#' @keywords internal
#' @noRd

Raw_DATACHG <- function(data, previous_data, spec, startDate, ...) {
  # Function body for Raw_DATACHG
  # Function body for Raw_SDRGCOMP
  inps <- list(...)

  curr_spec <- spec$Raw_DATACHG

  if ("Raw_DATACHG" %in% names(previous_data)) {
    dataset <- previous_data$Raw_DATACHG
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

  if (!("field" %in% names(curr_spec))) {
    curr_spec$field <- list(required = TRUE)
  }

  forms <- generate_form_df(32)

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
    n_changes = list(all_n, subject_nsv_visits, data$Raw_SUBJ),
    default = list(all_n, subject_nsv_visits, forms)
  )


  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_DATACHG, ...)

  return(res)
}

subject_nsv_visit_repeated <- function(n, data, ...) {
  res <- repeat_rows(n, data)
  return(list(
    subject_nsv = res$subject_nsv,
    visnam = res$instancename
  ))
}

form <- function(n, subject_nsv_visits, forms, ...) {
  rep(forms$form, nrow(subject_nsv_visits))
}

field <- function(n, subject_nsv_visits, forms, ...) {
  rep(forms$field, nrow(subject_nsv_visits))
}

n_changes <- function(n, subject_nsv_visits = NULL, Raw_SUBJ_data = NULL, ...) {
  # z-score style long-tail counts to create clearer statistical outliers.
  vals <- generate_zscore_outlier_values(
    n = n,
    mean = 0.9,
    sd = 0.9,
    min_value = 0,
    max_value = 20,
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
    vals <- as.integer(pmin(20, pmax(0, round(vals))))
  }

  vals
}
