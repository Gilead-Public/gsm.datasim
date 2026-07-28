#' Generate Raw VISIT Data
#'
#' Generate Raw VISIT Data based on `VISIT.yaml` from `gsm.mapping`.
#'
#' @inheritParams Raw_STUDY
#' @param startDate The beginning of dates to which the subjects' visits are
#' @returns a data.frame pertaining to the raw dataset plugged into `VISIT.yaml`
#' @family internal
#' @keywords internal
#' @noRd
Raw_VISIT <- function(data, previous_data, spec, startDate, SnapshotWidth, ...) {
  inps <- list(...)
  if ("Raw_VISIT" %in% names(previous_data)) {
    dataset <- previous_data$Raw_VISIT
    previous_row_num <- length(unique(dataset$subjid))
  } else {
    dataset <- NULL
    previous_row_num <- 0
  }

  n <- inps$n - previous_row_num
  if (n == 0) {
    return(dataset)
  }

  possible_visits <- data.frame(
    foldername = c("Screening", paste0("VISIT ", 1:5), "End of Treatment", "Follow-up"),
    instancename = c("Screening", paste0("VISIT ", 1:5), "End of Treatment", "Follow-up")
  )

  curr_spec <- spec$Raw_VISIT


  if (!("subjid" %in% names(curr_spec))) {
    curr_spec$subjid <- list(required = TRUE)
  }
  if (!("studyid" %in% names(curr_spec))) {
    curr_spec$studyid <- list(required = TRUE)
  }

  if (!("invid" %in% names(curr_spec))) {
    curr_spec$invid <- list(required = TRUE)
  }

  # Check if any existing spec entry maps to foldername/instancename via source_col
  has_foldername_source <- any(vapply(curr_spec, function(x) identical(x[["source_col"]], "foldername"), logical(1)))
  has_instancename_source <- any(vapply(curr_spec, function(x) identical(x[["source_col"]], "instancename"), logical(1)))

  if (!("foldername" %in% names(curr_spec)) && !has_foldername_source) {
    curr_spec$foldername <- list(required = TRUE)
  }

  if (!("instancename" %in% names(curr_spec)) && !has_instancename_source) {
    curr_spec$instancename <- list(required = TRUE)
  }

  if (!("visit_dt" %in% names(curr_spec))) {
    curr_spec$visit_dt <- list(required = TRUE)
  }

  if (all(c("subjid") %in% names(curr_spec))) {
    curr_spec$subjid_repeated <- list(required = TRUE)
    curr_spec$subjid <- NULL
  }

  if (all(c("invid") %in% names(curr_spec))) {
    curr_spec$invid_repeated <- list(required = TRUE)
    curr_spec$invid <- NULL
  }


  existing_subjs <- if (is.null(dataset)) unique(character(0)) else unique(dataset$subjid)
  available_subjs <- setdiff(unique(data$Raw_SUBJ$subjid), existing_subjs)
  subjs <- subjid(n, external_subjid = available_subjs, replace = FALSE)
  invids <- data.frame(subjid = subjs) |> 
    left_join(select(data$Raw_SUBJ, subjid, invid), by = "subjid") |>
    pull(invid)

  args <- list(
    subjid_repeated = list(nrow(possible_visits), subjs),
    invid_repeated = list(nrow(possible_visits), invids),
    studyid = list(n * nrow(possible_visits), data$Raw_STUDY$protocol_number[[1]]),
    visit_dt = list(n, startDate, possible_visits, SnapshotWidth),
    default = list(n, subjs, possible_visits)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_VISIT, ...)

  return(res)
}

subjid_repeated <- function(n, subjs, ...) {
  return(list(
    subjid = repeat_rows(n, subjs)
  ))
}

invid_repeated <- function(n, invids, ...) {
  return(list(
    invid = repeat_rows(n, invids)
  ))
}
foldername <- function(n, subjs, possible_visits, ...) {
  rep(possible_visits$foldername, length(subjs))
}

instancename <- function(n, subjs, possible_visits, ...) {
  rep(possible_visits$instancename, length(subjs))
}
visit_dt <- function(n, start_date, possible_Visits, SnapshotWidth, ...) {
  rep(generate_consecutive_random_dates(nrow(possible_Visits), start_date, period_to_days(SnapshotWidth)), n)
}
visit <- foldername
