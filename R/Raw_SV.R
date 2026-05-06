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
Raw_SV <- function(data, previous_data, spec, startDate, SnapshotWidth, ...) {
  inps <- list(...)
  if ("Raw_SV" %in% names(previous_data)) {
    dataset <- previous_data$Raw_SV
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
    foldername = c(
      "Screening", "Unscheduled", "Unscheduled", "Unscheduled", "Unscheduled",
      "Unscheduled", "Day 1", "Week 4", "Week 8", "Week 12", "Week 16", "Week 20",
      "Week 24", "Week 28", "Week 36", "Week 44", "Week 32", "Week 40", "Week 48",
      "Week 56", "Week 64", "Week 72", "Week 80", "Week 88", "Week 96", "Week 108",
      "Week 120", "Week 132", "Unscheduled", "Unscheduled", "Early Study Drug Discontinuation",
      "Unscheduled", "Unscheduled", "Unscheduled", "Follow-up Week 12", "Follow-up Week 24",
      "Follow-up Week 4", "Follow-up Week 8", "Follow-up Week 16", "Follow-up Week 20",
      "Week 144", "Unscheduled", "Unscheduled", "Unscheduled", "Unscheduled", "Unscheduled",
      "Unscheduled"
    ),
    instancename = c(
      "Screening", "Unscheduled (5)", "Unscheduled (4)", "Unscheduled (3)", "Unscheduled (1)",
      "Unscheduled (2)", "Day 1 (1)", "Week 4 (1)", "Week 8 (1)", "Week 12 (1)", "Week 16 (1)",
      "Week 20 (1)", "Week 24 (1)", "Week 28 (1)", "Week 36 (1)", "Week 44 (1)", "Week 32 (1)",
      "Week 40 (1)", "Week 48 (1)", "Week 56 (1)", "Week 64 (1)", "Week 72 (1)", "Week 80 (1)",
      "Week 88 (1)", "Week 96 (1)", "Week 108 (1)", "Week 120 (1)", "Week 132 (1)",
      "Unscheduled (7)", "Unscheduled (6)", "Early Study Drug Discontinuation (1)",
      "Unscheduled (9)", "Unscheduled (8)", "Unscheduled (10)", "Follow-up Week 12 (1)",
      "Follow-up Week 24 (1)", "Follow-up Week 4 (1)", "Follow-up Week 8 (1)",
      "Follow-up Week 16 (1)", "Follow-up Week 20 (1)", "Week 144 (1)", "Unscheduled (11)",
      "Unscheduled (15)", "Unscheduled (14)", "Unscheduled (13)", "Unscheduled (12)",
      "Unscheduled (16)"
    )
  )

  curr_spec <- spec$Raw_SV


  if (!("subjid" %in% names(curr_spec))) {
    curr_spec$subjid <- list(required = TRUE)
  }

  if (!("foldername" %in% names(curr_spec))) {
    curr_spec$foldername <- list(required = TRUE)
  }

  if (!("instancename" %in% names(curr_spec))) {
    curr_spec$instancename <- list(required = TRUE)
  }

  if (!("visit_dt" %in% names(curr_spec))) {
    curr_spec$visit_dt <- list(required = TRUE)
  }

  if (all(c("subjid") %in% names(curr_spec))) {
    curr_spec$subjid_repeated <- list(required = TRUE)
    curr_spec$subjid <- NULL
  }


  subjs <- subjid(n, external_subjid = data$Raw_SUBJ$subjid, replace = FALSE)
  args <- list(
    subjid_repeated = list(nrow(possible_visits), subjs),
    visit_dt = list(n, startDate, possible_visits, SnapshotWidth),
    default = list(n, subjs, possible_visits)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_SV, ...)

  return(res)
}

subjid_repeated <- function(n, subjs, ...) {
  return(list(
    subjid = repeat_rows(n, subjs)
  ))
}

foldername <- function(n, subjs, possible_visits, ...) {
  rep(possible_visits$foldername, length(subjs))
}

instancename <- function(n, subjs, possible_visits, ...) {
  rep(possible_visits$instancename, length(subjs))
}
