#' Generate Raw Study Data
#'
#' Generate Raw Study Data based on `STUDY.yaml` from `gsm.mapping`.
#' Includes information about a clinical study, target enrollment/recruitment numbers and
#' actual enrollment.
#'
#' @param data List of data frames pertaining to a simulated study which is passed through in a list of snapshots
#' @param previous_data List of data frames pertaining to prior snpashot with relation to `data`
#' @param spec  A list representing the combined specifications from `CombineSpecs` across the desired domains for a simulation.
#' @param ... Additional arguments to be passed.
#'
#' @returns a data.frame pertaining to the raw dataset plugged into `STUDY.yaml`
#' @family internal
#' @keywords internal
#' @noRd

Raw_STUDY <- function(data, previous_data, spec, ...) {
  inps <- list(...)

  curr_spec <- spec$Raw_STUDY

  if ("Raw_STUDY" %in% names(previous_data)) {
    dataset <- previous_data$Raw_STUDY
  } else {
    dataset <- NULL
  }

  args <- list(
    studyid = list(1, inps$StudyID),
    num_plan_site = list(inps$SiteCount),
    num_plan_subj = list(inps$ParticipantCount),
    act_fpfv = list(inps$MinDate, inps$MaxDate, dataset$act_fpfv),
    est_fpfv = list(inps$MinDate, inps$MaxDate, dataset$est_fpfv),
    est_lpfv = list(inps$GlobalMaxDate - 30, inps$GlobalMaxDate, dataset$est_lpfv),
    est_lplv = list(inps$GlobalMaxDate + 30, inps$GlobalMaxDate + 120, dataset$est_lplv),
    db_lock_dt = list(1, inps$GlobalMaxDate),
    phase = list(1, external_phase = c("P1", "P2", "P3", "P4")),
    default = list(1)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_STUDY, ...)
  return(res)
}

act_fpfv <- function(date_min, date_lim, prev_data, ...) {
  generate_random_fpfv(date_min, date_lim, FALSE, prev_data)
}

est_fpfv <- function(date_min, date_lim, prev_data, ...) {
  generate_random_fpfv(date_min, date_lim, FALSE, prev_data)
}

est_lplv <- function(date_min, date_lim, prev_data, ...) {
  generate_random_fpfv(date_min, date_lim, FALSE, prev_data)
}

est_lpfv <- function(date_min, date_lim, prev_data, ...) {
  generate_random_fpfv(date_min, date_lim, FALSE, prev_data)
}
db_lock_dt <- function(n, GlobalMaxDate, ...) {
  if (n == 1) {
    unlist(GlobalMaxDate)
  }
  return(rep(GlobalMaxDate, n))
}

studyid <- function(n, stid, ...) {
  # Function body for studyid
  if (n == 1) {
    unlist(stid)
  } else {
    sample(stid, n, replace = TRUE)
  }
}

phase <- function(n, external_phase = NULL, replace = TRUE, ...) {
  if (!is.null(external_phase)) {
    return(sample(external_phase, n, replace = replace))
  }
  # Function body for phase
  return("Blinded Study Drug Completion")
}

nickname <- function(n, ...) {
  # Function body for nickname
  word <- sample(c("OAK", "TREE", "GROOVE"), n, replace = TRUE)
  paste(word, sample(1:100, 1), sep = "-")
}

protocol_title <- function(n, ...) {
  # Function body for protocol_title
  letter <- sample(LETTERS, n, replace = TRUE)
  paste("Protocol Title", letter)
}

status <- function(n, stat = c("Active"), ...) {
  # Function body for status
  sample(stat,
    n,
    replace = TRUE
  )
}

# Status <- function(n, stat = c("Active"), ...) {
#   # Function body for status
#   sample(stat,
#          n,
#          replace = TRUE)
# }

therapeutic_area <- function(n, stat = c("Oncology", "Virology", "Inflammation"), ...) {
  # Function body for TA
  sample(stat,
    n,
    replace = TRUE
  )
}

protocol_indication <- function(n, stat = c("Cardiovascular Health", "Lung Function", "Hematology"), ...) {
  # Function body for protocol_indication
  sample(stat,
    n,
    replace = TRUE
  )
}

product <- function(n, ...) {
  # Function body for product
  num <- sample(1:50,
    n,
    replace = TRUE
  )
  paste("Product Name", num)
}

num_plan_site <- function(num_pl_site, ...) {
  # Function body for num_plan_site
  unlist(num_pl_site) |> as.integer()
}

num_plan_subj <- function(num_pl_subj, ...) {
  # Function body for num_plan_subj
  unlist(num_pl_subj) |> as.integer()
}
