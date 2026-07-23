Raw_Death <- function(data, previous_data, spec, startDate, ...) {
  inps <- list(...)

  curr_spec <- spec$Raw_Death

  if ("Raw_Death" %in% names(previous_data)) {
    dataset <- previous_data$Raw_Death
    previous_row_num <- nrow(dataset)
  } else {
    dataset <- NULL
    previous_row_num <- 0
  }

  n <- inps$n - previous_row_num
  if (n == 0) {
    return(dataset)
  }

  if (all(c("death_dt") %in% names(curr_spec))) {
    curr_spec$death_dt <- list(required = TRUE)
  }

  args <- list(
    subjid = list(n, external_subjid = data$Raw_SUBJ$subjid, replace = FALSE),
    studyid = list(n, data$Raw_STUDY$protocol_number[[1]]),
    death_dt = list(n, startDate),
    deathcls = list(n),
    default = list(n)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_Death, ...)

  return(res)
}

death_dt <- function(n, start_date, ...) {
  as.Date(sample(seq(from = start_date, to = start_date + 27, by = 1), n, replace = TRUE))
}

deathcls <- function(n, ...) {
  sample(
    c("Progressive Disease", "Adverse Event", "Disease Recurrence",
      "Not related to disease",
      "Related to long-term follow-up and not related to study drug"),
    size = n,
    prob = c(0.40, 0.25, 0.15, 0.10, 0.10),
    replace = TRUE
  )
}
