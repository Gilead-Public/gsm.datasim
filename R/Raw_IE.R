#' Generate Raw IE Data
#'
#' Generate Raw IE based on `IE.yaml` from `gsm.mapping`.
#'
#' @inheritParams Raw_STUDY
#' @returns a data.frame pertaining to the raw dataset plugged into `IE.yaml`
#' @family internal
#' @keywords internal
#' @noRd
Raw_IE <- function(data, previous_data, spec, ...) {
  inps <- list(...)

  curr_spec <- spec$Raw_IE

  if ("Raw_IE" %in% names(previous_data)) {
    dataset <- previous_data$Raw_IE
    previous_row_num <- nrow(dataset)
  } else {
    dataset <- NULL
    previous_row_num <- 0
  }

  n <- inps$n_IE - previous_row_num
  if (n == 0) {
    return(dataset)
  }

  if (all(c("subjid") %in% names(curr_spec))) {
    curr_spec$subject_to_ie <- list(required = TRUE)
    curr_spec$subjid <- NULL
  }
  if (all(c("tiver", "ietestcd", "ietest", "ieorres", "iecat") %in% names(curr_spec))) {
    curr_spec$tiver_ietestcd_ietest_ieorres_iecat <- list(required = TRUE)
    curr_spec$tiver <- NULL
    curr_spec$ietestcd <- NULL
    curr_spec$ietest <- NULL
    curr_spec$ieorres <- NULL
    curr_spec$iecat <- NULL
  }

  args <- list(
    studyid = list(n, data$Raw_STUDY$protocol_number[[1]]),
    subject_to_ie = list(n, data, previous_data$Raw_IE$subjid),
    tiver_ietestcd_ietest_ieorres_iecat = list(n, ...),
    default = list(n)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_IE, ...)

  return(res)
}

subject_to_ie <- function(n, data, previous_data, ...) {
  if (length(previous_data) != 0) {
    data_pool <- data$Raw_SUBJ[!(data$Raw_SUBJ$subjid %in% previous_data), ]
  } else {
    data_pool <- data$Raw_SUBJ
  }

  res <- data_pool %>%
    # filter(enrollyn == "N") %>%
    select(subjid)
  return(res)
}

tiver_ietestcd_ietest_ieorres_iecat <- function(n, iecode, ...) {
  tiver_dat <- tiver(n, ...)
  ietestcd_dat <- ietestcd(n, ...)
  ietest_dat <- ietest(n, iecode = ietestcd_dat, ...)
  ieorres_dat <- ieorres(n, iecode = ietestcd_dat, ...)
  iecat_dat <- iecat(n, iecode = ietestcd_dat, ...)

  return(list(
    tiver = tiver_dat,
    ietestcd = ietestcd_dat,
    ietest = ietest_dat,
    ieorres = ieorres_dat,
    iecat = iecat_dat
  ))
}

tiver <- function(n, ...) {
  return(rep("A1", n))
}
ietestcd <- function(n, ...) {
  return(sample(c(paste0("incl", 1:10), paste0("excl", 1:10)), n, replace = TRUE))
}
ietest <- function(n, iecode, ...) {
  stringr::str_replace_all(iecode, c("^INC" = "Inclusion ", "^EXC" = "Exclusion "))
}
ieorres <- function(n, iecode, ...) {
  sample(c("Yes", "No"), n, replace = TRUE)
}
iecat <- function(n, iecode, ...) {
  ifelse(stringr::str_detect(iecode, "INC"), "Inclusion", "Exclusion")
}
