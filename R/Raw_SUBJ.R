#' Generate Raw SUBJ Data
#'
#' Generate Raw SUBJ Data based on `SUBJ.yaml` from `gsm.mapping`.
#'
#' @inheritParams Raw_STUDY
#' @param startDate The beginning of dates to which the subjects can be enrolled/started
#' @param endDate The end of dates to which the subjects completed/leave study.
#'
#' @returns a data.frame pertaining to the raw dataset plugged into `SUBJ.yaml`
#' @family internal
#' @keywords internal
#' @noRd
Raw_SUBJ <- function(data, previous_data, spec, startDate, endDate, ...) {
  inps <- list(...)

  curr_spec <- spec$Raw_SUBJ

  if ("Raw_SUBJ" %in% names(previous_data)) {
    dataset <- previous_data$Raw_SUBJ
    previous_row_num <- nrow(dataset)
  } else {
    dataset <- NULL
    previous_row_num <- 0
  }

  n <- inps$n_subj - previous_row_num
  if (n == 0) {
    return(dataset)
  }

  if (!("enrolldt" %in% names(curr_spec))) {
    curr_spec$enrolldt <- list(required = TRUE)
  }

  if (all(c("invid", "country") %in% names(curr_spec))) {
    curr_spec$subject_site_synq <- list(required = TRUE)
    curr_spec$invid <- NULL
    curr_spec$country <- NULL
  }

  if (all(c("subjid", "subject_nsv") %in% names(curr_spec))) {
    curr_spec$subjid_subject_nsv <- list(required = TRUE)
    curr_spec$subjid <- NULL
    curr_spec$subject_nsv <- NULL
  }

  if (all(c("enrollyn", "enrolldt", "timeonstudy", "firstparticipantdate", "firstdosedate", "timeontreatment") %in% names(curr_spec))) {
    curr_spec$enrollyn_enrolldt_timeonstudy_firstparticipantdate_firstdosedate_timeontreatment <- list(required = TRUE)
    curr_spec$enrolldt <- NULL
    curr_spec$timeonstudy <- NULL
    curr_spec$enrollyn <- NULL

    curr_spec$firstparticipantdate <- NULL
    curr_spec$firstdosedate <- NULL
    curr_spec$timeontreatment <- NULL
  }

  args <- list(
    studyid = list(n, data$Raw_STUDY$protocol_number[[1]]),
    subjid_subject_nsv = list(n, previous_data$Raw_SUBJ$subjid),
    subject_site_synq = list(n, data$Raw_SITE),
    enrollyn_enrolldt_timeonstudy_firstparticipantdate_firstdosedate_timeontreatment = list(n, startDate, endDate),
    default = list(n, startDate)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_SUBJ, ...)

  # Recalculate for all data
  res$timeonstudy <- timeonstudy(n, res$enrolldt, endDate)
  res$timeontreatment <- as.integer(as.Date(endDate) - res$firstdosedate)

  return(res)
}

subjid <- function(n, external_subjid = NULL, replace = TRUE, previous_subjid = NULL, ...) {
  args <- list(...)

  if (!is.null(external_subjid)) {
    return(sample(external_subjid, n, replace = replace))
  }

  # Generate all possible 3-digit numbers as strings with leading zeros
  possible_numbers <- sprintf("%03d", 0:99999)

  # Create all possible strings starting with "0X" and ending with the 3-digit numbers
  possible_strings <- paste0("S", possible_numbers)

  # Exclude the old strings to avoid duplication
  new_strings_available <- setdiff(possible_strings, previous_subjid)

  # Check if there are enough unique strings to generate
  if (length(new_strings_available) < n) {
    stop("Not enough unique strings available to generate ", n, " new strings.")
  }
  res <- sample(new_strings_available, n, replace = FALSE)

  # Randomly sample 'n' unique strings from the available strings
  return(res)
}

subjid_subject_nsv <- function(n, dataset, ...) {
  subjid_dat <- subjid(n, previous_subjid = dataset, ...)
  subject_nsv_dat <- subject_nsv(n, subjid_dat, ...)
  return(list(
    subjid = subjid_dat,
    subject_nsv = subject_nsv_dat
  ))
}

subject_site_synq <- function(n, Raw_SITE_data, ...) {
  Raw_SITE_data[
    sample(nrow(Raw_SITE_data), n, replace = TRUE),
    c("pi_number", "country")
  ] %>%
    dplyr::rename("invid" = "pi_number")
}

enrollyn <- function(n, ...) {
  # Function body for enrollyn
  # if (isSubjDataset) {
  #   return("Y")
  # } else {
  #   return("N")
  # }
  sample(c("Y", "N"),
    prob = c(0.75, 0.25),
    n,
    replace = TRUE
  )
}

subject_nsv <- function(n, subjid, subject_nsv = NULL, replace = TRUE, ...) {
  # Function body for subject_nsv
  if (!is.null(subject_nsv)) {
    return(sample(subject_nsv, n, replace = replace))
  }
  return(paste0(subjid, "-XXXX"))
}

enrolldt <- function(n, startDate, endDate, enrollyn_dat, ...) {
  full_sample <- sample(seq(as.Date(startDate), as.Date(endDate), by = "day"), n, replace = TRUE)
  full_sample[enrollyn_dat == "N"] <- NA
  return(full_sample)
}


timeonstudy <- function(n, enrolldt, endDate, ...) {
  # Function body for timeonstudy
  as.numeric(as.Date(endDate) - as.Date(enrolldt)) %>% as.integer()
}

agerep <- function(n, ...) {
  sample(18:55, n, replace = T)
}

sex <- function(n, ...) {
  sample(c("M", "F"), n, replace = T)
}
race <- function(n, ...) {
  sample(c("White", "Asian", "Black", "Other"), n, replace = T)
}
#' Derive the upstream IP non-starter contract fields
#'
#' Impersonates the Stride derivation so simulated data carries the same seven
#' fields production data will. gsm never computes these outside the simulator.
#'
#' Runs over the whole frame on every snapshot, so days lapsed re-accrue and an
#' undosed subject advances from within- to outside-window as time passes.
#' Confirmed status is a deterministic function of `subjid` rather than a draw,
#' so it cannot flip back on a later snapshot.
#'
#' @param df a generated `Raw_SUBJ` frame carrying `subjid`, `enrollyn`,
#'   `enrolldt`, `firstdosedate`.
#' @param endDate the snapshot date, acting as "today".
#' @param nWindowDays days separating the two potential statuses.
#' @param nConfirmedShare share of never-dosed subjects that are Confirmed.
#' @param nKitAssignedShare share of never-dosed subjects with a kit assigned.
#' @returns `df` with the seven `drv_*` columns.
#' @keywords internal
#' @noRd
apply_ipns_derivations <- function(
  df,
  endDate,
  nWindowDays = 30,
  nConfirmedShare = 0.4,
  nKitAssignedShare = 0.5
) {
  if (is.null(df) || nrow(df) == 0 || !("subjid" %in% names(df))) {
    return(df)
  }

  enrolled <- df$enrollyn %in% "Y"
  dosed <- enrolled & !is.na(df$firstdosedate)
  undosed <- as.character(df$subjid) %in% nonstarter_subjids(df)

  df$drv_enrollment_dt <- dplyr::if_else(enrolled, as.Date(df$enrolldt), as.Date(NA))
  df$drv_ip_dosed <- ifelse(enrolled, ifelse(dosed, "Y", "N"), NA_character_)
  df$drv_ip_first_dose_dt <- dplyr::if_else(dosed, as.Date(df$firstdosedate), as.Date(NA))
  df$drv_enrl_first_dose_days <- ifelse(
    dosed,
    as.integer(df$firstdosedate - df$enrolldt) + 1L,
    NA_integer_
  )
  df$drv_days_lapsed_since_enrl <- ifelse(
    undosed,
    as.integer(as.Date(endDate) - df$enrolldt) + 1L,
    NA_integer_
  )

  # subjid() draws each subject's number uniformly at random, so its last two
  # digits bucket subjects faithfully and stay stable across snapshots.
  id <- as.integer(sub("^S", "", df$subjid))
  bucket <- id %% 100L
  confirmed <- undosed & bucket < round(nConfirmedShare * 100)

  df$drv_ip_nonstarter_status <- dplyr::case_when(
    !enrolled ~ NA_character_,
    dosed ~ "Dosed",
    confirmed ~ "Confirmed Non-Starter",
    df$drv_days_lapsed_since_enrl > nWindowDays ~
      "Potential Non-Starter outside window",
    TRUE ~ "Potential Non-Starter within window"
  )

  # Dosing implies a kit. For the rest, the hundreds and thousands digits keep
  # kit assignment independent of the Confirmed bucket.
  df$drv_kit_assigned <- dplyr::case_when(
    !enrolled ~ NA_character_,
    dosed | (id %/% 100L) %% 100L < round(nKitAssignedShare * 100) ~ "Y",
    TRUE ~ "N"
  )

  df
}

# Named after the spec column so add_new_var_data() calls it instead of the
# RNG-consuming type fallback; apply_ipns_derivations() fills in the value.
drv_kit_assigned <- function(n, ...) rep(NA_character_, n)

enrollyn_enrolldt_timeonstudy_firstparticipantdate_firstdosedate_timeontreatment <- function(n, startDate, endDate, nonstarter_rate = 0.1, ...) {
  enrollyn_dat <- enrollyn(n, ...)
  enrolldt_dat <- enrolldt(n, startDate, endDate, enrollyn_dat, ...)
  timeonstudy_dat <- timeonstudy(n, enrolldt_dat, endDate, ...)

  firstparticipantdate_dat <- enrolldt_dat
  firstdosedate_dat <- pmin(
    enrolldt_dat + sample(0:14, n, replace = TRUE),
    as.Date(endDate)
  )

  # IP non-starter scenario (#122): a deterministic subset of enrolled subjects
  # are enrolled but never dosed, so their firstdosedate is NA. Drawn after the
  # existing draws above, so with nonstarter_rate = 0 the other columns are
  # left unchanged.
  enrolled_idx <- which(enrollyn_dat == "Y")
  if (nonstarter_rate > 0 && length(enrolled_idx) > 0) {
    k <- min(max(1L, round(nonstarter_rate * length(enrolled_idx))), length(enrolled_idx))
    nonstarter_idx <- sample(enrolled_idx, size = k, replace = FALSE)
    firstdosedate_dat[nonstarter_idx] <- as.Date(NA)
  }
  timeontreatment_dat <- as.integer(as.Date(endDate) - firstdosedate_dat)

  return(list(
    enrollyn = enrollyn_dat,
    enrolldt = enrolldt_dat,
    timeonstudy = timeonstudy_dat,
    firstparticipantdate = firstparticipantdate_dat,
    firstdosedate = firstdosedate_dat,
    timeontreatment = timeontreatment_dat
  ))
}

# Reasons must not include drug names or phases.
ptd_reason_values <- c(
  "Adverse Event",
  "Lack of Efficacy",
  "Physician Decision",
  "Withdrawal by Subject",
  "Protocol Deviation",
  "Progressive Disease",
  "Lost to Follow-up"
)

#' Derive premature treatment discontinuation fields
#'
#' Uses subject IDs and first-dose dates without RNG draws. With unchanged
#' inputs and settings, dates and reasons remain stable across snapshots.
#'
#' @param df A `Raw_SUBJ` frame after [apply_ipns_derivations()].
#' @param endDate Snapshot date; future discontinuation dates remain `NA`.
#' @param nDiscontinuedShare Target share of dosed subjects selected to discontinue.
#' @returns `df` with discontinuation date, reason and inclusive days
#'   from enrollment to discontinuation.
#' @keywords internal
#' @noRd
apply_ptd_derivations <- function(df, endDate, nDiscontinuedShare = 0.3) {
  if (is.null(df) || nrow(df) == 0 || !("drv_ip_dosed" %in% names(df))) {
    return(df)
  }

  k <- as.integer(sub("^S", "", df$subjid))
  dosed <- df$drv_ip_dosed %in% "Y"
  discontinuing <- dosed & (k %/% 100L) %% 100L < round(nDiscontinuedShare * 100)
  lag <- ifelse(k %% 10L < 3L, 0L, 1L + (k %/% 10L) %% 27L)
  planned <- df$drv_ip_first_dose_dt + lag
  dated <- discontinuing & planned <= as.Date(endDate)

  first <- ptd_reason_values[k %% 7L + 1L]
  second <- ptd_reason_values[(k %% 7L + 1L + (k %/% 7L) %% 6L) %% 7L + 1L]
  shape <- (k %/% 10L) %% 100L
  reason <- dplyr::case_when(
    shape < 10L ~ NA_character_,
    shape < 25L ~ paste(first, second, sep = ", "),
    TRUE ~ first
  )

  df$drv_treatment_discontinuation_dt <- dplyr::if_else(dated, planned, as.Date(NA))
  # A reason without a date is a real delivery shape; the metric ignores it.
  df$drv_premature_discontinuation_reason <- dplyr::case_when(
    dated ~ reason,
    dosed & !discontinuing & k %% 100L >= 97L ~ first,
    TRUE ~ NA_character_
  )
  df$drv_days_lapsed_enrl_discontinuation <- ifelse(
    dated,
    as.integer(df$drv_treatment_discontinuation_dt - df$drv_enrollment_dt) + 1L,
    NA_integer_
  )

  df
}

# Named after the spec columns so add_new_var_data() skips the RNG-consuming
# type fallback; apply_ptd_derivations() fills in the values.
drv_treatment_discontinuation_dt <- function(n, ...) rep(as.Date(NA), n)
drv_premature_discontinuation_reason <- function(n, ...) rep(NA_character_, n)
drv_days_lapsed_enrl_discontinuation <- function(n, ...) rep(NA_integer_, n)
