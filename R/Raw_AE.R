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
  # apply_fatal_ae_scenarios() can carry seeded rows past the next target.
  if (n <= 0) {
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
    aeser = list(n, data$Raw_SUBJ),
    aetoxgr = list(n, data$Raw_SUBJ),
    studyid = list(n, data$Raw_STUDY$protocol_number[[1]]),
    default = list(n, startDate)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_AE, ...)

  return(res)
}

aeser <- function(n, Raw_SUBJ_data = NULL, ...) {
  # Favor serious AEs in hotspot sites so z-score metrics can separate entities.
  row_keys <- if (is.data.frame(Raw_SUBJ_data) && "subjid" %in% names(Raw_SUBJ_data)) {
    sample(Raw_SUBJ_data$subjid, n, replace = TRUE)
  } else {
    NULL
  }
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
  sample(c("term1", "term2"), n, replace = TRUE)
}
mdrsoc_nsv <- function(n, ...) {
  sample(c("soc1", "soc2"), n, replace = TRUE)
}
aetoxgr <- function(n, Raw_SUBJ_data = NULL, ...) {
  row_keys <- if (is.data.frame(Raw_SUBJ_data) && "subjid" %in% names(Raw_SUBJ_data)) {
    sample(Raw_SUBJ_data$subjid, n, replace = TRUE)
  } else {
    NULL
  }
  sample_categorical_with_hotspots(
    values = 1:5,
    n = n,
    base_prob = c(0.45, 0.30, 0.15, 0.07, 0.03),
    outlier_idx = 4:5,
    row_keys = row_keys,
    key_map = Raw_SUBJ_data,
    key_col = "subjid",
    site_col = "invid"
  )
}

aest_dt_aeen_dt <- function(n, startDate, endDate, ...) {
  aest_dat <- aest_dt(n, startDate, endDate, ...)
  aeen_dat <- aeen_dt(n, aest_dat, ...)
  return(list(
    aest_dt = aest_dat,
    aeen_dt = aeen_dat
  ))
}

# Baseline outcomes are never "Fatal": fatal AEs come only from the
# deterministic death-linked cases in apply_fatal_ae_scenarios().
aeout <- function(n, ...) {
  sample(
    c(
      "Recovered/Resolved",
      "Recovering/Resolving",
      "Not Recovered/Not Resolved",
      "Recovered/Resolved with Sequelae"
    ),
    n,
    replace = TRUE,
    prob = c(0.60, 0.20, 0.15, 0.05)
  )
}

aereldrv <- function(n, ...) {
  sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.3, 0.7))
}

aesdth <- function(n, ...) {
  rep("No", n)
}

# MedDRA terms reserved for seeded fatal AEs. Baseline terms never use them, so
# they double as the marker that keeps apply_fatal_ae_scenarios() idempotent.
fatal_ae_terms <- c(
  "Cardiac arrest" = "Cardiac disorders",
  "Sepsis" = "Infections and infestations",
  "Respiratory failure" = "Respiratory, thoracic and mediastinal disorders"
)

#' Seed deterministic fatal-AE cases linked to Raw_Death
#'
#' Gives the premature-death report (gsm.kri#304) a known mix of qualifying
#' and non-qualifying fatal AEs. Death subjects, in `Raw_Death` row order, cycle
#' through: one related fatal AE; two fatal AEs (related and not related); no
#' fatal AE; one not-related fatal AE; one grade-5 AE with missing outcome and
#' relatedness. The first AE row of a subject without a death record also
#' becomes fatal, so the report can prove it filters to the death population.
#'
#' Random fatal markers on death subjects are cleared first so each case is
#' exact. Rows seeded on an earlier snapshot are rebuilt rather than duplicated.
#'
#' @param Raw_AE A generated `Raw_AE` frame.
#' @param Raw_Death A generated `Raw_Death` frame with `subjid` and `death_dt`.
#' @returns `Raw_AE` with the scenario rows, or unchanged when either frame is
#'   empty or lacks the fatal-AE columns.
#' @keywords internal
#' @noRd
apply_fatal_ae_scenarios <- function(Raw_AE, Raw_Death) {
  ae_cols <- c("subjid", "aetoxgr", "aeout", "aereldrv", "aesdth", "mdrpt_nsv")
  if (
    is.null(Raw_AE) || nrow(Raw_AE) == 0 || !all(ae_cols %in% names(Raw_AE)) ||
      is.null(Raw_Death) || nrow(Raw_Death) == 0 ||
      !all(c("subjid", "death_dt") %in% names(Raw_Death))
  ) {
    return(Raw_AE)
  }

  grade <- function(value) {
    if (is.character(Raw_AE$aetoxgr)) as.character(value) else as.integer(value)
  }

  template <- Raw_AE[1, , drop = FALSE]
  ae <- Raw_AE[!Raw_AE$mdrpt_nsv %in% names(fatal_ae_terms), , drop = FALSE]

  deaths <- Raw_Death[!duplicated(Raw_Death$subjid), , drop = FALSE]
  death_ids <- as.character(deaths$subjid)

  on_death <- as.character(ae$subjid) %in% death_ids
  ae$aetoxgr[on_death & as.character(ae$aetoxgr) %in% "5"] <- grade(4)
  ae$aeout[on_death & ae$aeout %in% "Fatal"] <- "Not Recovered/Not Resolved"
  ae$aesdth[on_death] <- "No"

  other <- which(!on_death)[1]
  if (!is.na(other)) {
    ae$aetoxgr[other] <- grade(5)
    ae$aeout[other] <- "Fatal"
    ae$aesdth[other] <- "Yes"
    ae$aereldrv[other] <- "Yes"
  }

  # Relatedness per seeded row; the list position is the scenario.
  scenarios <- list("Yes", c("Yes", "No"), character(0), "No", NA_character_)
  rel <- scenarios[(seq_along(death_ids) - 1L) %% length(scenarios) + 1L]
  n_rows <- lengths(rel)
  if (sum(n_rows) == 0) {
    return(ae)
  }
  rel <- unlist(rel)
  row_death <- rep(seq_along(death_ids), n_rows)
  death_dt <- as.Date(deaths$death_dt[row_death])
  term <- names(fatal_ae_terms)[(seq_along(rel) - 1L) %% length(fatal_ae_terms) + 1L]

  seeded <- template[rep(1L, length(rel)), , drop = FALSE]
  values <- list(
    subjid = death_ids[row_death],
    studyid = if ("studyid" %in% names(deaths)) deaths$studyid[row_death],
    aest_dt = death_dt - 2,
    aeen_dt = death_dt,
    mdrpt_nsv = term,
    mdrsoc_nsv = unname(fatal_ae_terms[term]),
    aeser = "Y",
    aeongo = "N",
    aetoxgr = grade(5),
    aereldrv = rel,
    aerel = unname(c(Yes = "Y", No = "N")[rel]),
    # The missing-relatedness case qualifies on grade alone.
    aeout = ifelse(is.na(rel), NA_character_, "Fatal"),
    aesdth = ifelse(is.na(rel), NA_character_, "Yes")
  )
  for (col in intersect(names(Filter(Negate(is.null), values)), names(seeded))) {
    seeded[[col]] <- values[[col]]
  }

  res <- rbind(ae, seeded)
  rownames(res) <- NULL
  res
}
