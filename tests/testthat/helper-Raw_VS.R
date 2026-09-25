# Local reimplementation of the consecutive-repeat rolling-window rule.
#
# `Detect_ConsecutiveRepeats()` (gsm.kri#307) does not exist yet, so every
# assertion about metric values in this package's tests must compute the rate
# locally rather than calling downstream. Keep this deliberately naive and
# independent of the package implementation -- it is the check on
# `inject_targeted_runs()`, not a second copy of it.
#
# Convention (see #143): missing values are DROPPED first, then windows are
# formed over what remains. A run may therefore span a position that held an
# `NA`.
count_consecutive_repeat_windows <- function(values, nWindowLength = 3) {
  vals <- values[!is.na(values)]
  n_windows <- length(vals) - nWindowLength + 1
  if (n_windows < 1) {
    return(list(numerator = 0, denominator = 0))
  }

  numerator <- sum(vapply(
    seq_len(n_windows),
    function(i) {
      window <- vals[i:(i + nWindowLength - 1)]
      all(window == window[1])
    },
    logical(1)
  ))

  list(numerator = numerator, denominator = n_windows)
}

# Site-level rate: sum subject numerators and denominators, then divide.
# Mirrors how `Input_Rate` aggregates to the group level.
site_repeat_rate <- function(df, strValueCol, strGroupCol = "invid",
                             strSubjectCol = "subjid", nWindowLength = 3) {
  by_site <- split(df, df[[strGroupCol]])

  rates <- lapply(by_site, function(site_df) {
    by_subject <- split(site_df[[strValueCol]], site_df[[strSubjectCol]])
    counts <- lapply(by_subject, count_consecutive_repeat_windows, nWindowLength = nWindowLength)
    num <- sum(vapply(counts, function(x) x$numerator, numeric(1)))
    den <- sum(vapply(counts, function(x) x$denominator, numeric(1)))
    data.frame(
      numerator = num,
      denominator = den,
      rate = if (den > 0) num / den else NA_real_
    )
  })

  out <- do.call(rbind, rates)
  out[[strGroupCol]] <- names(rates)
  rownames(out) <- NULL
  out[, c(strGroupCol, "numerator", "denominator", "rate")]
}

make_vs_test_spec <- function() {
  list(
    subjid = list(required = TRUE),
    invid = list(required = TRUE),
    studyid = list(required = TRUE),
    instancename = list(required = TRUE),
    vs_dt = list(required = TRUE),
    vsperf_std = list(required = TRUE),
    weight = list(required = TRUE),
    sysbp = list(required = TRUE),
    diabp = list(required = TRUE)
  )
}

make_vs_full_spec <- function() {
  spec <- make_vs_test_spec()
  spec$height <- list(required = TRUE)
  spec$bmi <- list(required = TRUE)
  spec$pulse <- list(required = TRUE)
  spec$temp <- list(required = TRUE)
  spec$resp <- list(required = TRUE)
  spec
}

VS_VITAL_COLS <- c(
  "weight", "height", "bmi", "sysbp",
  "diabp", "pulse", "temp", "resp"
)

# `Raw_VISIT` now matters to `Raw_VS` generation: `vs_dt` is taken from
# `visit_dt` rather than being a constant, so the test fixture must carry a
# real per-visit schedule.
# `strDateClass` mirrors the real `Raw_VISIT` generator, which emits
# "%Y-%m-%d" character dates rather than `Date`s (see `visit_dt()` in
# R/Raw_VISIT.R). Tests use "character" to exercise the production path.
make_vs_test_data <- function(n_subjects = 20, n_visits = 6, n_sites = 3,
                              start_date = as.Date("2012-01-01"),
                              strDateClass = c("Date", "character")) {
  strDateClass <- match.arg(strDateClass)
  subjid <- sprintf("S%04d", seq_len(n_subjects))
  invid <- sprintf("0X%04d", (seq_len(n_subjects) %% n_sites) + 1)

  visits <- c("Screening", paste0("VISIT ", seq_len(max(n_visits - 1, 1))))[seq_len(n_visits)]
  visit_dates <- start_date + seq(0, by = 28, length.out = n_visits)
  if (strDateClass == "character") {
    visit_dates <- format(visit_dates, "%Y-%m-%d")
  }

  raw_visit <- do.call(
    rbind,
    lapply(subjid, function(s) {
      data.frame(
        subjid = s,
        instancename = visits,
        visit_dt = visit_dates,
        stringsAsFactors = FALSE
      )
    })
  )

  list(
    Raw_SUBJ = data.frame(
      subjid = subjid,
      invid = invid,
      stringsAsFactors = FALSE
    ),
    Raw_STUDY = data.frame(protocol_number = "PROT-VS"),
    Raw_VISIT = raw_visit
  )
}

make_vs_context <- function(data, spec = make_vs_test_spec(), n = NULL,
                            vs_risk_profile = NULL,
                            start_date = as.Date("2012-01-01")) {
  list(
    data = data,
    previous_data = list(),
    combined_specs = list(Raw_VS = spec),
    n = n %||% nrow(data$Raw_SUBJ),
    start_date = start_date,
    vs_risk_profile = vs_risk_profile
  )
}
