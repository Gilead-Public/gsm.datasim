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

# Mirrors the authoritative `Raw_VS` spec in gsm.mapping's VS.yaml
# (Gilead-Public/gsm.mapping#165). Two things drive the column names below:
#
#   * `Raw_VS` is the RAW extract, so `source_col` entries mean the emitted
#     columns are the source names -- `project`, `foldername`, `bsaentry` --
#     which `rename_raw_data_vars_per_spec()` applies on the way out.
#   * There is no `invid`; site is joined from `Mapped_SUBJ` when `Mapped_VS`
#     is built. Use `attach_vs_site()` in tests that need it.
make_vs_test_spec <- function() {
  list(
    studyid = list(required = TRUE, type = "character", source_col = "project"),
    subjid = list(required = TRUE, type = "character"),
    visit = list(required = TRUE, type = "character", source_col = "foldername"),
    vs_dt = list(required = TRUE, type = "Date"),
    vsperf_std = list(required = TRUE, type = "character"),
    weight = list(required = TRUE, type = "numeric"),
    sysbp = list(required = TRUE, type = "numeric"),
    diabp = list(required = TRUE, type = "numeric")
  )
}

make_vs_full_spec <- function() {
  spec <- make_vs_test_spec()
  spec$height <- list(required = TRUE, type = "numeric")
  spec$bsa <- list(required = TRUE, type = "numeric", source_col = "bsaentry")
  spec$pulse <- list(required = TRUE, type = "numeric")
  spec$temp <- list(required = TRUE, type = "numeric")
  spec$resp <- list(required = TRUE, type = "numeric")
  spec
}

# Emitted (post-rename) vital column names, so `bsa` appears as `bsaentry`.
VS_VITAL_COLS <- c(
  "weight", "height", "bsaentry", "sysbp",
  "diabp", "pulse", "temp", "resp"
)

# The emitted visit column name, per `source_col: foldername`.
VS_VISIT_COL <- "foldername"

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

# `Raw_VS` deliberately carries no `invid` (gsm.mapping#165 joins site on from
# `Mapped_SUBJ` when building `Mapped_VS`). Tests that assert per-site
# behaviour must therefore do that join themselves, exactly as the mapping
# does, rather than reading a column off the raw domain.
attach_vs_site <- function(vs_df, data) {
  vs_df$invid <- data$Raw_SUBJ$invid[match(vs_df$subjid, data$Raw_SUBJ$subjid)]
  vs_df
}
