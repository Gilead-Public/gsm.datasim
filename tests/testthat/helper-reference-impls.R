# Independent, deliberately naive reference implementations used as oracles
# in tests. They must stay separate from the package code they check.

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

# Independent, deliberately naive recount of all-identical rolling windows.
# Kept separate from the implementation so it can check it rather than echo it.
count_identical_windows_naive <- function(values, groups, window_length = 3) {
  total <- 0
  for (grp in unique(groups)) {
    v <- values[groups == grp]
    v <- v[!is.na(v)]
    if (length(v) < window_length) next
    for (i in seq_len(length(v) - window_length + 1)) {
      window <- v[i:(i + window_length - 1)]
      if (all(window == window[1])) total <- total + 1
    }
  }
  total
}
