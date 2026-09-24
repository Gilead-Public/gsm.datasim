#' Longitudinal data helpers
#'
#' Domain-neutral helpers for shaping longitudinal (subject x visit) data so it
#' can exercise sequence-based metrics such as the repeat-measure KRIs
#' (`kri0020a-f`). They operate on plain vectors and data frames, and know
#' nothing about vitals, sites, or any particular `Raw_*` domain.
#'
#' The intended composition is **sort first, then inject**:
#' `assign_schedule_dates()` puts records in the order the downstream metric
#' will use, and `inject_targeted_runs()` then writes runs over contiguous
#' positions. Reversing that order silently produces runs that are adjacent in
#' row order but not in time, which the metric will not count.
#'
#' @name utils-longitudinal
#' @keywords internal
NULL


#' Count rolling windows available in a sequence of measurements
#'
#' The denominator of the consecutive-repeat rate: a sequence of `V`
#' measurements admits `V - W + 1` rolling windows of length `W`, or none when
#' `V < W`.
#'
#' `nMeasurements` should count **non-missing** measurements only. Missing
#' records are dropped before windows are formed (see `inject_targeted_runs()`),
#' so they neither contribute to the denominator nor interrupt a run.
#'
#' @param nMeasurements Numeric vector of measurement counts.
#' @param nWindowLength Whole number `>= 2`. Rolling window length; the
#'   repeat-measure KRIs default to `3`.
#'
#' @returns Numeric vector the same length as `nMeasurements`, giving the
#'   number of windows available for each count.
#'
#' @keywords internal
count_repeat_windows <- function(nMeasurements, nWindowLength = 3) {
  stopifnot("`nMeasurements` must be numeric" = is.numeric(nMeasurements))
  .validate_window_length(nWindowLength)

  pmax(nMeasurements - nWindowLength + 1, 0)
}


#' Assign each site a risk band
#'
#' Partitions sites into `"red"`, `"amber"`, and `"normal"` bands by sampling
#' without replacement. Used to decide which sites should be given an elevated
#' consecutive-repeat rate before `inject_targeted_runs()` constructs it.
#'
#' Supply either percentages (`dPctRed` / `dPctAmber`) or explicit counts
#' (`nRed` / `nAmber`); counts take precedence when both are given.
#' Percentages are converted with [round()], so small site counts degrade
#' gracefully rather than erroring -- 10% of 3 sites is 0 red sites, not a
#' fractional one.
#'
#' @param vSites Character vector of site identifiers. Duplicates are ignored;
#'   each distinct site receives one band.
#' @param dPctRed,dPctAmber Proportions in `[0, 1]` of sites to place in the
#'   red and amber bands.
#' @param nRed,nAmber Optional explicit site counts, overriding the
#'   corresponding percentage.
#'
#' @returns Named character vector, one element per distinct site, with values
#'   `"red"`, `"amber"`, or `"normal"`.
#'
#' @keywords internal
allocate_site_risk <- function(vSites, dPctRed = 0.1, dPctAmber = 0.2,
                               nRed = NULL, nAmber = NULL) {
  sites <- unique(as.character(vSites))
  n_sites <- length(sites)
  if (n_sites == 0) {
    return(stats::setNames(character(0), character(0)))
  }

  n_red <- if (!is.null(nRed)) {
    .validate_count(nRed, "nRed")
  } else {
    .validate_proportion(dPctRed, "dPctRed")
    round(dPctRed * n_sites)
  }
  n_amber <- if (!is.null(nAmber)) {
    .validate_count(nAmber, "nAmber")
  } else {
    .validate_proportion(dPctAmber, "dPctAmber")
    round(dPctAmber * n_sites)
  }

  if (n_red + n_amber > n_sites) {
    stop(
      "Cannot allocate ", n_red, " red and ", n_amber, " amber bands across ",
      n_sites, " site(s): red + amber must not exceed the number of sites"
    )
  }

  shuffled <- if (n_sites == 1) sites else sample(sites)
  bands <- stats::setNames(rep("normal", n_sites), shuffled)

  if (n_red > 0) bands[seq_len(n_red)] <- "red"
  if (n_amber > 0) bands[n_red + seq_len(n_amber)] <- "amber"

  bands[sites]
}


#' Inject consecutive runs to hit a target repeat rate
#'
#' Overwrites contiguous blocks of `values` within each group so that the
#' proportion of length-`nWindowLength` rolling windows whose values are all
#' identical is approximately `dTargetRate`. The construction is deterministic
#' given the group sizes -- the rate is *built*, not sampled.
#'
#' Rows must be **already ordered within group** by the key the downstream
#' metric will order on; this function injects over contiguous positions and
#' does not sort. `NA` values are never overwritten and are excluded from the
#' window counts. Small groups quantize, since the target numerator is
#' `round(dTargetRate * D)`.
#'
#' @param values Numeric vector of measurements, ordered within group.
#' @param groups Vector the same length as `values` identifying each value's
#'   group (typically subject).
#' @param dTargetRate Target repeat rate in `[0, 1]`.
#' @param nWindowLength Whole number `>= 2`. Rolling window length.
#'
#' @returns `values` with runs injected, carrying a `"realized"` attribute: a
#'   list with the achieved `numerator` and `denominator`.
#'
#' @keywords internal
inject_targeted_runs <- function(values, groups, dTargetRate, nWindowLength = 3) {
  # Ordering is the caller's responsibility (see `assign_schedule_dates()`):
  # runs are written over contiguous positions, so sorting afterwards would
  # scatter them and silently break the rate guarantee.
  stopifnot(
    "`values` and `groups` must be the same length" =
      length(values) == length(groups)
  )
  .validate_window_length(nWindowLength)
  .validate_proportion(dTargetRate, "dTargetRate")

  # Index vector per group, restricted to non-missing values and preserving
  # the caller's within-group order. Per the convention set in #143, missing
  # records are DROPPED before windows form rather than acting as a barrier,
  # so a run may span a position that held an NA. This mirrors the downstream
  # metric, which is what keeps the constructed rate equal to the computed one.
  idx_by_group <- split(seq_along(values), groups)
  idx_by_group <- lapply(idx_by_group, function(idx) idx[!is.na(values[idx])])

  window_counts <- count_repeat_windows(
    vapply(idx_by_group, length, integer(1)),
    nWindowLength = nWindowLength
  )
  total_windows <- sum(window_counts)

  if (total_windows == 0) {
    return(.with_realized(values, 0, 0))
  }

  # `dTargetRate` is validated into [0, 1], so this never exceeds
  # `total_windows` -- which is also the loop's total capacity below, meaning
  # the loop always delivers the full numerator.
  target_numerator <- round(dTargetRate * total_windows)

  if (target_numerator == 0) {
    return(.with_realized(values, 0, total_windows))
  }

  # Shuffle group order so injected runs aren't concentrated in the
  # lowest-numbered subjects.
  eligible <- names(window_counts)[window_counts >= 1]
  if (length(eligible) > 1) eligible <- sample(eligible)

  remaining <- target_numerator
  achieved <- 0

  for (grp in eligible) {
    if (remaining <= 0) break

    idx <- idx_by_group[[grp]]
    capacity <- window_counts[[grp]]

    # A run of length `nWindowLength + k` contributes `k + 1` windows, and a
    # group's whole capacity is consumed by a single run covering it.
    take <- min(remaining, capacity)
    run_length <- nWindowLength + take - 1
    run_idx <- idx[seq_len(run_length)]

    # The run takes the value of the block it overwrites.
    values[run_idx] <- values[run_idx[1]]

    remaining <- remaining - take
    achieved <- achieved + take
  }

  .with_realized(values, achieved, total_windows)
}


#' Attach realized repeat counts to an injected value vector
#' @noRd
.with_realized <- function(values, numerator, denominator) {
  attr(values, "realized") <- list(
    numerator = as.numeric(numerator),
    denominator = as.numeric(denominator)
  )
  values
}


#' Join visit-schedule dates onto a subject-visit frame and sort
#'
#' Brings the authoritative visit schedule (`Raw_VISIT$visit_dt`) onto a
#' subject-visit frame and returns it sorted by group then date, so that
#' "adjacent rows" and "adjacent in time" mean the same thing. Without this,
#' run adjacency is undefined.
#'
#' Call this **before** `inject_targeted_runs()`; see the ordering contract
#' documented there.
#'
#' @param df Data frame of subject-visit records to date.
#' @param visits Data frame carrying the visit schedule, with the group and
#'   visit columns plus `visit_dt`.
#' @param strDateCol Name for the date column in the returned frame -- e.g.
#'   `"vs_dt"` for `Raw_VS`, `"lb_dt"` for a future `Raw_LB` adopter.
#' @param strGroupCol,strVisitCol Column names identifying the subject and the
#'   visit.
#'
#' @returns `df` with the date column added, sorted by group then date. The
#'   sort is stable, so repeated records within a visit keep a deterministic
#'   order.
#'
#' @keywords internal
assign_schedule_dates <- function(df, visits, strDateCol,
                                  strGroupCol = "subjid",
                                  strVisitCol = "instancename") {
  stopifnot(
    "`strDateCol` must be a single column name" =
      is.character(strDateCol) && length(strDateCol) == 1,
    "`visits` must contain a `visit_dt` column" = "visit_dt" %in% names(visits)
  )

  missing_cols <- setdiff(c(strGroupCol, strVisitCol), names(df))
  if (length(missing_cols) > 0) {
    stop("`df` is missing required column(s): ", paste(missing_cols, collapse = ", "))
  }

  key_df <- paste(df[[strGroupCol]], df[[strVisitCol]], sep = "\r")
  key_visits <- paste(visits[[strGroupCol]], visits[[strVisitCol]], sep = "\r")

  dates <- visits$visit_dt[match(key_df, key_visits)]

  if (anyNA(dates)) {
    unmatched <- unique(df[[strVisitCol]][is.na(dates)])
    stop(
      "No scheduled date found for ", sum(is.na(dates)), " record(s); ",
      "unmatched visit(s): ", paste(utils::head(unmatched, 5), collapse = ", ")
    )
  }

  df[[strDateCol]] <- dates
  df[order(df[[strGroupCol]], df[[strDateCol]], method = "radix"), , drop = FALSE]
}


# ---- validation helpers -----------------------------------------------------

.validate_window_length <- function(nWindowLength) {
  stopifnot(
    "`nWindowLength` must be a single whole number >= 2" =
      is.numeric(nWindowLength) && length(nWindowLength) == 1 &&
        !is.na(nWindowLength) && nWindowLength >= 2 &&
        nWindowLength == round(nWindowLength)
  )
  invisible(TRUE)
}

.validate_proportion <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x < 0 || x > 1) {
    stop("`", name, "` must be a single number between 0 and 1")
  }
  invisible(TRUE)
}

.validate_count <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x < 0 ||
    x != round(x)) {
    stop("`", name, "` must be a single non-negative whole number")
  }
  x
}
