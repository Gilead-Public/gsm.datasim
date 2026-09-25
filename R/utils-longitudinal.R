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
#' consecutive-repeat rate before `inject_targeted_runs()` constructs it. Supply
#' either percentages (`dPctRed` / `dPctAmber`) or explicit counts 
#' (`nRed` / `nAmber`); counts take precedence when both are given.
#' Percentages are converted with [round()], so small site counts degrade
#' gracefully rather than erroring -- 10% of 3 sites is 0 red sites, not a
#' fractional one.
#'
#' @section Persisting bands across snapshots:
#'
#' Supplying `nTotalSites` and `strSeedKey` switches allocation from "sample
#' the sites I can see" to "assign by rank over the final roster". Sites are
#' ranked by first appearance, bands are dealt across `nTotalSites` slots, and
#' the slot permutation is derived from `strSeedKey` rather than the ambient
#' RNG. A site therefore keeps its band no matter which snapshot is being
#' generated, and sites that enroll later claim unused slots without
#' disturbing bands already handed out (#143).
#'
#' This matters because snapshots are deltas: rows written in an early
#' snapshot are frozen, so a site's band has to be right the first time its
#' rows are generated. It relies on the site roster being append-only, which
#' is what makes rank a stable key.
#'
#' @param vSites Character vector of site identifiers. Duplicates are ignored;
#'   each distinct site receives one band. Order is significant when
#'   `nTotalSites` is supplied -- it is the rank order.
#' @param dPctRed,dPctAmber Proportions in `[0, 1]` of sites to place in the
#'   red and amber bands.
#' @param nRed,nAmber Optional explicit site counts, overriding the
#'   corresponding percentage.
#' @param nTotalSites Optional total number of sites the study will end up
#'   with. Percentages are taken over this rather than over `vSites`, so early
#'   snapshots allocate against the final roster size.
#' @param strSeedKey Optional string keying the deterministic slot
#'   permutation. Required for allocation to be reproducible across calls;
#'   include the vital so bands stay independent per vital.
#'
#' @returns Named character vector, one element per distinct site, with values
#'   `"red"`, `"amber"`, or `"normal"`.
#'
#' @keywords internal
allocate_site_risk <- function(vSites, dPctRed = 0.1, dPctAmber = 0.2,
                               nRed = NULL, nAmber = NULL,
                               nTotalSites = NULL, strSeedKey = NULL) {
  sites <- unique(as.character(vSites))
  n_sites <- length(sites)
  if (n_sites == 0) {
    return(stats::setNames(character(0), character(0)))
  }

  # Percentages are taken over the final roster when it is known, so snapshot 1
  # allocates 1 red out of 10 eventual sites rather than 0 red out of the 1
  # site that has enrolled so far.
  n_slots <- if (is.null(nTotalSites)) n_sites else max(nTotalSites, n_sites)

  red_from_pct <- is.null(nRed)
  amber_from_pct <- is.null(nAmber)

  n_red <- if (!red_from_pct) {
    .validate_count(nRed, "nRed")
  } else {
    .validate_proportion(dPctRed, "dPctRed")
    round(dPctRed * n_slots)
  }
  n_amber <- if (!amber_from_pct) {
    .validate_count(nAmber, "nAmber")
  } else {
    .validate_proportion(dPctAmber, "dPctAmber")
    round(dPctAmber * n_slots)
  }

  # Only absorb the rounding overshoot, not a genuinely oversized profile: cap
  # when the percentages are themselves valid (sum <= 1) and both came from
  # percentages. Anything else still errors below.
  if (red_from_pct && amber_from_pct &&
    n_red <= n_slots &&
    dPctRed + dPctAmber <= 1) {
    n_amber <- min(n_amber, n_slots - n_red)
  }

  if (n_red + n_amber > n_slots) {
    stop(
      "Cannot allocate ", n_red, " red and ", n_amber, " amber bands across ",
      n_slots, " site(s): red + amber must not exceed the number of sites"
    )
  }

  # Deal bands across slots, then map sites onto slots by rank. With a seed key
  # the permutation is a pure function of that key, so every snapshot computes
  # the same slot layout without carrying state across the snapshot boundary.
  slot_bands <- rep("normal", n_slots)
  if (n_red > 0) slot_bands[seq_len(n_red)] <- "red"
  if (n_amber > 0) slot_bands[n_red + seq_len(n_amber)] <- "amber"

  shuffled_slots <- if (n_slots == 1) {
    slot_bands
  } else if (is.null(strSeedKey)) {
    sample(slot_bands)
  } else {
    .keyed_permutation(slot_bands, strSeedKey)
  }

  stats::setNames(shuffled_slots[seq_len(n_sites)], sites)
}


# Deterministic permutation of `x` keyed by a string. Uses a temporary, fully
# restored RNG state so allocation neither consumes nor perturbs the caller's
# stream -- two vitals in the same generation pass must not shift each other's
# bands, and the surrounding value draws must stay reproducible.
.keyed_permutation <- function(x, strSeedKey) {
  seed <- sum(strtoi(charToRaw(strSeedKey), 16L) *
    seq_along(charToRaw(strSeedKey))) %% .Machine$integer.max

  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    old_seed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
    on.exit(assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
  } else {
    on.exit(
      suppressWarnings(rm(".Random.seed", envir = globalenv())),
      add = TRUE
    )
  }

  set.seed(seed)
  sample(x)
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
#'   list with the achieved `numerator` and `denominator`. The numerator is
#'   **recounted from the returned values**, not assumed from the construction,
#'   so it always matches what a window-counting metric will compute.
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
    # Recount rather than assume zero: the caller's values may already contain
    # identical windows by chance (rounded measurements collide), and the
    # `realized` contract is "what the metric will compute", not "what we
    # injected".
    return(.with_realized(
      values,
      .count_identical_windows(values, idx_by_group, nWindowLength),
      total_windows
    ))
  }

  # Shuffle group order so injected runs aren't concentrated in the
  # lowest-numbered subjects.
  eligible <- names(window_counts)[window_counts >= 1]
  if (length(eligible) > 1) eligible <- sample(eligible)

  remaining <- target_numerator

  for (grp in eligible) {
    if (remaining <= 0) break

    idx <- idx_by_group[[grp]]
    capacity <- window_counts[[grp]]

    # A run of length `nWindowLength + k` contributes `k + 1` windows, and a
    # group's whole capacity is consumed by a single run covering it.
    take <- min(remaining, capacity)
    run_length <- nWindowLength + take - 1
    run_idx <- idx[seq_len(run_length)]

    # The run takes a value from the block it overwrites. The element just past
    # the run may already carry that value -- rounded measurements collide
    # often enough that this is not rare -- and left alone it silently extends
    # the run by a window. Prefer a block value that differs from that
    # neighbour; no values are synthesised, so the group's value distribution
    # is unchanged.
    values[run_idx] <- .choose_run_value(values, idx, run_length)

    remaining <- remaining - take
  }

  # Recount rather than trust the construction: when a group holds a single
  # distinct value no choice avoids the tie. Counting the returned vector keeps
  # `realized` equal to what the downstream metric computes, which is the whole
  # point of the attribute.
  .with_realized(
    values,
    .count_identical_windows(values, idx_by_group, nWindowLength),
    total_windows
  )
}


#' Pick the value for an injected run, avoiding a tie with the next element
#'
#' Returns the first value in the overwritten block that differs from the
#' element immediately following the run, falling back to the block's first
#' value when there is no following element or no differing candidate.
#'
#' @noRd
.choose_run_value <- function(values, idx, run_length) {
  block <- values[idx[seq_len(run_length)]]
  if (run_length >= length(idx)) {
    return(block[1])
  }

  next_value <- values[idx[run_length + 1]]
  candidates <- block[block != next_value]
  if (length(candidates) == 0) {
    return(block[1])
  }

  candidates[1]
}


#' Count length-`nWindowLength` windows whose values are all identical
#' @noRd
.count_identical_windows <- function(values, idx_by_group, nWindowLength) {
  sum(vapply(
    idx_by_group,
    function(idx) {
      n <- length(idx)
      if (n < nWindowLength) {
        return(0L)
      }
      v <- values[idx]
      starts <- seq_len(n - nWindowLength + 1)
      sum(vapply(
        starts,
        function(i) {
          window <- v[i + seq_len(nWindowLength) - 1]
          all(window == window[1])
        },
        logical(1)
      ))
    },
    integer(1)
  ))
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
#'   visit columns plus `visit_dt`. `visit_dt` may be a `Date` or a
#'   `"%Y-%m-%d"` character vector; the `Raw_VISIT` generator produces the
#'   latter.
#' @param strDateCol Name for the date column in the returned frame -- e.g.
#'   `"vs_dt"` for `Raw_VS`, `"lb_dt"` for a future `Raw_LB` adopter.
#' @param strGroupCol,strVisitCol Column names identifying the subject and the
#'   visit.
#'
#' @returns `df` with the date column added as a `Date`, sorted by group then
#'   date. The sort is stable, so repeated records within a visit keep a
#'   deterministic order.
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

  # `Raw_VISIT$visit_dt` is a "%Y-%m-%d" character vector, but the date column
  # this replaces was a `Date` and downstream consumers depend on that class.
  # Normalize here so the schedule's storage type cannot leak into the output,
  # and so the sort below is chronological rather than lexicographic by luck.
  schedule <- .as_schedule_date(visits$visit_dt)
  dates <- schedule[match(key_df, key_visits)]

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

#' Coerce a visit schedule column to `Date`
#'
#' Errors on unparseable values rather than letting them become `NA`, which
#' `assign_schedule_dates()` would otherwise report as an unmatched visit.
#'
#' @noRd
.as_schedule_date <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  if (!is.character(x)) {
    stop("`visits$visit_dt` must be a Date or a character vector of dates")
  }

  parsed <- as.Date(x, format = "%Y-%m-%d")
  bad <- is.na(parsed) & !is.na(x)
  if (any(bad)) {
    stop(
      "`visits$visit_dt` has ", sum(bad), " value(s) that are not ",
      "\"%Y-%m-%d\" dates: ", paste(utils::head(unique(x[bad]), 5), collapse = ", ")
    )
  }

  parsed
}

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
