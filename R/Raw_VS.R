#' Column generators for Raw VS (Vital Signs) Data
#'
#' Generate Raw VS based on `VS.yaml` from `gsm.mapping`.
#' Wide format: one row per subject × visit with columns for all 8 vitals
#' measures: weight, height, bsa, sysbp, diabp, pulse, temp, resp.
#'
#' Domain generation itself is registered in `domain_registry.R` (`Raw_VS`
#' entry); the functions below are the per-column generators dispatched by
#' `add_new_var_data()` based on `spec$Raw_VS` column names.
#'
#' @keywords internal
#' @noRd


# The eight wide-format vital columns produced by this domain.
VS_VITALS <- c(
  "weight", "height", "bsa", "sysbp",
  "diabp", "pulse", "temp", "resp"
)

# Distribution parameters per vital. Held in one place so the generators below
# carry nothing but their identity.
VS_VITAL_PARAMS <- list(
  weight = list(mean = 75, sd = 10, digits = 1),
  height = list(mean = 170, sd = 10, digits = 1),
  bsa    = list(mean = 1.9, sd = 0.2, digits = 2),
  sysbp  = list(mean = 125, sd = 15, digits = 0),
  diabp  = list(mean = 80, sd = 10, digits = 0),
  pulse  = list(mean = 72, sd = 12, digits = 0),
  temp   = list(mean = 36.8, sd = 0.4, digits = 1),
  resp   = list(mean = 16, sd = 3, digits = 0)
)

# Default site-risk profile. Target rates sit mid-band relative to the
# 20% amber / 30% red thresholds specified in gsm.kri#306, so small-site
# quantization does not push a site across a boundary.
VS_DEFAULT_RISK_PROFILE <- list(
  dPctRed = 0.1,
  dPctAmber = 0.2,
  nWindowLength = 3,
  dRateNormal = 0.05,
  dRateAmber = 0.25,
  dRateRed = 0.45,
  vVitals = NULL
)


# Note: parallels `subj_visit_repeated()` in Raw_LB.R (n=1, one row per
# subject-visit, no test repeat factor). The visit column is emitted as
# `visit` per the VS.yaml spec, which carries `source_col: foldername` (Raw_LB
# uses `visnam`); `rename_raw_data_vars_per_spec()` renames it on the way out.
# Named distinctly from Raw_LB's `subj_visit_repeated()` to avoid colliding in
# the package namespace (generator functions are dispatched by bare name via
# `do.call()`).

#' Repeat subject visits
#'
#' @param n Number of rows to generate.
#' @param data Data frame of subject-visit records to repeat, carrying
#'   `subjid` and `instancename`.
#' @param visit_cols Names to emit the visit column under, one per visit alias
#'   the caller's spec declares. Defaults to the canonical `"visit"`.
#' @param ... Unused; absorbs other generator arguments.
#' @returns A list with element `subjid` plus one element per `visit_cols`.
#' @keywords internal
#' @noRd
vs_subj_visit_repeated <- function(n, data, visit_cols = "visit", ...) {
  res <- repeat_rows(n, data)
  out <- c(
    list(subjid = res$subjid),
    stats::setNames(
      rep(list(res$instancename), length(visit_cols)),
      visit_cols
    )
  )
  return(out)
}


# `Raw_VS` carries no `invid`: site is joined on from `Mapped_SUBJ` during
# `Mapped_VS` construction (Gilead-Public/gsm.mapping#165), so a
# `vs_invid_repeated()` generator would emit a column real extracts lack.
# Site identifiers are still resolved internally for run targeting.


# `vs_dt` is taken from the visit schedule rather than generated. The registry
# entry joins `Raw_VISIT$visit_dt` on and sorts by subject then date (see
# `assign_schedule_dates()`), so chronological order is well defined and
# injected runs are genuinely adjacent in time.

#' Assign visit dates from the visit schedule
#'
#' @param n Number of rows to generate.
#' @param dates Vector of dates supplied by the registry entry.
#' @param ... Unused; absorbs other generator arguments.
#' @returns The `dates` vector, unchanged.
#' @keywords internal
#' @noRd
vs_dt <- function(n, dates, ...) {
  return(dates)
}


#' Generate the vital-sign performed flag
#'
#' @param n Number of rows to generate.
#' @param performed Optional precomputed vector of `"Y"`/`"N"` values.
#' @param ... Unused; absorbs other generator arguments.
#' @returns A character vector of `"Y"`/`"N"` values of length `n`.
#' @keywords internal
#' @noRd
vsperf_std <- function(n, performed = NULL, ...) {
  # ~95% performed, ~5% not performed. The registry precomputes this so the
  # vital generators can blank the not-performed rows; when called without a
  # precomputed vector it falls back to generating its own.
  if (!is.null(performed)) {
    return(performed)
  }
  sample(c("Y", "N"), n, prob = c(0.95, 0.05), replace = TRUE)
}


#' Generate a vital-sign column
#'
#' @param n Number of rows to generate.
#' @param subjects Vector of subject IDs, ordered by subject then visit date.
#' @param sites Vector of site IDs aligned with `subjects`, or `NULL` for no
#'   site targeting.
#' @param performed Character vector of `vsperf_std` values; `"N"` rows are
#'   blanked.
#' @param lRiskProfile Risk profile list, or `NULL` for defaults.
#' @param ... Unused; absorbs other generator arguments.
#' @returns A numeric vector of length `n`.
#' @keywords internal
#' @noRd
weight <- function(n, subjects, sites = NULL, performed = NULL, lRiskProfile = NULL, ...) {
  .generate_vital(n, subjects, sites, performed, lRiskProfile, "weight")
}

#' @rdname weight
#' @noRd
height <- function(n, subjects, sites = NULL, performed = NULL, lRiskProfile = NULL, ...) {
  .generate_vital(n, subjects, sites, performed, lRiskProfile, "height")
}

#' @rdname weight
#' @noRd
bsa <- function(n, subjects, sites = NULL, performed = NULL, lRiskProfile = NULL, ...) {
  .generate_vital(n, subjects, sites, performed, lRiskProfile, "bsa")
}

#' @rdname weight
#' @noRd
sysbp <- function(n, subjects, sites = NULL, performed = NULL, lRiskProfile = NULL, ...) {
  .generate_vital(n, subjects, sites, performed, lRiskProfile, "sysbp")
}

#' @rdname weight
#' @noRd
diabp <- function(n, subjects, sites = NULL, performed = NULL, lRiskProfile = NULL, ...) {
  .generate_vital(n, subjects, sites, performed, lRiskProfile, "diabp")
}

#' @rdname weight
#' @noRd
pulse <- function(n, subjects, sites = NULL, performed = NULL, lRiskProfile = NULL, ...) {
  .generate_vital(n, subjects, sites, performed, lRiskProfile, "pulse")
}

#' @rdname weight
#' @noRd
temp <- function(n, subjects, sites = NULL, performed = NULL, lRiskProfile = NULL, ...) {
  .generate_vital(n, subjects, sites, performed, lRiskProfile, "temp")
}

#' @rdname weight
#' @noRd
resp <- function(n, subjects, sites = NULL, performed = NULL, lRiskProfile = NULL, ...) {
  .generate_vital(n, subjects, sites, performed, lRiskProfile, "resp")
}


#' Resolve a caller-supplied VS risk profile against the defaults
#'
#' @param lRiskProfile Named list, or `NULL` for defaults.
#' @returns A complete risk profile list.
#' @keywords internal
#' @noRd
.resolve_vs_risk_profile <- function(lRiskProfile = NULL) {
  if (is.null(lRiskProfile)) {
    return(VS_DEFAULT_RISK_PROFILE)
  }
  if (!is.list(lRiskProfile)) {
    stop("`vs_risk_profile` must be a list or NULL")
  }
  utils::modifyList(VS_DEFAULT_RISK_PROFILE, lRiskProfile)
}


#' Generate site-aware vital sign values with targeted consecutive runs
#'
#' Blanks not-performed rows BEFORE injecting runs, so window counts are
#' computed over performed measurements only. Reversing that order inflates
#' every site's realized rate relative to what the metric computes. Risk bands
#' are drawn independently per vital -- each vital is its own KRI.
#'
#' @param n Number of values to generate.
#' @param subjects Vector of subject IDs, ordered by subject then visit date.
#' @param sites Vector of site IDs aligned with `subjects`, or `NULL` for no
#'   site targeting.
#' @param performed Character vector of `vsperf_std` values; `"N"` rows are
#'   blanked.
#' @param lRiskProfile Risk profile list, or `NULL` for defaults.
#' @param strVital Name of the vital being generated.
#' @returns A numeric vector of length `n`.
#' @keywords internal
#' @noRd
.generate_vital <- function(n, subjects, sites = NULL, performed = NULL,
                            lRiskProfile = NULL, strVital) {
  params <- VS_VITAL_PARAMS[[strVital]]
  if (is.null(params)) {
    stop("Unknown vital: ", strVital)
  }

  values <- round(
    stats::rnorm(n, mean = params$mean, sd = params$sd),
    digits = params$digits
  )

  # Blank not-performed rows BEFORE injection -- see the note above.
  if (!is.null(performed)) {
    values[performed == "N"] <- NA_real_
  }

  if (is.null(sites)) {
    return(values)
  }

  profile <- .resolve_vs_risk_profile(lRiskProfile)

  # A profile may target only a subset of vitals; others get plain draws.
  if (!is.null(profile$vVitals) && !(strVital %in% profile$vVitals)) {
    return(values)
  }

  # Rows with a missing site ID cannot be targeted, so they must not consume
  # part of the red/amber allocation either -- otherwise a pseudo-site "NA"
  # absorbs a band and leaves real sites normal.
  known_sites <- sites[!is.na(sites)]
  if (length(known_sites) == 0) {
    return(values)
  }

  bands <- allocate_site_risk(
    known_sites,
    dPctRed = profile$dPctRed,
    dPctAmber = profile$dPctAmber
  )

  rate_for_band <- c(
    red = profile$dRateRed,
    amber = profile$dRateAmber,
    normal = profile$dRateNormal
  )

  # `bands` is keyed by sites drawn from `sites` itself, so every key matches
  # at least one row.
  for (site in names(bands)) {
    site_idx <- which(sites %in% site)

    values[site_idx] <- as.numeric(inject_targeted_runs(
      values[site_idx],
      subjects[site_idx],
      dTargetRate = rate_for_band[[bands[[site]]]],
      nWindowLength = profile$nWindowLength
    ))
  }

  values
}
