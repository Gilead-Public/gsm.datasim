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
weight <- function(n, subjects, sites = NULL, performed = NULL, lRiskProfile = NULL,
                   vAllSites = NULL, nTotalSites = NULL, strStudyId = NULL, ...) {
  .generate_vital(n, subjects, sites, performed, lRiskProfile,
    vAllSites = vAllSites, nTotalSites = nTotalSites, strStudyId = strStudyId,
    strVital = "weight"
  )
}

#' @rdname weight
#' @noRd
height <- function(n, subjects, sites = NULL, performed = NULL, lRiskProfile = NULL,
                   vAllSites = NULL, nTotalSites = NULL, strStudyId = NULL, ...) {
  .generate_vital(n, subjects, sites, performed, lRiskProfile,
    vAllSites = vAllSites, nTotalSites = nTotalSites, strStudyId = strStudyId,
    strVital = "height"
  )
}

#' @rdname weight
#' @noRd
bsa <- function(n, subjects, sites = NULL, performed = NULL, lRiskProfile = NULL,
                   vAllSites = NULL, nTotalSites = NULL, strStudyId = NULL, ...) {
  .generate_vital(n, subjects, sites, performed, lRiskProfile,
    vAllSites = vAllSites, nTotalSites = nTotalSites, strStudyId = strStudyId,
    strVital = "bsa"
  )
}

#' @rdname weight
#' @noRd
sysbp <- function(n, subjects, sites = NULL, performed = NULL, lRiskProfile = NULL,
                   vAllSites = NULL, nTotalSites = NULL, strStudyId = NULL, ...) {
  .generate_vital(n, subjects, sites, performed, lRiskProfile,
    vAllSites = vAllSites, nTotalSites = nTotalSites, strStudyId = strStudyId,
    strVital = "sysbp"
  )
}

#' @rdname weight
#' @noRd
diabp <- function(n, subjects, sites = NULL, performed = NULL, lRiskProfile = NULL,
                   vAllSites = NULL, nTotalSites = NULL, strStudyId = NULL, ...) {
  .generate_vital(n, subjects, sites, performed, lRiskProfile,
    vAllSites = vAllSites, nTotalSites = nTotalSites, strStudyId = strStudyId,
    strVital = "diabp"
  )
}

#' @rdname weight
#' @noRd
pulse <- function(n, subjects, sites = NULL, performed = NULL, lRiskProfile = NULL,
                   vAllSites = NULL, nTotalSites = NULL, strStudyId = NULL, ...) {
  .generate_vital(n, subjects, sites, performed, lRiskProfile,
    vAllSites = vAllSites, nTotalSites = nTotalSites, strStudyId = strStudyId,
    strVital = "pulse"
  )
}

#' @rdname weight
#' @noRd
temp <- function(n, subjects, sites = NULL, performed = NULL, lRiskProfile = NULL,
                   vAllSites = NULL, nTotalSites = NULL, strStudyId = NULL, ...) {
  .generate_vital(n, subjects, sites, performed, lRiskProfile,
    vAllSites = vAllSites, nTotalSites = nTotalSites, strStudyId = strStudyId,
    strVital = "temp"
  )
}

#' @rdname weight
#' @noRd
resp <- function(n, subjects, sites = NULL, performed = NULL, lRiskProfile = NULL,
                   vAllSites = NULL, nTotalSites = NULL, strStudyId = NULL, ...) {
  .generate_vital(n, subjects, sites, performed, lRiskProfile,
    vAllSites = vAllSites, nTotalSites = nTotalSites, strStudyId = strStudyId,
    strVital = "resp"
  )
}


# Fields that may appear at any level of a risk profile, and domains that
# support a profile at all. `vVitals` is a top-level/domain-level field only.
RISK_PROFILE_FIELDS <- setdiff(names(VS_DEFAULT_RISK_PROFILE), "vVitals")
RISK_PROFILE_DOMAINS <- "Raw_VS"


#' Resolve a caller-supplied risk profile for one domain (and vital)
#'
#' A profile is a named list whose names are scalar fields (`dPctRed`,
#' `dPctAmber`, `nWindowLength`, `dRateNormal`, `dRateAmber`, `dRateRed`,
#' `vVitals`) and/or domain names (currently only `Raw_VS`). A domain entry is
#' itself a list of scalar fields and/or vital names; a vital entry is a list
#' of scalar fields. More specific levels override less specific ones:
#' defaults < top level < domain < vital. Targeted vitals are the domain's
#' `vVitals` if given, else the vitals it names, else the top-level `vVitals`.
#'
#' The whole profile is validated on every call, whatever `strDomain` and
#' `strVital` are, so a malformed profile is rejected identically on every
#' route.
#'
#' @param lRiskProfile Named list, or `NULL` for defaults.
#' @param strDomain Domain to resolve for.
#' @param strVital Vital to resolve for, or `NULL` for the domain-level profile.
#' @returns A complete flat profile list. With `strVital` supplied, `NULL` if
#'   that vital is not targeted.
#' @keywords internal
#' @noRd
.resolve_risk_profile <- function(lRiskProfile = NULL,
                                  strDomain = "Raw_VS",
                                  strVital = NULL) {
  .validate_risk_profile(lRiskProfile)

  if (is.null(lRiskProfile)) {
    return(VS_DEFAULT_RISK_PROFILE)
  }

  .resolve_risk_profile_layers(lRiskProfile, strDomain, strVital)
}


#' Merge profile layers for one domain/vital without validating
#' @noRd
.resolve_risk_profile_layers <- function(lRiskProfile, strDomain, strVital) {
  top <- lRiskProfile[setdiff(names(lRiskProfile), RISK_PROFILE_DOMAINS)]
  dom <- as.list(lRiskProfile[[strDomain]])
  dom_vitals <- intersect(names(dom), VS_VITALS)
  dom_fields <- dom[setdiff(names(dom), dom_vitals)]

  profile <- utils::modifyList(VS_DEFAULT_RISK_PROFILE, top)
  profile <- utils::modifyList(profile, dom_fields)

  # An explicit domain-level `vVitals` wins; otherwise naming vitals targets
  # exactly those vitals.
  if (length(dom_vitals) > 0 && is.null(dom$vVitals)) {
    profile$vVitals <- dom_vitals
  }
  if (is.null(strVital)) {
    return(profile)
  }
  if (!is.null(profile$vVitals) && !(strVital %in% profile$vVitals)) {
    return(NULL)
  }
  profile <- utils::modifyList(profile, as.list(dom[[strVital]]))
  profile
}


#' Validate a risk profile of any nesting depth
#'
#' @param lRiskProfile Named list, or `NULL`.
#' @returns `TRUE` invisibly, or an error.
#' @keywords internal
#' @noRd
.validate_risk_profile <- function(lRiskProfile) {
  if (is.null(lRiskProfile)) {
    return(invisible(TRUE))
  }
  if (!is.list(lRiskProfile)) {
    stop("`risk_profile` must be a list or NULL")
  }

  # Catch misspellings against the profile as written.
  .check_names(lRiskProfile, c(names(VS_DEFAULT_RISK_PROFILE), RISK_PROFILE_DOMAINS),
    "risk_profile", "field(s)"
  )

  # Top-level fields alone must form a valid profile.
  .validate_resolved_risk_profile(
    .resolve_risk_profile_layers(lRiskProfile, RISK_PROFILE_DOMAINS[[1]], NULL)
  )

  for (domain in intersect(names(lRiskProfile), RISK_PROFILE_DOMAINS)) {
    dom <- lRiskProfile[[domain]]
    label <- paste0("risk_profile$", domain)
    if (!is.list(dom)) {
      stop(label, " must be a named list")
    }
    .check_names(dom, c(names(VS_DEFAULT_RISK_PROFILE), VS_VITALS), label, "field(s) or vital(s)")

    vital_names <- intersect(names(dom), VS_VITALS)
    # Vitals given overrides must be among the vitals being targeted.
    if (!is.null(dom$vVitals) && !all(vital_names %in% dom$vVitals)) {
      stop(
        label, " has settings for vital(s) not in `vVitals`: ",
        paste(setdiff(vital_names, dom$vVitals), collapse = ", ")
      )
    }

    .with_label(label, .validate_resolved_risk_profile(
      .resolve_risk_profile_layers(lRiskProfile, domain, NULL)
    ))

    for (vital in vital_names) {
      entry <- dom[[vital]]
      vlabel <- paste0(label, "$", vital)
      if (!is.null(entry) && !is.list(entry)) {
        stop(vlabel, " must be a named list")
      }
      .check_names(entry, RISK_PROFILE_FIELDS, vlabel, "field(s)")
      .with_label(vlabel, .validate_resolved_risk_profile(
        .resolve_risk_profile_layers(lRiskProfile, domain, vital)
      ))
    }
  }

  invisible(TRUE)
}


#' Error on unnamed or unrecognized names in a profile list
#' @noRd
.check_names <- function(x, strAllowed, strLabel, strWhat) {
  if (length(x) == 0) {
    return(invisible(TRUE))
  }
  nms <- names(x)
  if (is.null(nms) || any(is.na(nms) | nms == "")) {
    stop(strLabel, " must be a named list")
  }
  unknown <- setdiff(nms, strAllowed)
  if (length(unknown) > 0) {
    stop(
      strLabel, " contains unknown ", strWhat, ": ",
      paste(unknown, collapse = ", ")
    )
  }
  invisible(TRUE)
}


#' Prefix an error with the profile location it came from
#' @noRd
.with_label <- function(strLabel, expr) {
  tryCatch(expr, error = function(e) {
    stop("In ", strLabel, ": ", conditionMessage(e), call. = FALSE)
  })
}


#' Validate a fully-resolved VS risk profile
#'
#' Operates on the merged profile, so every field is present.
#'
#' @param profile A resolved risk profile list.
#' @returns `TRUE` invisibly, or an error describing the first problem found.
#' @keywords internal
#' @noRd
.validate_resolved_risk_profile <- function(profile) {
  is_proportion <- function(x) {
    is.numeric(x) && length(x) == 1 && !is.na(x) && x >= 0 && x <= 1
  }

  for (field in c("dPctRed", "dPctAmber", "dRateNormal", "dRateAmber", "dRateRed")) {
    if (!is_proportion(profile[[field]])) {
      stop("risk_profile$", field, " must be a single number between 0 and 1")
    }
  }

  # Bands must be ordered so red sites score above amber, and amber above
  # normal. The KRI thresholds themselves are configured downstream (gsm.kri),
  # so they are not hard-coded here; choosing rates on the correct side of
  # them is the caller's responsibility.
  if (!(profile$dRateNormal <= profile$dRateAmber &&
    profile$dRateAmber <= profile$dRateRed)) {
    stop(
      "risk_profile rates must satisfy dRateNormal <= dRateAmber <= dRateRed",
      " (effective values, after defaults are applied: ",
      profile$dRateNormal, ", ", profile$dRateAmber, ", ", profile$dRateRed, ")"
    )
  }

  if (profile$dPctRed + profile$dPctAmber > 1) {
    stop(
      "risk_profile$dPctRed + risk_profile$dPctAmber must not exceed 1",
      " (effective values, after defaults are applied: ",
      profile$dPctRed, " + ", profile$dPctAmber, ")"
    )
  }

  window <- profile$nWindowLength
  if (!is.numeric(window) || length(window) != 1 || is.na(window) ||
    window < 2 || window != round(window)) {
    stop("risk_profile$nWindowLength must be a single whole number >= 2")
  }

  vitals <- profile$vVitals
  if (!is.null(vitals)) {
    if (!is.character(vitals) || length(vitals) == 0) {
      stop("risk_profile$vVitals must be a non-empty character vector or NULL")
    }
    unknown_vitals <- setdiff(vitals, VS_VITALS)
    if (length(unknown_vitals) > 0) {
      stop(
        "risk_profile$vVitals contains unknown vital(s): ",
        paste(unknown_vitals, collapse = ", "),
        ". Valid vitals: ", paste(VS_VITALS, collapse = ", ")
      )
    }
  }

  invisible(TRUE)
}


#' Generate site-aware vital sign values with targeted consecutive runs
#'
#' Blanks not-performed rows BEFORE injecting runs, so window counts are
#' computed over performed measurements only. Reversing that order inflates
#' every site's realized rate relative to what the metric computes. Risk bands
#' are drawn independently per vital -- each vital is its own KRI.
#'
#' Bands persist across snapshots. `vAllSites` supplies the site roster in
#' first-appearance order and `nTotalSites` the eventual roster size, so a
#' site's band follows from its rank rather than from which sites happen to
#' have enrolled by the current snapshot (#143). The seed key folds in the
#' study and the vital, keeping bands stable per site and independent per
#' vital.
#'
#' @param n Number of values to generate.
#' @param subjects Vector of subject IDs, ordered by subject then visit date.
#' @param sites Vector of site IDs aligned with `subjects`, or `NULL` for no
#'   site targeting.
#' @param performed Character vector of `vsperf_std` values; `"N"` rows are
#'   blanked.
#' @param lRiskProfile Risk profile list, or `NULL` for defaults.
#' @param vAllSites Site roster in first-appearance order, or `NULL` to fall
#'   back to the sites present in `sites`.
#' @param nTotalSites Eventual number of sites in the study, or `NULL`.
#' @param strStudyId Study identifier, folded into the band seed key.
#' @param strVital Name of the vital being generated.
#' @returns A numeric vector of length `n`.
#' @keywords internal
#' @noRd
.generate_vital <- function(n, subjects, sites = NULL, performed = NULL,
                            lRiskProfile = NULL, vAllSites = NULL,
                            nTotalSites = NULL, strStudyId = NULL, strVital) {
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

  # `NULL` when the profile does not target this vital; it gets plain draws.
  profile <- .resolve_risk_profile(lRiskProfile, "Raw_VS", strVital)
  if (is.null(profile)) {
    return(values)
  }

  # Rows with a missing site ID cannot be targeted, so they must not consume
  # part of the red/amber allocation either -- otherwise a pseudo-site "NA"
  # absorbs a band and leaves real sites normal.
  known_sites <- sites[!is.na(sites)]
  if (length(known_sites) == 0) {
    return(values)
  }

  # Rank over the full roster where it is known, so a site's band does not
  # depend on which snapshot is being generated. Sites in `known_sites` that
  # the roster omits are appended rather than dropped -- allocation must cover
  # every site that has rows here.
  roster <- unique(as.character(vAllSites %||% character(0)))
  roster <- c(roster, setdiff(unique(as.character(known_sites)), roster))

  bands <- allocate_site_risk(
    roster,
    dPctRed = profile$dPctRed,
    dPctAmber = profile$dPctAmber,
    nTotalSites = nTotalSites,
    strSeedKey = paste(strStudyId %||% "", strVital, sep = "|")
  )

  rate_for_band <- c(
    red = profile$dRateRed,
    amber = profile$dRateAmber,
    normal = profile$dRateNormal
  )

  # `bands` is keyed by the full roster, which may include sites that have no
  # rows in this snapshot yet. They hold their band for later snapshots;
  # there is simply nothing to inject into now.
  for (site in names(bands)) {
    site_idx <- which(sites %in% site)
    if (length(site_idx) == 0) next

    values[site_idx] <- as.numeric(inject_targeted_runs(
      values[site_idx],
      subjects[site_idx],
      dTargetRate = rate_for_band[[bands[[site]]]],
      nWindowLength = profile$nWindowLength
    ))
  }

  values
}
