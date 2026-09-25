# ---- known-answer sequences -------------------------------------------------

test_that("count_repeat_windows matches the hand-computed worked examples (#143)", {
  # Worked examples from #143 / gsm.roadmap#250. The expected denominators are
  # worked out by hand rather than computed, so this checks the rule rather
  # than recording whatever the implementation happens to produce.
  #
  # Missing values are dropped before windows form, so `not_performed`
  # reduces to five measurements (10, 10, 10, 4, 5) and yields 3 windows, not
  # the 4 its six rows would suggest.
  cases <- list(
    scattered = list(values = c(10, 10, 5, 3, 10), denominator = 3),
    all_repeated = list(values = c(10, 10, 10, 10, 10), denominator = 3),
    one_run = list(values = c(10, 10, 10, 1, 5, 6), denominator = 4),
    not_performed = list(values = c(10, 10, NA, 10, 4, 5), denominator = 3)
  )

  for (nm in names(cases)) {
    case <- cases[[nm]]
    expect_equal(
      count_repeat_windows(sum(!is.na(case$values)), nWindowLength = 3),
      case$denominator,
      info = nm
    )
  }
})

test_that("Raw_VS is registered in the domain registry (#113)", {
  registry <- get_domain_registry()

  expect_true("Raw_VS" %in% names(registry))
  expect_equal(registry$Raw_VS$dataset, "Raw_VS")
  expect_true(is.function(registry$Raw_VS$count_fn))
  expect_true(is.function(registry$Raw_VS$generate_fn))
  expect_true(is.character(registry$Raw_VS$required_inputs))
})

test_that("Raw_VS migrated domain adapter generates a data frame with expected columns (#113, #143)", {
  set.seed(3841)

  data <- make_vs_test_data(n_subjects = 20, n_visits = 6)
  vs_df <- generate_domain_from_registry("Raw_VS", make_vs_context(data))

  expect_s3_class(vs_df, "data.frame")
  expect_true(nrow(vs_df) > 0)
  expect_true(all(
    c(
      "subjid",
      "invid",
      "studyid",
      "instancename",
      "vs_dt",
      "vsperf_std",
      "weight",
      "sysbp",
      "diabp"
    ) %in%
      names(vs_df)
  ))

  # One row per subject x visit -- no dot-flattened list columns from
  # the split_vars processing.
  expect_false(any(grepl("\\.", names(vs_df))))
  expect_equal(nrow(vs_df), 20 * 6)
})

test_that("Raw_VS invid is correctly attributed to each subject via Raw_SUBJ lookup (#113, #143)", {
  set.seed(514)

  data <- make_vs_test_data(n_subjects = 10, n_visits = 3)
  vs_df <- generate_domain_from_registry("Raw_VS", make_vs_context(data))

  expected_invid <- data$Raw_SUBJ[
    match(vs_df$subjid, data$Raw_SUBJ$subjid),
    "invid"
  ]
  expect_equal(vs_df$invid, expected_invid)
})

test_that("Raw_VS respects the cumulative snapshot pattern via previous_data (#113, #143)", {
  set.seed(9042)

  data <- make_vs_test_data(n_subjects = 20, n_visits = 4)
  vs_snapshot1 <- generate_domain_from_registry("Raw_VS", make_vs_context(data))

  # No new subjects -- second snapshot should return the same rows unchanged.
  context2 <- make_vs_context(data, start_date = as.Date("2012-02-01"))
  context2$previous_data <- list(Raw_VS = vs_snapshot1)
  vs_snapshot2 <- generate_domain_from_registry("Raw_VS", context2)

  expect_equal(nrow(vs_snapshot2), nrow(vs_snapshot1))
  expect_equal(length(unique(vs_snapshot2$subjid)), 20)
})

test_that("Raw_VS generates the full 8-vitals superset when spec'd (#113, #143)", {
  set.seed(4471)

  data <- make_vs_test_data(n_subjects = 40, n_visits = 6)
  vs_df <- generate_domain_from_registry(
    "Raw_VS",
    make_vs_context(data, spec = make_vs_full_spec())
  )

  expect_true(all(VS_VITAL_COLS %in% names(vs_df)))
  for (col in VS_VITAL_COLS) {
    expect_true(is.numeric(vs_df[[col]]), info = col)
  }
})

test_that("prepare_combined_specs_for_generation leaves an already-complete Raw_VS spec untouched (#113)", {
  complete_spec <- make_vs_test_spec()
  combined_specs <- list(
    Raw_STUDY = list(),
    Raw_VS = complete_spec
  )

  prepared <- prepare_combined_specs_for_generation(combined_specs)

  expect_identical(prepared$Raw_VS, complete_spec)
})

test_that("prepare_combined_specs_for_generation does not add a Raw_VS spec when Raw_VS is not requested (#113)", {
  combined_specs <- list(
    Raw_STUDY = list(),
    Raw_SUBJ = list(subjid = list(required = TRUE))
  )

  prepared <- prepare_combined_specs_for_generation(combined_specs)

  expect_false("Raw_VS" %in% names(prepared))
})

test_that("Raw_VS registry adapter falls back to a default instancename spec entry when the caller-supplied spec omits it (#113, #143)", {
  set.seed(6284)

  data <- make_vs_test_data(n_subjects = 15, n_visits = 4)

  # Spec deliberately omits `instancename` -- the registry entry should
  # inject its own `required = TRUE` default rather than erroring.
  spec_without_instancename <- make_vs_test_spec()
  spec_without_instancename$instancename <- NULL

  vs_df <- generate_domain_from_registry(
    "Raw_VS",
    make_vs_context(data, spec = spec_without_instancename)
  )

  expect_s3_class(vs_df, "data.frame")
  expect_true("instancename" %in% names(vs_df))
  expect_equal(nrow(vs_df), 15 * 4)
})

# ---- vs_dt now follows the visit schedule ----------------------------------

test_that("Raw_VS vs_dt follows the visit schedule and is non-decreasing within subject (#143)", {
  set.seed(7238)

  data <- make_vs_test_data(n_subjects = 12, n_visits = 5)
  vs_df <- generate_domain_from_registry("Raw_VS", make_vs_context(data))

  expect_s3_class(vs_df$vs_dt, "Date")

  # Dates match Raw_VISIT$visit_dt for the corresponding subject-visit.
  expected <- data$Raw_VISIT$visit_dt[
    match(
      paste(vs_df$subjid, vs_df$instancename),
      paste(data$Raw_VISIT$subjid, data$Raw_VISIT$instancename)
    )
  ]
  expect_equal(vs_df$vs_dt, expected)

  # Non-decreasing within subject, and distinct visits get distinct dates.
  for (s in unique(vs_df$subjid)) {
    subj_dates <- vs_df$vs_dt[vs_df$subjid == s]
    expect_false(is.unsorted(subj_dates), info = paste("subject", s))
    expect_equal(length(unique(subj_dates)), 5, info = paste("subject", s))
  }

  # The old constant-date behavior is gone.
  expect_gt(length(unique(vs_df$vs_dt)), 1)
})

# ---- site targeting: the headline behavior ---------------------------------

test_that("Raw_VS site-level repeat rates land in their intended bands (#143)", {
  set.seed(1587)

  data <- make_vs_test_data(n_subjects = 60, n_visits = 10, n_sites = 10)
  profile <- list(
    dPctRed = 0.2,
    dPctAmber = 0.3,
    nWindowLength = 3,
    dRateNormal = 0.05,
    dRateAmber = 0.25,
    dRateRed = 0.45
  )

  vs_df <- generate_domain_from_registry(
    "Raw_VS",
    make_vs_context(data, vs_risk_profile = profile)
  )

  rates <- site_repeat_rate(vs_df, strValueCol = "weight")
  rates <- rates[!is.na(rates$rate), ]

  # 10 sites at 20% / 30% -> 2 red, 3 amber, 5 normal.
  expect_equal(sum(rates$rate >= 0.30), 2)
  expect_equal(sum(rates$rate >= 0.20 & rates$rate < 0.30), 3)
  expect_equal(sum(rates$rate < 0.20), 5)
})

test_that("Raw_VS sites in different bands get materially different rates (#143)", {
  set.seed(2664)

  data <- make_vs_test_data(n_subjects = 60, n_visits = 10, n_sites = 10)
  profile <- list(
    dPctRed = 0.2,
    dPctAmber = 0.2,
    nWindowLength = 3,
    dRateNormal = 0.05,
    dRateAmber = 0.25,
    dRateRed = 0.45
  )

  vs_df <- generate_domain_from_registry(
    "Raw_VS",
    make_vs_context(data, vs_risk_profile = profile)
  )

  rates <- site_repeat_rate(vs_df, strValueCol = "weight")
  rates <- rates[!is.na(rates$rate), ]

  # Guards against a regression that makes allocation a no-op: the spread
  # between the highest and lowest site must be substantial.
  expect_gt(max(rates$rate) - min(rates$rate), 0.25)
  expect_gt(length(unique(round(rates$rate, 2))), 2)
})

test_that("Raw_VS risk assignment is drawn independently per vital (#143)", {
  set.seed(3925)

  data <- make_vs_test_data(n_subjects = 80, n_visits = 10, n_sites = 12)
  profile <- list(
    dPctRed = 0.25,
    dPctAmber = 0.25,
    nWindowLength = 3,
    dRateNormal = 0.05,
    dRateAmber = 0.25,
    dRateRed = 0.45
  )

  vs_df <- generate_domain_from_registry(
    "Raw_VS",
    make_vs_context(data, spec = make_vs_full_spec(), vs_risk_profile = profile)
  )

  red_weight <- site_repeat_rate(vs_df, strValueCol = "weight")
  red_sysbp <- site_repeat_rate(vs_df, strValueCol = "sysbp")

  weight_red_sites <- red_weight$invid[red_weight$rate >= 0.30]
  sysbp_red_sites <- red_sysbp$invid[red_sysbp$rate >= 0.30]

  # Same number of red sites per vital, but not the same sites -- a site red
  # on weight need not be red on sysbp.
  expect_equal(length(weight_red_sites), length(sysbp_red_sites))
  expect_false(setequal(weight_red_sites, sysbp_red_sites))
})

# ---- not-performed rows -----------------------------------------------------

test_that("Raw_VS blanks all vitals on rows where the measurement was not performed (#143)", {
  set.seed(4816)

  data <- make_vs_test_data(n_subjects = 50, n_visits = 8)
  vs_df <- generate_domain_from_registry(
    "Raw_VS",
    make_vs_context(data, spec = make_vs_full_spec())
  )

  not_performed <- vs_df$vsperf_std == "N"
  performed <- vs_df$vsperf_std == "Y"

  # The fixture is large enough that both branches must be present.
  expect_gt(sum(not_performed), 0)
  expect_gt(sum(performed), 0)

  for (col in VS_VITAL_COLS) {
    expect_true(
      all(is.na(vs_df[[col]][not_performed])),
      info = paste(col, "must be NA where vsperf_std == 'N'")
    )
    expect_false(
      any(is.na(vs_df[[col]][performed])),
      info = paste(col, "must be non-missing where vsperf_std == 'Y'")
    )
  }
})

test_that("Raw_VS site rates hold over performed measurements only, proving blank-then-inject order (#143)", {
  set.seed(5273)

  data <- make_vs_test_data(n_subjects = 60, n_visits = 12, n_sites = 10)
  profile <- list(
    dPctRed = 0.2,
    dPctAmber = 0.2,
    nWindowLength = 3,
    dRateNormal = 0.05,
    dRateAmber = 0.25,
    dRateRed = 0.45
  )

  vs_df <- generate_domain_from_registry(
    "Raw_VS",
    make_vs_context(data, vs_risk_profile = profile)
  )

  # `site_repeat_rate()` drops NA before forming windows, so this computes the
  # rate over performed measurements only. Under the wrong (inject-then-blank)
  # order the realized rates would be depressed relative to target.
  rates <- site_repeat_rate(vs_df, strValueCol = "weight")
  rates <- rates[!is.na(rates$rate), ]

  expect_equal(sum(rates$rate >= 0.30), 2)
  expect_equal(sum(rates$rate >= 0.20 & rates$rate < 0.30), 2)

  # There really were blanked rows in play.
  expect_gt(sum(is.na(vs_df$weight)), 0)
})

# ---- configuration ----------------------------------------------------------

test_that("Raw_VS falls back to generator defaults when no risk profile is supplied (#143)", {
  data <- make_vs_test_data(n_subjects = 40, n_visits = 8, n_sites = 10)

  set.seed(6391)
  with_profile <- generate_domain_from_registry(
    "Raw_VS",
    make_vs_context(data, vs_risk_profile = NULL)
  )

  # A context with no `vs_risk_profile` field at all must behave identically
  # to one that passes NULL -- existing callers are unaffected.
  context_absent <- make_vs_context(data)
  context_absent$vs_risk_profile <- NULL
  set.seed(6391)
  without_field <- generate_domain_from_registry("Raw_VS", context_absent)

  expect_equal(with_profile, without_field)

  # Defaults still produce a usable spread of site rates.
  rates <- site_repeat_rate(with_profile, strValueCol = "weight")
  expect_true(any(!is.na(rates$rate)))
})

test_that("Raw_VS honors a caller-supplied risk profile end to end (#143)", {
  data <- make_vs_test_data(n_subjects = 60, n_visits = 10, n_sites = 10)

  set.seed(7744)
  no_red <- generate_domain_from_registry(
    "Raw_VS",
    make_vs_context(data, vs_risk_profile = list(dPctRed = 0, dPctAmber = 0.2))
  )
  rates_no_red <- site_repeat_rate(no_red, strValueCol = "weight")
  expect_equal(sum(rates_no_red$rate >= 0.30, na.rm = TRUE), 0)

  set.seed(7744)
  many_red <- generate_domain_from_registry(
    "Raw_VS",
    make_vs_context(
      data,
      vs_risk_profile = list(dPctRed = 0.5, dPctAmber = 0.2)
    )
  )
  rates_many_red <- site_repeat_rate(many_red, strValueCol = "weight")
  expect_equal(sum(rates_many_red$rate >= 0.30, na.rm = TRUE), 5)
})

test_that("Raw_VS risk profile can be restricted to a subset of vitals (#143)", {
  set.seed(8205)

  data <- make_vs_test_data(n_subjects = 50, n_visits = 10, n_sites = 8)
  vs_df <- generate_domain_from_registry(
    "Raw_VS",
    make_vs_context(
      data,
      spec = make_vs_full_spec(),
      vs_risk_profile = list(
        dPctRed = 0.25,
        dPctAmber = 0.25,
        vVitals = c("weight", "pulse")
      )
    )
  )

  # Targeted vitals show red sites; untargeted ones do not.
  expect_gt(
    sum(
      site_repeat_rate(vs_df, strValueCol = "weight")$rate >= 0.30,
      na.rm = TRUE
    ),
    0
  )
  expect_equal(
    sum(
      site_repeat_rate(vs_df, strValueCol = "sysbp")$rate >= 0.30,
      na.rm = TRUE
    ),
    0
  )
})

test_that("validate_study_config rejects a malformed vs_risk_profile (#143)", {
  base <- create_study_config(
    "VS-VALIDATE",
    participant_count = 10,
    site_count = 3
  )

  bad_profiles <- list(
    percentage_over_one = list(dPctRed = 1.5, dPctAmber = 0.2),
    percentage_negative = list(dPctRed = -0.1, dPctAmber = 0.2),
    percentages_sum_over_one = list(dPctRed = 0.7, dPctAmber = 0.5),
    window_too_short = list(dPctRed = 0.1, dPctAmber = 0.2, nWindowLength = 1),
    window_not_whole = list(
      dPctRed = 0.1,
      dPctAmber = 0.2,
      nWindowLength = 2.5
    ),
    rate_out_of_range = list(dPctRed = 0.1, dPctAmber = 0.2, dRateRed = 1.4),
    unknown_vital = list(
      dPctRed = 0.1,
      dPctAmber = 0.2,
      vVitals = c("weight", "nonesuch")
    ),
    not_a_list = "red"
  )

  for (nm in names(bad_profiles)) {
    config <- base
    config$study_params$vs_risk_profile <- bad_profiles[[nm]]
    expect_error(validate_study_config(config), info = nm)
  }

  # A well-formed profile validates.
  good <- base
  good$study_params$vs_risk_profile <- list(
    dPctRed = 0.1,
    dPctAmber = 0.2,
    nWindowLength = 3,
    dRateNormal = 0.05,
    dRateAmber = 0.25,
    dRateRed = 0.45,
    vVitals = c("weight", "sysbp")
  )
  expect_true(validate_study_config(good))

  # NULL remains valid -- the argument is optional.
  null_profile <- base
  null_profile$study_params$vs_risk_profile <- NULL
  expect_true(validate_study_config(null_profile))
})

test_that("create_study_config carries vs_risk_profile into study_params (#143)", {
  profile <- list(dPctRed = 0.15, dPctAmber = 0.25, nWindowLength = 4)

  config <- create_study_config(
    "VS-CONFIG",
    participant_count = 20,
    site_count = 5,
    vs_risk_profile = profile
  )

  expect_equal(config$study_params$vs_risk_profile, profile)

  # Default is NULL, meaning generator defaults.
  plain <- create_study_config(
    "VS-PLAIN",
    participant_count = 20,
    site_count = 5
  )
  expect_null(plain$study_params$vs_risk_profile)
})

# ---- generator guards -------------------------------------------------------

test_that("vital generators skip site targeting when no sites are supplied (#143)", {
  subjects <- rep(paste0("S", 1:5), each = 6)

  set.seed(4182)
  values <- weight(length(subjects), subjects)

  expect_length(values, length(subjects))
  expect_false(anyNA(values))

  # With no sites there is nothing to target, so the values are plain draws.
  set.seed(4182)
  expect_equal(values, round(stats::rnorm(length(subjects), mean = 75, sd = 10), 1))
})

test_that("vital generation tolerates records with an unknown site (#143)", {
  subjects <- rep(paste0("S", 1:4), each = 6)
  sites <- rep(c("SITE1", NA_character_), each = 12)

  set.seed(5530)
  values <- .generate_vital(
    length(subjects), subjects, sites,
    performed = NULL, lRiskProfile = NULL, strVital = "weight"
  )

  expect_length(values, length(subjects))
  expect_false(anyNA(values))
})

test_that(".resolve_vs_risk_profile rejects a non-list profile (#143)", {
  expect_error(.resolve_vs_risk_profile("red"), "must be a list or NULL")
})

test_that(".generate_vital rejects an unrecognized vital (#143)", {
  expect_error(
    .generate_vital(5, rep("S1", 5), strVital = "glucose"),
    "Unknown vital: glucose"
  )
})

test_that("missing site IDs do not consume a risk band allocation (#148)", {
  # Two known sites plus a block of NA sites. The allocation must be computed
  # over the known sites only: 50% red of two known sites is exactly one red
  # site. Counting the pseudo-site would make it 50% of three, letting the
  # untargetable NA rows absorb a band.
  subjects <- rep(paste0("S", 1:12), each = 9)
  sites <- c(rep("SITE1", 36), rep("SITE2", 36), rep(NA_character_, 36))

  profile <- list(
    dPctRed = 0.5, dPctAmber = 0,
    dRateRed = 0.6, dRateAmber = 0.25, dRateNormal = 0
  )

  for (seed in c(6612, 1187, 4403, 8829)) {
    set.seed(seed)
    values <- .generate_vital(
      length(subjects), subjects, sites,
      performed = NULL, lRiskProfile = profile, strVital = "weight"
    )

    elevated <- vapply(
      c("SITE1", "SITE2"),
      function(site) {
        keep <- !is.na(sites) & sites == site
        count_identical_windows_naive(values[keep], subjects[keep], 3) > 0
      },
      logical(1)
    )

    expect_equal(sum(elevated), 1)
    expect_false(anyNA(values))
  }
})

test_that("all-missing site IDs leave values untargeted (#148)", {
  subjects <- rep(paste0("S", 1:4), each = 6)
  sites <- rep(NA_character_, length(subjects))

  set.seed(9034)
  values <- .generate_vital(
    length(subjects), subjects, sites,
    performed = NULL, lRiskProfile = NULL, strVital = "weight"
  )

  set.seed(9034)
  expect_equal(values, round(stats::rnorm(length(subjects), mean = 75, sd = 10), 1))
})
