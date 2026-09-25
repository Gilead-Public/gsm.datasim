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
      "project",
      "foldername",
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

test_that("Raw_VS omits invid, which Mapped_VS joins from Mapped_SUBJ (#113, #143)", {
  set.seed(514)

  data <- make_vs_test_data(n_subjects = 10, n_visits = 3)

  # Not in the authoritative VS.yaml spec, so it must not be generated.
  vs_df <- generate_domain_from_registry("Raw_VS", make_vs_context(data))
  expect_false("invid" %in% names(vs_df))

  # Even when a caller's spec asks for it, since real extracts lack it.
  spec_with_invid <- make_vs_test_spec()
  spec_with_invid$invid <- list(required = TRUE)
  with_invid <- generate_domain_from_registry(
    "Raw_VS",
    make_vs_context(data, spec = spec_with_invid)
  )
  expect_false("invid" %in% names(with_invid))

  # Site remains recoverable by the same subjid join the mapping performs.
  joined <- attach_vs_site(vs_df, data)
  expect_false(anyNA(joined$invid))
  expect_setequal(unique(joined$invid), unique(data$Raw_SUBJ$invid))
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

test_that("Raw_VS appends only newly enrolled subjects as the roster grows (#143)", {
  set.seed(4417)

  # The test above only exercises the `n <= 0` early return. This one covers
  # the case the delta model actually turns on: a snapshot where some subjects
  # are already frozen in `previous_data` and others are new.
  data1 <- make_vs_test_data(n_subjects = 10, n_visits = 4)
  vs_snapshot1 <- generate_domain_from_registry("Raw_VS", make_vs_context(data1))

  data2 <- make_vs_test_data(n_subjects = 15, n_visits = 4)
  context2 <- make_vs_context(data2, n = 15, start_date = as.Date("2012-02-01"))
  context2$previous_data <- list(Raw_VS = vs_snapshot1)
  vs_snapshot2 <- generate_domain_from_registry("Raw_VS", context2)

  # Frozen rows are carried through untouched.
  expect_equal(
    vs_snapshot2[seq_len(nrow(vs_snapshot1)), ],
    vs_snapshot1,
    ignore_attr = TRUE
  )

  # Every subject on the roster appears exactly once, and none twice.
  expect_equal(length(unique(vs_snapshot2$subjid)), 15)
  expect_equal(nrow(vs_snapshot2), 15 * 4)

  # The appended block must be the five *new* subjects, not a resample of the
  # cumulative roster, which would duplicate already-written visit rows.
  new_rows <- vs_snapshot2[-seq_len(nrow(vs_snapshot1)), ]
  expect_setequal(
    unique(new_rows$subjid),
    setdiff(data2$Raw_SUBJ$subjid, unique(vs_snapshot1$subjid))
  )
  # `foldername` is the visit column the spec's `source_col` renames to.
  expect_equal(sum(duplicated(vs_snapshot2[c("subjid", "foldername")])), 0)
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

test_that("Raw_VS registry adapter falls back to a default visit spec entry when the caller-supplied spec omits it (#113, #143)", {
  set.seed(6284)

  data <- make_vs_test_data(n_subjects = 15, n_visits = 4)

  # Spec deliberately omits `visit` -- the registry entry should
  # inject its own `required = TRUE` default rather than erroring.
  spec_without_visit <- make_vs_test_spec()
  spec_without_visit$visit <- NULL

  vs_df <- generate_domain_from_registry(
    "Raw_VS",
    make_vs_context(data, spec = spec_without_visit)
  )

  expect_s3_class(vs_df, "data.frame")
  # Renaming is driven by the caller's spec, which here has no `visit` entry
  # to carry `source_col`, so the column keeps its canonical name.
  expect_true("visit" %in% names(vs_df))
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
      paste(vs_df$subjid, vs_df$foldername),
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

  rates <- site_repeat_rate(attach_vs_site(vs_df, data), strValueCol = "weight")
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

  rates <- site_repeat_rate(attach_vs_site(vs_df, data), strValueCol = "weight")
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

  vs_sited <- attach_vs_site(vs_df, data)
  red_weight <- site_repeat_rate(vs_sited, strValueCol = "weight")
  red_sysbp <- site_repeat_rate(vs_sited, strValueCol = "sysbp")

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
  rates <- site_repeat_rate(attach_vs_site(vs_df, data), strValueCol = "weight")
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
  rates <- site_repeat_rate(attach_vs_site(with_profile, data), strValueCol = "weight")
  expect_true(any(!is.na(rates$rate)))
})

test_that("Raw_VS honors a caller-supplied risk profile end to end (#143)", {
  data <- make_vs_test_data(n_subjects = 60, n_visits = 10, n_sites = 10)

  set.seed(7744)
  no_red <- generate_domain_from_registry(
    "Raw_VS",
    make_vs_context(data, vs_risk_profile = list(dPctRed = 0, dPctAmber = 0.2))
  )
  rates_no_red <- site_repeat_rate(attach_vs_site(no_red, data), strValueCol = "weight")
  expect_equal(sum(rates_no_red$rate >= 0.30, na.rm = TRUE), 0)

  set.seed(7744)
  many_red <- generate_domain_from_registry(
    "Raw_VS",
    make_vs_context(
      data,
      vs_risk_profile = list(dPctRed = 0.5, dPctAmber = 0.2)
    )
  )
  rates_many_red <- site_repeat_rate(attach_vs_site(many_red, data), strValueCol = "weight")
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
      site_repeat_rate(attach_vs_site(vs_df, data), strValueCol = "weight")$rate >= 0.30,
      na.rm = TRUE
    ),
    0
  )
  expect_equal(
    sum(
      site_repeat_rate(attach_vs_site(vs_df, data), strValueCol = "sysbp")$rate >= 0.30,
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
    not_a_list = "red",
    unknown_field = list(dPctRed = 0.1, nRepeats = 3)
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

# The validator checks the profile as the generator will see it: omitted
# fields are filled from VS_DEFAULT_RISK_PROFILE before use, so validating the
# profile as written accepts partial profiles that cannot generate.
test_that("validate_vs_risk_profile checks the profile after defaults (#143)", {
  # dPctAmber is omitted, so it resolves to the default 0.2 -- 0.9 + 0.2 > 1.
  expect_error(
    validate_vs_risk_profile(list(dPctRed = 0.9)),
    "must not exceed 1"
  )
  # The message reports the effective values, not the written ones.
  expect_error(
    validate_vs_risk_profile(list(dPctRed = 0.9)),
    "after defaults are applied"
  )

  # Omitting dPctRed is the mirror case: resolves to the default 0.1.
  expect_error(
    validate_vs_risk_profile(list(dPctAmber = 0.95)),
    "must not exceed 1"
  )

  # A partial profile that is still valid once resolved is accepted.
  expect_true(validate_vs_risk_profile(list(dPctRed = 0.5)))
  expect_true(validate_vs_risk_profile(list(nWindowLength = 4)))

  # An explicit pair that sums within 1 is unaffected by defaults.
  expect_true(validate_vs_risk_profile(list(dPctRed = 0.9, dPctAmber = 0.05)))

  # A misspelled field is caught by name rather than silently ignored.
  expect_error(
    validate_vs_risk_profile(list(dPctRed = 0.1, dPctRedd = 0.2)),
    "unknown field\\(s\\): dPctRedd"
  )

  # vVitals must be a non-empty character vector; NULL means "all vitals".
  expect_error(
    validate_vs_risk_profile(list(vVitals = character(0))),
    "non-empty character vector"
  )
  expect_error(
    validate_vs_risk_profile(list(vVitals = 1:3)),
    "non-empty character vector"
  )
})

test_that("a partial vs_risk_profile is rejected at config time (#143)", {
  config <- create_study_config("VS-PARTIAL", participant_count = 10, site_count = 10)
  config$study_params$vs_risk_profile <- list(dPctRed = 0.9)

  # Previously this validated, then failed inside allocate_site_risk()
  # during generation against the resolved 0.9 + 0.2.
  expect_error(validate_study_config(config), "must not exceed 1")
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

test_that("missing site IDs do not consume a risk band allocation (#143)", {
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

test_that("all-missing site IDs leave values untargeted (#143)", {
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

test_that("Raw_VS vs_dt is a Date even when Raw_VISIT supplies character dates (#143)", {
  set.seed(3390)

  # The real `Raw_VISIT` generator emits "%Y-%m-%d" strings, not `Date`s. The
  # `vs_dt` column must still honour the `Date` contract the previous
  # `generic_date` generator established.
  data <- make_vs_test_data(n_subjects = 10, n_visits = 5, strDateClass = "character")
  expect_type(data$Raw_VISIT$visit_dt, "character")

  vs_df <- generate_domain_from_registry("Raw_VS", make_vs_context(data))

  expect_s3_class(vs_df$vs_dt, "Date")

  expected <- as.Date(data$Raw_VISIT$visit_dt)[
    match(
      paste(vs_df$subjid, vs_df$foldername),
      paste(data$Raw_VISIT$subjid, data$Raw_VISIT$instancename)
    )
  ]
  expect_equal(vs_df$vs_dt, expected)

  for (s in unique(vs_df$subjid)) {
    expect_false(is.unsorted(vs_df$vs_dt[vs_df$subjid == s]), info = paste("subject", s))
  }
})

test_that("Raw_VS vs_dt class does not depend on the schedule's storage type (#143)", {
  set.seed(8827)
  date_df <- generate_domain_from_registry(
    "Raw_VS",
    make_vs_context(make_vs_test_data(n_subjects = 8, n_visits = 4, strDateClass = "Date"))
  )

  set.seed(8827)
  chr_df <- generate_domain_from_registry(
    "Raw_VS",
    make_vs_context(make_vs_test_data(n_subjects = 8, n_visits = 4, strDateClass = "character"))
  )

  expect_equal(date_df$vs_dt, chr_df$vs_dt)
  expect_equal(date_df, chr_df)
})

# ---- the authoritative gsm.mapping spec ------------------------------------

# These drive generation from the real `VS.yaml` rather than a hand-built
# fixture. The fixture is what let the earlier `invid`/`bmi` guesses survive:
# it described columns real VS extracts do not have.
test_that("Raw_VS generates exactly the columns VS.yaml specifies (#113, #143)", {
  skip_if_not_installed("gsm.mapping", minimum_version = "1.1.6.9000")
  set.seed(6104)

  spec <- load_specs("workflow/1_mappings", NULL, "gsm.mapping")$Raw_VS
  skip_if(is.null(spec), "VS.yaml not available in the installed gsm.mapping")

  data <- make_vs_test_data(n_subjects = 8, n_visits = 5)
  vs_df <- generate_domain_from_registry("Raw_VS", make_vs_context(data, spec = spec))

  # `source_col` names the column in the raw extract, so that is what a raw
  # domain must emit.
  expected <- vapply(
    names(spec),
    function(nm) spec[[nm]]$source_col %||% nm,
    character(1)
  )

  expect_setequal(names(vs_df), unname(expected))
  # No invented columns, in either direction.
  expect_length(setdiff(names(vs_df), expected), 0)
  expect_length(setdiff(expected, names(vs_df)), 0)
})

test_that("Raw_VS emits no invid or bmi column under the real spec (#113, #143)", {
  skip_if_not_installed("gsm.mapping", minimum_version = "1.1.6.9000")
  set.seed(2219)

  spec <- load_specs("workflow/1_mappings", NULL, "gsm.mapping")$Raw_VS
  skip_if(is.null(spec), "VS.yaml not available in the installed gsm.mapping")

  vs_df <- generate_domain_from_registry(
    "Raw_VS",
    make_vs_context(make_vs_test_data(n_subjects = 6, n_visits = 4), spec = spec)
  )

  # `invid` is joined from Mapped_SUBJ during Mapped_VS construction, and the
  # vitals carry `bsa`, not `bmi`.
  expect_false("invid" %in% names(vs_df))
  expect_false("bmi" %in% names(vs_df))
  expect_true("bsaentry" %in% names(vs_df))
})

test_that("Raw_VS generates through the standard study config (#113, #143)", {
  skip_if_not_installed("gsm.mapping", minimum_version = "1.1.6.9000")
  set.seed(4471)

  # Exercises the config path end to end, which the hand-built fixtures
  # bypassed entirely -- it failed outright before this fix.
  config <- add_dataset_config(
    create_standard_study_config("DEMO", participant_count = 15, site_count = 3),
    "Raw_VS"
  )
  snapshot <- suppressMessages(generate_study_data(config))[[1]]

  expect_s3_class(snapshot$Raw_VS, "data.frame")
  expect_gt(nrow(snapshot$Raw_VS), 0)
  expect_s3_class(snapshot$Raw_VS$vs_dt, "Date")
  expect_false("invid" %in% names(snapshot$Raw_VS))
  # No dot-flattened list columns escaping the split_vars processing.
  expect_false(any(grepl(".", names(snapshot$Raw_VS), fixed = TRUE)))
})

# Specs predating gsm.mapping's VS.yaml name the visit column `instancename`
# or `foldername`. Adding `visit` alongside one of those left the original to
# fall through to the same-named `Raw_VISIT` generator with no
# `possible_visits`, producing an all-`NA` column instead of the schedule.
test_that("Raw_VS fills legacy visit aliases from the schedule (#143)", {
  for (alias in c("instancename", "foldername")) {
    set.seed(8823)

    spec <- make_vs_test_spec()
    spec$visit <- NULL
    spec[[alias]] <- list(required = TRUE, type = "character")

    data <- make_vs_test_data(n_subjects = 6, n_visits = 4)
    vs_df <- generate_domain_from_registry("Raw_VS", make_vs_context(data, spec = spec))

    expect_true(alias %in% names(vs_df), info = alias)
    expect_false(anyNA(vs_df[[alias]]), info = alias)
    expect_setequal(unique(vs_df[[alias]]), unique(data$Raw_VISIT$instancename))
    # The canonical name is not invented alongside the caller's alias.
    expect_false("visit" %in% names(vs_df), info = alias)
  }
})

test_that("Raw_VS fills every declared visit alias (#143)", {
  set.seed(1467)

  spec <- make_vs_test_spec()
  spec$instancename <- list(required = TRUE, type = "character")

  data <- make_vs_test_data(n_subjects = 5, n_visits = 4)
  vs_df <- generate_domain_from_registry("Raw_VS", make_vs_context(data, spec = spec))

  # `visit` carries source_col = "foldername"; `instancename` stands alone.
  expect_false(anyNA(vs_df$foldername))
  expect_false(anyNA(vs_df$instancename))
  expect_identical(vs_df$foldername, vs_df$instancename)
})

test_that("create_standard_study_config forwards vs_risk_profile (#143)", {
  profile <- list(dPctRed = 0.2, dPctAmber = 0.3, nWindowLength = 4)

  config <- create_standard_study_config(
    "DEMO",
    participant_count = 10, site_count = 3,
    vs_risk_profile = profile
  )

  expect_identical(config$study_params$vs_risk_profile, profile)
  expect_no_error(validate_study_config(config))
})

test_that("Raw_VS risk bands persist across snapshots (#143)", {
  # The regression this guards: bands were drawn independently inside every
  # snapshot, so the elevated site moved from snapshot to snapshot and no site
  # held a persistently elevated rate over the life of the study.
  set.seed(5518)

  config <- add_dataset_config(
    create_standard_study_config(
      "DEMO",
      participant_count = 100,
      site_count = 10,
      vs_risk_profile = list(dPctRed = 0.1, dPctAmber = 0.2)
    ),
    "Raw_VS"
  )
  data <- suppressMessages(generate_study_data(config))

  top_site_by_snapshot <- vapply(
    data,
    function(snapshot) {
      vs_sited <- attach_vs_site(snapshot$Raw_VS, snapshot)
      rates <- site_repeat_rate(as.data.frame(vs_sited), strValueCol = "sysbp")
      rates <- rates[!is.na(rates$rate), ]
      rates$invid[which.max(rates$rate)]
    },
    character(1)
  )

  # Snapshot 1 has a single enrolled site, which may legitimately be normal --
  # with one site there is no "most elevated" site to speak of. The guarantee
  # applies once the study has sites to distinguish between.
  multi_site <- top_site_by_snapshot[-1]
  expect_equal(length(unique(multi_site)), 1)

  # ...and that site is genuinely elevated, not merely the argmax of noise.
  final <- data[[length(data)]]
  final_rates <- site_repeat_rate(
    as.data.frame(attach_vs_site(final$Raw_VS, final)),
    strValueCol = "sysbp"
  )
  expect_gte(final_rates$rate[final_rates$invid == multi_site[[1]]], 0.30)
})

test_that("Raw_VS bands stay independent per vital across snapshots (#143)", {
  # Persistence must not collapse the vitals onto one shared band: each vital
  # is its own KRI, so a site red on sysbp need not be red on pulse.
  set.seed(7731)

  config <- add_dataset_config(
    create_standard_study_config(
      "DEMO",
      participant_count = 100,
      site_count = 10,
      vs_risk_profile = list(dPctRed = 0.1, dPctAmber = 0.2)
    ),
    "Raw_VS"
  )
  data <- suppressMessages(generate_study_data(config))
  final <- data[[length(data)]]
  vs_sited <- as.data.frame(attach_vs_site(final$Raw_VS, final))

  red_for <- function(vital) {
    rates <- site_repeat_rate(vs_sited, strValueCol = vital)
    sort(rates$invid[!is.na(rates$rate) & rates$rate >= 0.30])
  }

  expect_false(identical(red_for("sysbp"), red_for("pulse")))
})
