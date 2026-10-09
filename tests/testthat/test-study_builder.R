test_that("create_study_config() builds default structure and required datasets", {
  config <- create_study_config("S1", participant_count = 20, site_count = 4)

  expect_s3_class(config, "study_config")
  expect_equal(config$study_params$study_id, "S1")
  expect_equal(config$study_params$outlier_intensity, 1)
  expect_null(config$study_params$risk_profile)
  expect_equal(config$temporal_config$snapshot_count, 5)
  expect_equal(
    names(config$dataset_configs),
    c("Raw_STUDY", "Raw_SITE", "Raw_SUBJ", "Raw_ENROLL")
  )
  expect_equal(config$dataset_configs$Raw_SITE$count_formula(config), 4)
  expect_equal(config$dataset_configs$Raw_SUBJ$count_formula(config), 20)
  expect_equal(config$dataset_configs$Raw_ENROLL$count_formula(config), 20)
})

test_that("create_study_config() stores risk_profile", {
  rp <- list(dPctRed = 0.2)
  config <- create_study_config(risk_profile = rp)
  expect_identical(config$study_params$risk_profile, rp)
})

test_that("set_outlier_config() updates intensity", {
  config <- set_outlier_config(create_study_config(), intensity = 3)
  expect_equal(config$study_params$outlier_intensity, 3)
})

test_that("set_temporal_config() updates only supplied fields", {
  config <- create_study_config()
  updated <- set_temporal_config(
    config,
    start_date = "2024-02-01", snapshot_count = 7,
    snapshot_width = "weeks", end_date = "2024-12-31"
  )
  expect_equal(updated$temporal_config$start_date, as.Date("2024-02-01"))
  expect_equal(updated$temporal_config$snapshot_count, 7)
  expect_equal(updated$temporal_config$snapshot_width, "weeks")
  expect_equal(updated$temporal_config$end_date, as.Date("2024-12-31"))

  expect_identical(set_temporal_config(config), config)
})

test_that("set_temporal_config() ignores an unparseable end_date", {
  config <- create_study_config()
  updated <- set_temporal_config(config, end_date = "not-a-date")
  expect_null(updated$temporal_config$end_date)
})

test_that("add_dataset_config() and remove_dataset_config() round-trip", {
  config <- create_study_config()
  config <- add_dataset_config(
    config, "Raw_AE",
    enabled = FALSE, count_formula = 5, growth_pattern = "exp",
    dependencies = "Raw_SUBJ", custom_args = list(a = 1)
  )
  ae <- config$dataset_configs$Raw_AE
  expect_false(ae$enabled)
  expect_equal(ae$count_formula, 5)
  expect_equal(ae$growth_pattern, "exp")
  expect_equal(ae$dependencies, "Raw_SUBJ")
  expect_equal(ae$custom_args, list(a = 1))

  config <- remove_dataset_config(config, "Raw_AE")
  expect_disjoint(names(config$dataset_configs), "Raw_AE")
})

test_that("validate_study_config() accepts a valid config", {
  expect_true(validate_study_config(create_study_config()))
})

test_that("validate_study_config() rejects invalid parameters", {
  config <- create_study_config()

  bad <- config
  bad$temporal_config$snapshot_count <- 0
  expect_error(validate_study_config(bad), "snapshot_count")

  bad <- config
  bad$study_params$participant_count <- 0
  expect_error(validate_study_config(bad), "participant_count")

  bad <- config
  bad$study_params$site_count <- 0
  expect_error(validate_study_config(bad), "site_count")

  for (val in list(-1, "a", c(1, 2), NA_real_)) {
    bad <- config
    bad$study_params$outlier_intensity <- val
    expect_error(validate_study_config(bad), "outlier_intensity")
  }
})

test_that("validate_study_config() validates risk_profile", {
  config <- create_study_config(risk_profile = list(dPctRed = "bad"))
  expect_error(validate_study_config(config))
})

test_that("create_standard_study_config() includes all standard datasets by default", {
  config <- create_standard_study_config("S2")
  expect_contains(
    names(config$dataset_configs),
    c(
      "Raw_STUDY", "Raw_SITE", "Raw_SUBJ", "Raw_ENROLL", "Raw_AE", "Raw_PD",
      "Raw_LB", "Raw_VISIT", "Raw_DATACHG", "Raw_DATAENT", "Raw_QUERY",
      "Raw_PK", "Raw_SDRGCOMP", "Raw_STUDCOMP", "Raw_IE", "Raw_COUNTRY",
      "Raw_Death", "Raw_Randomization", "Raw_OverallResponse"
    )
  )
})

test_that("create_standard_study_config() honors disabled domains", {
  config <- create_standard_study_config(
    "S3",
    study = FALSE, subjects = FALSE, sites_data = FALSE,
    adverse_events = FALSE, protocol_deviations = FALSE, lab_data = FALSE,
    subject_visits = FALSE, visit_schedule = FALSE, enrollment = FALSE,
    data_changes = FALSE, data_entry = FALSE, queries = FALSE,
    pharmacokinetics = FALSE, study_drug_completion = FALSE,
    study_completion = FALSE, inclusion_exclusion = FALSE, country = FALSE,
    death = FALSE, randomization = FALSE, overall_response = FALSE,
    outlier_intensity = 2, risk_profile = list(dPctRed = 0.1)
  )
  # Raw_ENROLL is added by create_study_config() and re-added only if enabled
  expect_named(config$dataset_configs, "Raw_ENROLL")
  expect_equal(config$study_params$outlier_intensity, 2)
  expect_equal(config$study_params$risk_profile, list(dPctRed = 0.1))
})

test_that("create_standard_study_config() keeps VISIT if either visit flag is set", {
  config <- create_standard_study_config("S4", subject_visits = FALSE, visit_schedule = TRUE)
  expect_contains(names(config$dataset_configs), "Raw_VISIT")
})

test_that("create_longitudinal_study_data() builds a classed structure", {
  study <- create_longitudinal_study_data("ST", raw_data = list(), config = list(a = 1))
  expect_s3_class(study, "longitudinal_study")
  expect_equal(study$study_id, "ST")
  expect_null(study$analytics)
  expect_equal(study$config, list(a = 1))
})

test_that("summarize_longitudinal_study() prints summary and returns study invisibly", {
  study <- make_test_study(4)
  expect_output(res <- summarize_longitudinal_study(study), "Study ID: ST")
  expect_identical(res, study)
  expect_output(summarize_longitudinal_study(study), "\\.\\.\\.")
  expect_output(summarize_longitudinal_study(study), "Domains: AE, LB")
})

test_that("summarize_longitudinal_study() handles missing snapshots config and names", {
  study <- make_test_study(2)
  study$config$snapshots <- NULL
  names(study$raw_data) <- NULL
  expect_output(summarize_longitudinal_study(study), "Snapshots: 2")
  expect_output(summarize_longitudinal_study(study), "Unknown")
})

test_that("summarize_longitudinal_study() is silent when verbose = FALSE", {
  expect_silent(summarize_longitudinal_study(make_test_study(), verbose = FALSE))
})

test_that("summarize_longitudinal_study() handles an empty study", {
  study <- create_longitudinal_study_data(
    "E", list(),
    list(participants = 1, sites = 1, snapshots = 0, interval = "1 month", domains = "AE")
  )
  expect_output(summarize_longitudinal_study(study), "Interval", fixed = TRUE)
  expect_snapshot(summarize_longitudinal_study(study))
})

test_that("run_longitudinal_analytics() stores analytics from generate_analytics_layers()", {
  study <- make_test_study(2)
  seen <- list()
  local_mocked_bindings(
    generate_analytics_layers = function(raw_data, config, verbose = FALSE) {
      seen <<- list(raw_data = raw_data, verbose = verbose)
      list(done = TRUE)
    }
  )
  res <- run_longitudinal_analytics(study, verbose = TRUE)
  expect_equal(res$analytics, list(done = TRUE))
  expect_true(seen$verbose)
  expect_identical(seen$raw_data, study$raw_data)

  # config$verbose takes precedence over the argument
  study$config$verbose <- FALSE
  run_longitudinal_analytics(study, verbose = TRUE)
  expect_false(seen$verbose)
})

test_that("run_longitudinal_reporting() errors without analytics", {
  expect_error(run_longitudinal_reporting(make_test_study()), "No analytics results")
})

test_that("run_longitudinal_reporting() stores reporting from generate_reporting_layers()", {
  study <- make_test_study(2)
  study$analytics <- list(a = 1)
  seen <- list()
  local_mocked_bindings(
    generate_reporting_layers = function(analytics_results, config, verbose = FALSE) {
      seen <<- list(analytics = analytics_results, verbose = verbose)
      list(rep = TRUE)
    }
  )
  res <- run_longitudinal_reporting(study, verbose = TRUE)
  expect_equal(res$reporting, list(rep = TRUE))
  expect_true(seen$verbose)
  expect_equal(seen$analytics, list(a = 1))

  study$config$verbose <- FALSE
  run_longitudinal_reporting(study, verbose = TRUE)
  expect_false(seen$verbose)
})

test_that("get_snapshot_data() returns a snapshot or errors out of range", {
  study <- make_test_study(3)
  expect_identical(get_snapshot_data(study, 2), study$raw_data[[2]])
  expect_error(get_snapshot_data(study, 0), "not available")
  expect_error(get_snapshot_data(study, 4), "Study has 3 snapshots")
})

test_that("get_domain_timeline() collects a domain across snapshots", {
  study <- make_test_study(3)
  study$raw_data[[2]]$Raw_AE <- NULL
  tl <- get_domain_timeline(study, "AE")
  expect_named(tl, c("snapshot_1", "snapshot_3"))
  expect_equal(get_domain_timeline(study, "NOPE"), list())
})

test_that("get_available_domains() covers registry, study, and empty study", {
  expect_equal(get_available_domains(), names(get_domain_registry()))
  expect_setequal(get_available_domains(make_test_study(2)), c("Raw_AE", "Raw_LB"))
  empty <- create_longitudinal_study_data("E", list(), list())
  expect_identical(get_available_domains(empty), character(0))
})
