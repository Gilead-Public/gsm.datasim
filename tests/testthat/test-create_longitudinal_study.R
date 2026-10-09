test_that("create_longitudinal_study() builds a study without analytics", {
  test_at_log_threshold()
  local_mocked_bindings(
    generate_study_snapshots = function(...) fake_raw_data(3)
  )
  study <- create_longitudinal_study(
    "S-1", participants = 10, sites = 2, snapshots = 3, domains = "AE"
  )
  expect_s3_class(study, "longitudinal_study")
  expect_length(study$raw_data, 3)
  expect_equal(study$config$domains, "AE")
  expect_null(study$analytics)
  expect_null(study$reporting)
})

test_that("create_longitudinal_study() validates inputs", {
  test_at_log_threshold()
  expect_error(create_longitudinal_study(participants = 0), "Participants must be positive")
})

test_that("create_longitudinal_study() runs analytics and reporting when requested", {
  test_at_log_threshold()
  seen <- list()
  local_mocked_bindings(
    generate_study_snapshots = function(...) fake_raw_data(2),
    generate_analytics_layers = function(raw_data, config, verbose) {
      seen$analytics_config <<- config
      list(a = 1)
    },
    generate_reporting_layers = function(analytics_results, config, verbose) {
      seen$reporting_input <<- analytics_results
      list(r = 1)
    }
  )

  analytics_only <- create_longitudinal_study(run_analytics = TRUE)
  expect_equal(analytics_only$analytics, list(a = 1))
  expect_null(analytics_only$reporting)
  expect_equal(seen$analytics_config$domains, c("AE", "LB", "VISIT"))

  both <- expect_output(
    create_longitudinal_study(run_analytics = TRUE, run_reporting = TRUE, verbose = TRUE),
    "Running reporting pipeline"
  )
  expect_equal(both$reporting, list(r = 1))
  expect_equal(seen$reporting_input, list(a = 1))

  # run_reporting is ignored without run_analytics
  expect_null(create_longitudinal_study(run_reporting = TRUE)$reporting)
})

test_that("quick_longitudinal_study() rejects unknown study types", {
  test_at_log_threshold()
  expect_error(quick_longitudinal_study(study_type = "other"), "study_type must be")
})

test_that("quick_longitudinal_study() standard type normalizes the study ID", {
  test_at_log_threshold()
  local_mocked_bindings(
    generate_study_snapshots = function(...) fake_raw_data(2),
    generate_analytics_layers = function(...) list(results = list(Analysis_a = 1, other = 2)),
    generate_reporting_layers = function(...) list(s1 = list(), s2 = NULL)
  )
  study <- quick_longitudinal_study("gs-us 1.2", participants = 10, sites = 2, months_duration = 2)
  expect_equal(study$study_id, "GS-US-1-2")
  expect_equal(study$config$analytics_package, "gsm.kri")
  expect_null(study$analytics)

  expect_snapshot(
    study <- quick_longitudinal_study(
      "s", participants = 10, sites = 2, months_duration = 2,
      include_pipeline = TRUE, verbose = TRUE
    )
  )
})

test_that("quick_longitudinal_study() summarizes multi-snapshot analytics when verbose", {
  test_at_log_threshold()
  local_mocked_bindings(
    generate_study_snapshots = function(...) fake_raw_data(2),
    generate_analytics_layers = function(...) {
      list(
        s1 = list(results = list(Analysis_a = 1, site_b = 2, ignored = 3)),
        s2 = NULL,
        s3 = list(no_results = 1)
      )
    },
    generate_reporting_layers = function(...) list(s1 = list())
  )
  expect_snapshot(
    study <- quick_longitudinal_study("s", months_duration = 2, include_pipeline = TRUE, verbose = TRUE)
  )
})

test_that("quick_longitudinal_study() handles empty analytics when verbose", {
  test_at_log_threshold()
  local_mocked_bindings(
    generate_study_snapshots = function(...) fake_raw_data(2),
    generate_analytics_layers = function(...) list(),
    generate_reporting_layers = function(...) list()
  )
  expect_snapshot(
    study <- quick_longitudinal_study("s", months_duration = 2, include_pipeline = TRUE, verbose = TRUE)
  )
})

test_that("quick_longitudinal_study() endpoints type builds config from endpoint domains", {
  test_at_log_threshold()
  seen <- list()
  local_mocked_bindings(
    get_endpoints_domains = function() {
      data.frame(domain = c("X", "Y"), package = "gsm.endpoints")
    },
    generate_raw_data_for_endpoints = function(config, domain_pkg_df) {
      seen$config <<- config
      fake_raw_data(2)
    },
    generate_analytics_layers = function(...) list(a = 1),
    generate_reporting_layers = function(...) list(r = 1)
  )

  study <- quick_longitudinal_study("e", study_type = "endpoints", months_duration = 2)
  expect_equal(study$config$study_type, "endpoints")
  expect_equal(study$config$domains, c("X", "Y"))
  expect_contains(names(seen$config$dataset_configs), c("Raw_X", "Raw_Y"))
  expect_equal(seen$config$temporal_config$snapshot_count, 2)
  expect_null(study$analytics)

  expect_snapshot(
    piped <- quick_longitudinal_study(
      "e", study_type = "endpoints", months_duration = 2,
      include_pipeline = TRUE, verbose = TRUE
    )
  )
  expect_equal(piped$analytics, list(a = 1))
  expect_equal(piped$reporting, list(r = 1))
})

test_that("create_multiple_longitudinal_studies() generates sequentially and verbosely", {
  test_at_log_threshold()
  local_mocked_bindings(
    create_longitudinal_study = function(study_id, ...) {
      create_longitudinal_study_data(
        study_id, fake_raw_data(2),
        list(participants = 5, sites = 1, domains = "AE")
      )
    }
  )
  expect_snapshot(
    studies <- create_multiple_longitudinal_studies(
      c("A", "B"),
      run_analytics = TRUE, run_reporting = TRUE, verbose = TRUE
    )
  )
  expect_s3_class(studies, "multiple_longitudinal_studies")
  expect_named(studies, c("A", "B"))
})

test_that("create_multiple_longitudinal_studies() applies per-study vectors and overrides", {
  test_at_log_threshold()
  seen <- list()
  local_mocked_bindings(
    create_longitudinal_study = function(study_id, participants, domains, ...) {
      seen[[study_id]] <<- list(participants = participants, domains = domains)
      create_longitudinal_study_data(study_id, list(), list(participants = 1, sites = 1, domains = "AE"))
    }
  )
  create_multiple_longitudinal_studies(
    c("A", "B"),
    participants = c(10, 20), sites = c(1, 2), snapshots = c(2, 3),
    interval = c("1 month", "2 weeks"), outlier_intensity = c(1, 2),
    study_configs = list(B = list(domains = "PD"))
  )
  expect_equal(seen$A, list(participants = 10, domains = c("AE", "LB", "VISIT")))
  expect_equal(seen$B, list(participants = 20, domains = "PD"))
})

test_that("create_multiple_longitudinal_studies() exports studies when requested", {
  test_at_log_threshold()
  exported <- character()
  local_mocked_bindings(
    create_longitudinal_study = function(study_id, ...) {
      create_longitudinal_study_data(study_id, list(), list(participants = 1, sites = 1, domains = "AE"))
    },
    export_study_data = function(study, study_folder, ...) {
      exported <<- c(exported, study_folder)
      invisible(NULL)
    }
  )
  quiet <- create_multiple_longitudinal_studies(c("A", "B"), export_studies = TRUE)
  expect_equal(exported, c("A", "B"))

  expect_snapshot(
    invisible(create_multiple_longitudinal_studies(c("A"), export_studies = TRUE, verbose = TRUE))
  )
})

test_that("create_multiple_longitudinal_studies() supports parallel generation", {
  test_at_log_threshold()
  local_mocked_bindings(
    create_longitudinal_study = function(study_id, ...) create_longitudinal_study_data(study_id, list(), list(participants = 1, sites = 1, domains = "AE"))
  )
  local_mocked_bindings(
    detectCores = function(...) 4L,
    makeCluster = function(...) "cluster",
    clusterEvalQ = function(...) NULL,
    stopCluster = function(...) NULL,
    clusterMap = function(cl, fun, ..., SIMPLIFY = FALSE) mapply(fun, ..., SIMPLIFY = FALSE),
    .package = "parallel"
  )
  expect_snapshot(
    studies <- create_multiple_longitudinal_studies(c("A", "B"), parallel = TRUE, verbose = TRUE)
  )
  expect_named(studies, c("A", "B"))
})

test_that("create_multiple_longitudinal_studies() falls back when parallel is unavailable", {
  test_at_log_threshold()
  local_mocked_bindings(
    create_longitudinal_study = function(study_id, ...) create_longitudinal_study_data(study_id, list(), list(participants = 1, sites = 1, domains = "AE")),
    pkg_available = function(pkg) FALSE
  )
  expect_warning(
    studies <- create_multiple_longitudinal_studies("A", parallel = TRUE),
    "proceeding sequentially"
  )
  expect_named(studies, "A")
})

test_that("print and summary methods handle optional fields and results", {
  test_at_log_threshold()
  studies <- structure(
    list(
      A = fake_study(analytics = list(a = 1), reporting = list(r = 1)),
      B = create_longitudinal_study_data("B", list(), list())
    ),
    class = c("multiple_longitudinal_studies", "list")
  )

  expect_snapshot(
    print(studies)
  )

  summ <- summary(studies)
  expect_equal(summ$analytics_completed, 1)
  expect_equal(summ$reporting_completed, 1)
  expect_equal(summ$unique_domains, c("AE", "LB"))

  expect_snapshot(
    print(summ)
  )
})

test_that("export_multiple_studies() validates input and returns export paths", {
  test_at_log_threshold()
  expect_error(export_multiple_studies(list()), "multiple_longitudinal_studies")

  studies <- structure(
    list(A = fake_study(), B = fake_study()),
    class = c("multiple_longitudinal_studies", "list")
  )
  local_mocked_bindings(
    export_study_data = function(study, output_dir, study_folder, ...) file.path(output_dir, study_folder)
  )
  tmp <- withr::local_tempdir()

  paths <- export_multiple_studies(studies, output_dir = tmp)
  expect_equal(paths, list(A = file.path(tmp, "A"), B = file.path(tmp, "B")))

  expect_output(
    export_multiple_studies(studies, output_dir = tmp, verbose = TRUE),
    "All 2 studies exported to"
  )
})

test_that("study_portfolio() validates variants", {
  test_at_log_threshold()
  expect_error(study_portfolio(list(1, 2)), "fully named list")
  expect_error(study_portfolio("a"), "fully named list")
  expect_error(study_portfolio(list(a = list(), list())), "fully named list")
})

test_that("study_portfolio() extracts vectorised params and per-study overrides", {
  test_at_log_threshold()
  seen <- NULL
  local_mocked_bindings(
    create_multiple_longitudinal_studies = function(...) {
      seen <<- list(...)
      "ok"
    }
  )
  res <- study_portfolio(
    list(
      SMALL = list(participants = 50, domains = "AE"),
      BIG = list(sites = 25, outlier_intensity = 2)
    ),
    participants = 100, sites = 10, snapshots = 6,
    verbose = TRUE
  )
  expect_equal(res, "ok")
  expect_equal(seen$study_names, c("SMALL", "BIG"))
  expect_equal(unname(seen$participants), c(50, 100))
  expect_equal(unname(seen$sites), c(10, 25))
  expect_equal(unname(seen$outlier_intensity), c(1, 2))
  expect_equal(seen$study_configs, list(SMALL = list(domains = "AE")))
  expect_true(seen$verbose)

  study_portfolio(list(A = list(participants = 5)))
  expect_null(seen$study_configs)
})

test_that("create_multiple_longitudinal_studies() validates study names", {
  test_at_log_threshold()
  expect_error(create_multiple_longitudinal_studies(character(0)), "at least one study name")
  expect_error(create_multiple_longitudinal_studies(c("A", "A")), "duplicate values")
})
