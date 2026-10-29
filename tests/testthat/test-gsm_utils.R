test_that("generate_study_snapshots preserves longitudinal continuity (#165)", {
  test_at_log_threshold()
  set.seed(1234)
  snapshots <- generate_study_snapshots(
    study_id = "TEST",
    participants = 60,
    sites = 6,
    snapshots = 3,
    interval = "1 month",
    mappings = ensure_core_mappings(c("AE"))
  )

  expect_equal(length(snapshots), 3)
  expect_named(snapshots, c("2012-01-31", "2012-02-29", "2012-03-31"))

  ids <- lapply(snapshots, function(x) x$Raw_SUBJ$subjid)

  # Subjects enrolled in earlier snapshots persist into later ones.
  expect_in(ids[[1]], ids[[2]])
  expect_in(ids[[2]], ids[[3]])

  # Cohort grows rather than being regenerated from scratch.
  expect_true(length(ids[[1]]) < length(ids[[3]]))

  # Exposure accumulates beyond a single snapshot width.
  max_time <- vapply(
    snapshots,
    function(x) max(x$Raw_SUBJ$timeonstudy, na.rm = TRUE),
    numeric(1)
  )
  expect_true(all(diff(max_time) > 0))
  expect_gt(max_time[[3]], 28)

  # Enrollment dates are carried forward, not reset per snapshot.
  first_enroll <- vapply(
    snapshots,
    function(x) as.character(min(x$Raw_SUBJ$enrolldt, na.rm = TRUE)),
    character(1)
  )
  expect_equal(length(unique(first_enroll)), 1)
})

# --- config helpers --------------------------------------------------------

test_that("generate_raw_data_for_endpoints delegates to generate_snapshots_from_config", {
  test_at_log_threshold()
  seen <- NULL
  local_mocked_bindings(
    generate_snapshots_from_config = function(...) {
      seen <<- list(...)
      "snapshots"
    }
  )
  df <- data.frame(domain = "AE", package = "gsm.mapping")
  expect_equal(generate_raw_data_for_endpoints(create_study_config(), df), "snapshots")
  expect_identical(seen$domain_package_df, df)
  expect_equal(seen$default_package, "gsm.mapping")
  expect_false(seen$verbose)
})

test_that("get_enabled_mapping_names strips the Raw_ prefix and drops disabled datasets", {
  test_at_log_threshold()
  config <- create_study_config() |>
    add_dataset_config("Raw_AE", enabled = TRUE) |>
    add_dataset_config("Raw_LB", enabled = FALSE)
  expect_equal(get_enabled_mapping_names(config), c("STUDY", "SITE", "SUBJ", "ENROLL", "AE"))
})

test_that("resolve_domain_package_df handles default, lookup, fallback and empty cases", {
  test_at_log_threshold()
  config <- create_study_config() |>
    add_dataset_config("Raw_AE", enabled = TRUE)

  default <- resolve_domain_package_df(config)
  expect_equal(default$domain, c("STUDY", "SITE", "SUBJ", "ENROLL", "AE"))
  expect_true(all(default$package == "gsm.mapping"))

  lookup <- data.frame(
    domain = c("AE", "AE", "STUDY"),
    package = c("pkg.a", "pkg.dup", "pkg.s"),
    stringsAsFactors = FALSE
  )
  resolved <- resolve_domain_package_df(config, lookup, default_package = "fallback")
  expect_equal(resolved$domain, c("STUDY", "SITE", "SUBJ", "ENROLL", "AE"))
  expect_equal(resolved$package, c("pkg.s", "fallback", "fallback", "fallback", "pkg.a"))

  disabled <- config
  disabled$dataset_configs <- list(Raw_AE = list(enabled = FALSE))
  empty <- resolve_domain_package_df(disabled, lookup)
  expect_equal(nrow(empty), 0)
  expect_named(empty, c("domain", "package"))
})

test_that("resolve_domain_count picks the right count for each domain", {
  test_at_log_threshold()
  counts <- list(
    ae_count = c(1, 2), enrollment_count = list(3, 4), site_count = c(5, 6),
    pd_count = c(7, 8), subject_count = c(9, 10), sdrgcomp_count = c(11, 12),
    studcomp_count = c(13, 14), consents_count = c(15, 16), death_count = c(17, 18),
    anticancer_count = c(19, 20)
  )
  expected <- c(
    Raw_AE = 2, Raw_ENROLL = 4, Raw_SITE = 6, Raw_PD = 8, Raw_SUBJ = 10,
    Raw_SDRGCOMP = 12, Raw_STUDCOMP = 14, Raw_Consents = 16, Raw_Death = 18,
    Raw_AntiCancer = 20, Raw_IE = 4, Raw_Other = 10
  )
  for (type in names(expected)) {
    expect_equal(resolve_domain_count(type, counts, 2), expected[[type]], info = type)
  }
})

# --- generation loop -------------------------------------------------------

test_that("run_domain_generation_loop derives dates and handles gilda, IE and Randomization", {
  test_at_log_threshold()
  skip_if_not_installed("gsm.mapping")
  test_at_log_threshold()
  local_mocked_bindings(raw_gilda_study_data = function(...) data.frame(x = 1))

  config <- create_study_config("LOOP", participant_count = 30, site_count = 4) |>
    set_temporal_config(snapshot_count = 2, snapshot_width = "months")
  specs <- load_specs(
    workflow_path = "workflow/1_mappings",
    mappings = c("STUDY", "SITE", "SUBJ", "ENROLL", "IE", "Randomization"),
    package = "gsm.mapping"
  ) |>
    prepare_combined_specs_for_generation()

  snaps <- run_domain_generation_loop(specs, config, source_domains = c("gilda_STUDY", "IE"))

  expect_length(snaps, 2)
  expect_named(snaps, as.character(as.Date("2023-01-01") + c(0, 31) + 28))
  for (snap in snaps) expect_contains(names(snap), "raw_gilda_study_data")
  expect_contains(names(snaps[[2]]), c("Raw_IE", "Raw_Randomization"))
  expect_equal(anyDuplicated(snaps[[1]]$Raw_Randomization$subjid), 0L)

  no_gilda <- run_domain_generation_loop(specs, config, source_domains = "IE")
  expect_disjoint(names(no_gilda[[1]]), "raw_gilda_study_data")
})

test_that("run_domain_generation_loop honors explicit snapshot_dates and legacy fallback", {
  test_at_log_threshold()
  skip_if_not_installed("gsm.mapping")
  test_at_log_threshold()
  local_mocked_bindings(generate_domain_from_registry = function(...) NULL)

  config <- create_study_config("LEGACY", participant_count = 20, site_count = 3) |>
    set_temporal_config(snapshot_count = 1)
  config$temporal_config$snapshot_dates <- as.Date("2020-05-31")
  specs <- load_specs(
    workflow_path = "workflow/1_mappings",
    mappings = c("STUDY", "SITE", "SUBJ", "ENROLL", "AE"),
    package = "gsm.mapping"
  ) |>
    prepare_combined_specs_for_generation()

  snaps <- run_domain_generation_loop(specs, config, source_domains = "AE")
  expect_named(snaps, "2020-06-28")
  expect_contains(names(snaps[[1]]), "Raw_AE")
})

test_that("generate_snapshots_from_config handles defaults, verbosity and empty configs", {
  test_at_log_threshold()
  loop_seen <- NULL
  local_mocked_bindings(
    load_specs = function(...) list(),
    prepare_combined_specs_for_generation = function(x) x,
    run_domain_generation_loop = function(combined_specs, config, source_domains) {
      loop_seen <<- list(source_domains = source_domains, option = getOption("gsm.datasim.outlier_intensity"))
      list(snap = list(Raw_A = 1))
    }
  )

  config <- create_study_config()
  config$study_params$outlier_intensity <- NULL
  expect_output(
    res <- generate_snapshots_from_config(config, verbose = TRUE),
    "Generating raw data for package gsm.mapping"
  )
  expect_equal(res, list(snap = list(Raw_A = 1)))
  expect_equal(loop_seen$option, 1)

  config$study_params$outlier_intensity <- 3
  old <- getOption("gsm.datasim.outlier_intensity")
  generate_snapshots_from_config(config)
  expect_equal(loop_seen$option, 3)
  expect_identical(getOption("gsm.datasim.outlier_intensity"), old)

  config$dataset_configs <- list(Raw_AE = list(enabled = FALSE))
  expect_equal(generate_snapshots_from_config(config), list())
})

test_that("generate_snapshots_from_config merges snapshots across packages", {
  test_at_log_threshold()
  local_mocked_bindings(
    load_specs = function(...) list(),
    prepare_combined_specs_for_generation = function(x) x,
    run_domain_generation_loop = function(combined_specs, config, source_domains) {
      list(d1 = setNames(list(1), paste0("Raw_", source_domains[1])), d2 = list(Raw_Z = 2))
    }
  )
  config <- create_study_config() |>
    add_dataset_config("Raw_AE", enabled = TRUE)
  df <- data.frame(
    domain = c("STUDY", "SITE", "SUBJ", "ENROLL", "AE"),
    package = c("pkg.a", "pkg.a", "pkg.a", "pkg.a", "pkg.b")
  )
  res <- generate_snapshots_from_config(config, df)
  expect_named(res, c("d1", "d2"))
  expect_named(res$d1, c("Raw_STUDY", "Raw_AE"))
})

# --- study snapshot helpers ------------------------------------------------

test_that("validate_study_inputs rejects invalid inputs", {
  test_at_log_threshold()
  expect_error(validate_study_inputs(0, 1, 1, "AE"), "Participants")
  expect_error(validate_study_inputs(1, 0, 1, "AE"), "Sites")
  expect_error(validate_study_inputs(1, 1, 0, "AE"), "Snapshots")
  expect_error(validate_study_inputs(1, 1, 1, character(0)), "At least one domain")
  expect_null(validate_study_inputs(1, 1, 1, "AE"))
})

test_that("ensure_core_mappings prefixes domains and prepends core mappings", {
  test_at_log_threshold()
  expect_equal(
    ensure_core_mappings(c("AE", "SUBJ")),
    c("Raw_STUDY", "Raw_SITE", "Raw_SUBJ", "Raw_ENROLL", "Raw_AE")
  )
})

test_that("parse_interval_to_snapshot_width maps interval strings", {
  test_at_log_threshold()
  expect_equal(parse_interval_to_snapshot_width("1 Month"), "months")
  expect_equal(parse_interval_to_snapshot_width("2 weeks"), "weeks")
  expect_equal(parse_interval_to_snapshot_width("30 days"), "days")
  expect_equal(parse_interval_to_snapshot_width("quarterly"), "months")
})

test_that("generate_study_snapshots supports base_date, other intervals and verbose", {
  test_at_log_threshold()
  seen <- NULL
  local_mocked_bindings(
    generate_study_data = function(config, verbose = FALSE) {
      seen <<- config
      lapply(seq_len(config$temporal_config$snapshot_count), function(i) list())
    }
  )
  expect_output(
    res <- generate_study_snapshots(
      "S", 10, 2, 3, "2 weeks", "Raw_AE",
      base_date = "2020-01-01", verbose = TRUE
    ),
    "Generating 3 longitudinal snapshots"
  )
  expect_named(res, c("2020-01-01", "2020-01-08", "2020-01-15"))
  expect_equal(seen$temporal_config$snapshot_width, "weeks")
  expect_contains(names(seen$dataset_configs), "Raw_AE")
})

# --- analytics pipeline ----------------------------------------------------

test_that("execute_analytics_pipeline skips when required packages are missing", {
  test_at_log_threshold()
  local_mocked_bindings(pkg_available = function(pkg) pkg != "workr")
  expect_message(
    res <- execute_analytics_pipeline(list(), list(verbose = TRUE)),
    "workr package not available"
  )
  expect_null(res)

  local_mocked_bindings(pkg_available = function(pkg) pkg != "gsm.mapping")
  expect_message(
    res <- execute_analytics_pipeline(list(), list(verbose = TRUE)),
    "gsm.mapping package not available"
  )
  expect_null(res)
})

test_that("execute_analytics_pipeline warns when the workflow directory is missing", {
  test_at_log_threshold()
  skip_if_not_installed("workr")
  skip_if_not_installed("gsm.mapping")
  config <- list(verbose = TRUE, analytics_package = "not.a.real.package")
  expect_output(
    expect_warning(res <- execute_analytics_pipeline(list(), config), "workflow directory not found"),
    "workflows not accessible"
  )
  expect_null(res)
})

test_that("execute_analytics_pipeline runs workflows and tolerates failures", {
  test_at_log_threshold()
  skip_if_not_installed("workr")
  skip_if_not_installed("gsm.mapping")
  skip_if_not_installed("gsm.kri")
  mock_workflow_engine()

  raw <- list(
    snap1 = analytics_snapshot(c("Raw_IE", "Raw_ENROLL", "Raw_PD")),
    snap2 = list(Raw_SUBJ = data.frame(a = 1))
  )
  expect_snapshot(
    res <- execute_analytics_pipeline(raw, list(verbose = TRUE, analytics_package = "gsm.kri"))
  )
  expect_named(res, "snap1")
  expect_equal(res$snap1$summary$workflows_executed, "Analysis_ok")
  expect_contains(names(res$snap1$mapped), "Mapped_EXCLUSION")
  expect_disjoint(names(res$snap1$mapped), "Mapped_COUNTRY")
  expect_equal(res$snap1$summary$total_participants, 2)
})

test_that("execute_analytics_pipeline falls back to snapshot_N names and explicit workflows", {
  test_at_log_threshold()
  skip_if_not_installed("workr")
  skip_if_not_installed("gsm.mapping")
  skip_if_not_installed("gsm.kri")
  mock_workflow_engine()

  config <- list(
    study_params = list(analytics_package = "gsm.kri", analytics_workflows = "ok_wf")
  )
  res <- execute_analytics_pipeline(list(analytics_snapshot()), config)
  expect_named(res, "snapshot_1")
  expect_named(res$snapshot_1$lWorkflow, "ok_wf")
})

test_that("execute_analytics_pipeline returns NULL when no snapshot is processed", {
  test_at_log_threshold()
  skip_if_not_installed("workr")
  skip_if_not_installed("gsm.mapping")
  skip_if_not_installed("gsm.kri")
  mock_workflow_engine()

  expect_warning(
    res <- execute_analytics_pipeline(
      list(a = list(Raw_SUBJ = data.frame(a = 1))),
      list(analytics_package = "gsm.kri")
    ),
    "missing required datasets"
  )
  expect_null(res)
})

test_that("execute_analytics_pipeline converts unexpected errors to warnings", {
  test_at_log_threshold()
  skip_if_not_installed("workr")
  skip_if_not_installed("gsm.mapping")
  skip_if_not_installed("gsm.kri")
  local_mocked_bindings(
    MakeWorkflowList = function(...) stop("kaboom"),
    .package = "workr"
  )
  expect_snapshot(
    res <- execute_analytics_pipeline(list(), list(verbose = TRUE, analytics_package = "gsm.kri"))
  )
  expect_null(res)
})

test_that("generate_analytics_layers sets verbose and delegates", {
  test_at_log_threshold()
  seen <- NULL
  local_mocked_bindings(
    execute_analytics_pipeline = function(raw_data, config) {
      seen <<- config
      "analytics"
    }
  )
  expect_equal(generate_analytics_layers(list(), list(), verbose = TRUE), "analytics")
  expect_true(seen$verbose)
})

# --- organize_analytics_results -------------------------------------------

test_that("organize_analytics_results handles single, multiple and malformed input", {
  test_at_log_threshold()
  tbl <- data.frame(GroupID = "A")
  tbl_with_id <- data.frame(GroupID = "B", Metric_ID = "keep")
  results <- list(
    Analysis_kri1 = list(Analysis_Summary = tbl, extra = "text", Analysis_Flagged = tbl_with_id),
    Analysis_bad = "not a list",
    other = list()
  )

  expect_equal(organize_analytics_results(NULL), list())
  expect_equal(organize_analytics_results("x"), list())

  expect_output(
    single <- organize_analytics_results(list(results = results), verbose = TRUE),
    "single snapshot"
  )
  expect_named(single, "kri1")
  expect_equal(single$kri1$metric_id, "kri1")
  expect_equal(single$kri1$data_frames$Analysis_Summary$Metric_ID, "kri1")
  expect_equal(single$kri1$data_frames$Analysis_Flagged$Metric_ID, "keep")
  expect_equal(single$kri1$data_frames$extra, "text")

  expect_output(
    multi <- organize_analytics_results(list(s1 = list(results = results), s2 = list(results = NULL)), verbose = TRUE),
    "2 snapshots"
  )
  expect_named(multi, c("s1", "s2"))
  expect_equal(multi$s2, list())

  unnamed <- organize_analytics_results(list(list(results = results)))
  expect_named(unnamed, "1")
})

# --- reporting pipeline ----------------------------------------------------

test_that("execute_reporting_pipeline skips when prerequisites are missing", {
  test_at_log_threshold()
  local_mocked_bindings(pkg_available = function(pkg) pkg != "gsm.reporting")
  expect_message(
    res <- execute_reporting_pipeline(list(a = 1), list(verbose = TRUE)),
    "gsm.reporting package not available"
  )
  expect_null(res)

  local_mocked_bindings(pkg_available = function(pkg) pkg != "workr")
  expect_message(
    res <- execute_reporting_pipeline(list(a = 1), list(verbose = TRUE)),
    "workr package not available"
  )
  expect_null(res)

  local_mocked_bindings(pkg_available = function(pkg) TRUE)
  expect_message(
    res <- execute_reporting_pipeline(NULL, list(verbose = TRUE)),
    "No analytics results"
  )
  expect_null(res)
})

test_that("execute_reporting_pipeline runs per-snapshot reporting and tolerates problems", {
  test_at_log_threshold()
  skip_if_not_installed("workr")
  skip_if_not_installed("gsm.reporting")
  mock_workflow_engine()

  input <- reporting_input()
  input$ok2 <- input$ok
  input$ok2$mapped <- list(m = 2)

  expect_snapshot(
    res <- execute_reporting_pipeline(input, list(verbose = TRUE))
  )
  expect_named(res, c("ok", "ok2"))

  with_wf <- execute_reporting_pipeline(
    list(ok = input$ok),
    list(study_params = list(reporting_workflows = "rep_wf", reporting_package = "custom"))
  )
  expect_named(with_wf, "ok")
})

test_that("execute_reporting_pipeline warns when a snapshot's workflow fails", {
  test_at_log_threshold()
  skip_if_not_installed("workr")
  skip_if_not_installed("gsm.reporting")
  local_mocked_bindings(
    MakeWorkflowList = function(...) list(bad_wf = list()),
    RunWorkflows = function(...) stop("report boom"),
    .package = "workr"
  )
  expect_warning(
    res <- execute_reporting_pipeline(list(ok = reporting_input()$ok), list()),
    "Reporting pipeline failed for snapshot ok: report boom"
  )
  expect_null(res)
})

test_that("execute_reporting_pipeline converts unexpected errors to warnings", {
  test_at_log_threshold()
  skip_if_not_installed("workr")
  skip_if_not_installed("gsm.reporting")
  local_mocked_bindings(MakeWorkflowList = function(...) stop("kaboom"), .package = "workr")
  expect_warning(
    res <- execute_reporting_pipeline(list(a = 1), list()),
    "reporting pipeline failed: kaboom"
  )
  expect_null(res)
})

test_that("generate_reporting_layers sets verbose and delegates", {
  test_at_log_threshold()
  seen <- NULL
  local_mocked_bindings(
    execute_reporting_pipeline = function(analytics_results, config) {
      seen <<- config
      "reporting"
    }
  )
  expect_equal(generate_reporting_layers(list(), list(), verbose = TRUE), "reporting")
  expect_true(seen$verbose)
})

# --- top-level wrappers ----------------------------------------------------

test_that("generate_raw_data_from_config and generate_study_data delegate to the config generator", {
  test_at_log_threshold()
  seen <- NULL
  local_mocked_bindings(
    generate_snapshots_from_config = function(...) {
      seen <<- list(...)
      "raw"
    }
  )
  expect_equal(generate_raw_data_from_config(create_study_config(), verbose = TRUE), "raw")
  expect_true(seen$verbose)
  expect_null(seen$domain_package_df)
  expect_equal(generate_study_data(create_study_config()), "raw")
})
