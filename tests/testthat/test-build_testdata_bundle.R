test_that("read_testdata_config() returns the checked-in simulation parameters (#151)", {
  cfg <- read_testdata_config()
  expect_contains(
    names(cfg),
    c("seed", "study_id", "participants", "sites", "snapshots", "interval", "snapshot_dates")
  )
  expect_s3_class(cfg$snapshot_dates, "Date")
  expect_null(cfg$package_versions)
})

test_that("read_testdata_config() rejects inconsistent configs (#151)", {
  cfg <- read_testdata_config()
  cfg$snapshot_dates <- cfg$snapshot_dates[1:2]
  expect_error(read_testdata_config(cfg), "snapshot_dates")
  cfg <- read_testdata_config()
  cfg$seed <- NULL
  expect_error(read_testdata_config(cfg), "seed")
})

test_that(".sort_reporting() ranks MetricIDs naturally with srs last (#151)", {
  ids <- c(
    "Analysis_srs0001",
    "Analysis_kri0020a",
    "Analysis_kri0007-2",
    "Analysis_kri0007",
    "Analysis_kri0001"
  )
  df <- data.frame(MetricID = ids)
  expect_equal(
    .sort_reporting(df, "Reporting_Metrics")$MetricID,
    c(
      "Analysis_kri0001",
      "Analysis_kri0007",
      "Analysis_kri0007-2",
      "Analysis_kri0020a",
      "Analysis_srs0001"
    )
  )
})

test_that(".sort_reporting() orders results by SnapshotDate descending and Study params by curated order (#151)", {
  res <- data.frame(
    MetricID = "Analysis_kri0001",
    GroupLevel = "Site",
    GroupID = c("2", "1", "1"),
    SnapshotDate = as.Date(c("2025-02-01", "2025-02-01", "2025-03-01"))
  )
  out <- .sort_reporting(res, "Reporting_Results")
  expect_equal(
    out$SnapshotDate,
    as.Date(c("2025-03-01", "2025-02-01", "2025-02-01"))
  )
  expect_equal(out$GroupID, c("1", "1", "2"))

  grp <- data.frame(
    GroupLevel = "Study",
    GroupID = "S",
    Param = c("SiteCount", "nickname", "studyid")
  )
  expect_equal(
    .sort_reporting(grp, "Reporting_Groups")$Param,
    c("studyid", "nickname", "SiteCount")
  )

  an <- data.frame(b = c(2, 1, 1), a = c("x", "z", "y"))
  expect_equal(.sort_reporting(an, "Analysis_Summary")$a, c("y", "z", "x"))
})

test_that(".split_reporting() splits combined reporting tables by group level (#151)", {
  combined <- list(
    Reporting_Results = data.frame(
      GroupLevel = c("Site", "Country", "Study"),
      MetricID = c("Analysis_kri0001", "Analysis_cou0001", "Analysis_qtl0001"),
      GroupID = c(1, 2, 3),
      SnapshotDate = Sys.Date()
    ),
    Reporting_Groups = data.frame(
      GroupLevel = c("Study", "Site", "Country"),
      GroupID = c("S", "1", "US"),
      Param = "x"
    ),
    Reporting_Bounds = data.frame(
      MetricID = c("Analysis_kri0001", "Analysis_cou0001", "Analysis_qtl0001"),
      SnapshotDate = Sys.Date(),
      Threshold = 1,
      Denominator = 1
    ),
    Reporting_Metrics = data.frame(
      GroupLevel = c("Site", "Country", "Study"),
      MetricID = c("Analysis_kri0001", "Analysis_cou0001", "Analysis_qtl0001")
    )
  )
  out <- .split_reporting(combined)
  expect_named(
    out,
    c(
      "reportingResults",
      "reportingGroups",
      "reportingBounds",
      "reportingMetrics",
      "reportingResults_country",
      "reportingGroups_country",
      "reportingBounds_country",
      "reportingMetrics_country",
      "reportingResults_study",
      "reportingGroups_study",
      "reportingBounds_study",
      "reportingMetrics_study"
    ),
    ignore.order = TRUE
  )
  expect_equal(out$reportingGroups$GroupLevel, c("Site", "Study"))
  expect_equal(out$reportingGroups_study$GroupLevel, "Study")
  expect_equal(out$reportingBounds_country$MetricID, "Analysis_cou0001")
  expect_type(out$reportingResults$GroupID, "character")
  expect_s3_class(out$reportingResults$SnapshotDate, "Date")
})

test_that("build_testdata_bundle() writes the expected CSV and RDS files (#151)", {
  out <- build_fake_bundle()
  reporting <- paste0(
    rep(
      c("reportingResults", "reportingGroups", "reportingBounds", "reportingMetrics"),
      3
    ),
    rep(c("", "_country", "_study"), each = 4)
  )
  expect_setequal(
    list.files(out, "[.]csv$"),
    paste0(
      c(
        "analyticsInput",
        "analyticsSummary",
        "analyticsInput_country",
        "analyticsSummary_country",
        reporting
      ),
      ".csv"
    )
  )
  expect_setequal(
    list.files(out, "[.]rds$"),
    paste0(c("lSource", "analyticsInput", "analyticsSummary", reporting), ".rds")
  )
})

test_that("build_testdata_bundle() applies post-processing, combining, sorting, and types (#151)", {
  out <- build_fake_bundle()
  lSource <- readRDS(file.path(out, "lSource.rds"))
  expect_equal(lSource$Raw_SITE$site_status, c("Active", "Active"))
  # lSource is the final snapshot.
  expect_equal(nrow(lSource$Raw_SUBJ), 4)

  summ <- readRDS(file.path(out, "analyticsSummary.rds"))
  expect_equal(summ$GroupID, c("S1", "S2"))
  expect_type(summ$GroupID, "character")

  res <- readRDS(file.path(out, "reportingResults.rds"))
  expect_equal(
    res$MetricID,
    rep(c("Analysis_kri0001", "Analysis_kri0007"), c(4, 2))
  )
  expect_equal(res$SnapshotDate[1:2], as.Date(c("2025-03-01", "2025-03-01")))
  expect_type(res$GroupID, "character")

  bounds <- readRDS(file.path(out, "reportingBounds_country.rds"))
  expect_equal(bounds$SnapshotDate, as.Date(c("2025-03-01", "2025-02-01")))
  expect_equal(
    readRDS(file.path(out, "reportingMetrics.rds"))$MetricID,
    c("Analysis_kri0001", "Analysis_kri0007")
  )
})


test_that(".simulate_bundle_snapshots() passes config through, seeds, and restores the RNG (#151)", {
  skip_if_not_installed("withr")
  seen <- NULL
  local_mocked_bindings(create_longitudinal_study = function(...) {
    seen <<- list(args = list(...), draw = stats::runif(1))
    list(raw_data = list(a = fake_raw_snapshot(1), b = fake_raw_snapshot(2)))
  })
  cfg <- small_bundle_config()
  withr::local_seed(99)
  before <- .Random.seed
  snaps <- .simulate_bundle_snapshots(cfg)

  expect_identical(.Random.seed, before)
  expect_equal(seen$draw, withr::with_seed(cfg$seed, stats::runif(1)))
  fields <- c(
    "study_id",
    "participants",
    "sites",
    "snapshots",
    "interval",
    "domains"
  )
  expect_equal(seen$args[fields], cfg[fields])
  expect_false(seen$args$run_analytics)
  expect_false(seen$args$run_reporting)
  expect_null(names(snaps))
  expect_true(all(vapply(
    snaps,
    function(s) all(s$Raw_SITE$site_status == "Active"),
    logical(1)
  )))
})

test_that(".bundle_workflows() includes inactive gsm.kri metrics and the configured mappings (#151)", {
  skip_if_no_bundle_pipeline()
  calls <- list()
  local_mocked_bindings(
    MakeWorkflowList = function(
      strNames = NULL,
      strPath,
      strPackage,
      bActiveOnly = TRUE
    ) {
      calls[[strPackage]] <<- list(
        strNames = strNames,
        strPath = strPath,
        bActiveOnly = bActiveOnly
      )
      stats::setNames(
        list(list(steps = list(list(name = "Analyze_NormalApprox")))),
        strPackage
      )
    },
    .package = "workr"
  )
  local_mocked_bindings(CombineSpecs = function(...) list())
  cfg <- small_bundle_config()
  wfs <- .bundle_workflows(cfg)

  expect_equal(calls$gsm.mapping$strNames, c(cfg$domains, cfg$extra_mappings))
  expect_false(calls$gsm.kri$bActiveOnly)
  expect_null(calls$gsm.kri$strNames)
  expect_equal(calls$gsm.qtl$strPath, "workflow/2_metrics")
  expect_equal(calls$gsm.reporting$strPath, "workflow/3_reporting")
  expect_named(wfs, c("mappings", "spec", "metrics", "reporting"))
  expect_named(wfs$metrics, c("gsm.kri", "gsm.qtl"))
})

test_that(".qualify_workflow_steps() namespaces bare step functions (#151)", {
  skip_if_no_bundle_pipeline()
  wf <- list(
    a = list(
      steps = list(
        list(name = "Analyze_NormalApprox"),
        list(name = "gsm.kri::MakeWeights"),
        list(name = "paste0"),
        list(name = "not_a_function")
      )
    )
  )
  out <- .qualify_workflow_steps(wf, "gsm.kri")
  expect_equal(
    vapply(out$a$steps, `[[`, "", "name"),
    c(
      "gsm.core::Analyze_NormalApprox",
      "gsm.kri::MakeWeights",
      "paste0",
      "not_a_function"
    )
  )
})

test_that(".run_bundle_pipeline() chains mapping, metrics, and reporting and stamps SnapshotDate (#151)", {
  skip_if_no_bundle_pipeline()
  seen <- list()
  local_mocked_bindings(
    Ingest = function(raw, spec) list(Raw_SUBJ = raw$Raw_SUBJ),
    .package = "gsm.mapping"
  )
  local_mocked_bindings(
    RunWorkflows = function(lWorkflows, lData) {
      seen[[lWorkflows$stage]] <<- lData
      switch(
        lWorkflows$stage,
        mappings = list(Mapped_SUBJ = lData$Raw_SUBJ),
        metrics = list(Analysis_kri0001 = list()),
        reporting = list(
          Reporting_Results = data.frame(x = 1:2),
          Reporting_Bounds = data.frame(x = 1),
          Reporting_Groups = data.frame(x = 1)
        )
      )
    },
    .package = "workr"
  )
  wfs <- list(
    spec = list(),
    mappings = list(stage = "mappings"),
    metrics = list(stage = "metrics"),
    reporting = list(stage = "reporting")
  )
  out <- .run_bundle_pipeline(fake_raw_snapshot(1), wfs, as.Date("2025-02-01"))

  expect_identical(seen$metrics$lWorkflows, wfs$metrics)
  expect_identical(seen$reporting$lWorkflows, wfs$metrics)
  expect_identical(seen$reporting$lAnalyzed, out$analyzed)
  expect_contains(names(seen$reporting), "Mapped_SUBJ")
  expect_equal(
    out$reporting$Reporting_Results$SnapshotDate,
    as.Date(rep("2025-02-01", 2))
  )
  expect_equal(
    out$reporting$Reporting_Bounds$SnapshotDate,
    as.Date("2025-02-01")
  )
  expect_null(out$reporting$Reporting_Groups$SnapshotDate)
})

test_that("manifest records versions, checksums, size, and a data-only fingerprint (#151)", {
  skip_if_no_bundle_pipeline()
  out <- build_fake_bundle()
  expect_true(file.exists(file.path(out, "manifest.json")))
  m <- read_testdata_manifest(out)
  expect_equal(m, read_testdata_manifest(file.path(out, "manifest.json")))
  expect_match(m$fingerprint, "^[0-9a-f]{64}$")
  expect_match(m$built_at, "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$")
  expect_equal(m$simulation$seed, 1235)
  expect_equal(m$simulation$participants, 4)
  expect_equal(m$simulation$snapshot_dates, c("2025-02-01", "2025-03-01"))

  expect_setequal(
    names(m$package_versions),
    c(
      "gsm.datasim",
      "gsm.mapping",
      "gsm.kri",
      "gsm.qtl",
      "gsm.reporting",
      "workr",
      "R"
    )
  )
  for (pkg in setdiff(names(m$package_versions), "R")) {
    expect_equal(
      m$package_versions[[pkg]]$version,
      as.character(utils::packageVersion(pkg)),
      info = pkg
    )
  }
  expect_equal(
    m$package_versions$R$version,
    paste(R.version$major, R.version$minor, sep = ".")
  )

  files <- list.files(out, recursive = TRUE)
  files <- files[files != "manifest.json"]
  expect_setequal(names(m$checksums$sha256), files)
  expect_equal(m$size$files, length(files))
  expect_equal(m$size$bytes, sum(file.size(file.path(out, files))))
  f1 <- files[1]
  expect_equal(
    m$checksums$sha256[[f1]],
    digest::digest(file = file.path(out, f1), algo = "sha256")
  )
})

test_that("identical builds share a fingerprint regardless of build metadata (#151)", {
  skip_if_not_installed("withr")
  skip_if_no_bundle_pipeline()
  github_vars <- c(
    "GITHUB_REPOSITORY",
    "GITHUB_SHA",
    "GITHUB_SERVER_URL",
    "GITHUB_RUN_ID"
  )
  # Unset only while building `a`, so the test also passes on GitHub Actions.
  a <- withr::with_envvar(
    stats::setNames(rep(NA_character_, length(github_vars)), github_vars),
    read_testdata_manifest(build_fake_bundle())
  )
  withr::local_envvar(
    GITHUB_REPOSITORY = "o/r",
    GITHUB_SHA = "abc",
    GITHUB_SERVER_URL = "https://github.com",
    GITHUB_RUN_ID = "7"
  )
  b <- read_testdata_manifest(build_fake_bundle())
  expect_equal(b$built_by, "o/r@abc")
  expect_equal(b$workflow_run, "https://github.com/o/r/actions/runs/7")
  expect_null(a$built_by)
  expect_equal(a$fingerprint, b$fingerprint)
  expect_equal(a$checksums, b$checksums)
})

test_that("the fingerprint changes when the data changes (#151)", {
  skip_if_no_bundle_pipeline()
  out <- build_fake_bundle()
  cfg <- small_bundle_config()
  before <- .build_manifest(out, cfg)$fingerprint
  utils::write.csv(
    data.frame(x = 1),
    file.path(out, "extra.csv")
  )
  expect_false(identical(.build_manifest(out, cfg)$fingerprint, before))
})


test_that("package version recording reflects installed packages, not Remotes (#151)", {
  local_mocked_bindings(
    .pkg_description = function(pkg) {
      list(Version = "9.9.9", RemoteSha = "abc123")
    }
  )
  v <- .installed_package_versions("gsm.kri")
  expect_equal(v$gsm.kri, list(version = "9.9.9", remote_sha = "abc123"))
  expect_false(identical(
    v$gsm.kri$version,
    as.character(utils::packageVersion("gsm.kri"))
  ))
})

test_that("build_testdata_bundle() validates its arguments (#151)", {
  expect_error(build_testdata_bundle(NULL), "output_dir")
  expect_error(
    build_testdata_bundle(),
    "`output_dir` must be a single directory path"
  )
  expect_error(
    build_testdata_bundle(c("a", "b")),
    "`output_dir` must be a single directory path"
  )
})

test_that("build_testdata_bundle() requires the pipeline packages (#151)", {
  checked <- NULL
  local_mocked_bindings(
    check_installed = function(pkg, reason, ...) {
      checked <<- list(pkg = pkg, reason = reason)
      rlang::abort("missing")
    },
    .package = "rlang"
  )
  expect_error(build_testdata_bundle(tempfile()), "missing")
  expect_setequal(
    checked$pkg,
    c("gsm.mapping", "gsm.kri", "gsm.qtl", "gsm.reporting", "workr")
  )
  expect_equal(checked$reason, "to build the test data bundle.")
})

test_that("build_testdata_bundle() refuses a non-empty output_dir unless overwrite = TRUE (#151)", {
  skip_if_not_installed("withr")
  skip_if_no_bundle_pipeline()
  local_fake_bundle_stages()
  out <- withr::local_tempdir()
  writeLines("stale", file.path(out, "stale.txt"))
  expect_error(
    build_testdata_bundle(out, small_bundle_config()),
    "`output_dir` is not empty"
  )
  build_testdata_bundle(out, small_bundle_config(), overwrite = TRUE)
  expect_false(file.exists(file.path(out, "stale.txt")))
  expect_true(file.exists(file.path(out, "manifest.json")))
})

test_that("build_testdata_bundle() validates overwrite (#151)", {
  expect_error(
    build_testdata_bundle(tempfile(), overwrite = NA),
    "`overwrite` must be `TRUE` or `FALSE`"
  )
})
