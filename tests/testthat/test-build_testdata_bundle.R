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

test_that("snapshot.json follows the refresh snapshot format (#151)", {
  skip_if_no_bundle_pipeline()
  out <- build_fake_bundle()
  expect_false(file.exists(file.path(out, "manifest.json")))
  m <- read_testdata_snapshot(out)
  expect_equal(m, read_testdata_snapshot(file.path(out, "snapshot.json")))
  expect_equal(m$refresh_snapshot, 2L)
  expect_equal(m$key, paste0("sha256:", m$metadata$fingerprint))
  expect_match(m$created, "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$")

  rds <- list.files(out, "[.]rds$")
  csv <- list.files(out, "[.]csv$")
  expect_length(rds, 15)
  expect_length(csv, 16)
  # CSVs stay in the bundle but not in objects (#188).
  expect_setequal(names(m$objects), sub("[.]rds$", "", rds))
  for (nm in names(m$objects)) {
    expect_equal(m$objects[[nm]]$file, paste0(nm, ".rds"))
    expect_equal(
      m$objects[[nm]]$digest,
      gsm.core::fresh_digest(readRDS(file.path(out, m$objects[[nm]]$file))),
      info = nm
    )
  }
  expect_equal(
    m$metadata$fingerprint,
    gsm.core::fresh_fingerprint(lapply(m$objects, `[[`, "digest"))
  )

  expect_setequal(
    names(m$packages),
    c("gsm.datasim", "gsm.core", "gsm.mapping", "gsm.kri", "gsm.qtl", "gsm.reporting", "workr")
  )
  expect_equal(m$packages$gsm.core, list(version = "9.9.9", remote_sha = "abc123"))

  expect_match(m$metadata$fingerprint, "^[0-9a-f]{64}$")
  expect_equal(m$metadata$inputs_hash, gsm.core::fresh_inputs_hash(m$packages))
  # The bundle describes what built it, not the run or release.
  expect_null(m$metadata$channel)
  expect_null(m$metadata$built_by)
  expect_null(m$metadata$workflow_run)
  expect_equal(m$metadata$simulation$seed, 1235)
  expect_equal(m$metadata$simulation$snapshot_dates, c("2025-02-01", "2025-03-01"))
  files <- setdiff(list.files(out, recursive = TRUE), "snapshot.json")
  expect_equal(m$metadata$size$files, length(files))
  expect_equal(m$metadata$size$bytes, sum(file.size(file.path(out, files))))
})

test_that("read_testdata_object() reads RDS and CSV entries and verifies digests (#151, #188)", {
  skip_if_no_bundle_pipeline()
  out <- build_fake_bundle()
  rds <- read_testdata_object(out, "reportingResults")
  expect_identical(rds, readRDS(file.path(out, "reportingResults.rds")))
  csv <- read_testdata_object(out, "reportingResults.csv")
  expect_identical(csv, rds)
  expect_type(read_testdata_object(out, "analyticsInput_country.csv"), "list")
  expect_error(read_testdata_object(out, "nope"), "not in the snapshot")
  expect_error(read_testdata_object(out, "nope.csv"), "does not exist")
  # Rewriting with another RDS version/compression still verifies.
  saveRDS(rds, file.path(out, "reportingResults.rds"), version = 2, compress = "xz")
  expect_identical(read_testdata_object(out, "reportingResults"), rds)
  changed <- rds
  changed$GroupID[1] <- "changed"
  saveRDS(changed, file.path(out, "reportingResults.rds"))
  expect_error(read_testdata_object(out, "reportingResults"), "Digest mismatch")
})

test_that("local package installs are rejected unless allow_local = TRUE (#151)", {
  skip_if_no_bundle_pipeline()
  local_mocked_bindings(
    .pkg_description = function(pkg) {
      list(Version = "1.0.0", RemoteSha = if (pkg != "gsm.kri") "abc")
    }
  )
  expect_error(build_fake_bundle(fake_versions = FALSE), "RemoteSha.*gsm.kri")
  out <- build_fake_bundle(fake_versions = FALSE, allow_local = TRUE)
  m <- jsonlite::read_json(file.path(out, "snapshot.json"))
  expect_null(m$packages$gsm.kri$remote_sha)
  expect_true("remote_sha" %in% names(m$packages$gsm.kri))
})

test_that("identical builds share a fingerprint, key, and inputs hash (#151)", {
  skip_if_no_bundle_pipeline()
  a <- read_testdata_snapshot(build_fake_bundle())
  b <- read_testdata_snapshot(build_fake_bundle())
  expect_equal(a$key, b$key)
  expect_equal(a$metadata$fingerprint, b$metadata$fingerprint)
  expect_equal(a$metadata$inputs_hash, b$metadata$inputs_hash)
  expect_equal(a$objects, b$objects)
})

test_that("the fingerprint changes when the data changes (#151)", {
  skip_if_no_bundle_pipeline()
  local_fake_package_versions()
  out <- build_fake_bundle()
  cfg <- small_bundle_config()
  fp <- function() .build_snapshot_args(out, cfg, FALSE)$metadata$fingerprint
  before <- fp()
  path <- file.path(out, "reportingResults.rds")
  res <- readRDS(path)
  res$GroupID[1] <- "changed"
  saveRDS(res, path)
  expect_false(identical(fp(), before))
})

test_that("package version recording reflects installed packages, not Remotes (#151)", {
  local_fake_package_versions()
  v <- .installed_package_versions("gsm.kri")
  expect_equal(v, list(gsm.kri = list(version = "9.9.9", remote_sha = "abc123")))
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
  expect_false(any(c("channel", "key") %in% names(formals(build_testdata_bundle))))
  expect_error(build_testdata_bundle(tempfile(), allow_local = NA), "`allow_local`")
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
    c(
      "gsm.core",
      "gsm.mapping",
      "gsm.kri",
      "gsm.qtl",
      "gsm.reporting",
      "workr"
    )
  )
  expect_equal(checked$reason, "to build the test data bundle.")
})

test_that("build_testdata_bundle() refuses a non-empty output_dir unless overwrite = TRUE (#151)", {
  skip_if_not_installed("withr")
  skip_if_no_bundle_pipeline()
  local_fake_bundle_stages()
  local_fake_package_versions()
  out <- withr::local_tempdir()
  writeLines("stale", file.path(out, "stale.txt"))
  expect_error(
    build_testdata_bundle(out, small_bundle_config()),
    "`output_dir` is not empty"
  )
  build_testdata_bundle(out, small_bundle_config(), overwrite = TRUE)
  expect_false(file.exists(file.path(out, "stale.txt")))
  expect_true(file.exists(file.path(out, "snapshot.json")))
})

test_that("build_testdata_bundle() validates overwrite (#151)", {
  expect_error(
    build_testdata_bundle(tempfile(), overwrite = NA),
    "`overwrite` must be `TRUE` or `FALSE`"
  )
})

test_that("a rejected local build leaves an existing bundle intact (#151)", {
  skip_if_no_bundle_pipeline()
  out <- build_fake_bundle()
  before <- sort(list.files(out))
  local_mocked_bindings(
    .pkg_description = function(pkg) list(Version = "1.0.0"),
    .run_bundle_pipeline = function(...) stop("pipeline should not run")
  )
  expect_error(
    build_testdata_bundle(out, small_bundle_config(), overwrite = TRUE),
    "RemoteSha"
  )
  expect_equal(sort(list.files(out)), before)
})

test_that("the fingerprint ignores RDS headers and CSV contents (#188)", {
  skip_if_no_bundle_pipeline()
  local_fake_package_versions()
  out <- build_fake_bundle()
  fp <- function() {
    .build_snapshot_args(out, small_bundle_config(), FALSE)$metadata$fingerprint
  }
  before <- fp()
  for (path in list.files(out, "[.]rds$", full.names = TRUE)) {
    saveRDS(readRDS(path), path, version = 2, compress = "bzip2")
  }
  for (path in list.files(out, "[.]csv$", full.names = TRUE)) {
    writeLines("x
1", path)
  }
  utils::write.csv(data.frame(x = 1), file.path(out, "extra.csv"))
  expect_identical(fp(), before)
})

test_that("read_testdata_config() rejects invalid simulation inputs (#151)", {
  cfg <- read_testdata_config()
  expect_error(read_testdata_config(modifyList(cfg, list(participants = -1))), "Participants")
  expect_error(read_testdata_config(modifyList(cfg, list(sites = 0))), "Sites")
  bad <- cfg
  bad$domains <- character()
  expect_error(read_testdata_config(bad), "domain")
})

test_that("invalid config inputs leave an existing bundle intact (#151)", {
  skip_if_no_bundle_pipeline()
  out <- build_fake_bundle()
  before <- sort(list.files(out))
  cfg <- modifyList(small_bundle_config(), list(participants = -1))
  expect_error(build_testdata_bundle(out, cfg, overwrite = TRUE), "Participants")
  expect_equal(sort(list.files(out)), before)
})

test_that("a failing build leaves an existing bundle intact (#151)", {
  skip_if_no_bundle_pipeline()
  out <- build_fake_bundle()
  before <- tools::md5sum(file.path(out, sort(list.files(out))))
  local_mocked_bindings(.write_rds = function(...) stop("disk full"))
  expect_error(
    build_testdata_bundle(out, small_bundle_config(), overwrite = TRUE),
    "disk full"
  )
  expect_equal(tools::md5sum(file.path(out, sort(list.files(out)))), before)
  expect_equal(list.files(tempdir(), pattern = "^gsm.datasim-bundle-"), character())
})

test_that(".read_bundle_csv() keeps GroupID as character in every table (#151)", {
  skip_if_not_installed("withr")
  dir <- withr::local_tempdir()
  for (nm in c("reportingGroups_study", "reportingResults_study", "reportingMetrics")) {
    df <- data.frame(GroupID = "001", x = 1)
    if (nm == "reportingMetrics") df$GroupID <- NULL
    utils::write.csv(df, file.path(dir, paste0(nm, ".csv")), row.names = FALSE)
  }
  expect_identical(.read_bundle_csv("reportingGroups_study", dir)$GroupID, "001")
  expect_identical(.read_bundle_csv("reportingResults_study", dir)$GroupID, "001")
  expect_no_warning(.read_bundle_csv("reportingMetrics", dir))
})

local_publish_fixture <- function(env = parent.frame()) {
  out <- withr::local_tempdir(.local_envir = env)
  writeLines("old", file.path(out, "old.csv"))
  workspace <- .bundle_workspace()
  withr::defer(unlink(workspace, recursive = TRUE), envir = env)
  staging <- file.path(workspace, "staging")
  dir.create(file.path(staging, "sub"))
  writeLines("new", file.path(staging, "sub", "a.csv"))
  writeLines("{}", file.path(staging, "snapshot.json"))
  list(out = out, workspace = workspace, staging = staging)
}

same_dir <- function(a, b) .canonical_path(a) == .canonical_path(b)

test_that(".publish_bundle() replaces output_dir contents, not the dir (#151)", {
  skip_if_not_installed("withr")
  fx <- local_publish_fixture()
  marker <- file.path(fx$out, "..", basename(fx$out))
  .publish_bundle(fx$staging, fx$out, fx$workspace)
  expect_setequal(
    list.files(fx$out, recursive = TRUE),
    c("snapshot.json", "sub/a.csv")
  )
  expect_true(dir.exists(marker))
})

test_that(".publish_bundle() restores the old bundle if publishing fails (#151)", {
  skip_if_not_installed("withr")
  fx <- local_publish_fixture()
  before <- unname(tools::md5sum(file.path(fx$out, "old.csv")))
  real_copy <- .copy_into
  local_mocked_bindings(.copy_into = function(from, to) {
    # Fail the new-bundle copy partway through; allow backup and restore.
    if (same_dir(to, fx$out) && any(basename(from) == "snapshot.json")) {
      file.copy(from[1], to, recursive = TRUE)
      return(FALSE)
    }
    real_copy(from, to)
  })
  expect_error(
    .publish_bundle(fx$staging, fx$out, fx$workspace),
    "was restored"
  )
  expect_equal(list.files(fx$out), "old.csv")
  expect_equal(unname(tools::md5sum(file.path(fx$out, "old.csv"))), before)
  .cleanup_workspace(fx$workspace)
  expect_false(dir.exists(fx$workspace))
})

test_that(".publish_bundle() keeps the backup if restoring fails (#151)", {
  skip_if_not_installed("withr")
  fx <- local_publish_fixture()
  real_copy <- .copy_into
  local_mocked_bindings(.copy_into = function(from, to) {
    if (same_dir(to, fx$out)) {
      return(FALSE)
    }
    real_copy(from, to)
  })
  expect_error(
    .publish_bundle(fx$staging, fx$out, fx$workspace),
    "backed up in"
  )
  .cleanup_workspace(fx$workspace)
  expect_equal(readLines(file.path(fx$workspace, "backup", "old.csv")), "old")
})

test_that(".publish_bundle() restores the old bundle if copying throws (#151)", {
  skip_if_not_installed("withr")
  fx <- local_publish_fixture()
  real_copy <- .copy_into
  local_mocked_bindings(.copy_into = function(from, to) {
    if (same_dir(to, fx$out) && any(basename(from) == "snapshot.json")) {
      stop("copy blew up")
    }
    real_copy(from, to)
  })
  expect_error(.publish_bundle(fx$staging, fx$out, fx$workspace), "was restored")
  expect_equal(readLines(file.path(fx$out, "old.csv")), "old")
  expect_false(file.exists(file.path(fx$workspace, "KEEP")))
})

test_that(".publish_bundle() keeps the backup if restoring throws (#151)", {
  skip_if_not_installed("withr")
  fx <- local_publish_fixture()
  real_copy <- .copy_into
  local_mocked_bindings(.copy_into = function(from, to) {
    if (same_dir(to, fx$out)) stop("copy blew up")
    real_copy(from, to)
  })
  expect_error(.publish_bundle(fx$staging, fx$out, fx$workspace), "backed up in")
  .cleanup_workspace(fx$workspace)
  expect_equal(readLines(file.path(fx$workspace, "backup", "old.csv")), "old")
})

test_that(".write_csvs() output does not depend on options(scipen) (#151)", {
  skip_if_not_installed("withr")
  df <- list(t = data.frame(x = c(1e-8, 123456789012)))
  a <- withr::local_tempdir()
  b <- withr::local_tempdir()
  original_scipen <- getOption("scipen")
  withr::with_options(list(scipen = -9), .write_csvs(df, a))
  withr::with_options(list(scipen = 20), .write_csvs(df, b))
  expect_identical(readLines(file.path(a, "t.csv")), readLines(file.path(b, "t.csv")))
  expect_identical(getOption("scipen"), original_scipen)
})

test_that(".read_bundle_csv() reads UTF-8 text (#151)", {
  skip_if_not_installed("withr")
  dir <- withr::local_tempdir()
  txt <- "Z\u00fcrich \u00e9t\u00e9"
  .write_csvs(list(t = data.frame(GroupID = "1", Name = txt)), dir)
  x <- .read_bundle_csv("t", dir)
  expect_identical(x$Name, txt)
  expect_identical(x$GroupID, "1")
})

test_that("output_dir = tempdir() is redirected to a subdirectory (#151)", {
  sub <- file.path(tempdir(), "testdata-bundle")
  expect_message(out <- .resolve_output_dir(tempdir()), "testdata-bundle")
  expect_equal(out, sub)
  expect_message(
    .resolve_output_dir(paste0(tempdir(), "/")),
    "testdata-bundle"
  )
  expect_message(
    .resolve_output_dir(file.path(tempdir(), ".")),
    "testdata-bundle"
  )
  skip_on_os(c("mac", "linux", "solaris"))
  expect_message(
    .resolve_output_dir(chartr("/", "\\", toupper(tempdir()))),
    "testdata-bundle"
  )
})

test_that("build_testdata_bundle(tempdir()) leaves other tempdir() files alone (#151)", {
  skip_if_no_bundle_pipeline()
  local_fake_bundle_stages()
  local_fake_package_versions()
  sub <- file.path(tempdir(), "testdata-bundle")
  withr::defer(unlink(sub, recursive = TRUE))
  sentinel <- withr::local_tempfile(lines = "keep")
  suppressMessages(
    build_testdata_bundle(tempdir(), small_bundle_config(), overwrite = TRUE)
  )
  expect_true(file.exists(file.path(sub, "snapshot.json")))
  expect_equal(readLines(sentinel), "keep")
  expect_equal(
    list.files(tempdir(), pattern = "^gsm.datasim-bundle-"),
    character()
  )
})

test_that("ancestors of tempdir() are rejected as output_dir (#151)", {
  expect_error(.resolve_output_dir(dirname(tempdir())), "must not contain")
  inside <- file.path(tempdir(), "x")
  expect_equal(.resolve_output_dir(inside), inside)
})

test_that("bundle simulation ignores the caller's RNG kind (#151)", {
  skip_if_not_installed("withr")
  seen <- NULL
  local_mocked_bindings(create_longitudinal_study = function(...) {
    seen <<- RNGkind()
    list(raw_data = list())
  })
  withr::local_rng_version("3.5.0")
  .simulate_bundle_snapshots(small_bundle_config())
  expect_equal(seen, c("Mersenne-Twister", "Inversion", "Rejection"))
})

test_that(".write_csvs() writes LF line endings on every platform (#187)", {
  skip_if_not_installed("withr")
  dir <- withr::local_tempdir()
  .write_csvs(list(t = data.frame(x = 1:3, y = c("a", "b", "c"))), dir)
  bytes <- readBin(file.path(dir, "t.csv"), "raw", file.size(file.path(dir, "t.csv")))
  expect_false(any(bytes == as.raw(0x0d)))
  expect_identical(sum(bytes == as.raw(0x0a)), 4L)
})

test_that("read_testdata_object() reads CSVs converted to CRLF (#187)", {
  skip_if_no_bundle_pipeline()
  out <- build_fake_bundle()
  path <- file.path(out, "reportingResults.csv")
  lf <- readBin(path, "raw", file.size(path))
  expect_false(any(lf == as.raw(0x0d)))
  writeBin(charToRaw(gsub("\n", "\r\n", rawToChar(lf), fixed = TRUE)), path)
  expect_identical(
    read_testdata_object(out, "reportingResults.csv"),
    readRDS(file.path(out, "reportingResults.rds"))
  )
})

test_that("the fingerprint ignores CSV line endings (#187)", {
  skip_if_no_bundle_pipeline()
  out <- build_fake_bundle()
  lf <- .build_snapshot_args(out, small_bundle_config(), allow_local = TRUE)
  for (path in list.files(out, "[.]csv$", full.names = TRUE)) {
    txt <- rawToChar(readBin(path, "raw", file.size(path)))
    writeBin(charToRaw(gsub("\n", "\r\n", txt, fixed = TRUE)), path)
  }
  crlf <- .build_snapshot_args(out, small_bundle_config(), allow_local = TRUE)
  expect_identical(crlf$metadata$fingerprint, lf$metadata$fingerprint)
})
