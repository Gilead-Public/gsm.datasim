test_that("build_testdata_bundle() writes the four-layer bundle layout with a manifest (#151)", {
  bundle <- local_tiny_bundle()

  expect_s3_class(bundle, "testdata_bundle")
  expect_true(dir.exists(bundle$path))
  expect_equal(basename(bundle$path), "AA-AA-000-0000")
  expect_true(file.exists(file.path(bundle$path, "manifest.json")))

  # snapshot folders are named by the configured snapshot dates, not the engine dates
  snapshot_dirs <- sort(list.dirs(bundle$path, recursive = FALSE, full.names = FALSE))
  expect_equal(snapshot_dirs, c("2025-02-01", "2025-03-01"))

  for (snap in snapshot_dirs) {
    for (layer in c("raw", "mapped", "analytics", "reporting")) {
      layer_dir <- file.path(bundle$path, snap, layer)
      expect_true(dir.exists(layer_dir), info = paste(snap, layer))
      if (bundle$config$format %in% c("parquet", "both")) {
        expect_gt(length(list.files(layer_dir, pattern = "[.]parquet$")), 0)
      }
      if (bundle$config$format %in% c("csv", "both")) {
        expect_gt(length(list.files(layer_dir, pattern = "[.]csv$")), 0)
      }
    }
  }
})

test_that("the two silent gsm.core post-processing edits are applied explicitly (#151)", {
  bundle <- local_tiny_bundle()
  cfg <- bundle$config

  for (i in seq_along(cfg$snapshot_dates)) {
    snap <- as.character(cfg$snapshot_dates[[i]])
    site <- read_bundle_df(bundle, snap, "raw", "Raw_SITE")
    expect_true(all(site$site_status == "Active"), info = snap)

    results <- read_bundle_df(bundle, snap, "reporting", "Reporting_Results")
    expect_equal(unique(as.Date(results$SnapshotDate)), cfg$snapshot_dates[[i]], info = snap)
  }
})

test_that("manifest.json records provenance, parameters, package versions, tables and checksums (#151)", {
  bundle <- local_tiny_bundle()
  m <- read_testdata_manifest(bundle$path)

  expect_equal(m$bundle_version, "0.0.1")
  expect_equal(m$study_id, "AA-AA-000-0000")
  expect_equal(m$seed, 1234L)
  expect_equal(m$participants, 40L)
  expect_equal(m$sites, 4L)
  expect_equal(m$snapshots, c("2025-02-01", "2025-03-01"))
  expect_match(m$built_at, "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$")
  expect_true(nzchar(m$built_by))
  expect_true(all(c("gsm.datasim", "gsm.mapping", "gsm.kri", "gsm.reporting", "workr", "R") %in% names(m$package_versions)))
  expect_equal(m$package_versions$R, paste(R.version$major, R.version$minor, sep = "."))

  expect_true(is.data.frame(m$tables))
  expect_setequal(names(m$tables), c("snapshot", "layer", "table", "rows", "cols"))
  expect_setequal(unique(m$tables$layer), c("raw", "mapped", "analytics", "reporting"))

  expect_true(is.data.frame(m$files))
  expect_setequal(names(m$files), c("path", "bytes", "sha256"))
  expect_false("manifest.json" %in% m$files$path)
  # every listed file exists and its checksum verifies
  for (i in seq_len(nrow(m$files))) {
    f <- file.path(bundle$path, m$files$path[[i]])
    expect_true(file.exists(f), info = m$files$path[[i]])
    expect_equal(digest::digest(f, algo = "sha256", file = TRUE), m$files$sha256[[i]], info = m$files$path[[i]])
  }
  # and every data file on disk is listed
  on_disk <- list.files(bundle$path, recursive = TRUE, pattern = "[.](csv|parquet)$")
  expect_setequal(on_disk, m$files$path)
})

test_that("metric workflows from every configured analytics package are run (#151)", {
  bundle <- local_tiny_bundle()
  skip_if_not_installed("gsm.qtl")

  wf_names <- names(bundle$study$analytics[[1]]$lWorkflow)
  expect_true(any(grepl("^kri", wf_names)))
  expect_true(any(grepl("^qtl", wf_names)))
})

test_that("gsm.core functions referenced unqualified in metric workflows resolve inside the build (#151)", {
  bundle <- local_tiny_bundle()
  latest <- bundle$study$analytics[[length(bundle$study$analytics)]]
  # kri0001 is a plain Normal-approximation AE rate; it only runs if Analyze_NormalApprox resolved
  expect_true("Analysis_kri0001" %in% names(latest$results))
})

test_that("the raw layer of the last snapshot is shape-compatible with the frozen gsm.core::lSource (#151)", {
  bundle <- local_tiny_bundle()
  skip_if_not_installed("gsm.core")

  lSource <- gsm.core::lSource
  latest <- as.character(max(bundle$config$snapshot_dates))
  raw_dir <- file.path(bundle$path, latest, "raw")
  domains <- unique(sub("[.](parquet|csv)$", "", list.files(raw_dir, pattern = "[.](parquet|csv)$")))

  expect_setequal(domains, names(lSource))
  for (d in names(lSource)) {
    got <- read_bundle_df(bundle, latest, "raw", d)
    missing_cols <- setdiff(names(lSource[[d]]), names(got))
    expect_length(missing_cols, 0)
    if (!has_parquet()) next # csv round-trips lose Date/timestamp types
    shared <- intersect(names(lSource[[d]]), names(got))
    expect_equal(
      vapply(got[shared], function(x) class(x)[[1]], character(1)),
      vapply(lSource[[d]][shared], function(x) class(x)[[1]], character(1)),
      info = d
    )
  }
})

test_that("the same seed and config produce identical data files (#151)", {
  skip_if_no_pipeline()
  test_at_log_threshold()
  cfg <- tiny_testdata_config(participants = 20, sites = 2, analytics_packages = "gsm.kri")
  build <- function() {
    dir <- tempfile("bundle-")
    suppressWarnings(suppressMessages(build_testdata_bundle(cfg, output_dir = dir, overwrite = TRUE)))
  }
  a <- build()
  b <- build()
  fa <- a$manifest$files
  fb <- b$manifest$files
  expect_setequal(fa$path, fb$path)

  # The raw layer is generated by gsm.datasim itself and must be byte-identical.
  raw <- grepl("/raw/", fa$path, fixed = TRUE)
  expect_equal(fa$sha256[raw], fb$sha256[match(fa$path[raw], fb$path)], ignore_attr = TRUE)

  # Downstream layers come out of the mapping / metric / reporting workflows, some
  # of which do not fix their row order (e.g. SQL without ORDER BY), so compare the
  # data rather than the bytes: identical rows, in any order.
  sort_rows <- function(df) {
    df <- df[do.call(order, unname(as.list(df))), , drop = FALSE]
    rownames(df) <- NULL
    df
  }
  for (f in grep("[.]csv$", fa$path, value = TRUE)) {
    expect_equal(
      sort_rows(utils::read.csv(file.path(a$path, f), stringsAsFactors = FALSE)),
      sort_rows(utils::read.csv(file.path(b$path, f), stringsAsFactors = FALSE)),
      info = f
    )
  }
})

test_that("build_testdata_bundle() refuses to overwrite an existing bundle unless asked (#151)", {
  skip_if_no_pipeline()
  bundle <- local_tiny_bundle()
  expect_error(
    build_testdata_bundle(bundle$config, output_dir = dirname(bundle$path), overwrite = FALSE),
    "overwrite"
  )
})
