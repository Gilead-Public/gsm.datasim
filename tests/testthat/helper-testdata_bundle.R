# Shared fixture for the test data bundle tests (#151 / #155).
#
# Building even a tiny bundle runs the mapping -> metrics -> reporting pipeline,
# so the fixture is built once per test session and memoised. Tests that need
# the pipeline packages call `skip_if_no_pipeline()` first.

.testdata_fixture <- new.env(parent = emptyenv())

pipeline_packages <- c("workr", "gsm.mapping", "gsm.kri", "gsm.reporting", "gsm.core")

# TRUE when arrow is installed with Parquet support (some builds omit it).
has_parquet <- function() {
  requireNamespace("arrow", quietly = TRUE) &&
    isTRUE(tryCatch(arrow::arrow_with_parquet(), error = function(e) FALSE))
}

# Path of one table in a bundle, preferring parquet when present.
bundle_file <- function(bundle, snapshot, layer, table) {
  parquet <- file.path(bundle$path, snapshot, layer, paste0(table, ".parquet"))
  if (file.exists(parquet)) parquet else file.path(bundle$path, snapshot, layer, paste0(table, ".csv"))
}

# Read one table from a bundle (parquet or csv) via the package reader.
read_bundle_df <- function(bundle, snapshot, layer, table) {
  read_bundle_table(bundle_file(bundle, snapshot, layer, table))
}

skip_if_no_pipeline <- function() {
  for (pkg in pipeline_packages) testthat::skip_if_not_installed(pkg)
  if (!nzchar(system.file("workflow", package = "gsm.kri"))) {
    testthat::skip("gsm.kri workflows not accessible (package loaded but not installed)")
  }
  invisible(TRUE)
}

tiny_testdata_config <- function(...) {
  analytics <- intersect(c("gsm.kri", "gsm.qtl"), rownames(utils::installed.packages()))
  read_testdata_config(
    bundle_version = "0.0.1",
    participants = 40,
    sites = 4,
    snapshots = 2,
    snapshot_dates = c("2025-02-01", "2025-03-01"),
    analytics_packages = analytics,
    format = if (has_parquet()) "both" else "csv",
    ...
  )
}

# Build (once) and return the tiny bundle used across the test files.
local_tiny_bundle <- function() {
  skip_if_no_pipeline()
  if (is.null(.testdata_fixture$bundle)) {
    test_at_log_threshold()
    out_dir <- file.path(tempdir(), "gsm-testdata-fixture")
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
    .testdata_fixture$bundle <- suppressWarnings(suppressMessages(
      build_testdata_bundle(tiny_testdata_config(), output_dir = out_dir, overwrite = TRUE)
    ))
  }
  .testdata_fixture$bundle
}
