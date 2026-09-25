test_that("summarize_testdata_bundle() inventories every table and column in the bundle (#155)", {
  bundle <- local_tiny_bundle()
  s <- summarize_testdata_bundle(bundle)

  expect_s3_class(s, "testdata_summary")
  expect_equal(s$manifest$bundle_version, "0.0.1")

  expect_true(is.data.frame(s$tables))
  expect_setequal(names(s$tables), c("snapshot", "layer", "table", "rows", "cols", "file"))
  expect_setequal(unique(s$tables$layer), c("raw", "mapped", "analytics", "reporting"))
  expect_setequal(unique(s$tables$snapshot), c("2025-02-01", "2025-03-01"))

  site <- read_bundle_df(bundle, "2025-03-01", "raw", "Raw_SITE")
  row <- s$tables[s$tables$snapshot == "2025-03-01" & s$tables$layer == "raw" & s$tables$table == "Raw_SITE", ]
  expect_equal(nrow(row), 1)
  expect_equal(row$rows, nrow(site))
  expect_equal(row$cols, ncol(site))

  expect_true(is.data.frame(s$columns))
  expect_setequal(
    names(s$columns),
    c("snapshot", "layer", "table", "column", "type", "n_missing", "pct_missing", "n_distinct", "example")
  )
  status <- s$columns[s$columns$snapshot == "2025-03-01" & s$columns$table == "Raw_SITE" & s$columns$column == "site_status", ]
  expect_equal(nrow(status), 1)
  expect_equal(status$type, "character")
  expect_equal(status$n_distinct, 1L)
  expect_equal(status$n_missing, 0L)
  expect_equal(status$example, "Active")
})

test_that("summarize_testdata_bundle() accepts a bundle path as well as a bundle object (#155)", {
  bundle <- local_tiny_bundle()
  from_path <- summarize_testdata_bundle(bundle$path)
  from_obj <- summarize_testdata_bundle(bundle)
  expect_equal(from_path$tables, from_obj$tables)
  expect_equal(from_path$columns, from_obj$columns)
})

test_that("read_testdata_bundle() rehydrates a bundle from disk (#155)", {
  bundle <- local_tiny_bundle()
  b2 <- read_testdata_bundle(bundle$path)
  expect_s3_class(b2, "testdata_bundle")
  expect_equal(b2$path, bundle$path)
  expect_equal(b2$manifest$bundle_version, bundle$manifest$bundle_version)
  expect_error(read_testdata_bundle(tempfile()), "manifest.json")
})
