test_that("the shipped bundle config reproduces the gsm.core generation parameters (#151)", {
  cfg <- read_testdata_config()

  expect_s3_class(cfg, "testdata_config")
  expect_equal(cfg$study_id, "AA-AA-000-0000")
  expect_equal(cfg$seed, 1234L)
  expect_equal(cfg$participants, 1000L)
  expect_equal(cfg$sites, 150L)
  expect_equal(cfg$snapshots, 3L)
  expect_equal(cfg$interval, "1 month")
  expect_s3_class(cfg$snapshot_dates, "Date")
  expect_equal(cfg$snapshot_dates, as.Date(c("2025-02-01", "2025-03-01", "2025-04-01")))
  expect_setequal(
    cfg$domains,
    c("AE", "COUNTRY", "DATACHG", "DATAENT", "ENROLL", "LB", "VISIT", "Death", "OverallResponse",
      "Randomization", "PD", "PK", "QUERY", "STUDY", "STUDCOMP", "SDRGCOMP", "SITE", "SUBJ", "IE", "EXCLUSION")
  )
  expect_equal(cfg$analytics_packages, c("gsm.kri", "gsm.qtl"))
  expect_equal(cfg$reporting_package, "gsm.reporting")
  expect_equal(cfg$format, "both")
  expect_equal(cfg$post_processing$site_status, "Active")
  expect_true(all(c("gsm.mapping", "gsm.kri", "gsm.qtl", "gsm.reporting", "workr") %in% names(cfg$package_refs)))
  expect_match(cfg$bundle_version, "^[0-9]+[.][0-9]+[.][0-9]+$")
})

test_that("read_testdata_config() applies overrides and coerces types (#151)", {
  cfg <- read_testdata_config(
    participants = 40, sites = 4, snapshots = 2,
    snapshot_dates = c("2025-02-01", "2025-03-01"),
    analytics_packages = "gsm.kri"
  )
  expect_equal(cfg$participants, 40L)
  expect_equal(cfg$sites, 4L)
  expect_equal(cfg$snapshots, 2L)
  expect_equal(cfg$snapshot_dates, as.Date(c("2025-02-01", "2025-03-01")))
  expect_equal(cfg$analytics_packages, "gsm.kri")
  # untouched fields keep the shipped defaults
  expect_equal(cfg$seed, 1234L)
  expect_equal(cfg$study_id, "AA-AA-000-0000")
})

test_that("read_testdata_config() rejects inconsistent or invalid values (#151)", {
  expect_error(read_testdata_config(snapshots = 2), "snapshot_dates")
  expect_error(read_testdata_config(format = "feather"), "format")
  expect_error(read_testdata_config(participants = 0), "participants")
  expect_error(read_testdata_config(bundle_version = "1.0"), "bundle_version")
  expect_error(read_testdata_config(path = tempfile(fileext = ".yaml")), "not found|does not exist|cannot open")
})
