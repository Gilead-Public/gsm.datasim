test_that("scan_dt generates dates within two days of the start date", {
  set.seed(4417)
  start <- as.Date("2020-06-15")

  d <- scan_dt(50, start)

  expect_s3_class(d, "Date")
  expect_length(d, 50)
  expect_true(all(d >= start & d <= start + 2))
})

test_that("Raw_Baseline generates a complete dataset from scratch", {
  set.seed(4417)
  data <- make_baseline_data()
  start_date <- as.Date("2012-01-01")

  res <- Raw_Baseline(
    data,
    previous_data = list(),
    spec = make_baseline_spec(),
    startDate = start_date,
    n = 5
  )

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 5)
  expect_named(res, c("subjid", "studyid", "scan_dt"))
  expect_true(all(res$subjid %in% data$Raw_SUBJ$subjid))
  expect_equal(anyDuplicated(res$subjid), 0)
  expect_true(all(res$studyid == "PROT-001"))
  expect_true(all(res$scan_dt >= start_date & res$scan_dt <= start_date + 2))
})

test_that("Raw_Baseline appends only the delta rows to previous data", {
  set.seed(4417)
  data <- make_baseline_data()
  spec <- make_baseline_spec()
  start_date <- as.Date("2012-01-01")

  first <- Raw_Baseline(data, list(), spec, start_date, n = 3)
  second <- Raw_Baseline(
    data,
    previous_data = list(Raw_Baseline = first),
    spec = spec,
    startDate = start_date,
    n = 7
  )

  expect_equal(nrow(second), 7)
  expect_equal(second[seq_len(3), ], first)
})

test_that("Raw_Baseline returns previous data unchanged when the target count is met", {
  set.seed(4417)
  data <- make_baseline_data()
  spec <- make_baseline_spec()
  start_date <- as.Date("2012-01-01")

  first <- Raw_Baseline(data, list(), spec, start_date, n = 4)
  again <- Raw_Baseline(
    data,
    previous_data = list(Raw_Baseline = first),
    spec = spec,
    startDate = start_date,
    n = 4
  )

  expect_identical(again, first)
})

test_that("Raw_Baseline renames columns per source_col in the spec", {
  set.seed(4417)
  spec <- make_baseline_spec()
  spec$Raw_Baseline$scan_dt$source_col <- "SCAN_DT"

  res <- Raw_Baseline(
    make_baseline_data(),
    previous_data = list(),
    spec = spec,
    startDate = as.Date("2012-01-01"),
    n = 5
  )

  expect_true("SCAN_DT" %in% names(res))
  expect_false("scan_dt" %in% names(res))
})
