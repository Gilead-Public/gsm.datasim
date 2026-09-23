test_that("cmtrt samples only the expected anti-cancer treatments", {
  set.seed(8821)

  x <- cmtrt(500)

  expect_type(x, "character")
  expect_length(x, 500)
  expect_setequal(unique(x), c("RADIOTHERAPY", "OTHER CHEMOTHERAPY"))
})

test_that("cmst_dt jitters dates within five days of the start date", {
  set.seed(8821)
  start <- as.Date("2020-06-15")

  d <- cmst_dt(50, start)

  expect_s3_class(d, "Date")
  expect_length(d, 50)
  expect_true(all(d >= start - 5 & d <= start + 5))
})

test_that("Raw_AntiCancer generates a complete dataset from scratch", {
  set.seed(8821)
  data <- make_anticancer_data()
  start_date <- as.Date("2012-01-01")

  res <- Raw_AntiCancer(
    data,
    previous_data = list(),
    spec = make_anticancer_spec(),
    startDate = start_date,
    n = 5
  )

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 5)
  expect_named(res, c("subjid", "studyid", "cmtrt", "cmst_dt"))
  expect_true(all(res$subjid %in% data$Raw_SUBJ$subjid))
  expect_equal(anyDuplicated(res$subjid), 0)
  expect_true(all(res$studyid == "PROT-001"))
  expect_true(all(res$cmtrt %in% c("RADIOTHERAPY", "OTHER CHEMOTHERAPY")))
  expect_true(all(
    res$cmst_dt >= start_date - 5 & res$cmst_dt <= start_date + 5
  ))
})

test_that("Raw_AntiCancer appends only the delta rows to previous data", {
  set.seed(8821)
  data <- make_anticancer_data()
  spec <- make_anticancer_spec()
  start_date <- as.Date("2012-01-01")

  first <- Raw_AntiCancer(data, list(), spec, start_date, n = 3)
  second <- Raw_AntiCancer(
    data,
    previous_data = list(Raw_AntiCancer = first),
    spec = spec,
    startDate = start_date,
    n = 7
  )

  expect_equal(nrow(second), 7)
  expect_equal(second[seq_len(3), ], first)
})

test_that("Raw_AntiCancer returns previous data unchanged when the target count is met", {
  set.seed(8821)
  data <- make_anticancer_data()
  spec <- make_anticancer_spec()
  start_date <- as.Date("2012-01-01")

  first <- Raw_AntiCancer(data, list(), spec, start_date, n = 4)
  again <- Raw_AntiCancer(
    data,
    previous_data = list(Raw_AntiCancer = first),
    spec = spec,
    startDate = start_date,
    n = 4
  )

  expect_identical(again, first)
})

test_that("Raw_AntiCancer renames columns per source_col in the spec", {
  set.seed(8821)
  spec <- make_anticancer_spec()
  spec$Raw_AntiCancer$cmtrt$source_col <- "CMTRT"

  res <- Raw_AntiCancer(
    make_anticancer_data(),
    previous_data = list(),
    spec = spec,
    startDate = as.Date("2012-01-01"),
    n = 5
  )

  expect_true("CMTRT" %in% names(res))
  expect_false("cmtrt" %in% names(res))
})
