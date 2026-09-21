test_that("response_folder returns a constant vector", {
  expect_equal(response_folder(4), rep("final response", 4))
})

test_that("ovrlresp samples only the RECIST response categories", {
  set.seed(1905)

  x <- ovrlresp(1000)

  expect_length(x, 1000)
  expect_true(all(x %in% overallresponse_values))
})

test_that("ovrlresp accepts a Raw_SUBJ key map for site hotspotting", {
  set.seed(1905)
  subj <- make_overallresponse_data(50)$Raw_SUBJ

  x <- ovrlresp(200, Raw_SUBJ_data = subj)

  expect_length(x, 200)
  expect_true(all(x %in% overallresponse_values))
})

test_that("subjid_rs_dt draws visit dates belonging to the requested subjects", {
  set.seed(1905)
  data <- make_overallresponse_data()
  subjids <- data$Raw_SUBJ$subjid[1:5]

  res <- subjid_rs_dt(10, subjids, data$Raw_VISIT)

  expect_named(res, c("subjid", "rs_dt"))
  expect_length(res$subjid, 10)
  expect_true(all(res$subjid %in% subjids))
  expect_s3_class(res$rs_dt, "Date")
})

test_that("Raw_OverallResponse generates a complete dataset from scratch", {
  set.seed(1905)
  data <- make_overallresponse_data()

  res <- Raw_OverallResponse(
    data,
    previous_data = list(),
    spec = make_overallresponse_spec(),
    n = 6,
    split_vars = overallresponse_split
  )

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 6)
  expect_setequal(
    names(res),
    c("subjid", "rs_dt", "studyid", "ovrlresp", "response_folder")
  )
  expect_true(all(res$subjid %in% data$Raw_SUBJ$subjid))
  expect_true(all(res$studyid == "PROT-001"))
  expect_true(all(res$ovrlresp %in% overallresponse_values))
  expect_true(all(res$response_folder == "final response"))
})

test_that("Raw_OverallResponse appends only the delta rows to previous data", {
  set.seed(1905)
  data <- make_overallresponse_data()
  spec <- make_overallresponse_spec()

  first <- Raw_OverallResponse(
    data,
    list(),
    spec,
    n = 3,
    split_vars = overallresponse_split
  )
  second <- Raw_OverallResponse(
    data,
    previous_data = list(Raw_OverallResponse = first),
    spec = spec,
    n = length(unique(first$subjid)) + 4,
    split_vars = overallresponse_split
  )

  expect_gt(nrow(second), nrow(first))
  expect_equal(second[seq_len(nrow(first)), ], first)
})

# The short-circuit keys off the count of distinct subjects, not row count.
test_that("Raw_OverallResponse returns previous data unchanged when the target count is met", {
  set.seed(1905)
  data <- make_overallresponse_data()
  spec <- make_overallresponse_spec()

  first <- Raw_OverallResponse(
    data,
    list(),
    spec,
    n = 6,
    split_vars = overallresponse_split
  )
  again <- Raw_OverallResponse(
    data,
    previous_data = list(Raw_OverallResponse = first),
    spec = spec,
    n = length(unique(first$subjid)),
    split_vars = overallresponse_split
  )

  expect_identical(again, first)
})

test_that("Raw_OverallResponse renames columns per source_col in the spec", {
  set.seed(1905)
  spec <- make_overallresponse_spec()
  spec$Raw_OverallResponse$ovrlresp$source_col <- "OVRLRESP"

  res <- Raw_OverallResponse(
    make_overallresponse_data(),
    previous_data = list(),
    spec = spec,
    n = 6,
    split_vars = overallresponse_split
  )

  expect_true("OVRLRESP" %in% names(res))
  expect_false("ovrlresp" %in% names(res))
})
