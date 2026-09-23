test_that("subjectid builds sequential S-prefixed identifiers", {
  expect_equal(subjectid(3), c("S1", "S2", "S3"))
  expect_length(subjectid(10), 10)
})

test_that("subject_to_enrollment draws subjects and derives subjectid", {
  set.seed(5813)
  data <- make_enroll_data()

  res <- subject_to_enrollment(4, data, previous_data = NULL)

  expect_named(res, c("subjid", "invid", "country", "enrollyn", "subjectid"))
  expect_equal(nrow(res), 4)
  expect_equal(res$subjectid, paste0("XX-", res$subjid))
})

# Passing already-enrolled subjids exercises the pool-exclusion branch.
test_that("subject_to_enrollment excludes previously enrolled subjects", {
  set.seed(5813)
  data <- make_enroll_data()
  already <- data$Raw_SUBJ$subjid[1:6]

  res <- subject_to_enrollment(4, data, previous_data = already)

  expect_false(any(res$subjid %in% already))
})

test_that("Raw_ENROLL generates a complete dataset from scratch", {
  set.seed(5813)
  data <- make_enroll_data()

  res <- Raw_ENROLL(
    data,
    previous_data = list(),
    spec = make_enroll_spec(),
    startDate = as.Date("2012-01-01"),
    n_enroll = 5,
    split_vars = enroll_split
  )

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 5)
  expect_setequal(
    names(res),
    c("studyid", "invid", "country", "subjid", "subjectid", "enrollyn")
  )
  expect_true(all(res$studyid == "PROT-001"))
  expect_true(all(res$subjid %in% data$Raw_SUBJ$subjid))
})

test_that("Raw_ENROLL appends only the delta when previous data exists", {
  set.seed(5813)
  data <- make_enroll_data()
  spec <- make_enroll_spec()

  first <- Raw_ENROLL(
    data, list(), spec,
    startDate = as.Date("2012-01-01"),
    n_enroll = 4, split_vars = enroll_split
  )
  second <- Raw_ENROLL(
    data,
    previous_data = list(Raw_ENROLL = first),
    spec = spec,
    startDate = as.Date("2012-01-01"),
    n_enroll = 7,
    split_vars = enroll_split
  )

  expect_equal(nrow(second), 7)
  expect_equal(second[seq_len(nrow(first)), ], first)
})

test_that("Raw_ENROLL returns previous data unchanged when the target count is met", {
  set.seed(5813)
  data <- make_enroll_data()
  spec <- make_enroll_spec()

  first <- Raw_ENROLL(
    data, list(), spec,
    startDate = as.Date("2012-01-01"),
    n_enroll = 4, split_vars = enroll_split
  )
  again <- Raw_ENROLL(
    data,
    previous_data = list(Raw_ENROLL = first),
    spec = spec,
    startDate = as.Date("2012-01-01"),
    n_enroll = nrow(first),
    split_vars = enroll_split
  )

  expect_identical(again, first)
})

test_that("Raw_ENROLL renames columns per source_col in the spec", {
  set.seed(5813)
  spec <- make_enroll_spec()
  spec$Raw_ENROLL$studyid$source_col <- "STUDYID"

  res <- Raw_ENROLL(
    make_enroll_data(),
    previous_data = list(),
    spec = spec,
    startDate = as.Date("2012-01-01"),
    n_enroll = 4,
    split_vars = enroll_split
  )

  expect_true("STUDYID" %in% names(res))
  expect_false("studyid" %in% names(res))
})
