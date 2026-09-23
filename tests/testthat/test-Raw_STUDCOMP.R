test_that("compyn samples Y/N/NA and compreas samples its reason set", {
  set.seed(5528)

  expect_setequal(unique(compyn(1000)), c(NA, "N", "Y"))
  expect_setequal(unique(compreas(1000)), studcomp_compreas_values)
})

test_that("completion_date returns today's date repeated n times", {
  d <- completion_date(4)

  expect_s3_class(d, "Date")
  expect_equal(d, rep(as.Date(Sys.Date()), 4))
})

test_that("subjid_invid_unique excludes subjects already present in previous data", {
  set.seed(5528)
  subj <- make_studcomp_data(10)$Raw_SUBJ
  previous <- data.frame(subjid = c("S0001", "S0002"), stringsAsFactors = FALSE)

  res <- subjid_invid_unique(5, subj, previous, replace = FALSE)

  expect_named(res, c("subjid", "invid"))
  expect_length(res$subjid, 5)
  expect_false(any(res$subjid %in% previous$subjid))
  expect_equal(res$invid, subj$invid[match(res$subjid, subj$subjid)])
})

test_that("subjid_invid_unique samples with replacement by default", {
  set.seed(5528)
  subj <- make_studcomp_data(3)$Raw_SUBJ

  res <- subjid_invid_unique(10, subj, NULL)

  expect_length(res$subjid, 10)
  expect_true(all(res$subjid %in% subj$subjid))
})

test_that("Raw_STUDCOMP generates a complete dataset from scratch", {
  set.seed(5528)
  data <- make_studcomp_data()

  res <- Raw_STUDCOMP(
    data,
    previous_data = list(),
    spec = make_studcomp_spec(),
    startDate = as.Date("2012-01-01"),
    n = 5,
    split_vars = studcomp_split
  )

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 5)
  expect_setequal(
    names(res),
    c("subjid", "invid", "studyid", "compyn", "compreas")
  )
  expect_true(all(res$subjid %in% data$Raw_SUBJ$subjid))
  expect_equal(anyDuplicated(res$subjid), 0)
  expect_true(all(res$studyid == "PROT-001"))
  expect_true(all(res$compreas %in% studcomp_compreas_values))
})

test_that("Raw_STUDCOMP appends only the delta rows to previous data", {
  set.seed(5528)
  data <- make_studcomp_data()
  spec <- make_studcomp_spec()
  start_date <- as.Date("2012-01-01")

  first <- Raw_STUDCOMP(
    data,
    list(),
    spec,
    start_date,
    n = 3,
    split_vars = studcomp_split
  )
  second <- Raw_STUDCOMP(
    data,
    previous_data = list(Raw_STUDCOMP = first),
    spec = spec,
    startDate = start_date,
    n = 7,
    split_vars = studcomp_split
  )

  expect_equal(nrow(second), 7)
  expect_equal(second[seq_len(3), ], first)
  # New rows must be subjects not already completed.
  expect_equal(anyDuplicated(second$subjid), 0)
})

test_that("Raw_STUDCOMP returns previous data unchanged when the target count is met", {
  set.seed(5528)
  data <- make_studcomp_data()
  spec <- make_studcomp_spec()
  start_date <- as.Date("2012-01-01")

  first <- Raw_STUDCOMP(
    data,
    list(),
    spec,
    start_date,
    n = 4,
    split_vars = studcomp_split
  )
  again <- Raw_STUDCOMP(
    data,
    previous_data = list(Raw_STUDCOMP = first),
    spec = spec,
    startDate = start_date,
    n = 4,
    split_vars = studcomp_split
  )

  expect_identical(again, first)
})

test_that("Raw_STUDCOMP renames columns per source_col in the spec", {
  set.seed(5528)
  spec <- make_studcomp_spec()
  spec$Raw_STUDCOMP$compyn$source_col <- "COMPYN"

  res <- Raw_STUDCOMP(
    make_studcomp_data(),
    previous_data = list(),
    spec = spec,
    startDate = as.Date("2012-01-01"),
    n = 5,
    split_vars = studcomp_split
  )

  expect_true("COMPYN" %in% names(res))
  expect_false("compyn" %in% names(res))
})

# Raw_StudyCompletion is a separate, simpler generator living in the same file.
test_that("Raw_StudyCompletion generates, appends, and short-circuits", {
  set.seed(5528)
  data <- make_studcomp_data()
  spec <- list(
    Raw_StudyCompletion = list(
      subjid = list(required = TRUE, type = "character"),
      completion_date = list(required = TRUE, type = "Date")
    )
  )
  start_date <- as.Date("2012-01-01")

  first <- Raw_StudyCompletion(data, list(), spec, start_date, n = 3)
  expect_s3_class(first, "data.frame")
  expect_equal(nrow(first), 3)
  expect_named(first, c("subjid", "completion_date"))
  expect_equal(anyDuplicated(first$subjid), 0)

  second <- Raw_StudyCompletion(
    data,
    previous_data = list(Raw_StudyCompletion = first),
    spec = spec,
    startDate = start_date,
    n = 7
  )
  expect_equal(nrow(second), 7)
  expect_equal(second[seq_len(3), ], first)

  again <- Raw_StudyCompletion(
    data,
    previous_data = list(Raw_StudyCompletion = second),
    spec = spec,
    startDate = start_date,
    n = 7
  )
  expect_identical(again, second)
})
