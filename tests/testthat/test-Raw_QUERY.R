test_that("querystatus samples only the three query states", {
  set.seed(4762)

  x <- querystatus(1000)

  expect_length(x, 1000)
  expect_true(all(x %in% c("Answered", "Closed", "Open")))
})

test_that("queryage returns integers inside the clamped 1-359 range", {
  set.seed(4762)

  x <- queryage(500)

  expect_type(x, "integer")
  expect_length(x, 500)
  expect_true(all(x >= 1 & x <= 359))
})

# The subject_nsv_visits / Raw_SUBJ arguments enable the site-hotspot branch,
# which is otherwise skipped entirely.
test_that("queryage accepts a visit key map for site hotspotting", {
  set.seed(4762)
  data <- make_query_data()
  visits <- data.frame(
    subject_nsv = data$Raw_SUBJ$subject_nsv,
    instancename = "Visit 1",
    stringsAsFactors = FALSE
  )

  x <- queryage(
    nrow(visits),
    subject_nsv_visits = visits,
    Raw_SUBJ_data = data$Raw_SUBJ
  )

  expect_type(x, "integer")
  expect_true(all(x >= 1 & x <= 359))
})

test_that("Raw_QUERY generates a complete dataset from scratch", {
  set.seed(4762)
  data <- make_query_data()

  res <- Raw_QUERY(
    data,
    previous_data = list(),
    spec = make_query_spec(),
    startDate = as.Date("2012-01-01"),
    n = 5,
    split_vars = query_split
  )

  expect_s3_class(res, "data.frame")
  expect_setequal(
    names(res),
    c("subject_nsv", "visnam", "studyid", "querystatus", "queryage")
  )
  expect_true(all(res$subject_nsv %in% data$Raw_SUBJ$subject_nsv))
  expect_true(all(res$studyid == "PROT-001"))
  expect_true(all(res$querystatus %in% c("Answered", "Closed", "Open")))
  # Two query rows are generated per subject-visit pair.
  expect_equal(nrow(res) %% 2, 0)
})

test_that("Raw_QUERY returns previous data unchanged when the target count is met", {
  set.seed(4762)
  data <- make_query_data()
  spec <- make_query_spec()

  first <- Raw_QUERY(
    data,
    list(),
    spec,
    startDate = as.Date("2012-01-01"),
    n = 5,
    split_vars = query_split
  )
  again <- Raw_QUERY(
    data,
    previous_data = list(Raw_QUERY = first),
    spec = spec,
    startDate = as.Date("2012-01-01"),
    n = length(unique(first$subjid)),
    split_vars = query_split
  )

  expect_identical(again, first)
})

test_that("Raw_QUERY renames columns per source_col in the spec", {
  set.seed(4762)
  spec <- make_query_spec()
  spec$Raw_QUERY$querystatus$source_col <- "QUERYSTATUS"

  res <- Raw_QUERY(
    make_query_data(),
    previous_data = list(),
    spec = spec,
    startDate = as.Date("2012-01-01"),
    n = 5,
    split_vars = query_split
  )

  expect_true("QUERYSTATUS" %in% names(res))
  expect_false("querystatus" %in% names(res))
})

# visnam is injected by the generator when the spec omits it, which the
# fully-populated spec above never exercises.
test_that("Raw_QUERY injects visnam when absent from the spec", {
  set.seed(4762)
  spec <- list(
    Raw_QUERY = list(
      subject_nsv = list(required = TRUE, type = "character"),
      studyid = list(required = TRUE, type = "character"),
      querystatus = list(required = TRUE, type = "character"),
      queryage = list(required = TRUE, type = "integer")
    )
  )

  res <- Raw_QUERY(
    make_query_data(),
    previous_data = list(),
    spec = spec,
    startDate = as.Date("2012-01-01"),
    n = 5,
    split_vars = query_split
  )

  expect_true("visnam" %in% names(res))
  expect_true(all(res$visnam %in% c("Visit 1", "Visit 2")))
})
