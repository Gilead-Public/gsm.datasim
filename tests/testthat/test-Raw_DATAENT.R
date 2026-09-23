test_that("data_entry_lag returns integers inside the clamped 0-30 range", {
  set.seed(9137)

  x <- data_entry_lag(400)

  expect_length(x, 400)
  expect_true(all(x >= 0 & x <= 30))
})

# The subject_nsv_visits / Raw_SUBJ arguments enable the site-hotspot branch,
# which is otherwise skipped entirely.
test_that("data_entry_lag accepts a visit key map for site hotspotting", {
  set.seed(9137)
  data <- make_dataent_data()
  visits <- data.frame(
    subject_nsv = data$Raw_SUBJ$subject_nsv,
    instancename = "Visit 1",
    stringsAsFactors = FALSE
  )

  x <- data_entry_lag(
    nrow(visits),
    subject_nsv_visits = visits,
    Raw_SUBJ_data = data$Raw_SUBJ
  )

  expect_type(x, "integer")
  expect_true(all(x >= 0 & x <= 30))
})

test_that("Raw_DATAENT generates a complete dataset from scratch", {
  set.seed(9137)
  data <- make_dataent_data()

  res <- Raw_DATAENT(
    data,
    previous_data = list(),
    spec = make_dataent_spec(),
    startDate = as.Date("2012-01-01"),
    n = 3,
    split_vars = dataent_split
  )

  expect_s3_class(res, "data.frame")
  expect_setequal(
    names(res),
    c(
      "subject_nsv", "visnam", "studyid", "form", "visit_date",
      "data_entry_lag"
    )
  )
  expect_true(all(res$subject_nsv %in% data$Raw_SUBJ$subject_nsv))
  expect_true(all(res$studyid == "PROT-001"))
  # Eight forms are generated per subject-visit pair.
  expect_equal(nrow(res) %% 8, 0)
})

test_that("Raw_DATAENT appends only the delta when previous data exists", {
  set.seed(9137)
  data <- make_dataent_data()
  spec <- make_dataent_spec()

  first <- Raw_DATAENT(
    data,
    list(),
    spec,
    startDate = as.Date("2012-01-01"),
    n = 3,
    split_vars = dataent_split
  )
  second <- Raw_DATAENT(
    data,
    previous_data = list(Raw_DATAENT = first),
    spec = spec,
    startDate = as.Date("2012-01-01"),
    n = 5,
    split_vars = dataent_split
  )

  expect_gt(nrow(second), nrow(first))
  expect_equal(second[seq_len(nrow(first)), ], first)
})

# The short-circuit compares against the number of distinct subject_nsv values,
# not nrow(), so n must be the subject count to reach the early return.
test_that("Raw_DATAENT returns previous data unchanged when the target count is met", {
  set.seed(9137)
  data <- make_dataent_data()
  spec <- make_dataent_spec()

  first <- Raw_DATAENT(
    data,
    list(),
    spec,
    startDate = as.Date("2012-01-01"),
    n = 3,
    split_vars = dataent_split
  )
  again <- Raw_DATAENT(
    data,
    previous_data = list(Raw_DATAENT = first),
    spec = spec,
    startDate = as.Date("2012-01-01"),
    n = length(unique(first$subject_nsv)),
    split_vars = dataent_split
  )

  expect_identical(again, first)
})

test_that("Raw_DATAENT renames columns per source_col in the spec", {
  set.seed(9137)
  spec <- make_dataent_spec()
  spec$Raw_DATAENT$data_entry_lag$source_col <- "DATA_ENTRY_LAG"

  res <- Raw_DATAENT(
    make_dataent_data(),
    previous_data = list(),
    spec = spec,
    startDate = as.Date("2012-01-01"),
    n = 3,
    split_vars = dataent_split
  )

  expect_true("DATA_ENTRY_LAG" %in% names(res))
  expect_false("data_entry_lag" %in% names(res))
})

# visnam and form are injected by the generator when the spec omits them,
# which the fully-populated spec above never exercises.
test_that("Raw_DATAENT injects visnam and form when absent from the spec", {
  set.seed(9137)
  spec <- list(
    Raw_DATAENT = list(
      subject_nsv = list(required = TRUE, type = "character"),
      studyid = list(required = TRUE, type = "character"),
      data_entry_lag = list(required = TRUE, type = "integer")
    )
  )

  res <- Raw_DATAENT(
    make_dataent_data(),
    previous_data = list(),
    spec = spec,
    startDate = as.Date("2012-01-01"),
    n = 3,
    split_vars = dataent_split
  )

  expect_true(all(c("visnam", "form") %in% names(res)))
  expect_true(all(grepl("^form", res$form)))
})
