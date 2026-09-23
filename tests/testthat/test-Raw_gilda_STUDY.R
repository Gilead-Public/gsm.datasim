test_that("protocol passes the study id straight through", {
  expect_equal(protocol("STUDY-001"), "STUDY-001")
})

test_that("act_lplv stays inside its date window", {
  set.seed(3079)
  lo <- as.Date("2012-01-01")
  hi <- as.Date("2012-03-01")

  d <- act_lplv(lo, hi, NULL)

  expect_s3_class(d, "Date")
  expect_true(all(d >= lo & d <= hi))
})

test_that("raw_gilda_study_data generates a single-row study record", {
  set.seed(3079)
  inps <- study_inputs()

  res <- do.call(
    raw_gilda_study_data,
    c(
      list(
        data = list(), previous_data = list(),
        spec = make_gilda_study_spec()
      ),
      inps
    )
  )

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$protocol, "STUDY-001")
  expect_equal(res$num_plan_site, 10)
  expect_equal(res$num_plan_subj, 100)
  expect_true(res$phase %in% c("P1", "P2", "P3", "P4"))
})

# The previous_data branch feeds the prior snapshot's dates back into the
# act_*/est_* generators as their prev_data argument.
#
# `protocol` is dropped from the spec here: unlike Raw_STUDY's `studyid`, it
# takes no `...`, so add_new_var_data()'s carry-forward of the existing column
# value errors with "unused argument" on the second snapshot.
test_that("raw_gilda_study_data reuses the previous snapshot's record", {
  set.seed(3079)
  spec <- make_gilda_study_spec()
  spec$raw_gilda_study_data$protocol <- NULL
  inps <- study_inputs()

  first <- do.call(
    raw_gilda_study_data,
    c(list(data = list(), previous_data = list(), spec = spec), inps)
  )
  second <- do.call(
    raw_gilda_study_data,
    c(
      list(
        data = list(),
        previous_data = list(raw_gilda_study_data = first),
        spec = spec
      ),
      inps
    )
  )

  expect_s3_class(second, "data.frame")
  expect_equal(nrow(second), 2)
  expect_equal(second[1, ], first)
})

test_that("raw_gilda_study_data renames columns per source_col in the spec", {
  set.seed(3079)
  spec <- make_gilda_study_spec()
  spec$raw_gilda_study_data$protocol$source_col <- "PROTOCOL"

  res <- do.call(
    raw_gilda_study_data,
    c(
      list(data = list(), previous_data = list(), spec = spec),
      study_inputs()
    )
  )

  expect_true("PROTOCOL" %in% names(res))
  expect_false("protocol" %in% names(res))
})
