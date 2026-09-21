test_that("deemedimportant, dvterm and category sample their fixed value sets", {
  set.seed(3391)

  expect_setequal(unique(deemedimportant(500)), c("Yes", "No"))
  expect_equal(unique(dvterm(20)), "Inclusion/Exclusion description")
  expect_setequal(unique(category(500)), c("cat 1", "cat 2"))

  expect_length(deemedimportant(0), 0)
  expect_length(category(0), 0)
})

test_that("dvdecod samples only the expected deviation codes", {
  set.seed(3391)

  x <- dvdecod(1000)

  expect_type(x, "character")
  expect_length(x, 1000)
  expect_true(all(x %in% pd_dvdecod_values))
})

# The Raw_SUBJ argument is what lets dvdecod concentrate outliers on a site, so
# it is exercised separately from the no-map call above.
test_that("dvdecod accepts a Raw_SUBJ key map for site hotspotting", {
  set.seed(3391)
  subj <- make_pd_data(100)$Raw_SUBJ

  x <- dvdecod(200, Raw_SUBJ_data = subj)

  expect_length(x, 200)
  expect_true(all(x %in% pd_dvdecod_values))
})

test_that("Raw_PD generates a complete dataset from scratch", {
  set.seed(3391)
  data <- make_pd_data()

  res <- Raw_PD(
    data,
    previous_data = list(),
    spec = make_pd_spec(),
    startDate = as.Date("2012-01-01"),
    n = 5
  )

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 5)
  expect_named(
    res,
    c("subjid", "studyid", "dvdecod", "dvterm", "deemedimportant", "category")
  )
  expect_true(all(res$subjid %in% data$Raw_SUBJ$subjid))
  expect_true(all(res$studyid == "PROT-001"))
  expect_true(all(res$dvdecod %in% pd_dvdecod_values))
  expect_true(all(res$deemedimportant %in% c("Yes", "No")))
})

test_that("Raw_PD appends only the delta rows to previous data", {
  set.seed(3391)
  data <- make_pd_data()
  spec <- make_pd_spec()
  start_date <- as.Date("2012-01-01")

  first <- Raw_PD(data, list(), spec, start_date, n = 3)
  second <- Raw_PD(
    data,
    previous_data = list(Raw_PD = first),
    spec = spec,
    startDate = start_date,
    n = 7
  )

  expect_equal(nrow(second), 7)
  expect_equal(second[seq_len(3), ], first)
})

test_that("Raw_PD returns previous data unchanged when the target count is met", {
  set.seed(3391)
  data <- make_pd_data()
  spec <- make_pd_spec()
  start_date <- as.Date("2012-01-01")

  first <- Raw_PD(data, list(), spec, start_date, n = 4)
  again <- Raw_PD(
    data,
    previous_data = list(Raw_PD = first),
    spec = spec,
    startDate = start_date,
    n = 4
  )

  expect_identical(again, first)
})

test_that("Raw_PD renames columns per source_col in the spec", {
  set.seed(3391)
  spec <- make_pd_spec()
  spec$Raw_PD$dvdecod$source_col <- "DVDECOD"

  res <- Raw_PD(
    make_pd_data(),
    previous_data = list(),
    spec = spec,
    startDate = as.Date("2012-01-01"),
    n = 5
  )

  expect_true("DVDECOD" %in% names(res))
  expect_false("dvdecod" %in% names(res))
})
