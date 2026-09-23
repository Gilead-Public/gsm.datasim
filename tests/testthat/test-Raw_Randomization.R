test_that("subjid_invid_country draws matched subject/site/country triples", {
  set.seed(7714)
  subj <- make_randomization_data()$Raw_SUBJ

  res <- subjid_invid_country(5, subj)

  expect_named(res, c("subjid", "invid", "country"))
  expect_length(res$subjid, 5)
  # The three fields must stay aligned to the same source row.
  matched <- subj[match(res$subjid, subj$subjid), ]
  expect_equal(res$invid, matched$invid)
  expect_equal(res$country, matched$country)
})

test_that("rgmn_dt returns the start date itself rather than an n-length vector", {
  start <- as.Date("2020-06-15")

  d <- rgmn_dt(50, start)

  expect_s3_class(d, "Date")
  expect_equal(d, start)
})

test_that("Raw_Randomization generates a complete dataset from scratch", {
  set.seed(7714)
  data <- make_randomization_data()
  start_date <- as.Date("2012-01-01")

  res <- Raw_Randomization(
    data,
    previous_data = list(),
    spec = make_randomization_spec(),
    startDate = start_date,
    n = 5,
    split_vars = randomization_split
  )

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 5)
  expect_setequal(
    names(res),
    c("studyid", "subjid", "invid", "country", "rgmn_dt")
  )
  expect_true(all(res$subjid %in% data$Raw_SUBJ$subjid))
  expect_true(all(res$studyid == "PROT-001"))
  expect_true(all(res$rgmn_dt == start_date))
})

test_that("Raw_Randomization appends only the delta rows to previous data", {
  set.seed(7714)
  data <- make_randomization_data()
  spec <- make_randomization_spec()
  start_date <- as.Date("2012-01-01")

  first <- Raw_Randomization(
    data,
    list(),
    spec,
    start_date,
    n = 3,
    split_vars = randomization_split
  )
  second <- Raw_Randomization(
    data,
    previous_data = list(Raw_Randomization = first),
    spec = spec,
    startDate = start_date,
    n = 7,
    split_vars = randomization_split
  )

  expect_equal(nrow(second), 7)
  expect_equal(second[seq_len(3), ], first)
})

test_that("Raw_Randomization returns previous data unchanged when the target count is met", {
  set.seed(7714)
  data <- make_randomization_data()
  spec <- make_randomization_spec()
  start_date <- as.Date("2012-01-01")

  first <- Raw_Randomization(
    data,
    list(),
    spec,
    start_date,
    n = 4,
    split_vars = randomization_split
  )
  again <- Raw_Randomization(
    data,
    previous_data = list(Raw_Randomization = first),
    spec = spec,
    startDate = start_date,
    n = 4,
    split_vars = randomization_split
  )

  expect_identical(again, first)
})

test_that("Raw_Randomization renames columns per source_col in the spec", {
  set.seed(7714)
  spec <- make_randomization_spec()
  spec$Raw_Randomization$country$source_col <- "COUNTRY"

  res <- Raw_Randomization(
    make_randomization_data(),
    previous_data = list(),
    spec = spec,
    startDate = as.Date("2012-01-01"),
    n = 5,
    split_vars = randomization_split
  )

  expect_true("COUNTRY" %in% names(res))
  expect_false("country" %in% names(res))
})
