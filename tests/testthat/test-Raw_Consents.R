test_that("constype and conscat return constant vectors of the requested length", {
  expect_equal(constype(4), rep("WITHDRAWAL OF CONSENT", 4))
  expect_equal(conscat(4), rep("MAIN STUDY CONSENT", 4))
  expect_length(constype(0), 0)
  expect_length(conscat(0), 0)
})

# cons_dt ignores n and recycles a single date, so the generated column is
# filled by data.frame recycling rather than by the generator itself.
test_that("cons_dt returns the start date itself rather than an n-length vector", {
  start <- as.Date("2020-06-15")

  d <- cons_dt(50, start)

  expect_s3_class(d, "Date")
  expect_equal(d, start)
})

test_that("Raw_Consents generates a complete dataset from scratch", {
  set.seed(6632)
  data <- make_consents_data()
  start_date <- as.Date("2012-01-01")

  res <- Raw_Consents(
    data,
    previous_data = list(),
    spec = make_consents_spec(),
    startDate = start_date,
    n = 5
  )

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 5)
  expect_named(res, c("subjid", "studyid", "cons_dt", "constype", "conscat"))
  expect_true(all(res$subjid %in% data$Raw_SUBJ$subjid))
  expect_equal(anyDuplicated(res$subjid), 0)
  expect_true(all(res$studyid == "PROT-001"))
  expect_true(all(res$cons_dt == start_date))
  expect_true(all(res$constype == "WITHDRAWAL OF CONSENT"))
  expect_true(all(res$conscat == "MAIN STUDY CONSENT"))
})

test_that("Raw_Consents appends only the delta rows to previous data", {
  set.seed(6632)
  data <- make_consents_data()
  spec <- make_consents_spec()
  start_date <- as.Date("2012-01-01")

  first <- Raw_Consents(data, list(), spec, start_date, n = 3)
  second <- Raw_Consents(
    data,
    previous_data = list(Raw_Consents = first),
    spec = spec,
    startDate = start_date,
    n = 7
  )

  expect_equal(nrow(second), 7)
  expect_equal(second[seq_len(3), ], first)
})

test_that("Raw_Consents returns previous data unchanged when the target count is met", {
  set.seed(6632)
  data <- make_consents_data()
  spec <- make_consents_spec()
  start_date <- as.Date("2012-01-01")

  first <- Raw_Consents(data, list(), spec, start_date, n = 4)
  again <- Raw_Consents(
    data,
    previous_data = list(Raw_Consents = first),
    spec = spec,
    startDate = start_date,
    n = 4
  )

  expect_identical(again, first)
})

test_that("Raw_Consents renames columns per source_col in the spec", {
  set.seed(6632)
  spec <- make_consents_spec()
  spec$Raw_Consents$constype$source_col <- "CONSTYPE"

  res <- Raw_Consents(
    make_consents_data(),
    previous_data = list(),
    spec = spec,
    startDate = as.Date("2012-01-01"),
    n = 5
  )

  expect_true("CONSTYPE" %in% names(res))
  expect_false("constype" %in% names(res))
})
