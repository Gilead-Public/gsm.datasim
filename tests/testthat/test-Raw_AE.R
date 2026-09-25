test_that("aeongo and aerel sample only Y and N", {
  set.seed(2286)

  expect_setequal(unique(aeongo(500)), c("Y", "N"))
  expect_setequal(unique(aerel(500)), c("Y", "N"))
  expect_length(aeongo(0), 0)
})

test_that("mdrpt_nsv and mdrsoc_nsv sample from the AE term catalog", {
  set.seed(2286)
  catalog <- ae_term_catalog()

  expect_true(all(mdrpt_nsv(500) %in% catalog$mdrpt_nsv))
  expect_true(all(mdrsoc_nsv(500) %in% catalog$mdrsoc_nsv))
  expect_false(any(c("term1", "term2") %in% mdrpt_nsv(500)))
})

test_that("aest_dt draws within the start/end window and aeen_dt follows it", {
  set.seed(2286)
  start <- as.Date("2020-01-01")
  end <- as.Date("2020-12-31")

  st <- aest_dt(50, start, end)
  expect_s3_class(st, "Date")
  expect_length(st, 50)
  expect_true(all(st >= start & st <= end))

  en <- aeen_dt(50, st)
  expect_s3_class(en, "Date")
  # End dates sit one to three days after their start date.
  expect_true(all(en - st >= 1 & en - st <= 3))
})

test_that("aest_dt_aeen_dt returns an aligned pair of date vectors", {
  set.seed(2286)

  res <- aest_dt_aeen_dt(20, as.Date("2020-01-01"), as.Date("2020-12-31"))

  expect_named(res, c("aest_dt", "aeen_dt"))
  expect_length(res$aest_dt, 20)
  expect_true(all(res$aeen_dt > res$aest_dt))
})

test_that("aeser and aetoxgr work with and without a Raw_SUBJ key map", {
  set.seed(2286)
  subj <- make_ae_data(100)$Raw_SUBJ

  expect_setequal(unique(aeser(500)), c("Y", "N"))
  expect_true(all(aetoxgr(500) %in% 1:5))
  expect_type(aetoxgr(10), "integer")

  # Supplying Raw_SUBJ plus each record's subjid enables the site-hotspot path.
  expect_true(all(aeser(200, Raw_SUBJ_data = subj, row_keys = sample(subj$subjid, 200, TRUE)) %in% c("Y", "N")))
  expect_true(all(aetoxgr(200, mdrpt_nsv = rep("Nausea", 200), site_shift = 2) %in% 1:5))
})

test_that("Raw_AE generates a complete dataset from scratch", {
  set.seed(2286)
  data <- make_ae_data()
  start_date <- as.Date("2012-01-01")
  end_date <- as.Date("2012-12-31")

  res <- Raw_AE(
    data,
    previous_data = list(),
    spec = make_ae_spec(),
    startDate = start_date,
    endDate = end_date,
    n = 5,
    split_vars = ae_split
  )

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 5)
  expect_setequal(
    names(res),
    c(
      "subjid",
      "studyid",
      "aeser",
      "aest_dt",
      "aeen_dt",
      "mdrpt_nsv",
      "mdrsoc_nsv",
      "aetoxgr",
      "aeongo",
      "aerel"
    )
  )
  expect_true(all(res$subjid %in% data$Raw_SUBJ$subjid))
  expect_true(all(res$studyid == "PROT-001"))
  expect_true(all(res$aest_dt >= start_date & res$aest_dt <= end_date))
  expect_true(all(res$aeen_dt > res$aest_dt))

  catalog <- ae_term_catalog()
  expect_true(all(res$mdrpt_nsv %in% catalog$mdrpt_nsv))
  # SOC is the one paired with the record's preferred term.
  expect_equal(res$mdrsoc_nsv, catalog$mdrsoc_nsv[match(res$mdrpt_nsv, catalog$mdrpt_nsv)])
  expect_true(all(res$aetoxgr %in% 1:5))
})

test_that("Raw_AE appends only the delta rows to previous data", {
  set.seed(2286)
  data <- make_ae_data()
  spec <- make_ae_spec()

  first <- Raw_AE(
    data,
    list(),
    spec,
    startDate = as.Date("2012-01-01"),
    endDate = as.Date("2012-12-31"),
    n = 3,
    split_vars = ae_split
  )
  second <- Raw_AE(
    data,
    previous_data = list(Raw_AE = first),
    spec = spec,
    startDate = as.Date("2012-01-01"),
    endDate = as.Date("2012-12-31"),
    n = 7,
    split_vars = ae_split
  )

  expect_equal(nrow(second), 7)
  expect_equal(second[seq_len(3), ], first)
})

test_that("Raw_AE returns previous data unchanged when the target count is met", {
  set.seed(2286)
  data <- make_ae_data()
  spec <- make_ae_spec()

  first <- Raw_AE(
    data,
    list(),
    spec,
    startDate = as.Date("2012-01-01"),
    endDate = as.Date("2012-12-31"),
    n = 4,
    split_vars = ae_split
  )
  again <- Raw_AE(
    data,
    previous_data = list(Raw_AE = first),
    spec = spec,
    startDate = as.Date("2012-01-01"),
    endDate = as.Date("2012-12-31"),
    n = 4,
    split_vars = ae_split
  )

  expect_identical(again, first)
})

test_that("Raw_AE renames columns per source_col in the spec", {
  set.seed(2286)
  spec <- make_ae_spec()
  spec$Raw_AE$aeser$source_col <- "AESER"

  res <- Raw_AE(
    make_ae_data(),
    previous_data = list(),
    spec = spec,
    startDate = as.Date("2012-01-01"),
    endDate = as.Date("2012-12-31"),
    n = 5,
    split_vars = ae_split
  )

  expect_true("AESER" %in% names(res))
  expect_false("aeser" %in% names(res))
})
