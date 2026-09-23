test_that("pktpt repeats the timepoint grid once per subject", {
  possible_visits <- data.frame(pktpt = c("Cycle 1 Day 1", "Cycle 1 Day 2"))

  res <- pktpt(
    1,
    subjs = c("S1", "S2", "S3"),
    possible_visits = possible_visits
  )

  expect_length(res, 6)
  expect_equal(res, rep(c("Cycle 1 Day 1", "Cycle 1 Day 2"), 3))
})

test_that("pkperf returns one Y/N value per subject", {
  set.seed(6057)
  subjs <- sprintf("S%04d", 1:50)

  res <- pkperf(1, subjs = subjs, possible_visits = NULL)

  expect_length(res, 50)
  expect_true(all(res %in% c("Yes", "No")))
})

test_that("pkperf accepts a Raw_SUBJ key map for site hotspotting", {
  set.seed(6057)
  data <- make_pk_data(50)

  res <- pkperf(
    1,
    subjs = data$Raw_SUBJ$subjid,
    possible_visits = NULL,
    Raw_SUBJ_data = data$Raw_SUBJ
  )

  expect_length(res, 50)
  expect_true(all(res %in% c("Yes", "No")))
})

test_that("subjid_visit_pkdat draws aligned subject/visit/date triples", {
  set.seed(6057)
  data <- make_pk_data()
  subjs <- data$Raw_SUBJ$subjid[1:4]

  res <- subjid_visit_pkdat(2, subjs, data$Raw_VISIT)

  expect_named(res, c("subjid", "visit", "pkdat"))
  expect_length(res$subjid, 8)
  expect_true(all(res$subjid %in% subjs))
  expect_s3_class(res$pkdat, "Date")
})

test_that("Raw_PK generates a complete dataset from scratch", {
  set.seed(6057)
  data <- make_pk_data()

  res <- Raw_PK(
    data,
    previous_data = list(),
    spec = make_pk_spec(),
    n = 5,
    split_vars = pk_split
  )

  expect_s3_class(res, "data.frame")
  expect_setequal(
    names(res),
    c("subjid", "visit", "pkdat", "studyid", "pktpt", "pkperf")
  )
  expect_true(all(res$subjid %in% data$Raw_SUBJ$subjid))
  expect_true(all(res$studyid == "PROT-001"))
  expect_true(all(res$pkperf %in% c("Yes", "No")))
})

test_that("Raw_PK returns previous data unchanged when the target count is met", {
  set.seed(6057)
  data <- make_pk_data()
  spec <- make_pk_spec()

  first <- Raw_PK(data, list(), spec, n = 5, split_vars = pk_split)
  again <- Raw_PK(
    data,
    previous_data = list(Raw_PK = first),
    spec = spec,
    n = length(unique(first$subjid)),
    split_vars = pk_split
  )

  expect_identical(again, first)
})

test_that("Raw_PK renames columns per source_col in the spec", {
  set.seed(6057)
  spec <- make_pk_spec()
  spec$Raw_PK$pkperf$source_col <- "PKPERF"

  res <- Raw_PK(
    make_pk_data(),
    previous_data = list(),
    spec = spec,
    n = 5,
    split_vars = pk_split
  )

  expect_true("PKPERF" %in% names(res))
  expect_false("pkperf" %in% names(res))
})

# pktpt and pkperf are injected by the generator when the spec omits them,
# which the fully-populated spec above never exercises.
test_that("Raw_PK injects pktpt and pkperf when absent from the spec", {
  set.seed(6057)
  spec <- list(
    Raw_PK = list(
      subjid = list(required = TRUE, type = "character"),
      visit = list(required = TRUE, type = "character"),
      pkdat = list(required = TRUE, type = "Date"),
      studyid = list(required = TRUE, type = "character")
    )
  )

  res <- Raw_PK(
    make_pk_data(),
    previous_data = list(),
    spec = spec,
    n = 5,
    split_vars = pk_split
  )

  expect_true(all(c("pktpt", "pkperf") %in% names(res)))
  expect_true(all(res$pkperf %in% c("Yes", "No")))
})
