test_that("sdrgyn samples only Y and N", {
  set.seed(9163)

  x <- sdrgyn(1000)

  expect_type(x, "character")
  expect_length(x, 1000)
  expect_setequal(unique(x), c("Y", "N"))
})

test_that("Raw_SDRGCOMP generates a complete dataset from scratch", {
  set.seed(9163)
  data <- make_sdrgcomp_data()

  res <- Raw_SDRGCOMP(
    data,
    previous_data = list(),
    spec = make_sdrgcomp_spec(),
    startDate = as.Date("2012-01-01"),
    n = 5
  )

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 5)
  expect_named(res, c("subjid", "studyid", "sdrgyn"))
  expect_true(all(res$subjid %in% data$Raw_VISIT$subjid))
  expect_equal(anyDuplicated(res$subjid), 0)
  expect_true(all(res$studyid == "PROT-001"))
  expect_true(all(res$sdrgyn %in% c("Y", "N")))
})

# The available-subject pool is the setdiff against subjects already present,
# so a second call must not reuse a subject from the first.
test_that("Raw_SDRGCOMP draws new subjects when appending to previous data", {
  set.seed(9163)
  data <- make_sdrgcomp_data()
  spec <- make_sdrgcomp_spec()
  start_date <- as.Date("2012-01-01")

  first <- Raw_SDRGCOMP(data, list(), spec, start_date, n = 3)
  second <- Raw_SDRGCOMP(
    data,
    previous_data = list(Raw_SDRGCOMP = first),
    spec = spec,
    startDate = start_date,
    n = 7
  )

  expect_equal(nrow(second), 7)
  expect_equal(second[seq_len(3), ], first)
  expect_equal(anyDuplicated(second$subjid), 0)
})

test_that("Raw_SDRGCOMP returns previous data unchanged when the target count is met", {
  set.seed(9163)
  data <- make_sdrgcomp_data()
  spec <- make_sdrgcomp_spec()
  start_date <- as.Date("2012-01-01")

  first <- Raw_SDRGCOMP(data, list(), spec, start_date, n = 4)
  again <- Raw_SDRGCOMP(
    data,
    previous_data = list(Raw_SDRGCOMP = first),
    spec = spec,
    startDate = start_date,
    n = 4
  )

  expect_identical(again, first)
})

test_that("Raw_SDRGCOMP renames columns per source_col in the spec", {
  set.seed(9163)
  spec <- make_sdrgcomp_spec()
  spec$Raw_SDRGCOMP$sdrgyn$source_col <- "SDRGYN"

  res <- Raw_SDRGCOMP(
    make_sdrgcomp_data(),
    previous_data = list(),
    spec = spec,
    startDate = as.Date("2012-01-01"),
    n = 5
  )

  expect_true("SDRGYN" %in% names(res))
  expect_false("sdrgyn" %in% names(res))
})

# nonstarter_subjids lives in this file; its happy path is covered in
# test-nonstarter-generators.R, so only the guard clauses are exercised here.
test_that("nonstarter_subjids returns empty when required columns are absent", {
  expect_equal(nonstarter_subjids(NULL), character(0))
  expect_equal(
    nonstarter_subjids(data.frame(subjid = "S1", stringsAsFactors = FALSE)),
    character(0)
  )
})
