# Raw_SUBJ's generators are largely covered through the longitudinal and
# non-starter suites; these tests target the guard clauses and alternate
# branches those paths never reach.

test_that("subjid samples from an external pool when one is supplied", {
  set.seed(6318)
  pool <- sprintf("S%04d", 1:20)

  x <- subjid(5, external_subjid = pool, replace = FALSE)

  expect_length(x, 5)
  expect_true(all(x %in% pool))
  expect_equal(anyDuplicated(x), 0)
})

test_that("subjid generates fresh identifiers when no pool is supplied", {
  set.seed(6318)

  x <- subjid(10)

  expect_length(x, 10)
  expect_equal(anyDuplicated(x), 0)
  expect_true(all(grepl("^S[0-9]+$", x)))
})

test_that("subjid excludes identifiers already used in a previous snapshot", {
  set.seed(6318)
  previous <- c("S000", "S001", "S002")

  x <- subjid(50, previous_subjid = previous)

  expect_false(any(x %in% previous))
})

test_that("subjid errors when the identifier pool is exhausted", {
  expect_error(
    subjid(100001),
    "Not enough unique strings available"
  )
})

test_that("subject_nsv derives from subjid or samples an external pool", {
  set.seed(6318)

  expect_equal(subject_nsv(2, c("S1", "S2")), c("S1-XXXX", "S2-XXXX"))

  pool <- paste0("S", 1:10, "-XXXX")
  sampled <- subject_nsv(5, subjid = NULL, subject_nsv = pool, replace = FALSE)
  expect_length(sampled, 5)
  expect_true(all(sampled %in% pool))
})

test_that("subjid_subject_nsv returns an aligned id/nsv pair", {
  set.seed(6318)

  res <- subjid_subject_nsv(4, dataset = NULL)

  expect_named(res, c("subjid", "subject_nsv"))
  expect_equal(res$subject_nsv, paste0(res$subjid, "-XXXX"))
})

test_that("enrollyn samples Y and N", {
  set.seed(6318)

  x <- enrollyn(500)

  expect_length(x, 500)
  expect_setequal(unique(x), c("Y", "N"))
})

test_that("enrolldt blanks out dates for subjects who did not enrol", {
  set.seed(6318)
  enrollyn_dat <- c("Y", "N", "Y", "N")

  d <- enrolldt(4, as.Date("2012-01-01"), as.Date("2012-12-31"), enrollyn_dat)

  expect_s3_class(d, "Date")
  expect_true(all(is.na(d[enrollyn_dat == "N"])))
  expect_true(all(!is.na(d[enrollyn_dat == "Y"])))
})

test_that("subject_site_synq maps site rows onto invid and country", {
  set.seed(6318)
  sites <- data.frame(
    pi_number = sprintf("0X%03d", 1:5),
    country = c("US", "UK", "Japan", "US", "UK"),
    stringsAsFactors = FALSE
  )

  res <- subject_site_synq(10, sites)

  expect_named(res, c("invid", "country"))
  expect_equal(nrow(res), 10)
  expect_true(all(res$invid %in% sites$pi_number))
})

# apply_ipns_derivations short-circuits on unusable input rather than erroring;
# the happy paths are covered in test-nonstarter-generators.R.
test_that("apply_ipns_derivations returns its input unchanged when unusable", {
  expect_null(apply_ipns_derivations(NULL, endDate = as.Date("2025-01-01")))

  empty <- data.frame(subjid = character(0), stringsAsFactors = FALSE)
  expect_equal(apply_ipns_derivations(empty, as.Date("2025-01-01")), empty)

  no_subjid <- data.frame(other = 1:3)
  expect_equal(
    apply_ipns_derivations(no_subjid, as.Date("2025-01-01")),
    no_subjid
  )
})
