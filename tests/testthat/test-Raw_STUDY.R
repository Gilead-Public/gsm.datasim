test_that("studyid unwraps a single id and samples when more are requested", {
  set.seed(1642)

  expect_equal(studyid(1, "STUDY-001"), "STUDY-001")

  many <- studyid(10, c("A", "B"))
  expect_length(many, 10)
  expect_true(all(many %in% c("A", "B")))
})

test_that("phase samples from an external phase list when supplied", {
  set.seed(1642)

  x <- phase(20, external_phase = c("P1", "P2", "P3", "P4"))

  expect_length(x, 20)
  expect_true(all(x %in% c("P1", "P2", "P3", "P4")))
})

# With no external_phase the function ignores n and returns a single constant.
test_that("phase falls back to a constant when no external phase list is given", {
  expect_equal(phase(5), "Blinded Study Drug Completion")
})

test_that("db_lock_dt repeats the global max date", {
  gmd <- as.Date("2012-12-31")

  expect_equal(db_lock_dt(1, gmd), rep(gmd, 1))
  expect_equal(db_lock_dt(3, gmd), rep(gmd, 3))
})

test_that("nickname and protocol_title build labels from their word lists", {
  set.seed(1642)

  nick <- nickname(5)
  expect_length(nick, 5)
  expect_true(all(grepl("^(OAK|TREE|GROOVE)-[0-9]+$", nick)))

  titles <- protocol_title(5)
  expect_length(titles, 5)
  expect_true(all(grepl("^Protocol Title [A-Z]$", titles)))
})

test_that("the fpfv/lpfv date generators stay inside their windows", {
  set.seed(1642)
  lo <- as.Date("2012-01-01")
  hi <- as.Date("2012-03-01")

  for (fn in list(act_fpfv, est_fpfv, est_lplv, est_lpfv)) {
    d <- fn(lo, hi, NULL)
    expect_s3_class(d, "Date")
    expect_true(all(d >= lo & d <= hi))
  }
})

test_that("Raw_STUDY generates a single-row study record", {
  set.seed(1642)
  inps <- study_inputs()

  res <- do.call(
    Raw_STUDY,
    c(
      list(data = list(), previous_data = list(), spec = make_study_spec()),
      inps
    )
  )

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$studyid, "STUDY-001")
  expect_equal(res$num_plan_site, 10)
  expect_equal(res$num_plan_subj, 100)
  expect_true(res$phase %in% c("P1", "P2", "P3", "P4"))
})

# The previous_data branch carries prior dates forward into the next snapshot.
test_that("Raw_STUDY reuses the previous snapshot's record as its starting point", {
  set.seed(1642)
  spec <- make_study_spec()
  inps <- study_inputs()

  first <- do.call(
    Raw_STUDY,
    c(list(data = list(), previous_data = list(), spec = spec), inps)
  )
  second <- do.call(
    Raw_STUDY,
    c(
      list(data = list(), previous_data = list(Raw_STUDY = first), spec = spec),
      inps
    )
  )

  expect_s3_class(second, "data.frame")
  expect_equal(nrow(second), 2)
  expect_equal(second[1, ], first)
})

test_that("Raw_STUDY renames columns per source_col in the spec", {
  set.seed(1642)
  spec <- make_study_spec()
  spec$Raw_STUDY$nickname$source_col <- "NICKNAME"

  res <- do.call(
    Raw_STUDY,
    c(list(data = list(), previous_data = list(), spec = spec), study_inputs())
  )

  expect_true("NICKNAME" %in% names(res))
  expect_false("nickname" %in% names(res))
})
