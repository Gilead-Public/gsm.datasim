test_that("subj_visit_repeated repeats each visit row n times", {
  visits <- data.frame(
    subjid = c("S1", "S2"),
    instancename = c("Visit 1", "Visit 2"),
    stringsAsFactors = FALSE
  )

  res <- subj_visit_repeated(3, visits)

  expect_named(res, c("subjid", "visnam"))
  expect_equal(res$subjid, rep(c("S1", "S2"), each = 3))
  expect_equal(res$visnam, rep(c("Visit 1", "Visit 2"), each = 3))
})

test_that("battrnam and lbtstnam tile the test panel across visits", {
  tests <- data.frame(
    battrnam = c("CHEMISTRY PANEL", "HEMATOLOGY&DIFFERENTIAL PANEL"),
    lbtstnam = c("ALT (SGPT)", "Basophils"),
    stringsAsFactors = FALSE
  )
  subj_visits <- data.frame(subjid = c("S1", "S2", "S3"))

  expect_equal(
    battrnam(1, subj_visits, tests),
    rep(tests$battrnam, 3)
  )
  expect_equal(
    lbtstnam(1, subj_visits, tests),
    rep(tests$lbtstnam, 3)
  )
})

test_that("toxgrg_nsv returns values from the tox grade set", {
  set.seed(3074)

  x <- toxgrg_nsv(500)

  expect_length(x, 500)
  expect_true(all(x %in% c("", "0", "1", "2", "3", "4")))
})

# Passing subj_visits plus tests_n activates the row-key construction and the
# site-hotspot branch, including the short-row_keys padding path.
test_that("toxgrg_nsv builds row keys from subject visits for hotspotting", {
  set.seed(3074)
  data <- make_lb_data()
  subj_visits <- data$Raw_VISIT[, c("subjid", "instancename")]

  x <- toxgrg_nsv(
    50,
    subj_visits = subj_visits,
    Raw_SUBJ_data = data$Raw_SUBJ,
    tests_n = 2
  )

  expect_length(x, 50)
  expect_true(all(x %in% c("", "0", "1", "2", "3", "4")))
})

test_that("Raw_LB generates a complete dataset from scratch", {
  set.seed(3074)
  data <- make_lb_data()

  res <- Raw_LB(
    data,
    previous_data = list(),
    spec = make_lb_spec(),
    startDate = as.Date("2012-01-01"),
    n = 3,
    split_vars = lb_split
  )

  expect_s3_class(res, "data.frame")
  expect_setequal(
    names(res),
    c(
      "subjid",
      "visnam",
      "studyid",
      "battrnam",
      "lbtstnam",
      "lb_dt",
      "toxgrg_nsv"
    )
  )
  expect_true(all(res$subjid %in% data$Raw_SUBJ$subjid))
  expect_true(all(res$studyid == "PROT-001"))
  # 45 lab tests are generated per subject-visit row.
  expect_equal(nrow(res) %% 45, 0)
})

test_that("Raw_LB returns previous data unchanged when the target count is met", {
  set.seed(3074)
  data <- make_lb_data()
  spec <- make_lb_spec()

  first <- Raw_LB(
    data,
    list(),
    spec,
    startDate = as.Date("2012-01-01"),
    n = 3,
    split_vars = lb_split
  )
  again <- Raw_LB(
    data,
    previous_data = list(Raw_LB = first),
    spec = spec,
    startDate = as.Date("2012-01-01"),
    n = length(unique(first$subjid)),
    split_vars = lb_split
  )

  expect_identical(again, first)
})

test_that("Raw_LB renames columns per source_col in the spec", {
  set.seed(3074)
  spec <- make_lb_spec()
  spec$Raw_LB$toxgrg_nsv$source_col <- "TOXGRG"

  res <- Raw_LB(
    make_lb_data(),
    previous_data = list(),
    spec = spec,
    startDate = as.Date("2012-01-01"),
    n = 3,
    split_vars = lb_split
  )

  expect_true("TOXGRG" %in% names(res))
  expect_false("toxgrg_nsv" %in% names(res))
})

# battrnam, lbtstnam and visnam are injected by the generator when the spec
# omits them, which the fully-populated spec above never exercises.
test_that("Raw_LB injects battrnam, lbtstnam and visnam when absent from the spec", {
  set.seed(3074)
  spec <- list(
    Raw_LB = list(
      subjid = list(required = TRUE, type = "character"),
      studyid = list(required = TRUE, type = "character"),
      lb_dt = list(required = TRUE, type = "Date"),
      toxgrg_nsv = list(required = TRUE, type = "character")
    )
  )

  res <- Raw_LB(
    make_lb_data(),
    previous_data = list(),
    spec = spec,
    startDate = as.Date("2012-01-01"),
    n = 3,
    split_vars = lb_split
  )

  expect_true(all(c("battrnam", "lbtstnam", "visnam") %in% names(res)))
  expect_true(all(
    res$battrnam %in% c("CHEMISTRY PANEL", "HEMATOLOGY&DIFFERENTIAL PANEL")
  ))
})
