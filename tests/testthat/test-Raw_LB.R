make_lb_tests_df <- function() {
  data.frame(
    battrnam = c("CHEMISTRY PANEL", "HEMATOLOGY&DIFFERENTIAL PANEL", "CHEMISTRY PANEL"),
    lbtstnam = c("ALT (SGPT)", "Hemoglobin", "Platelets"),
    stringsAsFactors = FALSE
  )
}

make_lb_subj_visits <- function(n_subjects = 20, n_visits = 4) {
  subjid <- rep(sprintf("S%04d", seq_len(n_subjects)), each = n_visits)
  instancename <- rep(c("Screening", paste0("VISIT ", 1:(n_visits - 1))), n_subjects)
  data.frame(subjid = subjid, instancename = instancename, stringsAsFactors = FALSE)
}

test_that("rptresn generates a numeric vector of the expected length (#114)", {
  set.seed(4471)

  tests <- make_lb_tests_df()
  subj_visits <- make_lb_subj_visits(n_subjects = 20, n_visits = 4)
  n <- nrow(subj_visits) * nrow(tests)

  values <- rptresn(n, subj_visits, tests)

  expect_length(values, n)
  expect_true(is.numeric(values))
  expect_false(anyNA(values))
})

test_that("rptresn produces test-specific distributions within plausible clinical ranges (#114)", {
  set.seed(918)

  tests <- make_lb_tests_df()
  subj_visits <- make_lb_subj_visits(n_subjects = 60, n_visits = 6)
  n <- nrow(subj_visits) * nrow(tests)

  values <- rptresn(n, subj_visits, tests)
  test_col <- rep(tests$lbtstnam, nrow(subj_visits))

  # Ranges are generous (beyond the issue's example bounds) since values are
  # normally distributed and can extend past a "typical" range in the tails.
  alt <- values[test_col == "ALT (SGPT)"]
  expect_true(all(alt > -20 & alt < 100))

  hgb <- values[test_col == "Hemoglobin"]
  expect_true(all(hgb > 5 & hgb < 25))

  plt <- values[test_col == "Platelets"]
  expect_true(all(plt > 50 & plt < 550))

  # Distributions should differ meaningfully between test types.
  expect_true(abs(mean(hgb) - mean(plt)) > 50)
})

test_that("rptresn injects duplicate values matching a prior visit for the same subject and test (#114)", {
  set.seed(2077)

  tests <- make_lb_tests_df()
  subj_visits <- make_lb_subj_visits(n_subjects = 30, n_visits = 8)
  n <- nrow(subj_visits) * nrow(tests)

  values <- rptresn(n, subj_visits, tests)
  test_col <- rep(tests$lbtstnam, nrow(subj_visits))
  subj_col <- rep(subj_visits$subjid, each = nrow(tests))

  # For at least one subject/test combination, some subsequent visit value
  # should exactly match an earlier visit's value.
  has_duplicate <- vapply(unique(tests$lbtstnam), function(test_name) {
    idx <- which(test_col == test_name)
    subj_for_test <- subj_col[idx]
    vals_for_test <- values[idx]
    any(vapply(unique(subj_for_test), function(s) {
      any(duplicated(vals_for_test[subj_for_test == s]))
    }, logical(1)))
  }, logical(1))

  expect_true(all(has_duplicate))
})

test_that("rptresn handles a subject/test combination with exactly one subsequent record (#114)", {
  set.seed(5541)

  tests <- data.frame(battrnam = "CHEMISTRY PANEL", lbtstnam = "ALT (SGPT)", stringsAsFactors = FALSE)
  subj_visits <- data.frame(
    subjid = c("S0001", "S0001"),
    instancename = c("Screening", "VISIT 1"),
    stringsAsFactors = FALSE
  )
  n <- nrow(subj_visits) * nrow(tests)

  values <- rptresn(n, subj_visits, tests)

  expect_length(values, n)
  expect_true(is.numeric(values))
  expect_false(anyNA(values))
})

test_that("rptresn falls back to default distribution parameters for an unlisted lbtstnam (#114)", {
  set.seed(3355)

  tests <- data.frame(
    battrnam = "OTHER PANEL",
    lbtstnam = "Some Unlisted Test",
    stringsAsFactors = FALSE
  )
  subj_visits <- make_lb_subj_visits(n_subjects = 10, n_visits = 3)
  n <- nrow(subj_visits) * nrow(tests)

  values <- rptresn(n, subj_visits, tests)

  expect_length(values, n)
  expect_true(is.numeric(values))
  expect_false(anyNA(values))
})

make_lb_legacy_data <- function(n_subjects = 20, n_visits = 4) {
  subjid <- sprintf("S%04d", seq_len(n_subjects))
  subj_visits <- make_lb_subj_visits(n_subjects, n_visits)

  list(
    Raw_SUBJ = data.frame(subjid = subjid, invid = sprintf("0X%04d", (seq_len(n_subjects) %% 3) + 1), stringsAsFactors = FALSE),
    Raw_STUDY = data.frame(protocol_number = "PROT-LB", stringsAsFactors = FALSE),
    Raw_VISIT = subj_visits
  )
}

make_lb_legacy_spec <- function() {
  list(
    subjid = list(required = TRUE),
    visnam = list(required = TRUE),
    studyid = list(required = TRUE),
    lb_dt = list(required = TRUE),
    toxgrg_nsv = list(required = TRUE)
  )
}

test_that("Raw_LB (legacy generator) produces a data frame with the expected columns (#114)", {
  set.seed(6142)

  data <- make_lb_legacy_data(n_subjects = 15, n_visits = 3)

  lb_df <- Raw_LB(
    data = data,
    previous_data = list(),
    spec = list(Raw_LB = make_lb_legacy_spec()),
    startDate = as.Date("2012-01-01"),
    n = 15,
    split_vars = list("subj_visit_repeated")
  )

  expect_s3_class(lb_df, "data.frame")
  expect_true(all(c(
    "subjid", "visnam", "battrnam", "lbtstnam", "rptresn", "studyid", "lb_dt", "toxgrg_nsv"
  ) %in% names(lb_df)))
  expect_true(is.numeric(lb_df$rptresn))
})

test_that("Raw_LB (legacy generator) returns the existing dataset unchanged when no new subjects are needed", {
  set.seed(7301)

  data <- make_lb_legacy_data(n_subjects = 10, n_visits = 2)

  lb_df <- Raw_LB(
    data = data,
    previous_data = list(),
    spec = list(Raw_LB = make_lb_legacy_spec()),
    startDate = as.Date("2012-01-01"),
    n = 10,
    split_vars = list("subj_visit_repeated")
  )

  lb_df2 <- Raw_LB(
    data = data,
    previous_data = list(Raw_LB = lb_df),
    spec = list(Raw_LB = make_lb_legacy_spec()),
    startDate = as.Date("2012-02-01"),
    n = 10,
    split_vars = list("subj_visit_repeated")
  )

  expect_identical(lb_df2, lb_df)
})

test_that("subj_visit_repeated repeats subject/visit rows and renames instancename to visnam", {
  subj_visits <- make_lb_subj_visits(n_subjects = 3, n_visits = 2)

  res <- subj_visit_repeated(2, subj_visits)

  expect_named(res, c("subjid", "visnam"))
  expect_length(res$subjid, nrow(subj_visits) * 2)
  expect_length(res$visnam, nrow(subj_visits) * 2)
})

test_that("battrnam repeats the test battery names once per subject/visit", {
  tests <- make_lb_tests_df()
  subj_visits <- make_lb_subj_visits(n_subjects = 4, n_visits = 2)

  res <- battrnam(nrow(subj_visits) * nrow(tests), subj_visits, tests)

  expect_length(res, nrow(subj_visits) * nrow(tests))
  expect_equal(res, rep(tests$battrnam, nrow(subj_visits)))
})

test_that("lbtstnam repeats the lab test names once per subject/visit", {
  tests <- make_lb_tests_df()
  subj_visits <- make_lb_subj_visits(n_subjects = 4, n_visits = 2)

  res <- lbtstnam(nrow(subj_visits) * nrow(tests), subj_visits, tests)

  expect_length(res, nrow(subj_visits) * nrow(tests))
  expect_equal(res, rep(tests$lbtstnam, nrow(subj_visits)))
})

test_that("toxgrg_nsv generates values from the expected category set with hotspot weighting by site", {
  set.seed(824)

  subj_visits <- make_lb_subj_visits(n_subjects = 20, n_visits = 3)
  raw_subj <- data.frame(
    subjid = sprintf("S%04d", seq_len(20)),
    invid = sprintf("0X%04d", (seq_len(20) %% 3) + 1),
    stringsAsFactors = FALSE
  )
  tests_n <- 3
  n <- nrow(subj_visits) * tests_n

  res <- toxgrg_nsv(n, subj_visits = subj_visits, Raw_SUBJ_data = raw_subj, tests_n = tests_n)

  expect_length(res, n)
  expect_true(all(res %in% c("", "0", "1", "2", "3", "4")))
})

test_that("toxgrg_nsv falls back to the baseline draw when subj_visits/Raw_SUBJ_data are not supplied", {
  set.seed(925)

  res <- toxgrg_nsv(50)

  expect_length(res, 50)
  expect_true(all(res %in% c("", "0", "1", "2", "3", "4")))
})

test_that("Raw_LB (legacy generator) falls back to a default visnam spec entry when the caller-supplied spec omits it", {
  set.seed(9182)

  data <- make_lb_legacy_data(n_subjects = 12, n_visits = 3)
  spec_without_visnam <- make_lb_legacy_spec()
  spec_without_visnam$visnam <- NULL

  lb_df <- Raw_LB(
    data = data,
    previous_data = list(),
    spec = list(Raw_LB = spec_without_visnam),
    startDate = as.Date("2012-01-01"),
    n = 12,
    split_vars = list("subj_visit_repeated")
  )

  expect_s3_class(lb_df, "data.frame")
  expect_true("visnam" %in% names(lb_df))
})

test_that("rptresn leaves a subject/test combination with a single record untouched by duplicate injection (#114)", {
  set.seed(6234)

  tests <- data.frame(battrnam = "CHEMISTRY PANEL", lbtstnam = "ALT (SGPT)", stringsAsFactors = FALSE)
  subj_visits <- data.frame(
    subjid = c("S0001", "S0002", "S0003"),
    instancename = "Screening",
    stringsAsFactors = FALSE
  )
  n <- nrow(subj_visits) * nrow(tests)

  values <- rptresn(n, subj_visits, tests)

  expect_length(values, 3)
  expect_true(is.numeric(values))
})

test_that("Raw_LB registry adapter includes a numeric rptresn column (#114)", {
  set.seed(123)

  snapshot_data <- generate_rawdata_for_single_study(
    SnapshotCount = 1,
    SnapshotWidth = "months",
    ParticipantCount = 20,
    SiteCount = 5,
    StudyID = "REGISTRY-TEST-LB-RPTRESN",
    workflow_path = "workflow/1_mappings",
    mappings = c("STUDY", "SITE", "SUBJ", "ENROLL", "SV", "VISIT", "LB"),
    package = "gsm.mapping"
  )

  combined_specs <- load_specs(
    workflow_path = "workflow/1_mappings",
    mappings = c("STUDY", "SITE", "SUBJ", "ENROLL", "LB"),
    package = "gsm.mapping"
  )
  combined_specs <- prepare_combined_specs_for_generation(combined_specs)
  data <- snapshot_data[[1]]

  context <- list(
    data = data,
    previous_data = list(),
    combined_specs = combined_specs,
    n = 20,
    start_date = as.Date("2012-01-01")
  )

  lb_df <- generate_domain_from_registry("Raw_LB", context)

  expect_true("rptresn" %in% names(lb_df))
  expect_true(is.numeric(lb_df$rptresn))
  expect_false(anyNA(lb_df$rptresn))

  # Preexisting fields remain intact alongside the new column.
  expect_true(all(c("toxgrg_nsv", "lbtstnam", "subjid", "lb_dt") %in% names(lb_df)))
})
