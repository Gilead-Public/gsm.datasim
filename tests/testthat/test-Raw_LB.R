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
