# Exercises apply_ipns_derivations() through complete snapshot generation
# rather than calling it directly, so a placement mistake (post-processing
# fired inside a domain generator that can return early, or omitted from one
# of the two generation paths) shows up here even though the unit tests in
# test-nonstarter-generators.R already cover the derivation rules themselves.
#
# All three tests use ParticipantCount = 1 with a 2012-01-01 "months"-width,
# 2-snapshot study: count_gen() distributes 1 participant deterministically
# as c(1, 1), so snapshot 2 adds zero new subjects and Raw_SUBJ() takes its
# early-return path for that snapshot. seed 1 is fixed because it draws that
# lone subject enrolled and undosed, which is required for the window
# transition below; it is not tuned to any other property of the output.

subj_seed_config <- function(participant_count = 1, snapshot_count = 2) {
  list(
    SnapshotCount = snapshot_count,
    SnapshotWidth = "months",
    ParticipantCount = participant_count,
    SiteCount = 2,
    StudyID = "IPNS-SNAP",
    workflow_path = "workflow/1_mappings",
    mappings = "AE",
    package = "gsm.mapping",
    strStartDate = "2012-01-01",
    desired_specs = NULL
  )
}

test_that("a later snapshot with no new subjects still advances an undosed subject from within- to outside-window, legacy path (#140)", {
  skip_if_not_installed("gsm.mapping")
  set.seed(1)
  cfg <- subj_seed_config()
  snapshots <- suppressWarnings(do.call(generate_rawdata_for_single_study, cfg))

  s1 <- snapshots[[1]]$Raw_SUBJ
  s2 <- snapshots[[2]]$Raw_SUBJ

  # Confirms the zero-growth premise the rest of the test relies on.
  expect_equal(nrow(s1), 1L)
  expect_equal(nrow(s2), 1L)
  expect_equal(s1$subjid, s2$subjid)
  expect_equal(s1$enrollyn, "Y")
  expect_true(is.na(s1$firstdosedate))

  expect_equal(
    s1$drv_ip_nonstarter_status,
    "Potential Non-Starter within window"
  )
  expect_equal(
    s2$drv_ip_nonstarter_status,
    "Potential Non-Starter outside window"
  )

  # Re-accrual, not a carried-over value from snapshot 1.
  expect_true(s2$drv_days_lapsed_since_enrl > s1$drv_days_lapsed_since_enrl)
})

test_that("a later snapshot with no new subjects still advances an undosed subject from within- to outside-window, config-native path (#140)", {
  skip_if_not_installed("gsm.mapping")
  set.seed(1)
  config <- create_standard_study_config(
    "IPNS-SNAP",
    participant_count = 1,
    site_count = 2,
    adverse_events = FALSE,
    protocol_deviations = FALSE,
    lab_data = FALSE,
    subject_visits = FALSE,
    visit_schedule = FALSE,
    enrollment = TRUE,
    data_changes = FALSE,
    data_entry = FALSE,
    queries = FALSE,
    pharmacokinetics = FALSE,
    study_drug_completion = FALSE,
    study_completion = FALSE,
    inclusion_exclusion = FALSE,
    country = FALSE,
    death = FALSE,
    randomization = FALSE,
    overall_response = FALSE
  )
  config <- set_temporal_config(
    config,
    start_date = "2012-01-01",
    snapshot_count = 2,
    snapshot_width = "months"
  )
  snapshots <- suppressWarnings(generate_study_data(config))

  s1 <- snapshots[[1]]$Raw_SUBJ
  s2 <- snapshots[[2]]$Raw_SUBJ

  expect_equal(nrow(s1), 1L)
  expect_equal(nrow(s2), 1L)
  expect_equal(s1$subjid, s2$subjid)
  expect_equal(s1$enrollyn, "Y")
  expect_true(is.na(s1$firstdosedate))

  expect_equal(
    s1$drv_ip_nonstarter_status,
    "Potential Non-Starter within window"
  )
  expect_equal(
    s2$drv_ip_nonstarter_status,
    "Potential Non-Starter outside window"
  )
})

test_that("the final Raw_ENROLL reconciliation leaves every non-enrolled subject with NA in all six drv_ fields (#140)", {
  skip_if_not_installed("gsm.mapping")
  set.seed(42)
  cfg <- subj_seed_config(participant_count = 20, snapshot_count = 1)
  snapshots <- suppressWarnings(do.call(generate_rawdata_for_single_study, cfg))

  subj <- snapshots[[1]]$Raw_SUBJ
  unenrolled <- subj[subj$enrollyn == "N", ]

  # Non-vacuous: this seed/count draws at least one non-enrolled subject.
  expect_gt(nrow(unenrolled), 0)

  drv_cols <- grep("^drv_", names(subj), value = TRUE)
  expect_length(drv_cols, 6)
  expect_true(all(vapply(
    drv_cols,
    function(col) all(is.na(unenrolled[[col]])),
    logical(1)
  )))
})

test_that("the legacy and config-native generation paths produce the same six-column drv_ contract (#140)", {
  skip_if_not_installed("gsm.mapping")

  set.seed(7)
  legacy_cfg <- subj_seed_config(participant_count = 10, snapshot_count = 1)
  legacy_subj <- suppressWarnings(do.call(
    generate_rawdata_for_single_study,
    legacy_cfg
  ))[[1]]$Raw_SUBJ

  set.seed(7)
  config <- create_standard_study_config(
    "IPNS-CONTRACT",
    participant_count = 10,
    site_count = 2,
    adverse_events = FALSE,
    protocol_deviations = FALSE,
    lab_data = FALSE,
    subject_visits = FALSE,
    visit_schedule = FALSE,
    enrollment = TRUE,
    data_changes = FALSE,
    data_entry = FALSE,
    queries = FALSE,
    pharmacokinetics = FALSE,
    study_drug_completion = FALSE,
    study_completion = FALSE,
    inclusion_exclusion = FALSE,
    country = FALSE,
    death = FALSE,
    randomization = FALSE,
    overall_response = FALSE
  )
  config <- set_temporal_config(
    config,
    start_date = "2012-01-01",
    snapshot_count = 1
  )
  config_subj <- suppressWarnings(generate_study_data(config))[[1]]$Raw_SUBJ

  legacy_drv <- grep("^drv_", names(legacy_subj), value = TRUE)
  config_drv <- grep("^drv_", names(config_subj), value = TRUE)

  expect_setequal(legacy_drv, config_drv)
  expect_length(legacy_drv, 6)

  expected_types <- c(
    drv_enrollment_dt = "double", # Date is stored as a double
    drv_ip_dosed = "character",
    drv_ip_first_dose_dt = "double",
    drv_enrl_first_dose_days = "integer",
    drv_days_lapsed_since_enrl = "integer",
    drv_ip_nonstarter_status = "character"
  )
  for (col in names(expected_types)) {
    expect_type(legacy_subj[[col]], expected_types[[col]])
    expect_type(config_subj[[col]], expected_types[[col]])
  }
})
