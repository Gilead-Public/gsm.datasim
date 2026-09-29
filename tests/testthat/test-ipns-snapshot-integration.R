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
  test_at_log_threshold()
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
  test_at_log_threshold()
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

test_that("the final Raw_ENROLL reconciliation leaves every non-enrolled subject with NA in every drv_ field (#140, #157, #138)", {
  test_at_log_threshold()
  skip_if_not_installed("gsm.mapping")
  set.seed(42)
  cfg <- subj_seed_config(participant_count = 20, snapshot_count = 1)
  snapshots <- suppressWarnings(do.call(generate_rawdata_for_single_study, cfg))

  subj <- snapshots[[1]]$Raw_SUBJ
  unenrolled <- subj[subj$enrollyn == "N", ]

  # Non-vacuous: this seed/count draws at least one non-enrolled subject.
  expect_gt(nrow(unenrolled), 0)

  drv_cols <- grep("^drv_", names(subj), value = TRUE)
  expect_true(all(c("drv_kit_assigned", "drv_treatment_discontinuation_dt", "drv_premature_discontinuation_reason", "drv_days_lapsed_enrl_discontinuation") %in% drv_cols))
  expect_true(all(vapply(
    drv_cols,
    function(col) all(is.na(unenrolled[[col]])),
    logical(1)
  )))
})

test_that("the legacy and config-native generation paths produce the same drv_ contract (#140, #157, #138)", {
  test_at_log_threshold()
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

  expected_types <- c(
    drv_enrollment_dt = "double", # Date is stored as a double
    drv_ip_dosed = "character",
    drv_ip_first_dose_dt = "double",
    drv_enrl_first_dose_days = "integer",
    drv_days_lapsed_since_enrl = "integer",
    drv_ip_nonstarter_status = "character",
    drv_kit_assigned = "character",
    drv_treatment_discontinuation_dt = "double",
    drv_premature_discontinuation_reason = "character",
    drv_days_lapsed_enrl_discontinuation = "integer"
  )
  for (col in names(expected_types)) {
    expect_type(legacy_subj[[col]], expected_types[[col]])
    expect_type(config_subj[[col]], expected_types[[col]])
  }
})

# Same seed, size and snapshots as gsm.core's data-raw/simulate_longitudinal_data.R.
core_shaped_run <- local({
  run <- NULL
  function() {
    if (is.null(run)) {
      set.seed(1234)
      run <<- suppressWarnings(generate_rawdata_for_single_study(
        SnapshotCount = 3,
        SnapshotWidth = "months",
        ParticipantCount = 1000,
        SiteCount = 150,
        StudyID = "AA-AA-000-0000",
        workflow_path = "workflow/1_mappings",
        mappings = c("SUBJ", "ENROLL", "STUDCOMP", "SITE", "STUDY"),
        package = "gsm.mapping",
        desired_specs = NULL
      ))
    }
    run
  }
})

is_blank <- function(x) is.na(x) | x == ""

test_that("a core-shaped run carries every IP non-starter scenario the IP Compliance report needs (#157)", {
  test_at_log_threshold()
  skip_if_not_installed("gsm.mapping")
  snaps <- core_shaped_run()
  subj <- snaps[[3]]$Raw_SUBJ
  enr <- subj[subj$enrollyn %in% "Y", ]
  as_of <- max(enr$drv_enrollment_dt + enr$drv_days_lapsed_since_enrl - 1L, na.rm = TRUE)
  sc <- snaps[[3]]$Raw_STUDCOMP
  sc <- sc[sc$subjid %in% enr$subjid, ]
  i <- match(sc$subjid, enr$subjid)
  status <- enr$drv_ip_nonstarter_status[i]
  undosed_kit <- enr$drv_kit_assigned[enr$drv_ip_dosed == "N"]

  expect_setequal(unique(enr$drv_ip_nonstarter_status), c(
    "Dosed", "Confirmed Non-Starter",
    "Potential Non-Starter outside window", "Potential Non-Starter within window"
  ))
  expect_gte(sum(status == "Confirmed Non-Starter" & sc$compreas == "Withdrew Consent"), 3)
  expect_gte(sum(status == "Dosed" & sc$compyn %in% "Y"), 3)
  expect_gte(sum(undosed_kit == "Y"), 3)
  expect_gte(sum(undosed_kit == "N"), 3)

  confirmed <- enr$subjid[enr$drv_ip_nonstarter_status == "Confirmed Non-Starter"]
  expect_true(all(confirmed %in% sc$subjid[sc$compyn %in% "N"]))
  expect_false(any(grepl("^Potential", status) & !is_blank(sc$compyn)))
  expect_true(all(status[sc$compyn %in% "Y"] == "Dosed"))
  expect_identical(!is_blank(sc$compreas), sc$compyn %in% "N")

  anchor <- dplyr::coalesce(enr$drv_ip_first_dose_dt, enr$drv_enrollment_dt)[i]
  created <- as.Date(sc$mincreated_dts)
  expect_true(all(created >= anchor & created <= as_of))
  expect_true(all(enr$firstdosedate <= as_of, na.rm = TRUE))
  expect_true(any(enr$drv_enrl_first_dose_days %in% 1L))
  expect_true(any(enr$drv_enrl_first_dose_days > 1L, na.rm = TRUE))

  for (earlier in snaps[1:2]) {
    prev <- earlier$Raw_STUDCOMP
    now <- snaps[[3]]$Raw_STUDCOMP
    expect_equal(now[match(prev$subjid, now$subjid), names(prev)], prev, ignore_attr = TRUE)
    later <- subj[match(earlier$Raw_SUBJ$subjid, subj$subjid), ]
    expect_identical(later$drv_kit_assigned, earlier$Raw_SUBJ$drv_kit_assigned)
    was_confirmed <- earlier$Raw_SUBJ$drv_ip_nonstarter_status %in% "Confirmed Non-Starter"
    expect_true(all(later$drv_ip_nonstarter_status[was_confirmed] == "Confirmed Non-Starter"))
  }
})

native_subj_config <- function(study_id, participant_count, snapshot_count) {
  config <- create_standard_study_config(
    study_id,
    participant_count = participant_count,
    site_count = 2,
    adverse_events = FALSE, protocol_deviations = FALSE, lab_data = FALSE,
    subject_visits = FALSE, visit_schedule = FALSE, enrollment = TRUE,
    data_changes = FALSE, data_entry = FALSE, queries = FALSE,
    pharmacokinetics = FALSE, study_drug_completion = FALSE,
    study_completion = FALSE, inclusion_exclusion = FALSE, country = FALSE,
    death = FALSE, randomization = FALSE, overall_response = FALSE
  )
  set_temporal_config(
    config,
    start_date = "2012-01-01",
    snapshot_count = snapshot_count,
    snapshot_width = "months"
  )
}

test_that("a discontinuation date stays the same on a later snapshot in both generation paths (#138)", {
  test_at_log_threshold()
  skip_if_not_installed("gsm.mapping")
  set.seed(7)
  legacy <- suppressWarnings(do.call(
    generate_rawdata_for_single_study,
    subj_seed_config(participant_count = 300, snapshot_count = 2)
  ))
  set.seed(7)
  native <- suppressWarnings(generate_study_data(native_subj_config("PTD-SNAP", 300, 2)))

  for (snaps in list(legacy, native)) {
    s1 <- snaps[[1]]$Raw_SUBJ
    s2 <- snaps[[2]]$Raw_SUBJ
    dated <- s1$subjid[!is.na(s1$drv_treatment_discontinuation_dt)]
    expect_gt(length(dated), 0)
    expect_identical(
      s2$drv_treatment_discontinuation_dt[match(dated, s2$subjid)],
      s1$drv_treatment_discontinuation_dt[match(dated, s1$subjid)]
    )
  }
})

test_that("a core-shaped run carries every premature discontinuation scenario (#138)", {
  test_at_log_threshold()
  skip_if_not_installed("gsm.mapping")
  snaps <- core_shaped_run()
  subj <- snaps[[3]]$Raw_SUBJ
  sc <- snaps[[3]]$Raw_STUDCOMP
  d <- subj[subj$drv_ip_dosed %in% "Y", ]
  dated <- !is.na(d$drv_treatment_discontinuation_dt)
  reason <- d$drv_premature_discontinuation_reason
  completed <- d$subjid %in% sc$subjid[sc$compyn %in% "Y"]
  undosed <- subj[!subj$drv_ip_dosed %in% "Y", ]

  expect_gte(sum(table(d$invid[dated]) >= 3), 5)
  expect_true(any(dated & is.na(reason)))
  expect_true(any(dated & grepl(", ", reason)))
  expect_true(any(dated & completed))
  expect_true(any(!dated & completed))
  expect_true(any(!dated & !completed))
  expect_true(any(!dated & !is.na(reason)))
  expect_true(any(d$drv_days_lapsed_enrl_discontinuation %in% 1L))
  expect_true(all(is.na(undosed$drv_treatment_discontinuation_dt)))
  expect_true(all(is.na(undosed$drv_premature_discontinuation_reason)))
})
