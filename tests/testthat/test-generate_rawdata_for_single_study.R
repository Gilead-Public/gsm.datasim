test_that("generate_rawdata_for_single_study works (#96)", {
  test_at_log_threshold()
  snapshots <- generate_rawdata_for_single_study(
    SnapshotCount = 2,
    SnapshotWidth = "months",
    ParticipantCount = 50,
    SiteCount = 5,
    StudyID = "ABC",
    workflow_path = "workflow/1_mappings",
    mappings = "AE",
    package = "gsm.mapping",
    desired_specs = NULL
  )
  expect_equal(length(snapshots), 2)
  expect_equal(
    names(snapshots[[1]]),
    c(
      "Raw_STUDY",
      "Raw_SITE",
      "Raw_SUBJ",
      "Raw_ENROLL",
      "Raw_VISIT",
      "Raw_AE",
      "Raw_DATAENT"
    )
  )
})

# The IE and Randomization post-processing filters (unenrolled-subject removal
# and earliest-record-per-subject) only run when those domains are requested.
test_that("generate_rawdata_for_single_study post-processes IE and Randomization", {
  skip_if_not_installed("gsm.mapping")
  test_at_log_threshold()
  withr::local_options(lifecycle_verbosity = "quiet")

  snapshots <- generate_rawdata_for_single_study(
    SnapshotCount = 2,
    SnapshotWidth = "months",
    ParticipantCount = 30,
    SiteCount = 4,
    StudyID = "ABC",
    workflow_path = "workflow/1_mappings",
    mappings = c("IE", "Randomization"),
    package = "gsm.mapping",
    desired_specs = NULL
  )

  expect_length(snapshots, 2)
  expect_true(all(c("Raw_IE", "Raw_Randomization") %in% names(snapshots[[1]])))

  first <- snapshots[[1]]
  unenrolled <- first$Raw_SUBJ$subjid[first$Raw_SUBJ$enrollyn == "N"]
  expect_false(any(first$Raw_IE$subjid %in% unenrolled))
  # Only the earliest randomization record per subject survives.
  expect_equal(anyDuplicated(first$Raw_Randomization$subjid), 0L)
})

test_that("generate_rawdata_for_single_study accepts a vs_risk_profile (#148)", {
  expect_true("vs_risk_profile" %in% names(formals(generate_rawdata_for_single_study)))
  expect_true("vs_risk_profile" %in% names(formals(generate_snapshots_from_combined_specs)))
  expect_null(formals(generate_rawdata_for_single_study)$vs_risk_profile)
})
