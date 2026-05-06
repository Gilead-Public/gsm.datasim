test_that("generate_rawdata_for_single_study works", {
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
  expect_equal(names(snapshots[[1]]), c("Raw_STUDY", "Raw_SITE", "Raw_SUBJ", "Raw_ENROLL", "Raw_SV", "Raw_VISIT", "Raw_AE", "Raw_DATAENT"))
})
