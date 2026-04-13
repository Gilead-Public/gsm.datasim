test_that("generate_rawdata_for_single_study works", {
  result <- raw_data_generator(
    template_path = system.file("small_template.csv", package = "gsm.datasim"),
    mappings = c("AE")
  )

  expect_equal(length(result), 3)
  expect_equal(names(result), c("S0001", "S0004", "S0006"))
})

test_that("generate_rawdata_for_single_study works", {
  result <- raw_data_generator(
    SnapshotCount = 2,
    SnapshotWidth = "months",
    ParticipantCount = 50,
    SiteCount = 5,
    StudyID = "ABC",
    workflow_path = "workflow/1_mappings",
    mappings = "AE",
    package = "gsm.mapping"
  )

  expect_equal(length(result), 1) # 1 study
  expect_equal(names(result), c("ABC")) # 1 study with StudyID ABC
  expect_equal(length(result$ABC), 2) # 2 snapshots
})

test_that("raw_data_generator supports explicit legacy generation mode", {
  old_warned <- .gsm_datasim_runtime_state$legacy_mode_warned
  on.exit({ .gsm_datasim_runtime_state$legacy_mode_warned <- old_warned }, add = TRUE)
  .gsm_datasim_runtime_state$legacy_mode_warned <- FALSE

  expect_warning(
    raw_data_generator(
      SnapshotCount = 2,
      SnapshotWidth = "months",
      ParticipantCount = 50,
      SiteCount = 5,
      StudyID = "ABC_LEGACY_WARN_1",
      workflow_path = "workflow/1_mappings",
      mappings = "AE",
      package = "gsm.mapping",
      generation_mode = "legacy"
    ),
    "Prefer generation_mode = 'core'"
  )

  expect_silent(
    raw_data_generator(
      SnapshotCount = 2,
      SnapshotWidth = "months",
      ParticipantCount = 50,
      SiteCount = 5,
      StudyID = "ABC_LEGACY_WARN_2",
      workflow_path = "workflow/1_mappings",
      mappings = "AE",
      package = "gsm.mapping",
      generation_mode = "legacy"
    )
  )

  result <- raw_data_generator(
    SnapshotCount = 2,
    SnapshotWidth = "months",
    ParticipantCount = 50,
    SiteCount = 5,
    StudyID = "ABC_LEGACY",
    workflow_path = "workflow/1_mappings",
    mappings = "AE",
    package = "gsm.mapping",
    generation_mode = "legacy"
  )

  expect_equal(length(result), 1)
  expect_equal(names(result), c("ABC_LEGACY"))
  expect_equal(length(result$ABC_LEGACY), 2)
})
