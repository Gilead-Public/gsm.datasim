test_that("domain registry exposes required schema for migrated domains", {
  registry <- get_domain_registry()

  expect_true("Raw_AE" %in% names(registry))
  expect_true("Raw_LB" %in% names(registry))

  expected_fields <- sort(c("dataset", "required_inputs", "count_fn", "generate_fn"))

  ae_entry <- registry$Raw_AE
  expect_equal(sort(names(ae_entry)), expected_fields)
  expect_equal(ae_entry$dataset, "Raw_AE")
  expect_true(is.function(ae_entry$count_fn))
  expect_true(is.function(ae_entry$generate_fn))

  lb_entry <- registry$Raw_LB
  expect_equal(sort(names(lb_entry)), expected_fields)
  expect_equal(lb_entry$dataset, "Raw_LB")
  expect_true(is.function(lb_entry$count_fn))
  expect_true(is.function(lb_entry$generate_fn))
})

test_that("Raw_AE migrated domain adapter generates data frame", {
  set.seed(123)

  snapshot_data <- generate_rawdata_for_single_study(
    SnapshotCount = 1,
    SnapshotWidth = "months",
    ParticipantCount = 20,
    SiteCount = 5,
    StudyID = "REGISTRY-TEST",
    workflow_path = "workflow/1_mappings",
    mappings = c("STUDY", "SITE", "SUBJ", "ENROLL", "SV", "VISIT", "AE"),
    package = "gsm.mapping"
  )

  combined_specs <- load_specs(
    workflow_path = "workflow/1_mappings",
    mappings = c("STUDY", "SITE", "SUBJ", "ENROLL", "AE"),
    package = "gsm.mapping"
  )
  combined_specs <- prepare_combined_specs_for_generation(combined_specs)
  data <- snapshot_data[[1]]

  context <- list(
    data = data,
    previous_data = list(),
    combined_specs = combined_specs,
    n = 30,
    start_date = as.Date("2012-01-01"),
    end_date = as.Date("2012-01-31")
  )

  ae_df <- generate_domain_from_registry("Raw_AE", context)

  expect_s3_class(ae_df, "data.frame")
  expect_true(nrow(ae_df) >= 0)
})

test_that("Raw_LB migrated domain adapter generates data frame", {
  set.seed(123)

  snapshot_data <- generate_rawdata_for_single_study(
    SnapshotCount = 1,
    SnapshotWidth = "months",
    ParticipantCount = 20,
    SiteCount = 5,
    StudyID = "REGISTRY-TEST-LB",
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

  expect_s3_class(lb_df, "data.frame")
  expect_true(nrow(lb_df) >= 0)
})
