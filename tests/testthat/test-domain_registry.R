test_that("domain registry exposes required schema for migrated domains", {
  registry <- get_domain_registry()

  expect_contains(names(registry), "Raw_AE")
  expect_contains(names(registry), "Raw_LB")

  expected_fields <- sort(c(
    "dataset",
    "required_inputs",
    "count_fn",
    "generate_fn"
  ))

  ae_entry <- registry$Raw_AE
  expect_named(ae_entry, expected_fields, ignore.order = TRUE)
  expect_equal(ae_entry$dataset, "Raw_AE")
  expect_true(is.function(ae_entry$count_fn))
  expect_true(is.function(ae_entry$generate_fn))

  lb_entry <- registry$Raw_LB
  expect_named(lb_entry, expected_fields, ignore.order = TRUE)
  expect_equal(lb_entry$dataset, "Raw_LB")
  expect_true(is.function(lb_entry$count_fn))
  expect_true(is.function(lb_entry$generate_fn))
})

test_that("every domain registry entry conforms to the required schema (#124)", {
  registry <- get_domain_registry()

  expect_gt(length(registry), 0)

  expected_fields <- sort(c(
    "dataset",
    "required_inputs",
    "count_fn",
    "generate_fn"
  ))

  for (domain_name in names(registry)) {
    entry <- registry[[domain_name]]

    expect_equal(
      sort(names(entry)),
      expected_fields,
      info = paste("fields for", domain_name)
    )
    expect_identical(
      entry$dataset,
      domain_name,
      info = paste("dataset key for", domain_name)
    )
    expect_true(
      is.function(entry$count_fn),
      info = paste("count_fn for", domain_name)
    )
    expect_true(
      is.function(entry$generate_fn),
      info = paste("generate_fn for", domain_name)
    )
    expect_true(
      is.character(entry$required_inputs) && length(entry$required_inputs) > 0,
      info = paste("required_inputs for", domain_name)
    )
  }
})

test_that("Raw_AE migrated domain adapter generates data frame", {
  test_at_log_threshold()
  set.seed(123)
  lifecycle::expect_deprecated(
    snapshot_data <- generate_rawdata_for_single_study(
      SnapshotCount = 1,
      SnapshotWidth = "months",
      ParticipantCount = 20,
      SiteCount = 5,
      StudyID = "REGISTRY-TEST",
      workflow_path = "workflow/1_mappings",
      mappings = c("STUDY", "SITE", "SUBJ", "ENROLL", "SV", "VISIT", "AE"),
      package = "gsm.mapping"
    ),
    "generate_rawdata_for_single_study"
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
  test_at_log_threshold()
  set.seed(123)
  lifecycle::expect_deprecated(
    snapshot_data <- generate_rawdata_for_single_study(
      SnapshotCount = 1,
      SnapshotWidth = "months",
      ParticipantCount = 20,
      SiteCount = 5,
      StudyID = "REGISTRY-TEST-LB",
      workflow_path = "workflow/1_mappings",
      mappings = c("STUDY", "SITE", "SUBJ", "ENROLL", "SV", "VISIT", "LB"),
      package = "gsm.mapping"
    ),
    "generate_rawdata_for_single_study"
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

