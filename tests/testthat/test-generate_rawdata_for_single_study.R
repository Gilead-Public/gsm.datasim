test_that("generate_rawdata_for_single_study works (#96)", {
  test_at_log_threshold()
  withr::local_options(lifecycle_verbosity = "quiet")
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
  expect_contains(names(snapshots[[1]]), c("Raw_IE", "Raw_Randomization"))

  first <- snapshots[[1]]
  unenrolled <- first$Raw_SUBJ$subjid[first$Raw_SUBJ$enrollyn == "N"]
  expect_disjoint(first$Raw_IE$subjid, unenrolled)
  # Only the earliest randomization record per subject survives.
  expect_equal(anyDuplicated(first$Raw_Randomization$subjid), 0L)
})

test_that("generate_rawdata_for_single_study accepts a risk_profile (#143)", {
  expect_contains(names(formals(generate_rawdata_for_single_study)), "risk_profile")
  expect_contains(names(formals(generate_snapshots_from_combined_specs)), "risk_profile")
  expect_null(formals(generate_rawdata_for_single_study)$risk_profile)
})

test_that("prepare_combined_specs_for_generation filters to desired_specs", {
  specs <- list(
    Raw_AE = list(aest_dt = list(required = TRUE)),
    Raw_SUBJ = list(subjid = list(required = TRUE))
  )
  prepared <- prepare_combined_specs_for_generation(specs, desired_specs = c("Raw_SUBJ", "Raw_AE"))
  expect_named(prepared, c("Raw_SUBJ", "Raw_AE"))
})

test_that("dispatch_legacy_domain_generator routes each domain to its generator", {
  types <- c(
    "Raw_SITE", "Raw_SUBJ", "Raw_ENROLL", "Raw_IE", "Raw_VISIT", "Raw_STUDCOMP",
    "Raw_LB", "Raw_DATACHG", "Raw_DATAENT", "Raw_QUERY", "Raw_AE",
    "Raw_AntiCancer", "Raw_Baseline", "Raw_Consents", "Raw_Death",
    "Raw_Randomization", "Raw_OverallResponse", "Raw_PK", "Raw_PD"
  )
  mocks <- lapply(types, function(type) {
    force(type)
    function(...) data.frame(generator = type, nargs = ...length())
  })
  names(mocks) <- types
  do.call(testthat::local_mocked_bindings, c(mocks, list(.env = environment())))

  for (type in types) {
    res <- dispatch_legacy_domain_generator(
      data_type = type, data = list(), previous_data = list(), combined_specs = list(),
      n = 3, start_date = as.Date("2023-01-01"), end_date = as.Date("2023-02-01"),
      SnapshotCount = 2, SnapshotWidth = "months"
    )
    expect_equal(res$generator, type, info = type)
  }
})

test_that("generate_snapshots_from_combined_specs falls back to legacy generators", {
  skip_if_not_installed("gsm.mapping")
  test_at_log_threshold()
  withr::local_options(lifecycle_verbosity = "quiet")
  local_mocked_bindings(generate_domain_from_registry = function(...) NULL)

  snapshots <- generate_rawdata_for_single_study(
    SnapshotCount = 1, SnapshotWidth = "months", ParticipantCount = 20,
    SiteCount = 3, StudyID = "ABC", workflow_path = "workflow/1_mappings",
    mappings = "AE", package = "gsm.mapping"
  )
  expect_contains(names(snapshots[[1]]), c("Raw_SITE", "Raw_SUBJ", "Raw_AE"))
})
