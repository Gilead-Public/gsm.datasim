test_that("create_multiple_longitudinal_studies works with basic configuration", {
  skip_if_not_installed("gsm.mapping")
  
  studies <- create_multiple_longitudinal_studies(
    study_names = c("TEST-001", "TEST-002"),
    participants = 20,
    sites = 2,
    snapshots = 2,
    domains = c("AE"),
    run_analytics = FALSE,
    verbose = FALSE
  )
  
  # Check the return structure
  expect_s3_class(studies, "multiple_longitudinal_studies")
  expect_type(studies, "list")
  expect_equal(length(studies), 2)
  expect_equal(names(studies), c("TEST-001", "TEST-002"))
  
  # Check each study structure
  for (study_name in names(studies)) {
    study <- studies[[study_name]]
    expect_s3_class(study, "longitudinal_study")
    expect_equal(study$config$participants, 20)
    expect_equal(study$config$sites, 2)
    expect_equal(length(study$raw_data), 2) # 2 snapshots
    expect_equal(study$config$domains, "AE")
  }
})

test_that("create_multiple_longitudinal_studies works with per-study configuration", {
  skip_if_not_installed("gsm.mapping")
  
  studies <- create_multiple_longitudinal_studies(
    study_names = c("SMALL-001", "LARGE-001"),
    participants = c(10, 50),
    sites = c(1, 3),
    snapshots = 2,
    domains = c("AE"),
    study_configs = list(
      "SMALL-001" = list(interval = "2 weeks"),
      "LARGE-001" = list(interval = "1 month", domains = c("AE", "LB"))
    ),
    run_analytics = FALSE,
    verbose = FALSE
  )
  
  expect_equal(length(studies), 2)
  expect_equal(studies[["SMALL-001"]]$config$participants, 10)
  expect_equal(studies[["SMALL-001"]]$config$sites, 1)
  expect_equal(studies[["LARGE-001"]]$config$participants, 50)
  expect_equal(studies[["LARGE-001"]]$config$sites, 3)
  expect_equal(studies[["SMALL-001"]]$config$domains, "AE")
  expect_equal(studies[["LARGE-001"]]$config$domains, c("AE", "LB"))
})

test_that("create_multiple_longitudinal_studies validates inputs", {
  # Empty study names
  expect_error(
    create_multiple_longitudinal_studies(study_names = character(0)),
    "study_names must contain at least one study name"
  )
  
  # Duplicate study names
  expect_error(
    create_multiple_longitudinal_studies(study_names = c("TEST-001", "TEST-001")),
    "study_names contains duplicate values"
  )
})

test_that("print method works for multiple_longitudinal_studies", {
  skip_if_not_installed("gsm.mapping")
  
  studies <- create_multiple_longitudinal_studies(
    study_names = c("PRINT-TEST"),
    participants = 10,
    sites = 1,
    snapshots = 1,
    domains = c("AE"),
    run_analytics = FALSE,
    verbose = FALSE
  )
  
  # Test that print doesn't error
  expect_output(print(studies), "Multiple Longitudinal Studies Collection")
  expect_output(print(studies), "Number of studies: 1")
  expect_output(print(studies), "Study: PRINT-TEST")
})

test_that("summary method works for multiple_longitudinal_studies", {
  skip_if_not_installed("gsm.mapping")
  
  studies <- create_multiple_longitudinal_studies(
    study_names = c("SUMMARY-001", "SUMMARY-002"),
    participants = c(10, 20),
    sites = c(1, 2),
    snapshots = 2,
    domains = c("AE"),
    run_analytics = FALSE,
    verbose = FALSE
  )
  
  summ <- summary(studies)
  
  expect_s3_class(summ, "summary.multiple_longitudinal_studies")
  expect_equal(summ$n_studies, 2)
  expect_equal(summ$total_participants, 30)
  expect_equal(summ$total_sites, 3)
  expect_equal(summ$total_snapshots, 4) # 2 studies * 2 snapshots each
  
  # Test summary print method
  expect_output(print(summ), "Summary: Multiple Longitudinal Studies")
  expect_output(print(summ), "Number of studies: 2")
})