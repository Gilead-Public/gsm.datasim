make_action_log_results <- function() {
  data.frame(
    StudyID = rep("AA-AA-000-0000", 7),
    SnapshotDate = as.Date(c(
      "2026-01-01", "2026-02-01", "2026-03-01",
      "2026-01-01", "2026-02-01", "2026-03-01", "2026-03-01"
    )),
    GroupLevel = "Site",
    GroupID = c("1001", "1001", "1001", "1002", "1002", "1002", "1003"),
    MetricID = c(
      "Analysis_kri0001", "Analysis_kri0001", "Analysis_kri0001",
      "Analysis_kri0002", "Analysis_kri0002", "Analysis_kri0002",
      "Analysis_kri0003"
    ),
    Flag = c(1L, 1L, 1L, 1L, 1L, 1L, 0L),
    Country = c("US", "US", "US", "CA", "CA", "CA", "GB"),
    stringsAsFactors = FALSE
  )
}

test_that("simulate_risk_signal_work_items returns GetWorkItems-compatible records (#134)", {
  results <- make_action_log_results()

  work_items <- simulate_risk_signal_work_items(
    results,
    seed = 134,
    state_probabilities = c(
      "Awaiting Triage" = 1,
      "No Action" = 0,
      "Open Action" = 0,
      "Closed Action" = 0
    )
  )

  expect_type(work_items, "list")
  expect_length(work_items, 6)
  expect_named(
    work_items[[1]],
    c("id", "rev", "fields", "multilineFieldsFormat", "url", "browser_url")
  )
  expect_true(all(c(
    "System.TeamProject",
    "System.WorkItemType",
    "System.State",
    "Custom.SnapshotDate",
    "Custom.GroupLevel",
    "Custom.GroupID",
    "Custom.MetricID",
    "Custom.Flag"
  ) %in% names(work_items[[1]]$fields)))
  expect_equal(work_items[[1]]$fields$System.TeamProject, "AA-AA-000-0000")
  expect_equal(work_items[[1]]$fields$System.WorkItemType, "Risk Signal")
  expect_match(work_items[[1]]$browser_url, "AA-AA-000-0000/_workitems/edit/")
  expect_true(is.list(work_items[[1]]$fields$System.CreatedBy))
})

test_that("simulate_risk_signal_work_items supports deterministic state transitions (#134)", {
  transition_matrix <- matrix(
    0,
    nrow = 4,
    ncol = 4,
    dimnames = list(
      c("Awaiting Triage", "No Action", "Open Action", "Closed Action"),
      c("Awaiting Triage", "No Action", "Open Action", "Closed Action")
    )
  )
  transition_matrix["Awaiting Triage", "Open Action"] <- 1
  transition_matrix["No Action", "No Action"] <- 1
  transition_matrix["Open Action", "Closed Action"] <- 1
  transition_matrix["Closed Action", "Closed Action"] <- 1

  work_items <- simulate_risk_signal_work_items(
    make_action_log_results(),
    seed = 134,
    state_probabilities = c(
      "Awaiting Triage" = 1,
      "No Action" = 0,
      "Open Action" = 0,
      "Closed Action" = 0
    ),
    transition_matrix = transition_matrix
  )

  site_1001 <- work_items[vapply(
    work_items,
    function(item) item$fields$Custom.GroupID == "1001",
    logical(1)
  )]

  expect_equal(
    vapply(site_1001, function(item) item$fields$System.State, character(1)),
    c("Awaiting Triage", "Open Action", "Closed Action")
  )
  expect_identical(
    work_items,
    simulate_risk_signal_work_items(
      make_action_log_results(),
      seed = 134,
      state_probabilities = c(
        "Awaiting Triage" = 1,
        "No Action" = 0,
        "Open Action" = 0,
        "Closed Action" = 0
      ),
      transition_matrix = transition_matrix
    )
  )
})

test_that("project_action_log_domains conforms to source-neutral schemas (#134)", {
  skip_if_not_installed("grail.ado")
  skip_if_not(
    all(c("TabulateRiskSignals", "TabulateActions") %in%
      getNamespaceExports("grail.ado")),
    "grail.ado PR #121 projection APIs are unavailable"
  )

  work_items <- simulate_risk_signal_work_items(
    make_action_log_results(),
    seed = 134
  )
  domains <- project_action_log_domains(work_items)

  expect_s3_class(domains$all_risk_signals, "data.frame")
  expect_s3_class(domains$actions, "data.frame")
  expect_equal(nrow(domains$all_risk_signals), 6)
  expect_equal(nrow(domains$actions), 6)
  expect_true(all(domains$all_risk_signals$StudyID == "AA-AA-000-0000"))
  expect_identical(domains$actions$ActionID, domains$actions$RiskSignalID)
})

test_that("simulate_action_log delegates final report construction to grail (#134)", {
  skip_if_not_installed("grail.ado")
  skip_if_not_installed("grail")
  skip_if_not("BuildActionLog" %in% getNamespaceExports("grail"))

  action_log <- simulate_action_log(
    make_action_log_results(),
    seed = 134,
    extraction_date = as.Date("2026-03-08")
  )
  schema_names <- vapply(
    grail::ActionLogSchema$fields,
    function(field) field$name,
    character(1)
  )

  expect_s3_class(action_log, "data.frame")
  expect_identical(names(action_log), schema_names)
  expect_s3_class(action_log$SnapshotDate, "Date")
  expect_s3_class(action_log$ExtractionDate, "Date")
  expect_type(action_log$RiskSignalID, "integer")
  expect_type(action_log$RiskSignalDuplicateFlag, "logical")
  expect_true(all(action_log$ExtractionDate == as.Date("2026-03-08")))
  expect_setequal(action_log$Country, c("US", "CA"))
})

test_that("simulate_action_log exposes missing and duplicate scenarios (#134)", {
  skip_if_not_installed("grail.ado")
  skip_if_not_installed("grail")

  missing_action_log <- simulate_action_log(
    make_action_log_results(),
    missing_probability = 1,
    seed = 134
  )
  expect_s3_class(missing_action_log, "data.frame")
  expect_equal(nrow(missing_action_log), 0)
  expect_identical(
    names(missing_action_log),
    vapply(grail::ActionLogSchema$fields, function(field) field$name, character(1))
  )

  simulated <- simulate_action_log(
    make_action_log_results(),
    duplicate_probability = 1,
    seed = 134,
    include_intermediates = TRUE
  )

  expect_named(
    simulated,
    c("work_items", "all_risk_signals", "actions", "action_log")
  )
  expect_length(simulated$work_items, 12)
  expect_equal(nrow(simulated$all_risk_signals), 12)
  expect_equal(nrow(simulated$actions), 12)
  expect_equal(nrow(simulated$action_log), 12)
  expect_equal(sum(simulated$action_log$RiskSignalDuplicateFlag), 6)
  expect_equal(length(unique(simulated$action_log$RiskSignalID)), 12)
})

test_that("generated work items include the formal grail.ado raw schema (#134)", {
  skip_if_not_installed("grail.ado")

  work_items <- simulate_risk_signal_work_items(
    make_action_log_results(),
    seed = 134
  )
  schema_names <- vapply(
    grail.ado::RiskSignalWorkItemsSchema$fields,
    function(field) field$name,
    character(1)
  )

  expect_true(all(schema_names %in% names(work_items[[1]]$fields)))
})

test_that("source-neutral adapters accept the AA-derived grail.ado fixture (#134)", {
  skip_if_not_installed("grail.ado")
  skip_if_not_installed("grail")
  skip_if_not(
    all(c("TabulateRiskSignals", "TabulateActions") %in%
      getNamespaceExports("grail.ado")),
    "grail.ado PR #121 projection APIs are unavailable"
  )
  skip_if_not("BuildActionLog" %in% getNamespaceExports("grail"))

  reference_items <- grail.ado::RiskSignalWorkItems[1:3]
  domains <- project_action_log_domains(reference_items)
  reference_action_log <- grail::BuildActionLog(
    domains$all_risk_signals,
    domains$actions,
    dtExtractionDate = as.Date("2026-03-08")
  )

  expect_equal(nrow(domains$all_risk_signals), 3)
  expect_equal(nrow(domains$actions), 3)
  expect_identical(
    names(reference_action_log),
    vapply(grail::ActionLogSchema$fields, function(field) field$name, character(1))
  )
})

test_that("simulation preserves caller RNG state and validates study scope (#134)", {
  set.seed(134)
  previous_seed <- .Random.seed

  simulate_risk_signal_work_items(make_action_log_results(), seed = 42)

  expect_identical(.Random.seed, previous_seed)

  multiple_studies <- make_action_log_results()
  multiple_studies$StudyID[1] <- "ANOTHER-STUDY"
  expect_error(
    simulate_risk_signal_work_items(multiple_studies),
    "exactly one non-missing StudyID"
  )
  expect_error(
    simulate_risk_signal_work_items(
      make_action_log_results(),
      state_probabilities = c("Open Action" = 1)
    ),
    "state_probabilities"
  )
})