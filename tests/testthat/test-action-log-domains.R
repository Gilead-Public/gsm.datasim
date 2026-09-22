make_domain_results <- function() {
  data.frame(
    StudyID = "AA-AA-000-0000",
    SnapshotDate = as.Date(c(
      "2025-01-31", "2025-02-28", "2025-02-28", "2025-02-28"
    )),
    GroupLevel = "Site",
    GroupID = c("1001", "1001", "1002", "1003"),
    MetricID = c(
      "Analysis_kri0001", "Analysis_kri0001",
      "Analysis_kri0002", "Analysis_kri0003"
    ),
    Flag = 1L,
    State = c("No Action", "Open Action", "Closed Action", "No Action"),
    Country = c("US", "US", "CA", "GB"),
    FunctionalArea = "Central Monitoring",
    CTMSID = c(NA, NA, "CTMS-1002", NA),
    stringsAsFactors = FALSE
  )
}

test_that("simulate_action_log_domains projects PR 121 source-neutral domains (#134)", {
  simulated <- simulate_action_log_domains(
    make_domain_results(),
    seed = 134,
    work_item_id_start = 630000L
  )

  expect_named(simulated, c("work_items", "all_risk_signals", "actions"))
  expect_length(simulated$work_items, 4)
  expect_equal(nrow(simulated$all_risk_signals), 4)
  expect_equal(nrow(simulated$actions), 4)
  expect_identical(
    names(simulated$all_risk_signals),
    c(
      "RiskSignalID", "RiskSignalURL", "StudyID", "SnapshotDate",
      "GroupLevel", "GroupID", "Country", "MetricID", "MetricLabel",
      "MetricAbbreviation", "MetricType", "SignalDescription",
      "RiskSignalState", "RiskSignalCreatedDate", "RiskSignalResolvedDate"
    )
  )
  expect_identical(
    names(simulated$actions),
    c(
      "ActionID", "ActionURL", "RiskSignalID", "ActionState",
      "AssignedTo", "RecommendedAction", "ActionTaken",
      "FunctionalArea", "CTMSID", "ActionCreatedDate",
      "ActionResolvedDate"
    )
  )
  expect_identical(simulated$actions$ActionID, simulated$actions$RiskSignalID)
  expect_equal(
    simulated$actions$ActionState,
    c("No Action", "Open Action", "Closed Action", "No Action")
  )
  expect_equal(simulated$actions$CTMSID[3], "CTMS-1002")
  expect_false("FunctionalArea" %in% names(simulated$all_risk_signals))
  expect_true(all(simulated$all_risk_signals$Country %in% c("US", "CA", "GB")))
})

test_that("project_action_log_domains returns typed empty domains (#134)", {
  projected <- project_action_log_domains(list())

  expect_equal(nrow(projected$all_risk_signals), 0)
  expect_equal(nrow(projected$actions), 0)
  expect_identical(
    vapply(projected$all_risk_signals, typeof, character(1)),
    c(
      RiskSignalID = "integer", RiskSignalURL = "character",
      StudyID = "character", SnapshotDate = "character",
      GroupLevel = "character", GroupID = "character",
      Country = "character", MetricID = "character",
      MetricLabel = "character", MetricAbbreviation = "character",
      MetricType = "character", SignalDescription = "character",
      RiskSignalState = "character", RiskSignalCreatedDate = "character",
      RiskSignalResolvedDate = "character"
    )
  )
  expect_identical(
    vapply(projected$actions, typeof, character(1)),
    c(
      ActionID = "integer", ActionURL = "character",
      RiskSignalID = "integer", ActionState = "character",
      AssignedTo = "character", RecommendedAction = "character",
      ActionTaken = "character", FunctionalArea = "character",
      CTMSID = "character", ActionCreatedDate = "character",
      ActionResolvedDate = "character"
    )
  )
})

test_that("standard domain projection excludes QTL-associated actions (#134)", {
  results <- make_domain_results()[1:2, ]
  results$MetricType <- c("KRI", "QTL")
  projected <- simulate_action_log_domains(results, seed = 134)

  expect_equal(nrow(projected$all_risk_signals), 1)
  expect_equal(nrow(projected$actions), 1)
  expect_equal(projected$all_risk_signals$MetricType, "KRI")
})

test_that("source-neutral projections preserve deterministic histories (#134)", {
  first <- simulate_action_log_domains(make_domain_results(), seed = 134)
  second <- simulate_action_log_domains(make_domain_results(), seed = 134)

  expect_identical(first, second)
  expect_equal(
    first$all_risk_signals$SnapshotDate,
    c("2025-01-31", "2025-02-28", "2025-02-28", "2025-02-28")
  )
})

test_that("lookback scenarios exercise action-window boundaries (#134, #63)", {
  simulated <- simulate_action_log_lookback_scenarios()

  expect_named(simulated, c(
    "reporting_results", "expectations", "work_items",
    "all_risk_signals", "actions", "action_log"
  ))
  expect_equal(nrow(simulated$reporting_results), 15)
  expect_equal(nrow(simulated$action_log), 15)
  expect_equal(
    simulated$expectations$Scenario,
    c("older-open", "recent-closed", "current-open", "no-action", "awaiting-triage")
  )
  expect_identical(
    simulated$expectations[c("Lookback1", "Lookback2", "Lookback3")],
    data.frame(
      Lookback1 = c(FALSE, FALSE, TRUE, FALSE, FALSE),
      Lookback2 = c(FALSE, TRUE, TRUE, FALSE, FALSE),
      Lookback3 = c(TRUE, TRUE, TRUE, FALSE, FALSE)
    )
  )
  action_rows <- merge(
    simulated$all_risk_signals[c(
      "RiskSignalID", "SnapshotDate", "GroupID", "MetricID"
    )],
    simulated$actions[c("RiskSignalID", "ActionState")],
    by = "RiskSignalID"
  )
  expected_key <- with(
    simulated$reporting_results,
    paste(SnapshotDate, GroupID, MetricID, sep = "\r")
  )
  action_key <- with(
    action_rows,
    paste(substr(SnapshotDate, 1, 10), GroupID, MetricID, sep = "\r")
  )
  expect_equal(
    action_rows$ActionState,
    simulated$reporting_results$State[match(action_key, expected_key)]
  )
})

test_that("lookback scenarios require three increasing snapshots (#134)", {
  expect_error(
    simulate_action_log_lookback_scenarios(
      snapshot_dates = as.Date(c("2026-01-31", "2026-01-31", "2026-03-31"))
    ),
    "three increasing dates"
  )
})