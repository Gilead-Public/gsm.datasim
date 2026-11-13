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

test_that("review scenarios align with source flags without inventing decision dates (#134)", {
  results <- expand.grid(GroupID = sprintf("%04d", 1:8), SnapshotDate = as.Date(c("2025-01-01", "2025-04-01")))
  results$StudyID <- "SYNTHETIC-STUDY"
  results$GroupLevel <- "Site"
  results$MetricID <- "Analysis_kri0001"
  results$Flag <- 1
  results$Flag[results$GroupID == "0001" & results$SnapshotDate == max(results$SnapshotDate)] <- 0
  simulated <- simulate_action_log_reviews(results)
  expect_s3_class(simulated, "data.frame")
  expect_identical(simulated, simulate_action_log_reviews(results[nrow(results):1, ]))
  actions <- simulated
  expect_false(any(c("ActionedDate", "ActionedDateSource", "ActionScope") %in% names(actions)))
  expect_true(all(actions$ExtractionDate == max(results$SnapshotDate)))
  expect_setequal(actions$State, .action_log_states)
  expect_true(all(actions$State[actions$GroupID == "0001"] == "Closed Action"))
  matched <- merge(actions, results, by = c("StudyID", "SnapshotDate", "GroupLevel", "GroupID", "MetricID"))
  expect_equal(nrow(matched), nrow(actions))
  expect_true(all(matched$Flag != 0))
  expect_true(all(is.na(actions$ActionID[actions$State %in% c("No Action", "Awaiting Triage")])))
  expect_true(anyDuplicated(actions$RiskSignalID) > 0L)
  expect_equal(nrow(simulate_action_log_reviews(results[FALSE, ])), 0L)
  expect_gt(nrow(simulate_action_log_reviews(results[results$GroupID == "0002", ])), 0L)
  expect_equal(nrow(simulate_action_log_reviews(results, extraction_date = min(results$SnapshotDate) - 1)), 0L)
  results$GroupLevel <- "Country"
  expect_equal(nrow(simulate_action_log_reviews(results)), 0L)
})

test_that("review scenarios preserve day-90 and day-91 proxy boundaries (#134)", {
  date <- as.Date("2025-04-01")
  results <- data.frame(StudyID = "SYNTHETIC-STUDY", GroupLevel = "Site",
    GroupID = sprintf("%04d", 1:6), MetricID = "Analysis_kri0001", Flag = 1,
    SnapshotDate = date - c(0, 0, 0, 90, 0, 91))
  simulated <- simulate_action_log_reviews(results)
  dismissed <- simulated[simulated$State == "No Action", ]
  expect_equal(as.integer(dismissed$ExtractionDate - dismissed$SnapshotDate), c(91L, 90L))
  expect_true(all(is.na(dismissed$ActionID)))
})

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
      "GroupLevel", "GroupID", "GroupLabel", "Country", "MetricID", "MetricLabel",
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
      "ActionResolvedDate", "ActionCriticality"
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
      RiskSignalID = "character", RiskSignalURL = "character",
      StudyID = "character", SnapshotDate = "character",
      GroupLevel = "character", GroupID = "character", GroupLabel = "character",
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
      ActionID = "character", ActionURL = "character",
      RiskSignalID = "character", ActionState = "character",
      AssignedTo = "character", RecommendedAction = "character",
      ActionTaken = "character", FunctionalArea = "character",
      CTMSID = "character", ActionCreatedDate = "character",
      ActionResolvedDate = "character", ActionCriticality = "logical"
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

test_that("source-neutral scenarios retain unactioned and multi-action signals (#134)", {
  simulated <- simulate_action_log(
    make_domain_results(), seed = 134, include_intermediates = TRUE,
    actions_per_signal = c(0L, 2L, 1L, 0L),
    action_states = c("Open Action", "Closed Action", "Open Action")
  )
  expect_equal(nrow(simulated$all_risk_signals), 4)
  expect_equal(nrow(simulated$actions), 3)
  expect_equal(nrow(simulated$action_log), 5)
  expect_type(simulated$actions$ActionCriticality, "logical")
  expect_true(all(is.na(simulated$actions$ActionCriticality)))
  expect_equal(anyDuplicated(simulated$actions$ActionID), 0L)
  expect_equal(sum(is.na(simulated$action_log$ActionID)), 2)
  expect_false(any(simulated$action_log$RiskSignalDuplicateFlag))
  unactioned <- simulated$action_log[is.na(simulated$action_log$ActionID), ]
  expect_equal(unactioned$State, unactioned$RiskSignalState)
  expect_true(all(is.na(unactioned$FunctionalArea)))
  expect_true(any(simulated$action_log$ActionState !=
    simulated$action_log$RiskSignalState, na.rm = TRUE))
  expect_error(
    simulate_action_log(make_domain_results(), actions_per_signal = -1),
    "actions_per_signal"
  )
  expect_error(
    simulate_action_log(make_domain_results(), action_states = "Unknown"),
    "action_states"
  )
})

test_that("separate extractions can preserve different states for one signal (#134)", {
  results <- make_domain_results()[1, ]
  results$State <- "Awaiting Triage"
  earlier <- simulate_action_log(results, extraction_date = as.Date("2025-02-01"))
  results$State <- "No Action"
  later <- simulate_action_log(results, extraction_date = as.Date("2025-02-28"))
  expect_identical(earlier$RiskSignalID, later$RiskSignalID)
  expect_false(identical(earlier$State, later$State))
  expect_lt(earlier$ExtractionDate, later$ExtractionDate)
})