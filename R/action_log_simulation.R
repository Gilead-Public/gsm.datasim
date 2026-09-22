.action_log_states <- c(
  "Awaiting Triage",
  "No Action",
  "Open Action",
  "Closed Action"
)

.validate_probability <- function(value, name) {
  if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
      value < 0 || value > 1) {
    stop(name, " must be a single probability between 0 and 1.", call. = FALSE)
  }
}

.normalize_state_probabilities <- function(probabilities) {
  if (!is.numeric(probabilities) ||
      !setequal(names(probabilities), .action_log_states) ||
      anyNA(probabilities) || any(probabilities < 0) ||
      sum(probabilities) <= 0) {
    stop(
      "state_probabilities must be a non-negative named vector containing all supported states.",
      call. = FALSE
    )
  }

  probabilities <- probabilities[.action_log_states]
  probabilities / sum(probabilities)
}

.normalize_transition_matrix <- function(transition_matrix) {
  if (is.null(transition_matrix)) {
    return(NULL)
  }

  if (!is.matrix(transition_matrix) || !is.numeric(transition_matrix) ||
      !setequal(rownames(transition_matrix), .action_log_states) ||
      !setequal(colnames(transition_matrix), .action_log_states) ||
      anyNA(transition_matrix) || any(transition_matrix < 0)) {
    stop(
      "transition_matrix must be a non-negative named matrix containing all supported states.",
      call. = FALSE
    )
  }

  transition_matrix <- transition_matrix[
    .action_log_states,
    .action_log_states,
    drop = FALSE
  ]
  row_totals <- rowSums(transition_matrix)
  if (any(row_totals <= 0)) {
    stop("Each transition_matrix row must have positive probability mass.", call. = FALSE)
  }

  transition_matrix / row_totals
}

.with_simulation_seed <- function(seed, code) {
  if (is.null(seed)) {
    return(code())
  }
  if (!is.numeric(seed) || length(seed) != 1L || is.na(seed)) {
    stop("seed must be NULL or a single numeric value.", call. = FALSE)
  }

  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) {
    previous_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit({
    if (had_seed) {
      assign(".Random.seed", previous_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  set.seed(seed)
  code()
}

.action_log_value <- function(data, name, default) {
  if (name %in% names(data)) {
    return(data[[name]])
  }
  if (length(default) == 1L) {
    return(rep(default, nrow(data)))
  }
  default
}

.action_log_datetime <- function(date) {
  paste0(format(as.Date(date), "%Y-%m-%d"), "T00:00:00Z")
}

.synthetic_identity <- function() {
  list(
    displayName = "Synthetic Monitor",
    uniqueName = "synthetic.monitor@example.invalid"
  )
}

.sample_action_log_states <- function(data, state_probabilities, transition_matrix) {
  states <- rep(NA_character_, nrow(data))
  supplied_states <- if ("State" %in% names(data)) data$State else states
  history_key <- paste(data$GroupLevel, data$GroupID, data$MetricID, sep = "\r")

  for (key in unique(history_key)) {
    indexes <- which(history_key == key)
    indexes <- indexes[order(data$SnapshotDate[indexes])]
    previous_state <- NULL

    for (index in indexes) {
      if (!is.na(supplied_states[index])) {
        state <- supplied_states[index]
      } else {
        probabilities <- if (is.null(previous_state) || is.null(transition_matrix)) {
          state_probabilities
        } else {
          transition_matrix[previous_state, ]
        }
        state <- sample(names(probabilities), size = 1L, prob = probabilities)
      }
      states[index] <- state
      previous_state <- state
    }
  }

  states
}

.make_risk_signal_work_item <- function(row, work_item_id, organization, extraction_date) {
  project <- row$StudyID
  state <- row$State
  identity <- .synthetic_identity()
  resolved <- state %in% c("No Action", "Closed Action")
  assigned <- state %in% c("Open Action", "Closed Action")
  state_change_date <- min(as.Date(row$SnapshotDate) + 7, extraction_date)
  browser_url <- paste0(
    "https://dev.azure.com/", organization, "/", project,
    "/_workitems/edit/", work_item_id
  )

  fields <- list(
    "System.AreaPath" = paste0(project, "\\Triage"),
    "System.TeamProject" = project,
    "System.IterationPath" = project,
    "System.WorkItemType" = "Risk Signal",
    "System.State" = state,
    "System.Reason" = "Synthetic risk signal",
    "System.CreatedDate" = .action_log_datetime(as.Date(row$SnapshotDate) + 1),
    "System.CreatedBy" = identity,
    "System.ChangedDate" = .action_log_datetime(state_change_date),
    "System.ChangedBy" = identity,
    "System.CommentCount" = 0L,
    "System.Title" = paste(row$MetricLabel, "at", row$GroupLabel),
    "System.BoardColumn" = state,
    "System.BoardColumnDone" = resolved,
    "System.AssignedTo" = if (assigned) identity else NULL,
    "Microsoft.VSTS.Common.StateChangeDate" = .action_log_datetime(state_change_date),
    "Microsoft.VSTS.Common.ActivatedBy" = if (assigned) identity else NULL,
    "Microsoft.VSTS.Common.ClosedBy" = if (state == "Closed Action") identity else NULL,
    "Microsoft.VSTS.Common.ResolvedBy" = if (resolved) identity else NULL,
    "Microsoft.VSTS.Common.ResolvedDate" = if (resolved) {
      .action_log_datetime(state_change_date)
    } else {
      NA_character_
    },
    "Custom.SnapshotDate" = as.Date(row$SnapshotDate),
    "Custom.GroupLevel" = row$GroupLevel,
    "Custom.GroupID" = row$GroupID,
    "Custom.GroupLabel" = row$GroupLabel,
    "Custom.GroupInfo" = row$GroupInfo,
    "Custom.Country" = row$Country,
    "Custom.GroupStatus" = row$GroupStatus,
    "Custom.Enrollment" = as.integer(row$Enrollment),
    "Custom.MetricType" = row$MetricType,
    "Custom.MetricID" = row$MetricID,
    "Custom.MetricLabel" = row$MetricLabel,
    "Custom.MetricAbbreviation" = row$MetricAbbreviation,
    "Custom.Flag" = as.integer(row$Flag),
    "Custom.FlagLabel" = row$FlagLabel,
    "Custom.SignalDescription" = row$SignalDescription,
    "Custom.FunctionalArea" = row$FunctionalArea,
    "Custom.RecommendedAction" = row$RecommendedAction,
    "Custom.ActionTaken" = row$ActionTaken,
    "Custom.CTMSID" = as.character(row$CTMSID),
    "Custom.Analytics" = row$Analytics,
    "Custom.Source" = "gsm.datasim",
    "Custom.Protocol" = project
  )
  fields <- fields[!vapply(fields, is.null, logical(1))]

  list(
    id = as.integer(work_item_id),
    rev = 1L,
    fields = fields,
    multilineFieldsFormat = list(
      "Custom.GroupInfo" = "html",
      "Custom.SignalDescription" = "html",
      "Custom.RecommendedAction" = "html",
      "Custom.ActionTaken" = "html"
    ),
    url = paste0(
      "https://dev.azure.com/", organization, "/", project,
      "/_apis/wit/workItems/", work_item_id
    ),
    browser_url = browser_url
  )
}

#' Simulate Azure DevOps risk signal work items
#'
#' Creates synthetic records with the list structure returned by
#' `grail.ado::GetWorkItems()`. Input rows represent candidate KRI findings;
#' rows with a missing or zero `Flag` are excluded.
#'
#' @param df_results Data frame containing `StudyID`, `SnapshotDate`,
#'   `GroupLevel`, `GroupID`, `MetricID`, and `Flag`. Optional display and action
#'   columns override generated defaults.
#' @param state_probabilities Named probabilities for the four ActionLog states.
#' @param transition_matrix Optional state-by-state transition probability
#'   matrix used for repeated group/metric findings across snapshots.
#' @param missing_probability Probability that an eligible finding has no ADO
#'   work item.
#' @param duplicate_probability Probability that an eligible work item is
#'   duplicated within its snapshot.
#' @param seed Optional random seed. The caller's random-number state is
#'   restored after generation.
#' @param extraction_date Date represented by the synthetic extraction. Defaults
#'   to seven days after the latest snapshot.
#' @param work_item_id_start First synthetic ADO work item ID.
#' @param organization Azure DevOps organization used only to construct URLs.
#'
#' @return A list of synthetic ADO work items.
#' @export
simulate_risk_signal_work_items <- function(
    df_results,
    state_probabilities = c(
      "Awaiting Triage" = 0.2,
      "No Action" = 0.4,
      "Open Action" = 0.2,
      "Closed Action" = 0.2
    ),
    transition_matrix = NULL,
    missing_probability = 0,
    duplicate_probability = 0,
    seed = NULL,
    extraction_date = NULL,
    work_item_id_start = 900000L,
    organization = "Gilead-RND-CDS-RBQM") {
  required_columns <- c(
    "StudyID", "SnapshotDate", "GroupLevel", "GroupID", "MetricID", "Flag"
  )
  missing_columns <- setdiff(required_columns, names(df_results))

  if (!is.data.frame(df_results)) {
    stop("df_results must be a data frame.", call. = FALSE)
  }
  if (length(missing_columns) > 0L) {
    stop(
      "df_results is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }
  if (length(unique(df_results$StudyID)) != 1L || anyNA(df_results$StudyID)) {
    stop("df_results must contain exactly one non-missing StudyID.", call. = FALSE)
  }
  if (!is.character(organization) || length(organization) != 1L || is.na(organization)) {
    stop("organization must be a single character value.", call. = FALSE)
  }
  if (!is.numeric(work_item_id_start) || length(work_item_id_start) != 1L ||
      is.na(work_item_id_start) || work_item_id_start < 1) {
    stop("work_item_id_start must be a positive number.", call. = FALSE)
  }

  state_probabilities <- .normalize_state_probabilities(state_probabilities)
  transition_matrix <- .normalize_transition_matrix(transition_matrix)
  .validate_probability(missing_probability, "missing_probability")
  .validate_probability(duplicate_probability, "duplicate_probability")

  data <- df_results
  data$SnapshotDate <- as.Date(data$SnapshotDate)
  if (anyNA(data$SnapshotDate) || anyNA(data$GroupLevel) ||
      anyNA(data$GroupID) || anyNA(data$MetricID)) {
    stop("ActionLog key columns must not contain missing values.", call. = FALSE)
  }
  if ("State" %in% names(data)) {
    invalid_states <- setdiff(stats::na.omit(unique(data$State)), .action_log_states)
    if (length(invalid_states) > 0L) {
      stop("State contains unsupported values.", call. = FALSE)
    }
  }

  data <- data[!is.na(data$Flag) & data$Flag != 0, , drop = FALSE]
  if (nrow(data) == 0L) {
    return(list())
  }
  data <- data[order(
    data$SnapshotDate,
    data$GroupLevel,
    data$GroupID,
    data$MetricID
  ), , drop = FALSE]

  if (is.null(extraction_date)) {
    extraction_date <- max(data$SnapshotDate) + 7
  }
  extraction_date <- as.Date(extraction_date)
  if (length(extraction_date) != 1L || is.na(extraction_date)) {
    stop("extraction_date must be a single date.", call. = FALSE)
  }

  .with_simulation_seed(seed, function() {
    data$State <- .sample_action_log_states(
      data,
      state_probabilities,
      transition_matrix
    )

    if (missing_probability > 0) {
      data <- data[stats::runif(nrow(data)) >= missing_probability, , drop = FALSE]
    }
    if (nrow(data) == 0L) {
      return(list())
    }

    data$GroupLabel <- .action_log_value(data, "GroupLabel", data$GroupID)
    country <- .action_log_value(data, "Country", "UNKNOWN")
    country[is.na(country) | country == ""] <- "UNKNOWN"
    data$GroupInfo <- .action_log_value(
      data,
      "GroupInfo",
      paste0("Country: ", country)
    )
    data$GroupStatus <- .action_log_value(data, "GroupStatus", "Active")
    data$Enrollment <- .action_log_value(data, "Enrollment", NA_integer_)
    data$MetricType <- .action_log_value(data, "MetricType", "KRI")
    data$MetricLabel <- .action_log_value(data, "MetricLabel", data$MetricID)
    data$MetricAbbreviation <- .action_log_value(
      data,
      "MetricAbbreviation",
      data$MetricID
    )
    data$FlagLabel <- .action_log_value(data, "FlagLabel", as.character(data$Flag))
    data$SignalDescription <- .action_log_value(
      data,
      "SignalDescription",
      paste("Synthetic risk signal for", data$MetricID, "at", data$GroupID)
    )
    data$FunctionalArea <- .action_log_value(
      data,
      "FunctionalArea",
      "Central Monitoring"
    )
    data$RecommendedAction <- .action_log_value(
      data,
      "RecommendedAction",
      "Review the synthetic risk signal."
    )
    default_action_taken <- ifelse(
      data$State == "Closed Action",
      "Synthetic action completed.",
      ifelse(
        data$State == "No Action",
        "Synthetic triage found no action required.",
        NA_character_
      )
    )
    data$ActionTaken <- .action_log_value(data, "ActionTaken", default_action_taken)
    data$CTMSID <- .action_log_value(data, "CTMSID", NA_integer_)
    data$Analytics <- .action_log_value(data, "Analytics", "")

    if (duplicate_probability > 0) {
      duplicate_rows <- data[
        stats::runif(nrow(data)) < duplicate_probability,
        ,
        drop = FALSE
      ]
      data <- rbind(data, duplicate_rows)
      data <- data[order(
        data$SnapshotDate,
        data$GroupLevel,
        data$GroupID,
        data$MetricID
      ), , drop = FALSE]
    }

    work_item_ids <- seq.int(
      from = as.integer(work_item_id_start),
      length.out = nrow(data)
    )
    Map(
      function(index, work_item_id) {
        .make_risk_signal_work_item(
          data[index, , drop = FALSE],
          work_item_id,
          organization,
          extraction_date
        )
      },
      seq_len(nrow(data)),
      work_item_ids
    )
  })
}

#' Project synthetic ADO work items into source-neutral Action Log domains
#'
#' Uses the projection contract introduced by `grail.ado` PR #121. The same
#' non-QTL Azure DevOps Risk Signal work items are projected independently into
#' the inbound `AllRiskSignals` and `Actions` domains consumed by `{grail}`.
#' Final Action Log report construction remains owned by `{grail}`.
#'
#' @param work_items A list of ADO-compatible Risk Signal work items, typically
#'   returned by [simulate_risk_signal_work_items()].
#' @param organization Azure DevOps organization used to construct fallback
#'   work-item URLs.
#'
#' @return A named list containing `all_risk_signals` and `actions` data frames.
#' @export
project_action_log_domains <- function(
    work_items,
    organization = "Gilead-RND-CDS-RBQM") {
  if (!is.list(work_items)) {
    stop("work_items must be a list.", call. = FALSE)
  }
  if (!is.character(organization) || length(organization) != 1L ||
      is.na(organization)) {
    stop("organization must be a single character value.", call. = FALSE)
  }
  required_exports <- c("TabulateRiskSignals", "TabulateActions")
  if (!requireNamespace("grail.ado", quietly = TRUE) ||
      !all(required_exports %in% getNamespaceExports("grail.ado"))) {
    stop(
      "grail.ado with the source-neutral Action Log projection APIs is required.",
      call. = FALSE
    )
  }

  list(
    all_risk_signals = grail.ado::TabulateRiskSignals(
      work_items,
      strOrganization = organization
    ),
    actions = grail.ado::TabulateActions(
      work_items,
      strOrganization = organization
    )
  )
}

#' Simulate source-neutral Action Log input domains
#'
#' Generates deterministic ADO-compatible Risk Signal work items and projects
#' them into the source-neutral `AllRiskSignals` and `Actions` domains defined
#' by the current `{grail}` Action Log contract.
#'
#' @inheritParams simulate_risk_signal_work_items
#'
#' @return A named list containing the raw `work_items`, projected
#'   `all_risk_signals`, and projected `actions`.
#' @export
simulate_action_log_domains <- function(
    df_results,
    state_probabilities = c(
      "Awaiting Triage" = 0.2,
      "No Action" = 0.4,
      "Open Action" = 0.2,
      "Closed Action" = 0.2
    ),
    transition_matrix = NULL,
    missing_probability = 0,
    duplicate_probability = 0,
    seed = NULL,
    extraction_date = NULL,
    work_item_id_start = 900000L,
    organization = "Gilead-RND-CDS-RBQM") {
  work_items <- simulate_risk_signal_work_items(
    df_results = df_results,
    state_probabilities = state_probabilities,
    transition_matrix = transition_matrix,
    missing_probability = missing_probability,
    duplicate_probability = duplicate_probability,
    seed = seed,
    extraction_date = extraction_date,
    work_item_id_start = work_item_id_start,
    organization = organization
  )
  domains <- project_action_log_domains(
    work_items,
    organization = organization
  )

  c(list(work_items = work_items), domains)
}

#' Simulate an ActionLog history
#'
#' Creates raw ADO-compatible work items, projects them to source-neutral
#' domains with `grail.ado`, and builds the final report with
#' `grail::BuildActionLog()`. This convenience function can return the final
#' ActionLog alone or all intermediate schema layers.
#'
#' @inheritParams simulate_risk_signal_work_items
#' @param include_intermediates If `TRUE`, return raw work items,
#'   `all_risk_signals`, `actions`, and the final `action_log` in a named list.
#'
#' @return An ActionLog data frame, or a named list of all three schema layers.
#' @export
simulate_action_log <- function(
    df_results,
    state_probabilities = c(
      "Awaiting Triage" = 0.2,
      "No Action" = 0.4,
      "Open Action" = 0.2,
      "Closed Action" = 0.2
    ),
    transition_matrix = NULL,
    missing_probability = 0,
    duplicate_probability = 0,
    seed = NULL,
    extraction_date = NULL,
    work_item_id_start = 900000L,
    organization = "Gilead-RND-CDS-RBQM",
    include_intermediates = FALSE) {
  if (is.null(extraction_date)) {
    snapshot_dates <- as.Date(df_results$SnapshotDate)
    if (length(snapshot_dates) == 0L || all(is.na(snapshot_dates))) {
      stop("df_results must contain at least one valid SnapshotDate.", call. = FALSE)
    }
    extraction_date <- max(snapshot_dates, na.rm = TRUE) + 7
  }
  extraction_date <- as.Date(extraction_date)

  work_items <- simulate_risk_signal_work_items(
    df_results = df_results,
    state_probabilities = state_probabilities,
    transition_matrix = transition_matrix,
    missing_probability = missing_probability,
    duplicate_probability = duplicate_probability,
    seed = seed,
    extraction_date = extraction_date,
    work_item_id_start = work_item_id_start,
    organization = organization
  )
  domains <- project_action_log_domains(
    work_items,
    organization = organization
  )
  if (!requireNamespace("grail", quietly = TRUE) ||
      !"BuildActionLog" %in% getNamespaceExports("grail")) {
    stop(
      "grail with the source-neutral BuildActionLog API is required.",
      call. = FALSE
    )
  }
  action_log <- grail::BuildActionLog(
    dfRiskSignals = domains$all_risk_signals,
    dfActions = domains$actions,
    dtExtractionDate = extraction_date
  )

  if (isTRUE(include_intermediates)) {
    return(c(
      list(work_items = work_items),
      domains,
      list(action_log = action_log)
    ))
  }
  action_log
}

#' Simulate deterministic Action Log lookback scenarios
#'
#' Builds three-snapshot histories that exercise action-window boundaries for
#' downstream scoring tests. The scenarios cover an older open action, a recent
#' closed action, a current open action, no action, and awaiting triage.
#'
#' @param study_id Study identifier for the synthetic histories.
#' @param snapshot_dates Exactly three ordered snapshot dates.
#' @param work_item_id_start First synthetic ADO work item ID.
#' @param organization Azure DevOps organization used only to construct URLs.
#'
#' @return A named list containing `reporting_results`, scenario `expectations`,
#'   raw `work_items`, source-neutral `all_risk_signals` and `actions`, and the
#'   final `action_log`.
#' @export
simulate_action_log_lookback_scenarios <- function(
    study_id = "SYNTHETIC-STUDY",
    snapshot_dates = as.Date(c("2026-01-31", "2026-02-28", "2026-03-31")),
    work_item_id_start = 910000L,
    organization = "Gilead-RND-CDS-RBQM") {
  snapshot_dates <- as.Date(snapshot_dates)
  if (length(snapshot_dates) != 3L || anyNA(snapshot_dates) ||
      is.unsorted(snapshot_dates, strictly = TRUE)) {
    stop("snapshot_dates must contain exactly three increasing dates.", call. = FALSE)
  }
  if (!is.character(study_id) || length(study_id) != 1L ||
      is.na(study_id) || study_id == "") {
    stop("study_id must be a single non-empty character value.", call. = FALSE)
  }

  scenarios <- data.frame(
    Scenario = c(
      "older-open", "recent-closed", "current-open", "no-action",
      "awaiting-triage"
    ),
    GroupID = as.character(1001:1005),
    stringsAsFactors = FALSE
  )
  states <- list(
    c("Open Action", "No Action", "No Action"),
    c("No Action", "Closed Action", "No Action"),
    c("No Action", "No Action", "Open Action"),
    rep("No Action", 3),
    c("No Action", "No Action", "Awaiting Triage")
  )
  reporting_results <- do.call(rbind, lapply(seq_len(nrow(scenarios)), function(index) {
    data.frame(
      StudyID = study_id,
      SnapshotDate = snapshot_dates,
      GroupLevel = "Site",
      GroupID = scenarios$GroupID[index],
      MetricID = paste0("Analysis_kri", sprintf("%04d", index)),
      Flag = 1L,
      State = states[[index]],
      Country = "US",
      Scenario = scenarios$Scenario[index],
      stringsAsFactors = FALSE
    )
  }))
  expectations <- data.frame(
    scenarios,
    Lookback1 = c(FALSE, FALSE, TRUE, FALSE, FALSE),
    Lookback2 = c(FALSE, TRUE, TRUE, FALSE, FALSE),
    Lookback3 = c(TRUE, TRUE, TRUE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  simulated <- simulate_action_log(
    reporting_results,
    extraction_date = max(snapshot_dates) + 7,
    work_item_id_start = work_item_id_start,
    organization = organization,
    include_intermediates = TRUE
  )

  c(
    list(reporting_results = reporting_results, expectations = expectations),
    simulated
  )
}