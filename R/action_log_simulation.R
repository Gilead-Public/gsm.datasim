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
    "Custom.CTMSID" = as.integer(row$CTMSID),
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

.require_action_log_package <- function(package, caller) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(
      package,
      " must be installed to use ",
      caller,
      "().",
      call. = FALSE
    )
  }
}

.missing_schema_column <- function(type, size) {
  switch(
    tolower(type),
    string = rep(NA_character_, size),
    integer = rep(NA_integer_, size),
    boolean = rep(NA, size),
    date = rep(as.Date(NA), size),
    number = rep(NA_real_, size),
    datetime = rep(as.POSIXct(NA), size),
    rep(NA_character_, size)
  )
}

.coerce_schema_column <- function(value, type) {
  switch(
    tolower(type),
    string = as.character(value),
    integer = as.integer(value),
    boolean = as.logical(value),
    date = as.Date(value),
    number = as.numeric(value),
    datetime = as.POSIXct(value),
    value
  )
}

.apply_action_log_schema <- function(data, schema) {
  field_names <- vapply(schema$fields, function(field) field$name, character(1))

  for (field in schema$fields) {
    if (!(field$name %in% names(data))) {
      data[[field$name]] <- .missing_schema_column(field$type, nrow(data))
    } else {
      data[[field$name]] <- .coerce_schema_column(data[[field$name]], field$type)
    }
  }

  data[field_names]
}

#' Tabulate synthetic risk signal work items
#'
#' Passes synthetic work items through `grail.ado::TabulateWorkItems()` and
#' normalizes the result to `grail.ado::RiskSignalWorkItemsTableSchema`.
#'
#' @param work_items List returned by [simulate_risk_signal_work_items()].
#' @param organization Azure DevOps organization used to construct work item
#'   URLs.
#'
#' @return A data frame conforming to the grail.ado risk signal work item table
#'   schema.
#' @export
tabulate_risk_signal_work_items <- function(
    work_items,
    organization = "Gilead-RND-CDS-RBQM") {
  if (!is.list(work_items)) {
    stop("work_items must be a list.", call. = FALSE)
  }
  if (!is.character(organization) || length(organization) != 1L || is.na(organization)) {
    stop("organization must be a single character value.", call. = FALSE)
  }
  .require_action_log_package("grail.ado", "tabulate_risk_signal_work_items")

  work_item_table <- grail.ado::TabulateWorkItems(
    work_items,
    strOrganization = organization
  )
  .apply_action_log_schema(
    work_item_table,
    grail.ado::RiskSignalWorkItemsTableSchema
  )
}

#' Augment synthetic risk signal work items
#'
#' Passes a simulated risk signal work item table through
#' `grail::AugmentRiskSignalWorkItemsTable()` and normalizes the result to
#' `grail::ActionLogSchema`.
#'
#' @param work_item_table Data frame returned by
#'   [tabulate_risk_signal_work_items()].
#' @param extraction_date Synthetic ActionLog extraction date.
#' @param state_order Order used when sorting work items within a signal key.
#'
#' @return A data frame conforming to `grail::ActionLogSchema`.
#' @export
augment_risk_signal_work_items <- function(
    work_item_table,
    extraction_date = Sys.Date(),
    state_order = .action_log_states) {
  if (!is.data.frame(work_item_table)) {
    stop("work_item_table must be a data frame.", call. = FALSE)
  }
  extraction_date <- as.Date(extraction_date)
  if (length(extraction_date) != 1L || is.na(extraction_date)) {
    stop("extraction_date must be a single date.", call. = FALSE)
  }
  if (!is.character(state_order) || !setequal(state_order, .action_log_states)) {
    stop("state_order must contain all supported ActionLog states.", call. = FALSE)
  }
  .require_action_log_package("grail", "augment_risk_signal_work_items")

  action_log <- grail::AugmentRiskSignalWorkItemsTable(
    work_item_table,
    strState = state_order,
    dtExtractionDate = extraction_date
  )
  .apply_action_log_schema(action_log, grail::ActionLogSchema)
}

#' Simulate an ActionLog history
#'
#' Creates raw ADO-compatible work items, tabulates them with `grail.ado`, and
#' augments them with `grail`. This convenience function can return the final
#' ActionLog alone or all three intermediate schema layers.
#'
#' @inheritParams simulate_risk_signal_work_items
#' @param include_intermediates If `TRUE`, return raw work items, the tabulated
#'   work item data, and the augmented ActionLog in a named list.
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
  work_item_table <- tabulate_risk_signal_work_items(
    work_items,
    organization = organization
  )
  action_log <- augment_risk_signal_work_items(
    work_item_table,
    extraction_date = extraction_date
  )

  if (isTRUE(include_intermediates)) {
    return(list(
      work_items = work_items,
      work_item_table = work_item_table,
      action_log = action_log
    ))
  }
  action_log
}