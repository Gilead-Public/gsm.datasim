# Simulate a frozen ActionLog extraction

Creates raw ADO-compatible work items, projects them to source-neutral
domains, and builds a deterministic final ActionLog without network
access. Multiple signal snapshots describe the current extracted states
of those signals, not a state-change history. Generate and retain
separate extractions to represent what was known at different scoring
runs.

## Usage

``` r
simulate_action_log(
  df_results,
  state_probabilities = c(`Awaiting Triage` = 0.2, `No Action` = 0.4, `Open Action` =
    0.2, `Closed Action` = 0.2),
  transition_matrix = NULL,
  missing_probability = 0,
  duplicate_probability = 0,
  seed = NULL,
  extraction_date = NULL,
  work_item_id_start = 900000L,
  organization = "Gilead-RND-CDS-RBQM",
  include_intermediates = FALSE,
  actions_per_signal = 1L,
  action_states = NULL
)
```

## Arguments

- df_results:

  Data frame containing `StudyID`, `SnapshotDate`, `GroupLevel`,
  `GroupID`, `MetricID`, and `Flag`. Optional display and action columns
  override generated defaults.

- state_probabilities:

  Named probabilities for the four ActionLog states.

- transition_matrix:

  Optional state-by-state transition probability matrix used for
  repeated group/metric findings across snapshots.

- missing_probability:

  Probability that an eligible finding has no ADO work item.

- duplicate_probability:

  Probability that an eligible work item is duplicated within its
  snapshot.

- seed:

  Optional random seed. The caller's random-number state is restored
  after generation.

- extraction_date:

  Date represented by the synthetic extraction. Defaults to seven days
  after the latest snapshot.

- work_item_id_start:

  First synthetic ADO work item ID.

- organization:

  Azure DevOps organization used only to construct URLs.

- include_intermediates:

  If `TRUE`, return raw work items, `all_risk_signals`, `actions`, and
  the final `action_log` in a named list.

- actions_per_signal:

  Non-negative action count, or one count per generated signal. Zero
  retains an unactioned signal; counts above one generate distinct
  character action identifiers.

- action_states:

  Optional supported action state, or one state per generated action,
  independent of the risk-signal state.

## Value

An ActionLog data frame, or a named list of all three schema layers.
