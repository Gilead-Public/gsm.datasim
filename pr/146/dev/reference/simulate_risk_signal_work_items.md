# Simulate Azure DevOps risk signal work items

Creates synthetic records with the list structure returned by
`grail.ado::GetWorkItems()`. Input rows represent candidate KRI
findings; rows with a missing or zero `Flag` are excluded.

## Usage

``` r
simulate_risk_signal_work_items(
  df_results,
  state_probabilities = c(`Awaiting Triage` = 0.2, `No Action` = 0.4, `Open Action` =
    0.2, `Closed Action` = 0.2),
  transition_matrix = NULL,
  missing_probability = 0,
  duplicate_probability = 0,
  seed = NULL,
  extraction_date = NULL,
  work_item_id_start = 900000L,
  organization = "Gilead-RND-CDS-RBQM"
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

## Value

A list of synthetic ADO work items.
