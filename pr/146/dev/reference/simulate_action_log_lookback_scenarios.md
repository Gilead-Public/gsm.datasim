# Simulate deterministic Action Log lookback scenarios

Builds three-snapshot histories that exercise action-window boundaries
for downstream scoring tests. The scenarios cover an older open action,
a recent closed action, a current open action, no action, and awaiting
triage.

## Usage

``` r
simulate_action_log_lookback_scenarios(
  study_id = "SYNTHETIC-STUDY",
  snapshot_dates = as.Date(c("2026-01-31", "2026-02-28", "2026-03-31")),
  work_item_id_start = 910000L,
  organization = "Gilead-RND-CDS-RBQM"
)
```

## Arguments

- study_id:

  Study identifier for the synthetic histories.

- snapshot_dates:

  Exactly three ordered snapshot dates.

- work_item_id_start:

  First synthetic ADO work item ID.

- organization:

  Azure DevOps organization used only to construct URLs.

## Value

A named list containing `reporting_results`, scenario `expectations`,
raw `work_items`, source-neutral `all_risk_signals` and `actions`, and
the final `action_log`.
