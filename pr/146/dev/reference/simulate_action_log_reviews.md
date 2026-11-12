# Simulate source-aligned review scenarios

Assigns reproducible Open, Closed, No Action and Awaiting Triage states
to actual flagged site/KRI snapshots. Previously flagged, currently
green findings receive Closed states. Snapshot dates remain proxies: no
verified decision dates or coverage are manufactured. Row order does not
affect output.

## Usage

``` r
simulate_action_log_reviews(
  df_results,
  extraction_date = NULL,
  work_item_id_start = 900001L
)
```

## Arguments

- df_results:

  Reporting results with StudyID, Date SnapshotDate, GroupLevel,
  GroupID, MetricID and numeric Flag columns.

- extraction_date:

  Frozen extraction date; defaults to the latest result.

- work_item_id_start:

  First synthetic work item identifier.

## Value

A canonical ActionLog data frame.
