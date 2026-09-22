# Project synthetic ADO work items into source-neutral Action Log domains

Uses the projection contract introduced by `grail.ado` PR \#121. The
same non-QTL Azure DevOps Risk Signal work items are projected
independently into the inbound `AllRiskSignals` and `Actions` domains
consumed by `{grail}`. Final Action Log report construction remains
owned by `{grail}`.

## Usage

``` r
project_action_log_domains(work_items, organization = "Gilead-RND-CDS-RBQM")
```

## Arguments

- work_items:

  A list of ADO-compatible Risk Signal work items, typically returned by
  [`simulate_risk_signal_work_items()`](https://gilead-public.github.io/gsm.datasim/dev/reference/simulate_risk_signal_work_items.md).

- organization:

  Azure DevOps organization used to construct fallback work-item URLs.

## Value

A named list containing `all_risk_signals` and `actions` data frames.
