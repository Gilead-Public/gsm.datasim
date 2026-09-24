# Join visit-schedule dates onto a subject-visit frame and sort

Brings the authoritative visit schedule (`Raw_VISIT$visit_dt`) onto a
subject-visit frame and returns it sorted by group then date, so that
"adjacent rows" and "adjacent in time" mean the same thing. Without
this, run adjacency is undefined.

## Usage

``` r
assign_schedule_dates(
  df,
  visits,
  strDateCol,
  strGroupCol = "subjid",
  strVisitCol = "instancename"
)
```

## Arguments

- df:

  Data frame of subject-visit records to date.

- visits:

  Data frame carrying the visit schedule, with the group and visit
  columns plus `visit_dt`.

- strDateCol:

  Name for the date column in the returned frame – e.g. `"vs_dt"` for
  `Raw_VS`, `"lb_dt"` for a future `Raw_LB` adopter.

- strGroupCol, strVisitCol:

  Column names identifying the subject and the visit.

## Value

`df` with the date column added, sorted by group then date. The sort is
stable, so repeated records within a visit keep a deterministic order.

## Details

Call this **before**
[`inject_targeted_runs()`](https://gilead-public.github.io/gsm.datasim/dev/reference/inject_targeted_runs.md);
see the ordering contract documented there.
