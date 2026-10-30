# Split combined reporting tables by group level

Results and Metrics split by `GroupLevel`; Groups keep Study rows plus
the rows for each level; Bounds split by `MetricID` prefix
(`Analysis_kri`, `Analysis_cou`, `Analysis_qtl`). Type rules are applied
(`GroupID` character, `SnapshotDate` Date) and each table is sorted with
[`.sort_reporting()`](https://gilead-public.github.io/gsm.datasim/dev/reference/dot-sort_reporting.md).

## Usage

``` r
.split_reporting(combined)
```

## Arguments

- combined:

  List with `Reporting_Results`, `Reporting_Groups`, `Reporting_Bounds`,
  and `Reporting_Metrics`.

## Value

Named list of the 12 `reporting*` objects.
