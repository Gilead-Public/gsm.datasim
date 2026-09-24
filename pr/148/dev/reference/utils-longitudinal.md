# Longitudinal data helpers

Domain-neutral helpers for shaping longitudinal (subject x visit) data
so it can exercise sequence-based metrics such as the repeat-measure
KRIs (`kri0020a-f`). They operate on plain vectors and data frames, and
know nothing about vitals, sites, or any particular `Raw_*` domain.

## Details

The intended composition is **sort first, then inject**:
[`assign_schedule_dates()`](https://gilead-public.github.io/gsm.datasim/dev/reference/assign_schedule_dates.md)
puts records in the order the downstream metric will use, and
[`inject_targeted_runs()`](https://gilead-public.github.io/gsm.datasim/dev/reference/inject_targeted_runs.md)
then writes runs over contiguous positions. Reversing that order
silently produces runs that are adjacent in row order but not in time,
which the metric will not count.
