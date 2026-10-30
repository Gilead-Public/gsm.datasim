# Count rolling windows available in a sequence of measurements

The denominator of the consecutive-repeat rate: a sequence of `V`
measurements admits `V - W + 1` rolling windows of length `W`, or none
when `V < W`.

## Usage

``` r
count_repeat_windows(nMeasurements, nWindowLength = 3)
```

## Arguments

- nMeasurements:

  Numeric vector of measurement counts.

- nWindowLength:

  Whole number `>= 2`. Rolling window length; the repeat-measure KRIs
  default to `3`.

## Value

Numeric vector the same length as `nMeasurements`, giving the number of
windows available for each count.

## Details

`nMeasurements` should count **non-missing** measurements only. Missing
records are dropped before windows are formed (see
[`inject_targeted_runs()`](https://gilead-public.github.io/gsm.datasim/dev/reference/inject_targeted_runs.md)),
so they neither contribute to the denominator nor interrupt a run.
