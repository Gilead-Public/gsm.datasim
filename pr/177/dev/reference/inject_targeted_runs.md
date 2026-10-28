# Inject consecutive runs to hit a target repeat rate

Overwrites contiguous blocks of `values` within each group so that the
proportion of length-`nWindowLength` rolling windows whose values are
all identical is approximately `dTargetRate`. The construction is
deterministic given the group sizes – the rate is *built*, not sampled.

## Usage

``` r
inject_targeted_runs(values, groups, dTargetRate, nWindowLength = 3)
```

## Arguments

- values:

  Numeric vector of measurements, ordered within group.

- groups:

  Vector the same length as `values` identifying each value's group
  (typically subject).

- dTargetRate:

  Target repeat rate in `[0, 1]`.

- nWindowLength:

  Whole number `>= 2`. Rolling window length.

## Value

`values` with runs injected, carrying a `"realized"` attribute: a list
with the achieved `numerator` and `denominator`. The numerator is
**recounted from the returned values**, not assumed from the
construction, so it always matches what a window-counting metric will
compute.

## Details

Rows must be **already ordered within group** by the key the downstream
metric will order on; this function injects over contiguous positions
and does not sort. `NA` values are never overwritten and are excluded
from the window counts. Small groups quantize, since the target
numerator is `round(dTargetRate * D)`.
