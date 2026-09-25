# Assign each site a risk band

Partitions sites into `"red"`, `"amber"`, and `"normal"` bands by
sampling without replacement. Used to decide which sites should be given
an elevated consecutive-repeat rate before
[`inject_targeted_runs()`](https://gilead-public.github.io/gsm.datasim/dev/reference/inject_targeted_runs.md)
constructs it. Supply either percentages (`dPctRed` / `dPctAmber`) or
explicit counts (`nRed` / `nAmber`); counts take precedence when both
are given. Percentages are converted with
[`round()`](https://rdrr.io/r/base/Round.html), so small site counts
degrade gracefully rather than erroring – 10% of 3 sites is 0 red sites,
not a fractional one.

## Usage

``` r
allocate_site_risk(
  vSites,
  dPctRed = 0.1,
  dPctAmber = 0.2,
  nRed = NULL,
  nAmber = NULL
)
```

## Arguments

- vSites:

  Character vector of site identifiers. Duplicates are ignored; each
  distinct site receives one band.

- dPctRed, dPctAmber:

  Proportions in `[0, 1]` of sites to place in the red and amber bands.

- nRed, nAmber:

  Optional explicit site counts, overriding the corresponding
  percentage.

## Value

Named character vector, one element per distinct site, with values
`"red"`, `"amber"`, or `"normal"`.
