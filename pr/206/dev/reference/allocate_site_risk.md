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
  nAmber = NULL,
  nTotalSites = NULL,
  strSeedKey = NULL
)
```

## Arguments

- vSites:

  Character vector of site identifiers. Duplicates are ignored; each
  distinct site receives one band. Order is significant when
  `nTotalSites` is supplied – it is the rank order.

- dPctRed, dPctAmber:

  Proportions in `[0, 1]` of sites to place in the red and amber bands.

- nRed, nAmber:

  Optional explicit site counts, overriding the corresponding
  percentage.

- nTotalSites:

  Optional total number of sites the study will end up with. Percentages
  are taken over this rather than over `vSites`, so early snapshots
  allocate against the final roster size.

- strSeedKey:

  Optional string keying the deterministic slot permutation. Required
  for allocation to be reproducible across calls; include the vital so
  bands stay independent per vital.

## Value

Named character vector, one element per distinct site, with values
`"red"`, `"amber"`, or `"normal"`.

## Persisting bands across snapshots

Supplying `nTotalSites` and `strSeedKey` switches allocation from
"sample the sites I can see" to "assign by rank over the final roster".
Sites are ranked by first appearance, bands are dealt across
`nTotalSites` slots, and the slot permutation is derived from
`strSeedKey` rather than the ambient RNG. A site therefore keeps its
band no matter which snapshot is being generated, and sites that enroll
later claim unused slots without disturbing bands already handed out
(#143).

This matters because snapshots are deltas: rows written in an early
snapshot are frozen, so a site's band has to be right the first time its
rows are generated. It relies on the site roster being append-only,
which is what makes rank a stable key.
