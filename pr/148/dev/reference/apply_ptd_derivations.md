# Derive premature treatment discontinuation fields

Uses subject IDs and first-dose dates without RNG draws. With unchanged
inputs and settings, dates and reasons remain stable across snapshots.

## Usage

``` r
apply_ptd_derivations(df, endDate, nDiscontinuedShare = 0.3)
```

## Arguments

- df:

  A `Raw_SUBJ` frame after
  [`apply_ipns_derivations()`](https://gilead-public.github.io/gsm.datasim/dev/reference/apply_ipns_derivations.md).

- endDate:

  Snapshot date; future discontinuation dates remain `NA`.

- nDiscontinuedShare:

  Target share of dosed subjects selected to discontinue.

## Value

`df` with discontinuation date, reason and inclusive days from
enrollment to discontinuation.
