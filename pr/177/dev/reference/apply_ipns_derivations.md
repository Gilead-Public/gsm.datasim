# Derive the upstream IP non-starter contract fields

Impersonates the Stride derivation so simulated data carries the same
seven fields production data will. gsm never computes these outside the
simulator.

## Usage

``` r
apply_ipns_derivations(
  df,
  endDate,
  nWindowDays = 30,
  nConfirmedShare = 0.4,
  nKitAssignedShare = 0.5
)
```

## Arguments

- df:

  a generated `Raw_SUBJ` frame carrying `subjid`, `enrollyn`,
  `enrolldt`, `firstdosedate`.

- endDate:

  the snapshot date, acting as "today".

- nWindowDays:

  days separating the two potential statuses.

- nConfirmedShare:

  share of never-dosed subjects that are Confirmed.

- nKitAssignedShare:

  share of never-dosed subjects with a kit assigned.

## Value

`df` with the seven `drv_*` columns.

## Details

Runs over the whole frame on every snapshot, so days lapsed re-accrue
and an undosed subject advances from within- to outside-window as time
passes. Confirmed status is a deterministic function of `subjid` rather
than a draw, so it cannot flip back on a later snapshot.
