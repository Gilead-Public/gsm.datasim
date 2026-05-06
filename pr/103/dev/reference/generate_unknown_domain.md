# Generate Data for an Unknown Domain

Creates a complete `data.frame` for a domain that has no dedicated
generator function in the domain registry or as a legacy `Raw_*()`
function. Each column in the spec is generated using
[`generate_column_by_type()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/generate_column_by_type.md),
with an attempt to reuse existing named generator functions for columns
whose names match a known generator (e.g. `subjid`, `studyid`).

## Usage

``` r
generate_unknown_domain(
  domain_name,
  domain_spec,
  n,
  context = list(),
  previous_data = NULL
)
```

## Arguments

- domain_name:

  Character. The domain name (e.g. `"Raw_CUSTOM"`).

- domain_spec:

  Named list. Column specifications as returned by `CombineSpecs()` for
  this domain.

- n:

  Integer. Target total number of rows for this snapshot (cumulative).

- context:

  List with `data`, `start_date`, `end_date` (same structure as passed
  to
  [`generate_column_by_type()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/generate_column_by_type.md)).

- previous_data:

  Optional `data.frame` from the prior snapshot for this domain. When
  provided, existing rows are retained and only the delta
  (`n - nrow(previous_data)`) new rows are generated.

## Value

A `data.frame` with `n` rows and one column per spec entry (or more rows
if `previous_data` already exceeds `n`).

## Details

When `previous_data` is supplied (a `data.frame` from the prior
snapshot), the function uses a cumulative delta pattern: it keeps all
existing rows and only generates `n - nrow(previous_data)` new rows,
then binds them together. This mirrors the behavior of the core domain
registry generators for longitudinal multi-snapshot generation.
