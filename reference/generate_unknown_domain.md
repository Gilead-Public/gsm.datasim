# Generate Data for an Unknown Domain

Creates a complete `data.frame` for a domain that has no dedicated
generator function in the domain registry or as a legacy `Raw_*()`
function. Each column in the spec is generated using
[`generate_column_by_type()`](https://gilead-biostats.github.io/gsm.datasim/reference/generate_column_by_type.md),
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
  [`generate_column_by_type()`](https://gilead-biostats.github.io/gsm.datasim/reference/generate_column_by_type.md)).

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

## Lifecycle

**\[experimental\]**

## Examples

``` r
spec <- list(
  subjid   = list(type = "character"),
  measure_dt = list(type = "date"),
  value    = list(type = "numeric")
)
domain_data <- generate_unknown_domain("Raw_CUSTOM", spec, n = 10)
head(domain_data)
#>      subjid measure_dt value
#> 1 SUBJ-0001 2012-02-12 74.47
#> 2 SUBJ-0002 2012-09-17 38.26
#> 3 SUBJ-0003 2012-10-26 49.96
#> 4 SUBJ-0004 2012-02-03 56.20
#> 5 SUBJ-0005 2012-03-10 60.87
#> 6 SUBJ-0006 2012-09-16 85.31
```
