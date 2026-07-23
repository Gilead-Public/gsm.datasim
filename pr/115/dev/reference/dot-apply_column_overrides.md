# Apply Column Overrides to a Generated Domain Data Frame

Post-processes a domain `data.frame` by applying user-supplied column
specifications from `column_overrides`. Columns may be added (if new) or
replaced (if already present).

## Usage

``` r
.apply_column_overrides(df, domain, column_overrides)
```

## Details

Each column value in the override list can be:

- A **function** with signature `function(n, df)` — receives row count
  and the full domain `data.frame`; useful for deriving values from
  other columns.

- A **function** with signature `function(n)` — receives only the row
  count.

- A **vector** — sampled with replacement to `n` rows.

- A **scalar** — repeated to fill all `n` rows.
