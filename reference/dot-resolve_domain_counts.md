# Resolve Row Counts for Each Domain

Applies heuristic multipliers for known domain patterns, user overrides,
and falls back to `n_participants` for unknown domains.

## Usage

``` r
.resolve_domain_counts(
  domain_names,
  n_participants,
  n_sites,
  user_counts = NULL
)
```
