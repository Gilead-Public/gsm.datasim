# Normalize bundle config types

Coerces `snapshot_dates` to `Date` and `domains`/`extra_mappings` to
character (`extra_mappings` defaults to empty).

## Usage

``` r
.normalize_config(config)
```

## Arguments

- config:

  A config list.

## Value

The normalized config.
