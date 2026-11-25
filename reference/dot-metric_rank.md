# Rank MetricIDs naturally

Orders e.g. kri0001 \< kri0007 \< kri0007-2 \< kri0020a, with srs
metrics last.

## Usage

``` r
.metric_rank(id)
```

## Arguments

- id:

  Character vector of MetricIDs.

## Value

Integer rank for each element of `id`.
