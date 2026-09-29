# Parse interval string to snapshot width

Parse interval string to snapshot width

## Usage

``` r
parse_interval_to_snapshot_width(interval)
```

## Arguments

- interval:

  Interval string (e.g., "1 month", "2 weeks")

## Value

Snapshot width for temporal configuration

## Examples

``` r
parse_interval_to_snapshot_width("1 month")
#> [1] "months"
parse_interval_to_snapshot_width("2 weeks")
#> [1] "weeks"
parse_interval_to_snapshot_width("30 days")
#> [1] "days"
```
