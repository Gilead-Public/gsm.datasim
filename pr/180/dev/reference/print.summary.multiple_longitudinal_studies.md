# Print method for summary of multiple longitudinal studies

Print method for summary of multiple longitudinal studies

## Usage

``` r
# S3 method for class 'summary.multiple_longitudinal_studies'
print(x, ...)
```

## Arguments

- x:

  A summary.multiple_longitudinal_studies object

- ...:

  Additional arguments (unused)

## Examples

``` r
if (FALSE) { # \dontrun{
studies <- create_multiple_longitudinal_studies(
  study_names = c("TRIAL-001", "TRIAL-002"),
  participants = 50, sites = 5, snapshots = 2
)
print(summary(studies))
} # }
```
