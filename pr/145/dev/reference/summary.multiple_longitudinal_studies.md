# Summary method for multiple longitudinal studies

Summary method for multiple longitudinal studies

## Usage

``` r
# S3 method for class 'multiple_longitudinal_studies'
summary(object, ...)
```

## Arguments

- object:

  A multiple_longitudinal_studies object

- ...:

  Additional arguments (unused)

## Examples

``` r
if (FALSE) { # \dontrun{
studies <- create_multiple_longitudinal_studies(
  study_names = c("TRIAL-001", "TRIAL-002"),
  participants = 50, sites = 5, snapshots = 2
)
summary(studies)
} # }
```
