# Export multiple longitudinal studies to disk

Convenience function to export all studies in a
multiple_longitudinal_studies object

## Usage

``` r
export_multiple_studies(
  studies,
  output_dir = ".",
  overwrite = FALSE,
  save_rds = FALSE,
  verbose = FALSE
)
```

## Arguments

- studies:

  A multiple_longitudinal_studies object

- output_dir:

  Root directory for export (default ".")

- overwrite:

  Whether to overwrite existing files (default FALSE)

- save_rds:

  Whether to save RDS files alongside CSVs (default FALSE)

- verbose:

  Whether to print progress messages (default FALSE)

## Value

Invisible list of study export paths
