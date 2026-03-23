# Export Complete Study Data to Disk

Writes a `longitudinal_study` object to a structured folder hierarchy.
Each snapshot date becomes a subdirectory containing up to four
subfolders:

## Usage

``` r
export_study_data(
  study,
  output_dir = ".",
  study_folder = NULL,
  format = "csv",
  overwrite = FALSE,
  save_rds = FALSE,
  verbose = FALSE
)
```

## Arguments

- study:

  A `longitudinal_study` object (output of
  [`create_longitudinal_study`](https://gilead-biostats.github.io/gsm.datasim/reference/create_longitudinal_study.md)
  or
  [`quick_longitudinal_study`](https://gilead-biostats.github.io/gsm.datasim/reference/quick_longitudinal_study.md)),
  or a `multiple_longitudinal_studies` object (output of
  [`create_multiple_longitudinal_studies`](https://gilead-biostats.github.io/gsm.datasim/reference/create_multiple_longitudinal_studies.md)).
  When a collection of studies is passed, each study is exported to its
  own subfolder under `output_dir`.

- output_dir:

  Root directory under which the study folder is created. Defaults to
  the current working directory.

- study_folder:

  Optional name for the top-level study folder. Defaults to
  `study$study_id` with characters that are invalid in folder names
  replaced by underscores.

- format:

  Export format. Options are "csv" (default), "parquet", or "both". When
  "parquet" is used, the arrow package must be installed.

- overwrite:

  If `TRUE`, existing files are silently overwritten. If `FALSE`
  (default), an error is raised when the target study folder already
  exists.

- save_rds:

  If `TRUE`, an `analytics_full.rds` file is also written per snapshot,
  preserving any non-data.frame objects (workflow lists, summaries,
  etc.) that cannot be expressed as flat files. Defaults to `FALSE`.

- verbose:

  If `TRUE`, prints progress messages. Defaults to `FALSE`.

## Value

Invisibly returns the path to the top-level study folder (for a single
study) or a named list of paths (for multiple studies).

## Details

    <output_dir>/<study_id>/
      <snapshot_date>/
        raw/          # Raw_*.csv  (from study$raw_data)
        mapped/       # Mapped_*.csv  (from study$analytics[[date]]$mapped)
        analytics/    # <metric>_<table>.csv  (from study$analytics[[date]]$results)
        reporting/    # Reporting_*.csv  (from study$reporting[[date]])

Folders are only created when the corresponding data actually exists.
Non-data.frame leaves inside `analytics` are silently skipped; they can
be preserved alongside the CSVs by setting `save_rds = TRUE`, which
writes a companion `analytics_full.rds` per snapshot.
