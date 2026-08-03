# Generate raw data for a single study (Deprecated)

**\[deprecated\]**

This function is deprecated. Please use the study configuration approach
instead:
[`create_study_config()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/create_study_config.md),
[`add_dataset_config()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/add_dataset_config.md),
and
[`generate_study_data()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/generate_study_data.md).
See
[`vignette("study-setup", package = "gsm.datasim")`](https://gilead-biostats.github.io/gsm.datasim/dev/articles/study-setup.md)
for a full walkthrough.

## Usage

``` r
generate_rawdata_for_single_study(
  SnapshotCount,
  SnapshotWidth,
  ParticipantCount,
  SiteCount,
  StudyID,
  workflow_path,
  mappings,
  package,
  strStartDate = "2012-01-01",
  desired_specs = NULL
)
```

## Arguments

- SnapshotCount:

  Number of snapshots to generate.

- SnapshotWidth:

  Width of each snapshot interval (in days).

- ParticipantCount:

  Number of participants.

- SiteCount:

  Number of sites.

- StudyID:

  Study identifier string.

- workflow_path:

  Path to the workflow YAML files.

- mappings:

  Named list of column mappings.

- package:

  Package name used to locate specs.

- strStartDate:

  Study start date as a string (default `"2012-01-01"`).

- desired_specs:

  Optional character vector of dataset names to keep.

## Value

A named list of snapshot data frames, named by snapshot end date.

## See also

[`create_study_config()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/create_study_config.md),
[`add_dataset_config()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/add_dataset_config.md),
[`generate_study_data()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/generate_study_data.md)
