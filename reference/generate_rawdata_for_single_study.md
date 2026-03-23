# Generate Raw Data for a Single Study

Loads mapping specifications from the given package, prepares them for
generation, and produces snapshot data for a single study.

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

  Number of snapshots to generate

- SnapshotWidth:

  Width of each snapshot (e.g., "months")

- ParticipantCount:

  Number of participants

- SiteCount:

  Number of sites

- StudyID:

  Study identifier string

- workflow_path:

  Path to workflow specifications (e.g., "workflow/1_mappings")

- mappings:

  Character vector of mapping names to use

- package:

  Package containing the workflow specifications

- strStartDate:

  Start date for snapshot generation (default "2012-01-01")

- desired_specs:

  Optional character vector of dataset names to keep

## Value

A named list of snapshots, each containing generated raw data frames
