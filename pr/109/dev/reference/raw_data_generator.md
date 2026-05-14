# Generate Raw Data for Study Snapshots

This function generates raw data for study snapshots based on provided
participant count, site count, study ID, and snapshot count. If any of
these parameters are `NULL`, it loads a template CSV file to iterate
over multiple study configurations. The generated raw data is saved as
an RDS file.

## Usage

``` r
raw_data_generator(
  ParticipantCount = NULL,
  SiteCount = NULL,
  StudyID = NULL,
  SnapshotCount = NULL,
  SnapshotWidth = NULL,
  template_path = system.file("template.csv", package = "gsm.datasim"),
  workflow_path = "workflow/1_mappings",
  generate_reports = FALSE,
  mappings = NULL,
  package = "gsm.mapping",
  strStartDate = "2012-01-01",
  save = FALSE,
  generation_mode = c("core", "legacy")
)
```

## Arguments

- ParticipantCount:

  An integer specifying the number of participants for the study. If
  `NULL`, the function will use values from the template file.

- SiteCount:

  An integer specifying the number of sites for the study. If `NULL`,
  the function will use values from the template file.

- StudyID:

  A string specifying the study identifier. If `NULL`, the function will
  use values from the template file.

- SnapshotCount:

  An integer specifying the number of snapshots for the study. If
  `NULL`, the function will use values from the template file.

- SnapshotWidth:

  A character specifying the frequency of snapshots, defaults to
  "months". Accepts "days", "weeks", "months" and "years". User can also
  place a number and unit such as "3 months".

- template_path:

  A string specifying the path to the template CSV file. Default is
  `"~/gsm.datasim/inst/template.csv"`.

- workflow_path:

  A string specifying the path to the workflow mappings. Default is
  `"workflow/1_mappings"`.

- generate_reports:

  A boolean, specifying whether or not to produce reports upon
  execution. Default is FALSE.

- mappings:

  A string specifying the names of the workflows to run.

- package:

  A string specifying the package in which the workflows used in
  `MakeWorkflowList()` are located. Default is "gsm".

- strStartDate:

  A string to denote when the first snapshot of simulated data occurs

- save:

  A boolean, specifying whether or not this should be saved out as an
  RDS

- generation_mode:

  Generation backend to use: "core" (default) or "legacy".

## Value

A list of raw data generated for each study snapshot, saved as an RDS
file in `"data-raw/raw_data.RDS"`.

## Details

The function performs the following steps:

1.  If `ParticipantCount`, `SiteCount`, `StudyID`, or `SnapshotCount` is
    `NULL`, the function reads the `template.csv` file to get the
    necessary parameters for multiple studies.

2.  It generates raw data for study snapshots based on either provided
    parameters or the template file.

3.  The generated data is saved as an RDS file and returned as a list.

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate raw data using specified parameters
data <- raw_data_generator(
  ParticipantCount = 100,
  SiteCount = 10,
  StudyID = "Study01",
  SnapshotCount = 5,
  SnapshotWidth = "months"
)

# Generate raw data using a template file
data <- raw_data_generator()
} # }
```
