# Generate raw data for a single study (Deprecated)

**\[deprecated\]**

This function is deprecated. Please use the study configuration approach
instead:
[`create_study_config()`](https://gilead-public.github.io/gsm.datasim/dev/reference/create_study_config.md),
[`add_dataset_config()`](https://gilead-public.github.io/gsm.datasim/dev/reference/add_dataset_config.md),
and
[`generate_study_data()`](https://gilead-public.github.io/gsm.datasim/dev/reference/generate_study_data.md).
See
[`vignette("study-setup", package = "gsm.datasim")`](https://gilead-public.github.io/gsm.datasim/dev/articles/study-setup.md)
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
  desired_specs = NULL,
  risk_profile = NULL
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

- risk_profile:

  Optional named list controlling site-targeted consecutive-run
  injection. Only `Raw_VS` currently supports it. `NULL` uses the
  generator defaults. Recognized fields are `dPctRed` and `dPctAmber`
  (share of sites in each band), `nWindowLength` (rolling window length,
  whole number `>= 2`), `dRateNormal` / `dRateAmber` / `dRateRed`
  (target repeat rate per band), and `vVitals` (character vector of
  vitals to target, or `NULL` for all eight). Fields can be given at
  three levels, with more specific levels overriding less specific ones:

  - Top level: applies to every domain that supports it.

  - Domain level: `list(Raw_VS = list(dPctRed = 0.3))`.

  - Vital level: `list(Raw_VS = list(sysbp = list(dRateRed = 0.6)))`.
    Each vital's bands are allocated independently, so vitals can have
    different profiles. The targeted vitals are the domain's `vVitals`
    if given (named vitals must then be among them), else the vitals the
    domain names, else the top-level `vVitals`, else all eight.
    `vVitals` cannot be set inside a vital.

## Value

A named list of snapshot data frames, named by snapshot end date.

## See also

[`create_study_config()`](https://gilead-public.github.io/gsm.datasim/dev/reference/create_study_config.md),
[`add_dataset_config()`](https://gilead-public.github.io/gsm.datasim/dev/reference/add_dataset_config.md),
[`generate_study_data()`](https://gilead-public.github.io/gsm.datasim/dev/reference/generate_study_data.md)
