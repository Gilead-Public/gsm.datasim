# Create Study Configuration for Standard Datasets

Convenience function to create a study configuration with standard
clinical datasets.

## Usage

``` r
create_standard_study_config(
  study_id = "STUDY001",
  participant_count = 100,
  site_count = 10,
  analytics_package = NULL,
  analytics_workflows = NULL,
  study = TRUE,
  subjects = TRUE,
  sites_data = TRUE,
  adverse_events = TRUE,
  protocol_deviations = TRUE,
  lab_data = TRUE,
  subject_visits = TRUE,
  visit_schedule = TRUE,
  enrollment = TRUE,
  data_changes = TRUE,
  data_entry = TRUE,
  queries = TRUE,
  pharmacokinetics = TRUE,
  study_drug_completion = TRUE,
  study_completion = TRUE,
  inclusion_exclusion = TRUE,
  exclusions = TRUE,
  country = TRUE
)
```

## Arguments

- study_id:

  Study identifier

- participant_count:

  Number of participants

- site_count:

  Number of sites

- analytics_package:

  Analytics package to use

- analytics_workflows:

  Specific workflows to run

- study:

  Include study metadata (Raw_STUDY)

- subjects:

  Include subject demographics (Raw_SUBJ)

- sites_data:

  Include site information (Raw_SITE)

- adverse_events:

  Include adverse event data

- protocol_deviations:

  Include protocol deviation data

- lab_data:

  Include laboratory data

- subject_visits:

  Include subject visit data (Raw_SV)

- visit_schedule:

  Include visit schedule data (Raw_VISIT)

- enrollment:

  Include enrollment data

- data_changes:

  Include data change tracking (Raw_DATACHG)

- data_entry:

  Include data entry tracking (Raw_DATAENT)

- queries:

  Include query data (Raw_QUERY)

- pharmacokinetics:

  Include pharmacokinetics data

- study_drug_completion:

  Include study drug completion (Raw_SDRGCOMP)

- study_completion:

  Include overall study completion (Raw_STUDCOMP)

- inclusion_exclusion:

  Include inclusion/exclusion criteria (Raw_IE)

- exclusions:

  Include exclusion tracking (Raw_EXCLUSION)

- country:

  Include country mapping

## Value

Study configuration with standard datasets
