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
  country = TRUE,
  death = TRUE,
  randomization = TRUE,
  overall_response = TRUE,
  outlier_intensity = 1
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

  Include subject visit data (Raw_VISIT)

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

- country:

  Include country mapping

- death:

  Include death events (Raw_Death)

- randomization:

  Include randomization data (Raw_Randomization)

- overall_response:

  Include overall response data (Raw_OverallResponse)

- outlier_intensity:

  Global multiplier for outlier-like values in domain generators.

## Value

Study configuration with standard datasets

## Examples

``` r
# All default datasets
config <- create_standard_study_config("TRIAL001", participant_count = 100, site_count = 10)
names(config$dataset_configs)
#>  [1] "Raw_STUDY"           "Raw_SITE"            "Raw_SUBJ"           
#>  [4] "Raw_ENROLL"          "Raw_AE"              "Raw_PD"             
#>  [7] "Raw_LB"              "Raw_VISIT"           "Raw_DATACHG"        
#> [10] "Raw_DATAENT"         "Raw_QUERY"           "Raw_PK"             
#> [13] "Raw_SDRGCOMP"        "Raw_STUDCOMP"        "Raw_IE"             
#> [16] "Raw_COUNTRY"         "Raw_Death"           "Raw_Randomization"  
#> [19] "Raw_OverallResponse"

# Select a subset of domains
config <- create_standard_study_config(
  "TRIAL002",
  participant_count = 50,
  adverse_events = TRUE, lab_data = TRUE,
  pharmacokinetics = FALSE, overall_response = FALSE
)
```
