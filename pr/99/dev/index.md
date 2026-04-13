# Test Data Simulator

## Overview

The [gsm.datasim](https://github.com/Gilead-BioStats/gsm.datasim)
package provides functionality to produce sample data for use in the
testing and development of applications and packages related to
Risk-Based Quality Monitoring (RBQM) of Clinical Trials. Given inputs
regarding the synthetic Study’s desired ID, Participant Count, Site
Count, Snapshot Count, and the Key Risk Indicators to be constructed,
the package generates data pertaining to:

- Participant demographics
- Adverse effects
- Trial discontinuation
- Lab results
- Visit information
- Study and site location
- I/E criteria
- Study metadata

By Default, the package constructs data that would create all KRIs that
are available in the [`{gsm}`](https://github.com/Gilead-BioStats/gsm)
package.

## Installation \*Temporarily broken due to repo permissions issue\*

You can install the development version of
[gsm.datasim](https://github.com/Gilead-BioStats/gsm.datasim) from
GitHub with:

    # install.packages("pak")
    pak::pak("Gilead-BioStats/gsm.datasim@dev")

## Sample Code

     # Generate raw data using specified parameters
     data <- raw_data_generator(ParticipantCount = 100, SiteCount = 10, StudyID = "Study01", SnapshotCount = 5)

     # Generate raw data using specified parameters and Mappings
     data <- raw_data_generator(ParticipantCount = 100,
                                SiteCount = 10,
                                StudyID = "Study01",
                                SnapshotCount = 5,
                                workflow_path = "workflow/1_mappings",
                                mappings = c("Raw_STUDY", "Raw_SITE", "Raw_SUBJ", "Raw_ENROLL",)

     # Generate raw data using a template file (currently it runs on a smaller template but not always on a larger one),
     # this is our current goto sim process:
     data <- raw_data_generator(template_path = "~/gsm.datasim/data-raw/small_template.csv")

     # Longitudinal study with increased outlier prevalence (more analysis flags)
     study <- quick_longitudinal_study(
         study_name = "DEMO-OUTLIERS",
         participants = 200,
         sites = 15,
         months_duration = 6,
         outlier_intensity = 2.5
     )

     # Same option is available in create_longitudinal_study()
     study2 <- create_longitudinal_study(
         study_id = "DEMO-OUTLIERS-2",
         participants = 150,
         sites = 10,
         snapshots = 4,
         interval = "1 month",
         domains = c("AE", "LB", "VISIT", "QUERY"),
         outlier_intensity = 2
     )

     # Generate multiple studies at once
     studies <- create_multiple_longitudinal_studies(
         study_names = c("STUDY-001", "STUDY-002", "STUDY-003"),
         participants = 200,
         sites = 12,
         snapshots = 6,
         domains = c("AE", "LB", "VISIT", "PD"),
         run_analytics = TRUE,
         verbose = TRUE
     )

See `inst/examples/run_gsm_workflows.R` for a script to run all gsm
workflows using data generated from `raw_data_generator`. Result will be
all analysis and reporting data along with html report outputs.
