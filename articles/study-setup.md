# Setting up Clinical Studies with gsm.datasim

``` r
library(gsm.datasim)
#> Registered S3 method overwritten by 'logger':
#>   method         from 
#>   print.loglevel log4r
```

## Introduction

The `gsm.datasim` package provides a comprehensive framework for
generating synthetic clinical trial data. This vignette demonstrates how
to set up both single snapshot and longitudinal studies, covering the
key functions and configuration options available.

### Overview of Study Types

The package supports two main types of studies:

1.  **Single Snapshot Studies**: Generate data for a single time point
2.  **Longitudinal Studies**: Generate data across multiple time points
    to simulate study progression

## Single Snapshot Studies

A single snapshot study generates clinical data for one specific time
point. This is useful for testing analytics workflows or when you need a
static dataset.

### Basic Single Snapshot Setup

The simplest way to create a single snapshot study is using the study
configuration system:

``` r
# Create a basic study configuration
config <- create_study_config(
  study_id = "STUDY001",
  participant_count = 100,
  site_count = 5
)

# Add clinical domains (datasets)
config <- add_dataset_config(config, "Raw_AE", enabled = TRUE)     # Adverse Events
config <- add_dataset_config(config, "Raw_LB", enabled = TRUE)     # Laboratory Data
config <- add_dataset_config(config, "Raw_VISIT", enabled = TRUE)  # Visit Data

# Generate the study data
raw_data <- generate_study_data(config, verbose = TRUE)

# Examine the generated data structure
names(raw_data)
```

### Configuring Temporal Settings

Even for single snapshots, you can control the temporal aspects:

``` r
# Create configuration with specific temporal settings
config <- create_study_config(
  study_id = "STUDY002", 
  participant_count = 200,
  site_count = 10
)

# Set temporal configuration
config <- set_temporal_config(
  config,
  start_date = "2023-06-01",
  snapshot_count = 1,           # Single snapshot
  snapshot_width = "months"
)

# Add domains with custom configurations
config <- add_dataset_config(config, "Raw_AE", enabled = TRUE)
config <- add_dataset_config(config, "Raw_LB", enabled = TRUE, 
                            growth_pattern = "exponential")

# Generate data
raw_data <- generate_study_data(config, verbose = TRUE)
```

### Adding Analytics Pipeline

You can run analytics on your single snapshot study. Results are
returned as-is from the gsm.core pipeline — a named list where each
snapshot contains `$results` (named by workflow,
e.g. `Analysis_kri0001`), `$mapped`, `$lWorkflow`, and `$summary`:

``` r
# Create study with analytics
config <- create_study_config(
  study_id = "STUDY003",
  participant_count = 150,
  site_count = 8,
  analytics_package = "gsm.kri"
)

# Add clinical domains
config <- add_dataset_config(config, "Raw_AE", enabled = TRUE)
config <- add_dataset_config(config, "Raw_LB", enabled = TRUE)

# Generate data
raw_data <- generate_study_data(config, verbose = TRUE)

# Run analytics pipeline — returns raw gsm.core results per snapshot
analytics_results <- generate_analytics_layers(raw_data, config, verbose = TRUE)

# Each snapshot contains $results, $mapped, $lWorkflow, $summary
names(analytics_results[[1]])

# Access a specific metric result across all its data frames
kri_result <- analytics_results[[1]]$results$Analysis_kri0001
names(kri_result)  # e.g. Analysis_Summary, Analysis_Flagged, Analysis_Analyzed
```

### Adding the Reporting Pipeline

The reporting pipeline runs gsm.reporting workflows on top of the
analytics results. Pass the analytics output directly to
[`generate_reporting_layers()`](https://gilead-biostats.github.io/gsm.datasim/reference/generate_reporting_layers.md):

``` r
# Run reporting pipeline on analytics results
reporting_results <- generate_reporting_layers(
  analytics_results = analytics_results,
  config = config,
  verbose = TRUE
)

# Results are keyed by snapshot name, matching analytics_results
names(reporting_results)
```

## Longitudinal Studies

Longitudinal studies simulate data evolution over time, which is
essential for understanding how clinical metrics change throughout a
study.

### Basic Longitudinal Study

The
[`create_longitudinal_study()`](https://gilead-biostats.github.io/gsm.datasim/reference/create_longitudinal_study.md)
function provides the easiest way to set up a longitudinal study. By
default it runs the analytics pipeline; set `run_reporting = TRUE` to
also run the reporting pipeline:

``` r
# Create a longitudinal study with default settings
study <- create_longitudinal_study(
  study_id = "LONG-001",
  participants = 100,
  sites = 5,
  snapshots = 6,                    # 6 time points
  interval = "1 month",             # Monthly snapshots
  domains = c("AE", "LB", "VISIT"), # Clinical domains to include
  run_analytics = TRUE,             # Run analytics pipeline
  run_reporting = TRUE,             # Also run reporting pipeline
  verbose = TRUE
)

# Examine the study structure
names(study)  # includes $raw_data, $analytics, $reporting

# View snapshot dates
names(study$raw_data)

# Analytics results: raw gsm.core output per snapshot
if (!is.null(study$analytics)) {
  names(study$analytics[[1]]$results)  # e.g. "Analysis_kri0001", ...
}

# Reporting results per snapshot
if (!is.null(study$reporting)) {
  names(study$reporting)
}
```

### Advanced Longitudinal Configuration

For more control over the longitudinal study setup:

``` r
# Create a complex longitudinal study
study <- create_longitudinal_study(
  study_id = "ADVANCED-001",
  participants = 500,
  sites = 25,
  snapshots = 12,                               # 1 year of monthly data
  interval = "1 month",
  domains = c("AE", "LB", "VISIT", "PD", "PK"), # Multiple domains
  run_analytics = TRUE,
  analytics_package = "gsm.kri",           # Specify analytics package
  verbose = TRUE
)

# Generate study summary
summary_data <- summarize_longitudinal_study(study)
print(summary_data)
```

### Custom Base Dates

You can specify a custom starting date for your longitudinal study:

``` r
# Use the lower-level function for more control
mappings <- ensure_core_mappings(c("AE", "LB", "VISIT"))

raw_data <- generate_study_snapshots(
  study_id = "CUSTOM-001",
  participants = 100,
  sites = 8,
  snapshots = 6,
  interval = "2 months",                        # Bi-monthly snapshots
  mappings = mappings,
  base_date = "2022-01-15",                    # Custom start date
  verbose = TRUE
)

# Create study object
config <- list(
  participants = 100,
  sites = 8,
  snapshots = 6,
  interval = "2 months",
  domains = c("AE", "LB", "VISIT")
)

study <- create_longitudinal_study_data(
  study_id = "CUSTOM-001",
  raw_data = raw_data,
  config = config
)
```

### Different Time Intervals

The package supports various time intervals for longitudinal studies:

``` r
# Weekly snapshots
weekly_study <- create_longitudinal_study(
  study_id = "WEEKLY-001",
  participants = 50,
  sites = 3,
  snapshots = 12,
  interval = "1 week",              # Weekly intervals
  domains = c("AE", "VISIT"),
  verbose = TRUE
)

# Quarterly snapshots  
quarterly_study <- create_longitudinal_study(
  study_id = "QUARTERLY-001",
  participants = 300,
  sites = 15,
  snapshots = 8,
  interval = "3 months",            # Quarterly intervals  
  domains = c("AE", "LB", "PD"),
  verbose = TRUE
)
```

### Quick Study Creation

For rapid prototyping, use the
[`quick_longitudinal_study()`](https://gilead-biostats.github.io/gsm.datasim/reference/quick_longitudinal_study.md)
function. Both analytics and reporting run by default when
`include_pipeline = TRUE`:

``` r
# Create a study with sensible defaults — analytics and reporting both run by default
quick_study <- quick_longitudinal_study(
  study_name = "QUICK-PROTO-001",
  participants = 200,
  sites = 10,
  months_duration = 18,             # 18-month study
  study_type = "standard",
  include_pipeline = TRUE,          # default: runs analytics + reporting
  verbose = TRUE
)

# Analytics and reporting results are available on the study object
names(quick_study)  # includes $analytics and $reporting
```

## Study Configuration Best Practices

### Domain Selection

Choose clinical domains based on your study objectives:

``` r
# Oncology study domains
oncology_domains <- c("AE", "LB", "PD", "OverallResponse", 
                     "VISIT", "DATACHG", "QUERY")

# Safety study domains  
safety_domains <- c("AE", "LB", "VISIT", "EXCLUSION",
                   "Death", "STUDCOMP")

# PK/PD study domains
pkpd_domains <- c("PK", "PD", "LB", "VISIT", "AE")

# Create study with oncology focus
onc_study <- create_longitudinal_study(
  study_id = "ONCOLOGY-001",
  participants = 300,
  sites = 20,
  snapshots = 24,
  interval = "1 month", 
  domains = oncology_domains,
  verbose = TRUE
)
```

### Analytics and Reporting Configuration

Configure the analytics package and specific workflows, and optionally
customise the reporting package. The analytics pipeline returns raw
gsm.core results per snapshot; pass them directly to the reporting
pipeline.

``` r
# Create study with specific analytics workflows
study <- create_longitudinal_study(
  study_id = "MONITORED-001",
  participants = 400,
  sites = 30,
  snapshots = 12,
  interval = "1 month",
  domains = c("AE", "LB", "VISIT", "QUERY"),
  run_analytics = TRUE,
  analytics_package = "gsm.kri",
  analytics_workflows = c("kri0001", "kri0002"), # Specific workflows
  run_reporting = TRUE,
  verbose = TRUE
)

# Access analytics: $results keyed by workflow name
if (!is.null(study$analytics)) {
  first_snap <- study$analytics[[1]]

  # All metric results for the first snapshot
  metric_names <- names(first_snap$results)

  # Access a specific metric and its data frames
  kri <- first_snap$results[[metric_names[1]]]
  kri$Analysis_Summary   # summary-level data frame
  kri$Analysis_Flagged   # flagged entities
  kri$Analysis_Analyzed  # full analyzed data
}

# Access reporting: keyed by snapshot date
if (!is.null(study$reporting)) {
  names(study$reporting)        # snapshot dates
  study$reporting[[1]]          # reporting output for first snapshot
}
```

You can also run the pipelines separately on an existing study object:

``` r
# Build raw data only
study <- create_longitudinal_study(
  study_id = "STEPWISE-001",
  participants = 200,
  sites = 10,
  snapshots = 6,
  interval = "1 month",
  domains = c("AE", "LB"),
  run_analytics = FALSE   # skip pipelines for now
)

# Run analytics when ready
study <- run_longitudinal_analytics(study, verbose = TRUE)

# Run reporting on top of analytics
study <- run_longitudinal_reporting(study, verbose = TRUE)
```

## Data Examination and Validation

### Exploring Generated Data

``` r
# After generating a study, examine the data structure
study <- create_longitudinal_study(
  study_id = "EXPLORE-001",
  participants = 50,
  sites = 3,
  snapshots = 3,
  interval = "1 month",
  domains = c("AE", "LB"),
  verbose = TRUE,
  run_analytics = FALSE
)

# Check data for each snapshot
for (i in 1:length(study$raw_data)) {
  snapshot_name <- names(study$raw_data)[i]
  snapshot_data <- study$raw_data[[i]]
  
  cat("Snapshot:", snapshot_name, "\n")
  cat("Datasets:", paste(names(snapshot_data), collapse = ", "), "\n")
  
  # Check sample sizes
  if ("Raw_SUBJ" %in% names(snapshot_data)) {
    cat("Subjects:", nrow(snapshot_data$Raw_SUBJ), "\n")
  }
  if ("Raw_AE" %in% names(snapshot_data)) {
    cat("AE Records:", nrow(snapshot_data$Raw_AE), "\n") 
  }
  cat("\n")
}
```

### Data Quality Checks

``` r
# Function to validate study data
validate_study_data <- function(study) {
  validation_results <- list()
  
  for (snapshot_name in names(study$raw_data)) {
    snapshot <- study$raw_data[[snapshot_name]]
    
    # Check required datasets
    required_datasets <- c("Raw_STUDY", "Raw_SITE", "Raw_SUBJ", "Raw_ENROLL")
    missing_required <- setdiff(required_datasets, names(snapshot))
    
    validation_results[[snapshot_name]] <- list(
      datasets_present = names(snapshot),
      missing_required = missing_required,
      total_subjects = if ("Raw_SUBJ" %in% names(snapshot)) nrow(snapshot$Raw_SUBJ) else 0,
      total_sites = if ("Raw_SITE" %in% names(snapshot)) nrow(snapshot$Raw_SITE) else 0
    )
  }
  
  return(validation_results)
}

# Validate a study
validation <- validate_study_data(study)
str(validation)
```

## Exporting Study Data

Once you have a complete study object — with raw data, analytics, and
(optionally) reporting — you can write everything to disk with
[`export_study_data()`](https://gilead-biostats.github.io/gsm.datasim/reference/export_study_data.md).
The function creates a structured folder hierarchy under a root
directory:

    <output_dir>/<study_id>/
      <snapshot_date>/
        raw/          # Raw_*.csv
        mapped/       # Mapped_*.csv  (present when analytics ran)
        analytics/    # <metric>_<table>.csv  (present when analytics ran)
        reporting/    # Reporting_*.csv  (present when reporting ran)

Folders are only created when the corresponding data exists for a
snapshot.

### Basic Export

``` r
# Generate a small study to export
study <- create_longitudinal_study(
  study_id     = "EXPORT-001",
  participants = 100,
  sites        = 5,
  snapshots    = 3,
  interval     = "1 month",
  domains      = c("AE", "LB"),
  run_analytics = TRUE,
  run_reporting = TRUE,
  verbose       = FALSE
)

# Export to a temporary directory — returns the path to the study folder invisibly
study_path <- export_study_data(
  study      = study,
  output_dir = tempdir(),
  verbose    = TRUE       # prints per-snapshot progress
)

# Confirm the folder structure
list.dirs(study_path, full.names = FALSE, recursive = TRUE)
```

### Controlling the Output Location

``` r
# Write to a specific project folder
study_path <- export_study_data(
  study        = study,
  output_dir   = "~/my_studies",    # root folder
  study_folder = "EXPORT-001-v2",   # override the auto-generated folder name
  overwrite    = TRUE               # allow writing into an existing folder
)
```

### Preserving Non-CSV Objects

The analytics pipeline stores workflow lists, summaries, and other
objects that cannot be flattened to CSV. Set `save_rds = TRUE` to write
a companion `analytics_full.rds` file per snapshot:

``` r
study_path <- export_study_data(
  study      = study,
  output_dir = tempdir(),
  overwrite  = TRUE,
  save_rds   = TRUE   # also writes analytics_full.rds per snapshot
)

# Reload the full analytics object for a specific snapshot
analytics_snap <- readRDS(file.path(study_path, names(study$raw_data)[1],
                                    "analytics_full.rds"))
names(analytics_snap)  # $results, $mapped, $lWorkflow, $summary
```

### Inspecting the Exported Files

``` r
# List all CSV files written across all snapshots
all_csvs <- list.files(study_path, pattern = "\\.csv$", recursive = TRUE)
cat(length(all_csvs), "CSV files written\n")

# Raw data lives in <snapshot>/raw/
raw_csvs <- all_csvs[grepl("/raw/", all_csvs)]
cat("Raw CSVs:", paste(basename(raw_csvs), collapse = ", "), "\n")

# Read a specific snapshot's adverse event data back in
snap_date <- names(study$raw_data)[1]
ae_path   <- file.path(study_path, snap_date, "raw", "Raw_AE.csv")
ae_df     <- read.csv(ae_path)
nrow(ae_df)
```

## Conclusion

The `gsm.datasim` package provides flexible tools for creating both
single snapshot and longitudinal clinical studies. Key takeaways:

- Use
  [`create_study_config()`](https://gilead-biostats.github.io/gsm.datasim/reference/create_study_config.md)
  and
  [`generate_study_data()`](https://gilead-biostats.github.io/gsm.datasim/reference/generate_study_data.md)
  for single snapshots
- Use
  [`create_longitudinal_study()`](https://gilead-biostats.github.io/gsm.datasim/reference/create_longitudinal_study.md)
  for multi-snapshot studies
- Configure domains based on your study type and monitoring needs
- Leverage the analytics pipeline for automated quality monitoring
- Use
  [`quick_longitudinal_study()`](https://gilead-biostats.github.io/gsm.datasim/reference/quick_longitudinal_study.md)
  for rapid prototyping
- Use
  [`export_study_data()`](https://gilead-biostats.github.io/gsm.datasim/reference/export_study_data.md)
  to write completed studies to a structured folder hierarchy

The modular design allows you to start simple and add complexity as
needed, making it suitable for both exploratory work and production data
generation scenarios.
