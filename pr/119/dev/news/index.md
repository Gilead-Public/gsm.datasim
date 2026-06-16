# Changelog

## gsm.datasim (development version)

## gsm.datasim v2.0.0

This major release introduces a comprehensive study builder API,
longitudinal study support, workflow-driven data generation, a domain
registry, and parquet export — representing a significant expansion of
the package’s capabilities.

### Breaking Changes

- [`generate_rawdata_for_single_study()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/generate_rawdata_for_single_study.md)
  and
  [`raw_data_generator()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/raw_data_generator.md)
  are now deprecated in favour of the new study builder API
  ([`create_study_config()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/create_study_config.md) +
  [`generate_study_data()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/generate_study_data.md)).
  Existing calls continue to work but will emit a deprecation warning.

### New Study Builder API

A composable, config-driven approach to study data generation:

- [`create_study_config()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/create_study_config.md)
  — create a study configuration object with participant count, site
  count, snapshot settings, and domain selections.
- [`create_standard_study_config()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/create_standard_study_config.md)
  — convenience wrapper with sensible defaults for standard RBQM
  domains.
- [`set_outlier_config()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/set_outlier_config.md)
  — add outlier injection settings to a config.
- [`set_temporal_config()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/set_temporal_config.md)
  — configure snapshot count, interval, and start date.
- [`add_dataset_config()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/add_dataset_config.md)
  /
  [`remove_dataset_config()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/remove_dataset_config.md)
  — enable or disable individual domain datasets within a config.
- [`validate_study_config()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/validate_study_config.md)
  — validate a config object before data generation.

### Longitudinal Study Functions

End-to-end support for generating and analysing longitudinal
multi-snapshot studies:

- [`create_longitudinal_study()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/create_longitudinal_study.md)
  — generate a complete longitudinal study (raw data + optional
  analytics and reporting layers) from a config.
- [`quick_longitudinal_study()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/quick_longitudinal_study.md)
  — one-call convenience wrapper for common longitudinal study setups.
- [`create_multiple_longitudinal_studies()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/create_multiple_longitudinal_studies.md)
  /
  [`export_multiple_studies()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/export_multiple_studies.md)
  — batch generation and export across a portfolio of studies.
- [`study_portfolio()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/study_portfolio.md)
  — define a set of study variants for batch processing.
- [`summarize_longitudinal_study()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/summarize_longitudinal_study.md)
  — print a summary of a longitudinal study object.
- [`run_longitudinal_analytics()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/run_longitudinal_analytics.md)
  /
  [`run_longitudinal_reporting()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/run_longitudinal_reporting.md)
  — run the analytics or reporting pipeline on an existing longitudinal
  study object.
- [`get_snapshot_data()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/get_snapshot_data.md)
  /
  [`get_domain_timeline()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/get_domain_timeline.md)
  /
  [`get_available_domains()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/get_available_domains.md)
  — extract snapshot-level and domain-level data from a longitudinal
  study.

### Analytics and Reporting Pipelines

- [`generate_analytics_layers()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/generate_analytics_layers.md)
  — run the gsm analytics pipeline across all snapshots.
- [`generate_reporting_layers()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/generate_reporting_layers.md)
  — run the gsm reporting pipeline on analytics results.
- [`execute_analytics_pipeline()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/execute_analytics_pipeline.md)
  /
  [`execute_reporting_pipeline()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/execute_reporting_pipeline.md)
  — lower-level pipeline execution helpers.

### Domain Registry and Column Data Inference

- [`get_domain_registry()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/get_domain_registry.md)
  — retrieve the built-in registry of known domains and their column
  specifications.
- [`generate_column_by_type()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/generate_column_by_type.md)
  — generate a single column of data driven by its type specification.
- [`generate_unknown_domain()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/generate_unknown_domain.md)
  — generate data for a domain not present in the registry using
  inferred column types.
- [`infer_column_type()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/infer_column_type.md)
  — infer a column’s data type from its name and spec.

### Workflow-Driven Data Generation

- [`generate_data_from_workflows()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/generate_data_from_workflows.md)
  — generate raw domain data directly from `gsm.mapping` workflow
  specifications, supporting custom workflow paths and spec overrides.
- [`generate_raw_data_for_endpoints()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/generate_raw_data_for_endpoints.md)
  — generate raw data for endpoint-specific domains using
  workflow-driven specs.
- [`prepare_combined_specs_for_generation()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/prepare_combined_specs_for_generation.md)
  — merge and prepare column specs from multiple workflow sources.

### Export Enhancements

- [`export_study_data()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/export_study_data.md)
  — export study data to disk with support for `"csv"` (default),
  `"parquet"`, or `"both"` formats via the `format` argument (requires
  the `arrow` package).
- [`get_snapshot_data()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/get_snapshot_data.md)
  — retrieve a specific snapshot’s data from an exported study.

### New and Extended Domain Support

- Added gradual enrollment simulation across all data generation
  functions.
- New domain generators: `Raw_VISIT`, `Raw_Death`, `Raw_Consents`,
  `Raw_Baseline`, `Raw_Randomization`, `Raw_GENERAL`, `Raw_gilda_STUDY`.
- Extended existing domain generators (`Raw_AE`, `Raw_LB`, `Raw_PD`,
  `Raw_PK`, `Raw_DATACHG`, `Raw_DATAENT`, `Raw_QUERY`, `Raw_SDRGCOMP`,
  `Raw_STUDCOMP`) with additional columns and timestamp support.
- [`ensure_core_mappings()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/ensure_core_mappings.md)
  — guarantee that the required core mapping domains are always present
  in a domain list.
- Conditional `EXCLUSION` mapping: only included when `IE`, `ENROLL`,
  and `PD` are all present in the raw data.

### Utilities

- [`validate_study_inputs()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/validate_study_inputs.md)
  — validate participant, site, snapshot, and domain inputs at the study
  boundary.
- [`parse_interval_to_snapshot_width()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/parse_interval_to_snapshot_width.md)
  — convert interval strings (e.g. `"1 month"`) to numeric snapshot
  widths.

## gsm.datasim v1.1.3

This patch release makes the following updates:

- Added `strStartDate` to
  [`generate_rawdata_for_single_study()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/generate_rawdata_for_single_study.md)
  and
  [`raw_data_generator()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/raw_data_generator.md)
  to allow arbitrary snapshot start dates.
- Added `db_lock_dt` generation in `Raw_STUDY` based on the snapshot
  global max date.
- Updated default count scaling for `SDRGCOMP` and `AntiCancer` records
  in
  [`generate_rawdata_for_single_study()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/generate_rawdata_for_single_study.md).

## gsm.datasim v1.1.2

This release resolves a bug involving duplicate records in the
randomization domain.

## gsm.datasim v1.1.1

This release adds new contributor guidelines and standardized issue
templates.

## gsm.datasim v1.1.0

### Notable Changes:

**New Mapping Workflows Support:** - Additional workflow support for
“IE” domain - dates/timestamp support for many domains now available.

## gsm.datasim v1.0.0

We are excited to announce the first major release of the `gsm.datasim`
package, a collection of functions to generate synthetic test data for
the RBQM of Clinical Trials based on several parameters.

### Notable Changes:

**User-facing functions** -
[`generate_rawdata_for_single_study()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/generate_rawdata_for_single_study.md)
can create snapshot(s) for a single study when provided with the proper
parameters and appropriate mapping specifications. -
[`raw_data_generator()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/raw_data_generator.md)
is a wrapper to run
[`generate_rawdata_for_single_study()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/generate_rawdata_for_single_study.md)
to create multiple studies if providing a template/dataset containing a
variety of these parameters.

**New Mapping Workflows Support:** - All workflows that exist in
`gsm.mapping`’s `inst/workflow/1_mappings` are now supported, with
“STUDY”, “SUBJ”, “SITE” and “ENROLL” always being required. - There are
many `gsm.endpoints` specific domains, such as `Anticancer` and
`OverallResponse` that now have support as well.

**Replacing `clindata` with `gsm.datasim`:** - The object
[`gsm.core::lSource`](https://gilead-biostats.github.io/gsm.core/reference/lSource.html)
was created using `gsm.datasim` for examples, tests, and vignettes
across the `gsm` ecosystem. This object is based on “core mappings”
which include: “AE”, “COUNTRY”, “DATACHG”, “DATAENT”, “ENROLL”, “LB”,
“PD”, “PK”, “QUERY”, “STUDY”, “STUDCOMP”, “SDRGCOMP”, “SITE”, “SUBJ”
