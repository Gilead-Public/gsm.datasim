# gsm.datasim (development version)

# gsm.datasim v2.0.0

This major release introduces a comprehensive study builder API, longitudinal study support, workflow-driven data generation, a domain registry, and parquet export — representing a significant expansion of the package's capabilities.

## Breaking Changes

- `generate_rawdata_for_single_study()` and `raw_data_generator()` are now deprecated in favour of the new study builder API (`create_study_config()` + `generate_study_data()`). Existing calls continue to work but will emit a deprecation warning.

## New Study Builder API

A composable, config-driven approach to study data generation:

- `create_study_config()` — create a study configuration object with participant count, site count, snapshot settings, and domain selections.
- `create_standard_study_config()` — convenience wrapper with sensible defaults for standard RBQM domains.
- `set_outlier_config()` — add outlier injection settings to a config.
- `set_temporal_config()` — configure snapshot count, interval, and start date.
- `add_dataset_config()` / `remove_dataset_config()` — enable or disable individual domain datasets within a config.
- `validate_study_config()` — validate a config object before data generation.

## Longitudinal Study Functions

End-to-end support for generating and analysing longitudinal multi-snapshot studies:

- `create_longitudinal_study()` — generate a complete longitudinal study (raw data + optional analytics and reporting layers) from a config.
- `quick_longitudinal_study()` — one-call convenience wrapper for common longitudinal study setups.
- `create_multiple_longitudinal_studies()` / `export_multiple_studies()` — batch generation and export across a portfolio of studies.
- `study_portfolio()` — define a set of study variants for batch processing.
- `summarize_longitudinal_study()` — print a summary of a longitudinal study object.
- `run_longitudinal_analytics()` / `run_longitudinal_reporting()` — run the analytics or reporting pipeline on an existing longitudinal study object.
- `get_snapshot_data()` / `get_domain_timeline()` / `get_available_domains()` — extract snapshot-level and domain-level data from a longitudinal study.

## Analytics and Reporting Pipelines

- `generate_analytics_layers()` — run the gsm analytics pipeline across all snapshots.
- `generate_reporting_layers()` — run the gsm reporting pipeline on analytics results.
- `execute_analytics_pipeline()` / `execute_reporting_pipeline()` — lower-level pipeline execution helpers.

## Domain Registry and Column Data Inference

- `get_domain_registry()` — retrieve the built-in registry of known domains and their column specifications.
- `generate_column_by_type()` — generate a single column of data driven by its type specification.
- `generate_unknown_domain()` — generate data for a domain not present in the registry using inferred column types.
- `infer_column_type()` — infer a column's data type from its name and spec.

## Workflow-Driven Data Generation

- `generate_data_from_workflows()` — generate raw domain data directly from `gsm.mapping` workflow specifications, supporting custom workflow paths and spec overrides.
- `generate_raw_data_for_endpoints()` — generate raw data for endpoint-specific domains using workflow-driven specs.
- `prepare_combined_specs_for_generation()` — merge and prepare column specs from multiple workflow sources.

## Export Enhancements

- `export_study_data()` — export study data to disk with support for `"csv"` (default), `"parquet"`, or `"both"` formats via the `format` argument (requires the `arrow` package).
- `get_snapshot_data()` — retrieve a specific snapshot's data from an exported study.

## New and Extended Domain Support

- Added gradual enrollment simulation across all data generation functions.
- New domain generators: `Raw_VISIT`, `Raw_Death`, `Raw_Consents`, `Raw_Baseline`, `Raw_Randomization`, `Raw_GENERAL`, `Raw_gilda_STUDY`.
- Extended existing domain generators (`Raw_AE`, `Raw_LB`, `Raw_PD`, `Raw_PK`, `Raw_DATACHG`, `Raw_DATAENT`, `Raw_QUERY`, `Raw_SDRGCOMP`, `Raw_STUDCOMP`) with additional columns and timestamp support.
- `ensure_core_mappings()` — guarantee that the required core mapping domains are always present in a domain list.
- Conditional `EXCLUSION` mapping: only included when `IE`, `ENROLL`, and `PD` are all present in the raw data.

## Utilities

- `validate_study_inputs()` — validate participant, site, snapshot, and domain inputs at the study boundary.
- `parse_interval_to_snapshot_width()` — convert interval strings (e.g. `"1 month"`) to numeric snapshot widths.

# gsm.datasim v1.1.3

This patch release makes the following updates:

- Added `strStartDate` to `generate_rawdata_for_single_study()` and `raw_data_generator()` to allow arbitrary snapshot start dates.
- Added `db_lock_dt` generation in `Raw_STUDY` based on the snapshot global max date.
- Updated default count scaling for `SDRGCOMP` and `AntiCancer` records in `generate_rawdata_for_single_study()`.

# gsm.datasim v1.1.2
This release resolves a bug involving duplicate records in the randomization domain.

# gsm.datasim v1.1.1
This release adds new contributor guidelines and standardized issue templates.

# gsm.datasim v1.1.0

## Notable Changes:
**New Mapping Workflows Support:**
- Additional workflow support for "IE" domain
- dates/timestamp support for many domains now available.

# gsm.datasim v1.0.0

We are excited to announce the first major release of the `gsm.datasim` package, 
a collection of functions to generate synthetic test data for the RBQM of Clinical Trials based on several parameters.

## Notable Changes:
**User-facing functions**
- `generate_rawdata_for_single_study()` can create snapshot(s) for a single study when provided with the proper parameters and appropriate mapping specifications.
- `raw_data_generator()` is a wrapper to run `generate_rawdata_for_single_study()` to create multiple studies if providing a template/dataset containing a variety of these parameters.

**New Mapping Workflows Support:**
- All workflows that exist in `gsm.mapping`'s `inst/workflow/1_mappings` are now supported, 
with "STUDY", "SUBJ", "SITE" and "ENROLL" always being required. 
- There are many `gsm.endpoints` specific domains, such as `Anticancer` and `OverallResponse` 
that now have support as well.

**Replacing `clindata` with `gsm.datasim`:**
- The object `gsm.core::lSource` was created using `gsm.datasim` for examples, tests, and vignettes across the `gsm` ecosystem.
This object is based on "core mappings" which include: "AE", "COUNTRY", "DATACHG", "DATAENT", "ENROLL", "LB", "PD", "PK", "QUERY", "STUDY", "STUDCOMP", "SDRGCOMP", "SITE", "SUBJ" 
