# Changelog

## gsm.datasim (development version)

## gsm.datasim v2.0.0

This major release introduces a composable study builder API,
longitudinal multi-snapshot study support, workflow-driven data
generation, a domain registry, and parquet export. Analytics and
reporting are now powered by the `workr` workflow engine.

### Breaking Changes

- [`generate_rawdata_for_single_study()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/generate_rawdata_for_single_study.md)
  and
  [`raw_data_generator()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/raw_data_generator.md)
  are deprecated in favour of the study builder API
  ([`create_study_config()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/create_study_config.md) +
  [`generate_study_data()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/generate_study_data.md)).
  Existing calls still work but emit a deprecation warning.
- `gsm.core`, `gsm.kri`, `gsm.mapping`, and `gsm.reporting` moved from
  hard dependencies to `Suggests`, as pipelines now run on the `workr`
  engine. Install those packages only if you run the analytics/reporting
  pipelines.

### New Features

- **Study builder API:** build study configs with
  [`create_study_config()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/create_study_config.md)
  (or
  [`create_standard_study_config()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/create_standard_study_config.md)),
  refine them with
  [`set_outlier_config()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/set_outlier_config.md),
  [`set_temporal_config()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/set_temporal_config.md),
  and
  [`add_dataset_config()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/add_dataset_config.md)
  /
  [`remove_dataset_config()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/remove_dataset_config.md),
  validate with
  [`validate_study_config()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/validate_study_config.md),
  and generate data via
  [`generate_study_data()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/generate_study_data.md).
- **Longitudinal studies:**
  [`create_longitudinal_study()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/create_longitudinal_study.md)
  and
  [`quick_longitudinal_study()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/quick_longitudinal_study.md)
  generate complete multi-snapshot studies with optional
  analytics/reporting;
  [`create_multiple_longitudinal_studies()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/create_multiple_longitudinal_studies.md)
  and
  [`export_multiple_studies()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/export_multiple_studies.md)
  handle portfolios. Extract results with
  [`get_snapshot_data()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/get_snapshot_data.md),
  [`get_domain_timeline()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/get_domain_timeline.md),
  and
  [`get_available_domains()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/get_available_domains.md)
  (the latter also serves as a standalone registry lookup when called
  with no arguments), and run pipelines on existing studies with
  [`run_longitudinal_analytics()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/run_longitudinal_analytics.md)
  /
  [`run_longitudinal_reporting()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/run_longitudinal_reporting.md).
- **Domain registry & workflow-driven generation:**
  [`get_domain_registry()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/get_domain_registry.md)
  exposes known domains;
  [`generate_data_from_workflows()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/generate_data_from_workflows.md)
  and
  [`generate_raw_data_for_endpoints()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/generate_raw_data_for_endpoints.md)
  generate data directly from `gsm.mapping` workflow specs;
  [`generate_unknown_domain()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/generate_unknown_domain.md)
  handles domains outside the registry via inferred column types.
- **Parquet export:**
  [`export_study_data()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/export_study_data.md)
  now supports `"csv"` (default), `"parquet"`, or `"both"` via the
  `format` argument (parquet requires the `arrow` package).
- **New domains:** added `Raw_VISIT`, `Raw_Death` (including a
  `deathcls` classification column), `Raw_Consents`, `Raw_Baseline`,
  `Raw_Randomization`, `Raw_OverallResponse`, and `Raw_GENERAL`, plus
  gradual enrollment simulation and timestamp support across existing
  domains.

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
