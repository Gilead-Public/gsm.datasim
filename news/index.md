# Changelog

## gsm.datasim v1.1.3

This patch release makes the following updates:

- Added `strStartDate` to
  [`generate_rawdata_for_single_study()`](https://gilead-biostats.github.io/gsm.datasim/reference/generate_rawdata_for_single_study.md)
  and
  [`raw_data_generator()`](https://gilead-biostats.github.io/gsm.datasim/reference/raw_data_generator.md)
  to allow arbitrary snapshot start dates.
- Added `db_lock_dt` generation in `Raw_STUDY` based on the snapshot
  global max date.
- Updated default count scaling for `SDRGCOMP` and `AntiCancer` records
  in
  [`generate_rawdata_for_single_study()`](https://gilead-biostats.github.io/gsm.datasim/reference/generate_rawdata_for_single_study.md).

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
[`generate_rawdata_for_single_study()`](https://gilead-biostats.github.io/gsm.datasim/reference/generate_rawdata_for_single_study.md)
can create snapshot(s) for a single study when provided with the proper
parameters and appropriate mapping specifications. -
[`raw_data_generator()`](https://gilead-biostats.github.io/gsm.datasim/reference/raw_data_generator.md)
is a wrapper to run
[`generate_rawdata_for_single_study()`](https://gilead-biostats.github.io/gsm.datasim/reference/generate_rawdata_for_single_study.md)
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
