# Test Data Explorer widget

An interactive htmlwidget that summarises a test data bundle and lets
users browse every table in it: a header built from `manifest.json`, an
inventory tree (snapshot, layer, table with rows and columns), a
sortable, searchable, paginated table viewer with per-column summaries,
and a few overview charts (rows per table across snapshots, subjects per
site, adverse events per subject, enrolment over time).

## Usage

``` r
explore_testdata(
  bundle,
  max_rows = 1000,
  full_layers = "raw",
  full_max_rows = 5000,
  width = NULL,
  height = NULL,
  elementId = NULL
)
```

## Arguments

- bundle:

  A `testdata_bundle` or the path to a bundle folder.

- max_rows:

  Maximum number of rows embedded for tables that are not in
  `full_layers` of the latest snapshot.

- full_layers:

  Layers of the latest snapshot that get the larger `full_max_rows`
  budget.

- full_max_rows:

  Maximum number of rows embedded for tables in `full_layers` of the
  latest snapshot.

- width, height:

  Widget size (CSS units); defaults fill the container.

- elementId:

  Optional element id for the widget.

## Value

An htmlwidget of class `Widget_TestDataExplorer`.

## Details

The table viewer and tree are adapted from the `open.gismo` / `workr`
front-end modules and charts use `gsm.viz`; see
`inst/htmlwidgets/lib/testdata-explorer/VENDORED.md`.

Tables in `full_layers` of the latest snapshot (by default the `raw/`
layer, i.e. the `lSource` equivalent) embed up to `full_max_rows` rows;
every other table is capped at `max_rows` rows so the page stays small
(a full-size bundle's `Raw_LB` alone would be tens of megabytes).
Summary statistics always cover the full tables, and the header links to
the complete bundle.

## Examples

``` r
if (FALSE) { # \dontrun{
bundle <- read_testdata_bundle("~/gsm-testdata/AA-AA-000-0000")
explore_testdata(bundle)
} # }
```
