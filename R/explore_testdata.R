#' Test Data Explorer widget
#'
#' An interactive htmlwidget that summarises a test data bundle and lets users
#' browse every table in it: a header built from `manifest.json`, an inventory
#' tree (snapshot, layer, table with rows and columns), a sortable, searchable,
#' paginated table viewer with per-column summaries, and a few overview charts
#' (rows per table across snapshots, subjects per site, adverse events per
#' subject, enrolment over time).
#'
#' The table viewer and tree are adapted from the `open.gismo` / `workr`
#' front-end modules and charts use `gsm.viz`; see
#' `inst/htmlwidgets/lib/testdata-explorer/VENDORED.md`.
#'
#' Tables in `full_layers` of the latest snapshot (by default the `raw/` layer,
#' i.e. the `lSource` equivalent) embed up to `full_max_rows` rows; every other
#' table is capped at `max_rows` rows so the page stays small (a full-size
#' bundle's `Raw_LB` alone would be tens of megabytes). Summary statistics
#' always cover the full tables, and the header links to the complete bundle.
#'
#' @param bundle A `testdata_bundle` or the path to a bundle folder.
#' @param max_rows Maximum number of rows embedded for tables that are not in
#'   `full_layers` of the latest snapshot.
#' @param full_layers Layers of the latest snapshot that get the larger
#'   `full_max_rows` budget.
#' @param full_max_rows Maximum number of rows embedded for tables in
#'   `full_layers` of the latest snapshot.
#' @param width,height Widget size (CSS units); defaults fill the container.
#' @param elementId Optional element id for the widget.
#'
#' @return An htmlwidget of class `Widget_TestDataExplorer`.
#' @examples
#' \dontrun{
#' bundle <- read_testdata_bundle("~/gsm-testdata/AA-AA-000-0000")
#' explore_testdata(bundle)
#' }
#' @export
explore_testdata <- function(bundle,
                             max_rows = 1000,
                             full_layers = "raw",
                             full_max_rows = 5000,
                             width = NULL,
                             height = NULL,
                             elementId = NULL) {
  rlang::check_installed("htmlwidgets", reason = "to render the Test Data Explorer.")
  if (!is.numeric(max_rows) || length(max_rows) != 1 || is.na(max_rows) || max_rows < 1) {
    stop("`max_rows` must be a single positive number.")
  }
  max_rows <- as.integer(floor(max_rows))
  if (!is.numeric(full_max_rows) || length(full_max_rows) != 1 || is.na(full_max_rows) || full_max_rows < 1) {
    stop("`full_max_rows` must be a single positive number.")
  }
  full_max_rows <- as.integer(floor(full_max_rows))
  bundle <- as_testdata_bundle(bundle)
  summary <- summarize_testdata_bundle(bundle)

  snapshots <- sort(unique(summary$tables$snapshot))
  latest <- if (length(snapshots)) max(snapshots) else NA_character_

  data <- list()
  for (i in seq_len(nrow(summary$tables))) {
    t <- summary$tables[i, ]
    df <- read_bundle_table(file.path(bundle$path, t$file))
    full <- t$layer %in% full_layers && identical(t$snapshot, latest)
    keep <- utils::head(df, if (full) full_max_rows else max_rows)
    data[[paste(t$snapshot, t$layer, t$table, sep = "/")]] <- list(
      columns = I(names(df)),
      types = I(unname(vapply(df, function(x) class(x)[[1]], character(1)))),
      rows = .rows_for_json(keep),
      total_rows = nrow(df),
      truncated = nrow(keep) < nrow(df)
    )
  }

  x <- list(
    manifest = bundle$manifest,
    tables = summary$tables,
    columns = summary$columns,
    data = data,
    charts = explorer_charts(bundle, summary, latest),
    latest = latest,
    options = list(max_rows = max_rows, full_layers = I(full_layers), full_max_rows = full_max_rows)
  )
  attr(x, "TOJSON_ARGS") <- list(dataframe = "rows", na = "null", null = "null")

  htmlwidgets::createWidget(
    name = "Widget_TestDataExplorer",
    x = x,
    width = width,
    height = height,
    package = "gsm.datasim",
    elementId = elementId,
    sizingPolicy = htmlwidgets::sizingPolicy(
      defaultWidth = "100%", defaultHeight = 720,
      viewer.fill = TRUE, browser.fill = TRUE,
      knitr.figure = FALSE, knitr.defaultWidth = "100%", knitr.defaultHeight = 720
    )
  )
}

# Row-oriented, JSON-ready copy of a data frame: dates as ISO strings, factors
# as character; each row an unnamed list so it serialises as an array.
.rows_for_json <- function(df) {
  if (!nrow(df)) {
    return(list())
  }
  cols <- lapply(df, function(x) {
    if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
    if (inherits(x, "POSIXt")) return(format(x, "%Y-%m-%d %H:%M:%S"))
    if (is.factor(x)) return(as.character(x))
    x
  })
  purrr::transpose(unname(cols))
}

# Overview chart data computed from the latest snapshot's raw layer.
explorer_charts <- function(bundle, summary, latest) {
  empty <- function(...) {
    args <- list(...)
    as.data.frame(args, stringsAsFactors = FALSE)
  }
  read_raw <- function(table) {
    row <- summary$tables[summary$tables$snapshot == latest & summary$tables$layer == "raw" & summary$tables$table == table, ]
    if (!nrow(row)) return(NULL)
    read_bundle_table(file.path(bundle$path, row$file[[1]]))
  }
  pick <- function(df, candidates) {
    hit <- intersect(candidates, names(df))
    if (length(hit)) hit[[1]] else NULL
  }

  rows_by_snapshot <- summary$tables[, c("snapshot", "layer", "table", "rows")]
  rownames(rows_by_snapshot) <- NULL

  subj <- if (!is.na(latest)) read_raw("Raw_SUBJ") else NULL
  site_col <- if (!is.null(subj)) pick(subj, c("invid", "siteid", "site_id")) else NULL
  subjects_per_site <- if (!is.null(site_col)) {
    counts <- sort(table(subj[[site_col]]), decreasing = TRUE)
    empty(site = names(counts), subjects = as.integer(counts))
  } else {
    empty(site = character(), subjects = integer())
  }

  ae <- if (!is.na(latest)) read_raw("Raw_AE") else NULL
  subj_col_ae <- if (!is.null(ae)) pick(ae, c("subjid", "subjectid", "usubjid")) else NULL
  subj_col <- if (!is.null(subj)) pick(subj, c("subjid", "subjectid", "usubjid")) else NULL
  aes_per_subject <- if (!is.null(subj_col_ae) && !is.null(subj_col)) {
    per_subject <- table(factor(ae[[subj_col_ae]], levels = unique(subj[[subj_col]])))
    # labels chosen so that a plain string sort keeps them in numeric order
    bins <- cut(as.integer(per_subject), breaks = c(-Inf, 0, 1, 2, 3, 5, 10, Inf),
                labels = c("0", "1", "2", "3", "4 to 5", "6 to 10", "over 10"))
    counts <- table(bins)
    empty(aes = names(counts), subjects = as.integer(counts))
  } else {
    empty(aes = character(), subjects = integer())
  }

  enrol_col <- if (!is.null(subj)) pick(subj, c("enrolldt", "enroll_dt", "drv_enrollment_dt")) else NULL
  enrollment_over_time <- if (!is.null(enrol_col)) {
    d <- as.Date(subj[[enrol_col]])
    d <- d[!is.na(d)]
    if (length(d)) {
      month <- format(d, "%Y-%m")
      counts <- table(month)
      empty(month = names(counts), enrolled = as.integer(counts), cumulative = cumsum(as.integer(counts)))
    } else {
      empty(month = character(), enrolled = integer(), cumulative = integer())
    }
  } else {
    empty(month = character(), enrolled = integer(), cumulative = integer())
  }

  list(
    rows_by_snapshot = rows_by_snapshot,
    subjects_per_site = subjects_per_site,
    aes_per_subject = aes_per_subject,
    enrollment_over_time = enrollment_over_time
  )
}
