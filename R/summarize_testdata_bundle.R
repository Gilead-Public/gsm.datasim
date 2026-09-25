#' Summarise a test data bundle
#'
#' Inventories every table and column in a bundle written by
#' [build_testdata_bundle()]: one row per table with its size, and one row per
#' column with its type, missingness, cardinality, and an example value. This is
#' the data behind [explore_testdata()] and is useful on its own for a quick
#' look at what a bundle version contains.
#'
#' @param bundle A `testdata_bundle` (from [build_testdata_bundle()] or
#'   [read_testdata_bundle()]) or the path to a bundle folder.
#'
#' @return An object of class `testdata_summary`: a list with `manifest`,
#'   `tables` (data frame: `snapshot`, `layer`, `table`, `rows`, `cols`,
#'   `file`), `columns` (data frame: `snapshot`, `layer`, `table`, `column`,
#'   `type`, `n_missing`, `pct_missing`, `n_distinct`, `example`), and `path`.
#' @examples
#' \dontrun{
#' s <- summarize_testdata_bundle("~/gsm-testdata/AA-AA-000-0000")
#' s$tables
#' subset(s$columns, table == "Raw_SUBJ")
#' }
#' @export
summarize_testdata_bundle <- function(bundle) {
  bundle <- as_testdata_bundle(bundle)
  tables <- list_bundle_tables(bundle$path)

  table_rows <- vector("list", nrow(tables))
  column_rows <- vector("list", nrow(tables))
  for (i in seq_len(nrow(tables))) {
    t <- tables[i, ]
    df <- read_bundle_table(file.path(bundle$path, t$file))
    table_rows[[i]] <- data.frame(
      snapshot = t$snapshot, layer = t$layer, table = t$table,
      rows = nrow(df), cols = ncol(df), file = t$file,
      stringsAsFactors = FALSE
    )
    column_rows[[i]] <- summarize_columns(df, t$snapshot, t$layer, t$table)
  }

  structure(
    list(
      manifest = bundle$manifest,
      tables = .rbind_or_empty(table_rows, c("snapshot", "layer", "table", "rows", "cols", "file")),
      columns = .rbind_or_empty(column_rows, c("snapshot", "layer", "table", "column", "type",
                                               "n_missing", "pct_missing", "n_distinct", "example")),
      path = bundle$path
    ),
    class = c("testdata_summary", "list")
  )
}

# Per-column statistics for one table.
summarize_columns <- function(df, snapshot, layer, table) {
  n <- nrow(df)
  rows <- lapply(names(df), function(col) {
    x <- df[[col]]
    missing <- sum(is.na(x))
    present <- x[!is.na(x)]
    example <- if (length(present)) .format_example(present[[1]]) else NA_character_
    data.frame(
      snapshot = snapshot, layer = layer, table = table, column = col,
      type = class(x)[[1]],
      n_missing = as.integer(missing),
      pct_missing = if (n > 0) round(100 * missing / n, 1) else 0,
      n_distinct = length(unique(present)),
      example = example,
      stringsAsFactors = FALSE
    )
  })
  .rbind_or_empty(rows, c("snapshot", "layer", "table", "column", "type",
                          "n_missing", "pct_missing", "n_distinct", "example"))
}

.format_example <- function(v) {
  if (inherits(v, "Date")) return(format(v, "%Y-%m-%d"))
  if (inherits(v, "POSIXt")) return(format(v, "%Y-%m-%d %H:%M:%S"))
  as.character(v)
}

.rbind_or_empty <- function(rows, cols) {
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) {
    out <- as.data.frame(stats::setNames(replicate(length(cols), character(0), simplify = FALSE), cols),
                         stringsAsFactors = FALSE)
    return(out)
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# Files in a bundle, one row per table (parquet preferred over csv).
list_bundle_tables <- function(path) {
  layers <- c("raw", "mapped", "analytics", "reporting")
  snapshots <- sort(list.dirs(path, recursive = FALSE, full.names = FALSE))
  snapshots <- snapshots[vapply(snapshots, function(s) any(dir.exists(file.path(path, s, layers))), logical(1))]

  rows <- list()
  for (snap in snapshots) {
    for (layer in layers) {
      dir <- file.path(path, snap, layer)
      if (!dir.exists(dir)) next
      files <- list.files(dir, pattern = "[.](parquet|csv)$")
      if (!length(files)) next
      tbls <- unique(sub("[.](parquet|csv)$", "", files))
      for (tbl in sort(tbls)) {
        file <- if (paste0(tbl, ".parquet") %in% files) paste0(tbl, ".parquet") else paste0(tbl, ".csv")
        rows[[length(rows) + 1]] <- data.frame(
          snapshot = snap, layer = layer, table = tbl,
          file = file.path(snap, layer, file), stringsAsFactors = FALSE
        )
      }
    }
  }
  .rbind_or_empty(rows, c("snapshot", "layer", "table", "file"))
}

# Read one bundle table (parquet or csv) as a data frame.
read_bundle_table <- function(file) {
  if (grepl("[.]parquet$", file)) {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("Reading parquet bundle tables requires the arrow package.")
    }
    return(as.data.frame(arrow::read_parquet(file)))
  }
  utils::read.csv(file, stringsAsFactors = FALSE)
}

#' @export
print.testdata_summary <- function(x, ...) {
  m <- x$manifest
  cat("<testdata_summary> ", m$study_id, " v", m$bundle_version, "\n", sep = "")
  if (nrow(x$tables)) {
    agg <- stats::aggregate(rows ~ snapshot + layer, data = x$tables, FUN = sum)
    cnt <- stats::aggregate(table ~ snapshot + layer, data = x$tables, FUN = length)
    agg$tables <- cnt$table[match(paste(agg$snapshot, agg$layer), paste(cnt$snapshot, cnt$layer))]
    agg$layer <- factor(agg$layer, levels = c("raw", "mapped", "analytics", "reporting"))
    agg <- agg[order(agg$snapshot, agg$layer), c("snapshot", "layer", "tables", "rows")]
    rownames(agg) <- NULL
    print(agg, row.names = FALSE)
  } else {
    cat("  (no tables)\n")
  }
  invisible(x)
}
