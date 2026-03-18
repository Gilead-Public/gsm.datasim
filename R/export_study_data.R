#' Export Complete Study Data to Disk
#'
#' Writes a `longitudinal_study` object to a structured folder hierarchy.
#' Each snapshot date becomes a subdirectory containing up to four subfolders:
#'
#' ```
#' <output_dir>/<study_id>/
#'   <snapshot_date>/
#'     raw/          # Raw_*.parquet  (from study$raw_data)
#'     mapped/       # Mapped_*.parquet  (from study$analytics[[date]]$mapped)
#'     analytics/    # <metric>_<table>.parquet  (from study$analytics[[date]]$results)
#'     reporting/    # Reporting_*.parquet  (from study$reporting[[date]])
#' ```
#'
#' Folders are only created when the corresponding data actually exists.
#' Non-data.frame leaves inside `analytics` are silently skipped; they can be
#' preserved alongside the Parquet files by setting `save_rds = TRUE`, which writes a
#' companion `analytics_full.rds` per snapshot.
#'
#' @param study A `longitudinal_study` object (output of
#'   \code{\link{create_longitudinal_study}} or
#'   \code{\link{quick_longitudinal_study}}).
#' @param output_dir Root directory under which the study folder is created.
#'   Defaults to the current working directory.
#' @param study_folder Optional name for the top-level study folder.  Defaults
#'   to \code{study$study_id} with characters that are invalid in folder names
#'   replaced by underscores.
#' @param overwrite If \code{TRUE}, existing Parquet files are silently
#'   overwritten.  If \code{FALSE} (default), an error is raised when the
#'   target study folder already exists.
#' @param compression Compression codec passed to \code{arrow::write_parquet}.
#'   Defaults to \code{"snappy"}.
#' @param save_rds If \code{TRUE}, an \code{analytics_full.rds} file is also
#'   written per snapshot, preserving any non-data.frame objects (workflow
#'   lists, summaries, etc.) that cannot be expressed as flat Parquet files.
#'   Defaults to \code{FALSE}.
#' @param verbose If \code{TRUE}, prints progress messages.  Defaults to
#'   \code{FALSE}.
#'
#' @return Invisibly returns the path to the top-level study folder.
#' @export
export_study_data <- function(study,
                              output_dir    = ".",
                              study_folder  = NULL,
                              overwrite     = FALSE,
                              compression   = "snappy",
                              save_rds      = FALSE,
                              verbose       = FALSE) {

  # ── Input validation ────────────────────────────────────────────────────────

  if (!inherits(study, "longitudinal_study")) {
    stop("`study` must be a longitudinal_study object (output of create_longitudinal_study).")
  }
  if (!is.character(output_dir) || length(output_dir) != 1) {
    stop("`output_dir` must be a single character string.")
  }
  if (!is.character(compression) || length(compression) != 1 || nchar(compression) == 0) {
    stop("`compression` must be a single non-empty character string.")
  }

  .verbose <- isTRUE(verbose)

  # ── Build study root path ────────────────────────────────────────────────────

  if (is.null(study_folder)) {
    study_folder <- gsub("[^A-Za-z0-9._-]", "_", study$study_id)
  }
  study_root <- file.path(output_dir, study_folder)

  if (dir.exists(study_root) && !isTRUE(overwrite)) {
    stop(
      "Study folder already exists: ", study_root,
      "\nUse `overwrite = TRUE` to allow writing into an existing folder."
    )
  }

  dir.create(study_root, recursive = TRUE, showWarnings = FALSE)
  if (.verbose) message("Exporting study '", study$study_id, "' to: ", study_root)

  # ── Per-snapshot export ──────────────────────────────────────────────────────

  snapshot_names <- names(study$raw_data) %||% seq_along(study$raw_data)

  purrr::walk(snapshot_names, function(snap_name) {

    snap_dir <- file.path(study_root, snap_name)
    dir.create(snap_dir, recursive = TRUE, showWarnings = FALSE)
    if (.verbose) message("  Snapshot: ", snap_name)

    # -- raw/ ----------------------------------------------------------------
    raw_snap <- study$raw_data[[snap_name]]
    if (!is.null(raw_snap) && length(raw_snap) > 0) {
      raw_dir <- file.path(snap_dir, "raw")
      dir.create(raw_dir, showWarnings = FALSE)
      n_raw <- .write_df_list(raw_snap, raw_dir,
                              overwrite = overwrite,
                              compression = compression)
      if (.verbose) message("    raw/  (", n_raw, " datasets)")
    }

    # -- mapped/ and analytics/ (from study$analytics) ----------------------
    analytics_snap <- if (!is.null(study$analytics)) study$analytics[[snap_name]] else NULL

    if (!is.null(analytics_snap)) {

      # mapped/
      mapped <- analytics_snap$mapped
      if (!is.null(mapped) && length(mapped) > 0) {
        mapped_dfs <- Filter(is.data.frame, mapped)
        if (length(mapped_dfs) > 0) {
          mapped_dir <- file.path(snap_dir, "mapped")
          dir.create(mapped_dir, showWarnings = FALSE)
          n_mapped <- .write_df_list(mapped_dfs, mapped_dir,
                                     overwrite = overwrite,
                                     compression = compression)
          if (.verbose) message("    mapped/  (", n_mapped, " datasets)")
        }
      }

      # analytics/
      results <- analytics_snap$results
      if (!is.null(results) && length(results) > 0) {
        analytics_dir <- file.path(snap_dir, "analytics")
        dir.create(analytics_dir, showWarnings = FALSE)
        n_written <- .write_analytics_results(results, analytics_dir,
                                              overwrite = overwrite,
                                              compression = compression)
        if (.verbose) message("    analytics/  (", n_written, " tables)")
      }

      # Optional: full RDS for anything that isn't a data.frame
      if (isTRUE(save_rds)) {
        rds_path <- file.path(snap_dir, "analytics_full.rds")
        saveRDS(analytics_snap, rds_path)
        if (.verbose) message("    analytics_full.rds written")
      }
    }

    # -- reporting/ ----------------------------------------------------------
    reporting_snap <- if (!is.null(study$reporting)) study$reporting[[snap_name]] else NULL

    if (!is.null(reporting_snap) && length(reporting_snap) > 0) {
      reporting_dfs <- Filter(is.data.frame, reporting_snap)
      if (length(reporting_dfs) > 0) {
        reporting_dir <- file.path(snap_dir, "reporting")
        dir.create(reporting_dir, showWarnings = FALSE)
        n_reporting <- .write_df_list(reporting_dfs, reporting_dir,
                                      overwrite = overwrite,
                                      compression = compression)
        if (.verbose) message("    reporting/  (", n_reporting, " datasets)")
      }
    }
  })

  if (.verbose) message("Export complete: ", study_root)
  invisible(study_root)
}

# ── Internal helpers ──────────────────────────────────────────────────────────

# Write one data frame to Parquet, respecting overwrite behavior.
.write_df_parquet <- function(df, parquet_path, overwrite, compression) {
  if (file.exists(parquet_path) && !isTRUE(overwrite)) {
    warning("Skipping existing file (use overwrite = TRUE): ", parquet_path)
    return(FALSE)
  }

  arrow::write_parquet(df, parquet_path, compression = compression)
  TRUE
}

# Write a flat named list of data frames to <dir>/<name>.parquet
.write_df_list <- function(df_list, dir, overwrite, compression) {
  n_written <- purrr::imap_int(df_list, function(obj, nm) {
    if (!is.data.frame(obj)) return(0L)
    parquet_path <- file.path(dir, paste0(nm, ".parquet"))
    as.integer(.write_df_parquet(obj, parquet_path,
                                 overwrite = overwrite,
                                 compression = compression))
  })

  sum(n_written)
}

# Write analytics results to <analytics_dir>.
# Results is a named list where each entry is either:
#   - a data.frame (written directly as <metric>.parquet)
#   - a list of data.frames (written as <metric>_<table>.parquet)
#   - a deeper nested list (recursed one level; anything else is skipped)
# Returns the total count of Parquet files written.
.write_analytics_results <- function(results, dir, overwrite, compression) {
  n_written <- purrr::imap_int(results, function(metric, metric_name) {

    if (is.data.frame(metric)) {
      # Top-level data frame — write directly
      parquet_path <- file.path(dir, paste0(metric_name, ".parquet"))
      return(as.integer(.write_df_parquet(metric, parquet_path,
                                          overwrite = overwrite,
                                          compression = compression)))
    }

    if (is.list(metric)) {
      # Named list of tables inside one metric — e.g. kri0001$Analysis_Input
      metric_writes <- purrr::imap_int(metric, function(tbl, table_name) {
        if (!is.data.frame(tbl)) return(0L)
        safe_table <- gsub("[^A-Za-z0-9._-]", "_", table_name)
        parquet_path <- file.path(dir, paste0(metric_name, "_", safe_table, ".parquet"))
        as.integer(.write_df_parquet(tbl, parquet_path,
                                     overwrite = overwrite,
                                     compression = compression))
      })
      return(sum(metric_writes))
    }

    # Non-list, non-data.frame objects are silently skipped
    0L
  })

  sum(n_written)
}
