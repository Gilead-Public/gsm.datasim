#' Export Complete Study Data to Disk
#'
#' Writes a `longitudinal_study` object to a structured folder hierarchy.
#' Each snapshot date becomes a subdirectory containing up to four subfolders:
#'
#' ```
#' <output_dir>/<study_id>/
#'   <snapshot_date>/
#'     raw/          # Raw_*.csv  (from study$raw_data)
#'     mapped/       # Mapped_*.csv  (from study$analytics[[date]]$mapped)
#'     analytics/    # <metric>_<table>.csv  (from study$analytics[[date]]$results)
#'     reporting/    # Reporting_*.csv  (from study$reporting[[date]])
#' ```
#'
#' Folders are only created when the corresponding data actually exists.
#' Non-data.frame leaves inside `analytics` are silently skipped; they can be
#' preserved alongside the CSVs by setting `save_rds = TRUE`, which writes a
#' companion `analytics_full.rds` per snapshot.
#'
#' @param study A `longitudinal_study` object (output of
#'   \code{\link{create_longitudinal_study}} or
#'   \code{\link{quick_longitudinal_study}}), or a
#'   \code{multiple_longitudinal_studies} object (output of
#'   \code{\link{create_multiple_longitudinal_studies}}).  When a collection of
#'   studies is passed, each study is exported to its own subfolder under
#'   \code{output_dir}.
#' @param output_dir Root directory under which the study folder is created.
#'   Defaults to the current working directory.
#' @param study_folder Optional name for the top-level study folder.  Defaults
#'   to \code{study$study_id} with characters that are invalid in folder names
#'   replaced by underscores.
#' @param format Export format. Options are "csv" (default), "parquet", or "both".
#'   When "parquet" is used, the arrow package must be installed.
#' @param overwrite If \code{TRUE}, existing files are silently
#'   overwritten.  If \code{FALSE} (default), an error is raised when the
#'   target study folder already exists.
#' @param save_rds If \code{TRUE}, an \code{analytics_full.rds} file is also
#'   written per snapshot, preserving any non-data.frame objects (workflow
#'   lists, summaries, etc.) that cannot be expressed as flat files.
#'   Defaults to \code{FALSE}.
#' @param verbose If \code{TRUE}, prints progress messages.  Defaults to
#'   \code{FALSE}.
#'
#' @return Invisibly returns the path to the top-level study folder (for a
#'   single study) or a named list of paths (for multiple studies).
#' @examples
#' \dontrun{
#' study <- create_longitudinal_study(
#'   "STUDY-001", participants = 50, sites = 5, snapshots = 2
#' )
#' export_study_data(study, output_dir = tempdir(), overwrite = TRUE)
#'
#' # Export as parquet format
#' export_study_data(study, output_dir = tempdir(), format = "parquet", overwrite = TRUE)
#' }
#' @export
export_study_data <- function(study,
                              output_dir = ".",
                              study_folder = NULL,
                              format = "csv",
                              overwrite = FALSE,
                              save_rds = FALSE,
                              verbose = FALSE) {
  # ── Input validation ────────────────────────────────────────────────────────

  # Handle multiple_longitudinal_studies by exporting each study individually
  if (inherits(study, "multiple_longitudinal_studies")) {
    export_paths <- vector("list", length(study))
    names(export_paths) <- names(study)
    for (study_name in names(study)) {
      export_paths[[study_name]] <- export_study_data(
        study        = study[[study_name]],
        output_dir   = output_dir,
        study_folder = study_name,
        format       = format,
        overwrite    = overwrite,
        save_rds     = save_rds,
        verbose      = verbose
      )
    }
    return(invisible(export_paths))
  }

  if (!inherits(study, "longitudinal_study")) {
    stop("`study` must be a longitudinal_study or multiple_longitudinal_studies object.")
  }
  if (!is.character(output_dir) || length(output_dir) != 1) {
    stop("`output_dir` must be a single character string.")
  }
  if (!format %in% c("csv", "parquet", "both")) {
    stop("`format` must be one of: 'csv', 'parquet', 'both'")
  }
  if (format %in% c("parquet", "both") && !requireNamespace("arrow", quietly = TRUE)) {
    stop("The arrow package is required for parquet export. Install with: install.packages('arrow')")
  }

  vcat <- function(...) if (isTRUE(verbose)) message(...)

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
  vcat("Exporting study '", study$study_id, "' to: ", study_root, sep = "")

  # ── Per-snapshot export ──────────────────────────────────────────────────────

  snapshot_names <- names(study$raw_data) %||% seq_along(study$raw_data)

  for (snap_name in snapshot_names) {
    snap_dir <- file.path(study_root, snap_name)
    dir.create(snap_dir, recursive = TRUE, showWarnings = FALSE)
    vcat("  Snapshot: ", snap_name, sep = "")

    # -- raw/ ----------------------------------------------------------------
    raw_snap <- study$raw_data[[snap_name]]
    if (!is.null(raw_snap) && length(raw_snap) > 0) {
      raw_dir <- file.path(snap_dir, "raw")
      dir.create(raw_dir, showWarnings = FALSE)
      .write_df_list(raw_snap, raw_dir, format = format, overwrite = overwrite, verbose = verbose)
      vcat("    raw/  (", length(raw_snap), " datasets)", sep = "")
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
          .write_df_list(mapped_dfs, mapped_dir, format = format, overwrite = overwrite, verbose = verbose)
          vcat("    mapped/  (", length(mapped_dfs), " datasets)", sep = "")
        }
      }

      # analytics/
      results <- analytics_snap$results
      if (!is.null(results) && length(results) > 0) {
        analytics_dir <- file.path(snap_dir, "analytics")
        dir.create(analytics_dir, showWarnings = FALSE)
        n_written <- .write_analytics_results(results, analytics_dir,
          format = format, overwrite = overwrite, verbose = verbose
        )
        vcat("    analytics/  (", n_written, " tables)", sep = "")
      }

      # Optional: full RDS for anything that isn't a data.frame
      if (isTRUE(save_rds)) {
        rds_path <- file.path(snap_dir, "analytics_full.rds")
        saveRDS(analytics_snap, rds_path)
        vcat("    analytics_full.rds written", sep = "")
      }
    }

    # -- reporting/ ----------------------------------------------------------
    reporting_snap <- if (!is.null(study$reporting)) study$reporting[[snap_name]] else NULL

    if (!is.null(reporting_snap) && length(reporting_snap) > 0) {
      reporting_dfs <- Filter(is.data.frame, reporting_snap)
      if (length(reporting_dfs) > 0) {
        reporting_dir <- file.path(snap_dir, "reporting")
        dir.create(reporting_dir, showWarnings = FALSE)
        .write_df_list(reporting_dfs, reporting_dir, format = format, overwrite = overwrite, verbose = verbose)
        vcat("    reporting/  (", length(reporting_dfs), " datasets)", sep = "")
      }
    }
  }

  vcat("Export complete: ", study_root, sep = "")
  invisible(study_root)
}

# ── Internal helpers ──────────────────────────────────────────────────────────

# Write a flat named list of data frames to files in the specified format
.write_df_list <- function(df_list, dir, format = "csv", overwrite, verbose) {
  vcat <- function(...) if (isTRUE(verbose)) message(...)
  for (nm in names(df_list)) {
    obj <- df_list[[nm]]
    if (!is.data.frame(obj)) next

    if (format %in% c("csv", "both")) {
      csv_path <- file.path(dir, paste0(nm, ".csv"))
      if (file.exists(csv_path) && !isTRUE(overwrite)) {
        warning("Skipping existing file (use overwrite = TRUE): ", csv_path)
      } else {
        utils::write.csv(obj, csv_path, row.names = FALSE)
      }
    }

    if (format %in% c("parquet", "both")) {
      parquet_path <- file.path(dir, paste0(nm, ".parquet"))
      if (file.exists(parquet_path) && !isTRUE(overwrite)) {
        warning("Skipping existing file (use overwrite = TRUE): ", parquet_path)
      } else {
        arrow::write_parquet(obj, parquet_path)
      }
    }
  }
}

# Write analytics results to files in the specified format.
# Results is a named list where each entry is either:
#   - a data.frame (written directly as <metric>.csv/.parquet)
#   - a list of data.frames (written as <metric>_<table>.csv/.parquet)
#   - a deeper nested list (recursed one level; anything else is skipped)
# Returns the total count of files written.
.write_analytics_results <- function(results, dir, format = "csv", overwrite, verbose) {
  n_written <- 0L

  for (metric_name in names(results)) {
    metric <- results[[metric_name]]

    if (is.data.frame(metric)) {
      # Top-level data frame — write directly
      if (format %in% c("csv", "both")) {
        csv_path <- file.path(dir, paste0(metric_name, ".csv"))
        if (!file.exists(csv_path) || isTRUE(overwrite)) {
          utils::write.csv(metric, csv_path, row.names = FALSE)
          n_written <- n_written + 1L
        } else {
          warning("Skipping existing file (use overwrite = TRUE): ", csv_path)
        }
      }

      if (format %in% c("parquet", "both")) {
        parquet_path <- file.path(dir, paste0(metric_name, ".parquet"))
        if (!file.exists(parquet_path) || isTRUE(overwrite)) {
          arrow::write_parquet(metric, parquet_path)
          n_written <- n_written + 1L
        } else {
          warning("Skipping existing file (use overwrite = TRUE): ", parquet_path)
        }
      }
    } else if (is.list(metric)) {
      # Named list of tables inside one metric — e.g. kri0001$Analysis_Input
      for (table_name in names(metric)) {
        tbl <- metric[[table_name]]
        if (!is.data.frame(tbl)) next
        safe_table <- gsub("[^A-Za-z0-9._-]", "_", table_name)

        if (format %in% c("csv", "both")) {
          csv_path <- file.path(dir, paste0(metric_name, "_", safe_table, ".csv"))
          if (!file.exists(csv_path) || isTRUE(overwrite)) {
            utils::write.csv(tbl, csv_path, row.names = FALSE)
            n_written <- n_written + 1L
          } else {
            warning("Skipping existing file (use overwrite = TRUE): ", csv_path)
          }
        }

        if (format %in% c("parquet", "both")) {
          parquet_path <- file.path(dir, paste0(metric_name, "_", safe_table, ".parquet"))
          if (!file.exists(parquet_path) || isTRUE(overwrite)) {
            arrow::write_parquet(tbl, parquet_path)
            n_written <- n_written + 1L
          } else {
            warning("Skipping existing file (use overwrite = TRUE): ", parquet_path)
          }
        }
      }
    }
    # Non-list, non-data.frame objects are silently skipped
  }

  n_written
}
