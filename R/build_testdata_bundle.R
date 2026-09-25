#' Build the shared gsm test data bundle
#'
#' Reproduces the test data that used to ship inside `gsm.core` (`lSource` and
#' the `reporting*` / `analytics*` objects) from a checked-in configuration,
#' running the mapping, metric, and reporting workflows through `workr`, and
#' writes the result as a versioned bundle:
#'
#' ```
#' <output_dir>/<study_id>/
#'   manifest.json
#'   <snapshot_date>/
#'     raw/          Raw_*.parquet / .csv
#'     mapped/       Mapped_*.parquet / .csv
#'     analytics/    Analysis_<metric>_<table>.parquet / .csv
#'     reporting/    Reporting_*.parquet / .csv
#' ```
#'
#' The two edits the original maintainer script applied silently are explicit,
#' configurable steps: `Raw_SITE$site_status` is forced to
#' `config$post_processing$site_status` on every snapshot, and `SnapshotDate`
#' on the reporting tables is stamped with `config$snapshot_dates`, which also
#' name the snapshot folders.
#'
#' @param config A `testdata_config`, see [read_testdata_config()].
#' @param output_dir Directory under which the `<study_id>` bundle folder is
#'   created.
#' @param overwrite If `TRUE`, an existing bundle folder is removed and rebuilt.
#' @param verbose Print progress and pipeline output.
#'
#' @return An object of class `testdata_bundle`: a list with `path` (the bundle
#'   folder), `manifest` (the parsed `manifest.json`), `config`, and `study`
#'   (the in-memory `longitudinal_study` that was exported).
#' @seealso [read_testdata_bundle()], [summarize_testdata_bundle()],
#'   [explore_testdata()]
#' @examples
#' \dontrun{
#' # the production bundle (takes a while)
#' bundle <- build_testdata_bundle(output_dir = "~/gsm-testdata")
#'
#' # a reduced bundle for local work
#' cfg <- read_testdata_config(
#'   participants = 40, sites = 4, snapshots = 2,
#'   snapshot_dates = c("2025-02-01", "2025-03-01")
#' )
#' bundle <- build_testdata_bundle(cfg, output_dir = tempdir(), overwrite = TRUE)
#' bundle$manifest$tables
#' }
#' @export
build_testdata_bundle <- function(config = read_testdata_config(),
                                  output_dir = tempdir(),
                                  overwrite = FALSE,
                                  verbose = FALSE) {
  if (!inherits(config, "testdata_config")) {
    stop("`config` must be a `testdata_config`; see read_testdata_config().")
  }
  if (!is.character(output_dir) || length(output_dir) != 1) {
    stop("`output_dir` must be a single directory path.")
  }
  .check_pipeline_packages(config)

  vcat <- function(...) if (isTRUE(verbose)) message(...)
  quietly <- function(expr) if (isTRUE(verbose)) expr else suppressMessages(expr)

  study_folder <- gsub("[^A-Za-z0-9._-]", "_", config$study_id)
  bundle_root <- file.path(output_dir, study_folder)
  if (dir.exists(bundle_root)) {
    if (!isTRUE(overwrite)) {
      stop(
        "Bundle folder already exists: ", bundle_root,
        "\nUse `overwrite = TRUE` to rebuild it."
      )
    }
    unlink(bundle_root, recursive = TRUE)
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # Quiet the package logger unless asked for progress; restore on exit.
  if (!isTRUE(verbose)) {
    old_threshold <- logger::log_threshold()
    withr::defer(logger::log_threshold(old_threshold))
    logger::log_threshold(logger::WARN)
  }

  set.seed(config$seed)

  # -- 1. raw layer -----------------------------------------------------------
  vcat("Generating ", config$snapshots, " snapshot(s) of raw data ...")
  raw <- quietly(generate_study_snapshots(
    study_id = config$study_id,
    participants = config$participants,
    sites = config$sites,
    snapshots = config$snapshots,
    interval = config$interval,
    mappings = ensure_core_mappings(config$domains),
    base_date = config$start_date,
    verbose = verbose
  ))
  raw <- apply_site_status(raw, config$post_processing$site_status)

  study <- create_longitudinal_study_data(
    study_id = config$study_id,
    raw_data = raw,
    config = list(
      participants = config$participants,
      sites = config$sites,
      snapshots = config$snapshots,
      interval = config$interval,
      domains = config$domains,
      study_type = "standard",
      analytics_package = config$analytics_packages,
      analytics_workflows = NULL,
      verbose = verbose
    )
  )

  # -- 2. mapped + analytics layers ------------------------------------------
  vcat("Running mapping and metric workflows (", paste(config$analytics_packages, collapse = ", "), ") ...")
  study <- with_pipeline_search_path(
    config$attach_packages,
    quietly(run_longitudinal_analytics(study, verbose = verbose))
  )
  if (is.null(study$analytics)) {
    stop("The analytics pipeline produced no results; the bundle would have no mapped, analytics, or reporting layer.")
  }

  # -- 3. reporting layer -----------------------------------------------------
  vcat("Running reporting workflows (", config$reporting_package, ") ...")
  study <- with_pipeline_search_path(
    config$attach_packages,
    quietly(run_longitudinal_reporting(study, verbose = verbose))
  )
  study <- stamp_snapshot_dates(study, config$snapshot_dates)

  # -- 4. export + manifest ---------------------------------------------------
  vcat("Exporting bundle to ", bundle_root, " ...")
  quietly(export_study_data(
    study,
    output_dir = output_dir,
    study_folder = study_folder,
    format = config$format,
    overwrite = TRUE,
    save_rds = FALSE,
    verbose = verbose
  ))
  manifest <- write_testdata_manifest(bundle_root, config, study)
  vcat("Done: ", nrow(manifest$files), " files, ", nrow(manifest$tables), " tables.")

  structure(
    list(
      path = normalizePath(bundle_root),
      manifest = manifest,
      config = config,
      study = study
    ),
    class = c("testdata_bundle", "list")
  )
}

# -- post-processing -----------------------------------------------------------

# Force Raw_SITE$site_status to `value` on every snapshot (the original script
# did this by hand for all three snapshots). NULL disables the step.
apply_site_status <- function(raw, value) {
  if (is.null(value)) {
    return(raw)
  }
  for (i in seq_along(raw)) {
    site <- raw[[i]]$Raw_SITE
    if (is.data.frame(site) && "site_status" %in% names(site)) {
      site$site_status <- rep(as.character(value), nrow(site))
      raw[[i]]$Raw_SITE <- site
    }
  }
  raw
}

# Rename the engine-generated snapshot keys to the configured snapshot dates
# and stamp those dates on every reporting table that carries SnapshotDate.
stamp_snapshot_dates <- function(study, snapshot_dates) {
  old <- names(study$raw_data)
  new <- as.character(snapshot_dates)
  if (length(old) != length(new)) {
    stop("Expected ", length(new), " generated snapshots but found ", length(old), ".")
  }
  rename <- function(x) {
    if (is.null(x)) {
      return(x)
    }
    idx <- match(names(x), old)
    names(x) <- new[idx]
    x
  }
  study$raw_data <- rename(study$raw_data)
  study$analytics <- rename(study$analytics)
  study$reporting <- rename(study$reporting)

  for (snap in names(study$reporting)) {
    date <- as.Date(snap)
    tables <- study$reporting[[snap]]
    for (nm in names(tables)) {
      tbl <- tables[[nm]]
      if (is.data.frame(tbl) && "SnapshotDate" %in% names(tbl)) {
        tbl$SnapshotDate <- rep(date, nrow(tbl))
        tables[[nm]] <- tbl
      }
    }
    study$reporting[[snap]] <- tables
  }
  study
}

# Temporarily attach packages whose functions the workflows reference
# unqualified (workr resolves bare names through the search path).
with_pipeline_search_path <- function(packages, code) {
  packages <- packages[vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  packages <- packages[!paste0("package:", packages) %in% search()]
  run <- function(i) {
    if (i > length(packages)) {
      return(force(code))
    }
    withr::with_package(packages[[i]], run(i + 1), character.only = TRUE)
  }
  run(1)
}

.check_pipeline_packages <- function(config) {
  needed <- unique(c("workr", config$mapping_package, config$analytics_packages, config$reporting_package))
  if (config$format %in% c("parquet", "both")) needed <- c(needed, "arrow")
  missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop(
      "Building the test data bundle requires package(s) that are not installed: ",
      paste(missing, collapse = ", "), "."
    )
  }
  invisible(TRUE)
}

# -- manifest ------------------------------------------------------------------

# Write manifest.json for an exported bundle and return it as read back from disk.
write_testdata_manifest <- function(bundle_root, config, study) {
  files <- list.files(bundle_root, recursive = TRUE, pattern = "[.](csv|parquet)$")
  files <- sort(files)
  files_df <- data.frame(
    path = files,
    bytes = as.numeric(file.size(file.path(bundle_root, files))),
    sha256 = vapply(
      file.path(bundle_root, files),
      function(f) digest::digest(f, algo = "sha256", file = TRUE),
      character(1),
      USE.NAMES = FALSE
    ),
    stringsAsFactors = FALSE
  )

  pkgs <- unique(c(
    "gsm.datasim", config$mapping_package, config$analytics_packages,
    config$reporting_package, config$attach_packages, "workr", "arrow"
  ))
  versions <- lapply(pkgs, function(p) {
    if (requireNamespace(p, quietly = TRUE)) as.character(utils::packageVersion(p)) else NA_character_
  })
  names(versions) <- pkgs
  versions$R <- paste(R.version$major, R.version$minor, sep = ".")

  repo <- Sys.getenv("GITHUB_REPOSITORY", unset = "")
  sha <- Sys.getenv("GITHUB_SHA", unset = "")
  run_id <- Sys.getenv("GITHUB_RUN_ID", unset = "")
  built_by <- if (nzchar(repo)) paste0(repo, "@", substr(sha, 1, 12)) else "local"
  workflow_run <- if (nzchar(repo) && nzchar(run_id)) {
    sprintf("%s/%s/actions/runs/%s", Sys.getenv("GITHUB_SERVER_URL", unset = "https://github.com"), repo, run_id)
  } else {
    NULL
  }

  manifest <- list(
    bundle_version = config$bundle_version,
    built_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    built_by = built_by,
    workflow_run = workflow_run,
    seed = config$seed,
    study_id = config$study_id,
    participants = config$participants,
    sites = config$sites,
    snapshots = as.character(config$snapshot_dates),
    interval = config$interval,
    domains = config$domains,
    mapping_package = config$mapping_package,
    analytics_packages = config$analytics_packages,
    reporting_package = config$reporting_package,
    format = config$format,
    package_versions = versions,
    package_refs = config$package_refs,
    tables = bundle_table_inventory(study),
    files = files_df
  )

  jsonlite::write_json(
    manifest,
    file.path(bundle_root, "manifest.json"),
    auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null", digits = NA,
    dataframe = "rows"
  )
  read_testdata_manifest(bundle_root)
}

# One row per exported table: snapshot, layer, table, rows, cols. Mirrors the
# file naming used by export_study_data().
bundle_table_inventory <- function(study) {
  rows <- list()
  add <- function(snapshot, layer, table, df) {
    rows[[length(rows) + 1]] <<- data.frame(
      snapshot = snapshot, layer = layer, table = table,
      rows = nrow(df), cols = ncol(df), stringsAsFactors = FALSE
    )
  }
  for (snap in names(study$raw_data)) {
    for (nm in names(study$raw_data[[snap]])) {
      df <- study$raw_data[[snap]][[nm]]
      if (is.data.frame(df)) add(snap, "raw", nm, df)
    }
    a <- study$analytics[[snap]]
    for (nm in names(a$mapped)) {
      if (is.data.frame(a$mapped[[nm]])) add(snap, "mapped", nm, a$mapped[[nm]])
    }
    for (metric in names(a$results)) {
      res <- a$results[[metric]]
      if (is.data.frame(res)) {
        add(snap, "analytics", metric, res)
      } else if (is.list(res)) {
        for (tbl in names(res)) {
          if (is.data.frame(res[[tbl]])) {
            add(snap, "analytics", paste0(metric, "_", gsub("[^A-Za-z0-9._-]", "_", tbl)), res[[tbl]])
          }
        }
      }
    }
    r <- study$reporting[[snap]]
    for (nm in names(r)) {
      if (is.data.frame(r[[nm]])) add(snap, "reporting", nm, r[[nm]])
    }
  }
  if (!length(rows)) {
    return(data.frame(snapshot = character(), layer = character(), table = character(),
                      rows = integer(), cols = integer(), stringsAsFactors = FALSE))
  }
  do.call(rbind, rows)
}

#' Read a bundle manifest
#'
#' @param path A bundle folder (containing `manifest.json`) or the manifest
#'   file itself.
#' @return The parsed manifest as a list; `tables` and `files` are data frames.
#' @examples
#' \dontrun{
#' m <- read_testdata_manifest("~/gsm-testdata/AA-AA-000-0000")
#' m$bundle_version
#' }
#' @export
read_testdata_manifest <- function(path) {
  file <- if (dir.exists(path)) file.path(path, "manifest.json") else path
  if (!file.exists(file)) {
    stop("No manifest.json found at: ", path)
  }
  jsonlite::fromJSON(file, simplifyDataFrame = TRUE)
}

#' Read a test data bundle from disk
#'
#' Rehydrates a bundle written by [build_testdata_bundle()] (or downloaded from
#' a release) so it can be summarised or explored. The data itself is read
#' lazily by the consuming functions.
#'
#' @param path The bundle folder containing `manifest.json`.
#' @return An object of class `testdata_bundle` with `path` and `manifest`.
#' @examples
#' \dontrun{
#' bundle <- read_testdata_bundle("~/gsm-testdata/AA-AA-000-0000")
#' summarize_testdata_bundle(bundle)
#' }
#' @export
read_testdata_bundle <- function(path) {
  if (!is.character(path) || length(path) != 1) {
    stop("`path` must be a single bundle folder.")
  }
  manifest <- read_testdata_manifest(path)
  structure(
    list(path = normalizePath(path), manifest = manifest, config = NULL, study = NULL),
    class = c("testdata_bundle", "list")
  )
}

# Accept a bundle object or a bundle path.
as_testdata_bundle <- function(bundle) {
  if (inherits(bundle, "testdata_bundle")) {
    return(bundle)
  }
  if (is.character(bundle) && length(bundle) == 1) {
    return(read_testdata_bundle(bundle))
  }
  stop("`bundle` must be a `testdata_bundle` or the path to a bundle folder.")
}

#' @export
print.testdata_bundle <- function(x, ...) {
  m <- x$manifest
  cat("<testdata_bundle> ", m$study_id, " v", m$bundle_version, "\n", sep = "")
  cat("  path:      ", x$path, "\n", sep = "")
  cat("  built:     ", m$built_at, " by ", m$built_by, "\n", sep = "")
  cat("  snapshots: ", paste(m$snapshots, collapse = ", "), "\n", sep = "")
  if (is.data.frame(m$tables) && nrow(m$tables)) {
    per_layer <- table(factor(m$tables$layer, levels = c("raw", "mapped", "analytics", "reporting")))
    cat("  tables:    ", paste(sprintf("%s=%d", names(per_layer), as.integer(per_layer)), collapse = " "), "\n", sep = "")
  }
  if (is.data.frame(m$files)) {
    cat("  files:     ", nrow(m$files), " (", format(structure(sum(m$files$bytes), class = "object_size"), units = "auto"), ")\n", sep = "")
  }
  invisible(x)
}
