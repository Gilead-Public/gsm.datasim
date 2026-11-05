#' Build the shared gsm test data bundle
#'
#' Generate shared example data and write it to disk with a `snapshot.json`
#' describing the build.
#'
#' The pipeline is:
#'
#' 1. Simulate raw data with [create_longitudinal_study()] using the config's
#'    seed and parameters.
#' 2. Set `Raw_SITE$site_status` to `"Active"` on every snapshot.
#' 3. Map each snapshot with `gsm.mapping::Ingest()` and the `gsm.mapping`
#'    mapping workflows (config `domains` plus `extra_mappings`).
#' 4. Run every `gsm.kri` metric workflow, including inactive ones (so the
#'    example reports show them), and the `gsm.qtl` metrics.
#' 5. Run the `gsm.reporting` workflows, then stamp `SnapshotDate` from
#'    `snapshot_dates` onto `Reporting_Results` and `Reporting_Bounds`.
#' 6. Combine reporting tables across snapshots and split them by group level.
#' 7. Write CSVs of the `kri0001` and `cou0001` analytics (`Analysis_Input` and
#'    `Analysis_Summary`) from the final snapshot and of the 12 `reporting*`
#'    tables.
#' 8. Write RDS files of `lSource` (the final raw snapshot) and of 14 tables
#'    read back from those CSVs.
#'
#' Rows are sorted on explicit keys so identical inputs always produce
#' byte-identical files.
#'
#' @section Layout:
#' ```
#' <output_dir>/
#'   snapshot.json
#'   analytics{Input,Summary}{,_country}.csv
#'   reporting{Results,Groups,Bounds,Metrics}{,_country,_study}.csv
#'   lSource.rds
#'   analytics{Input,Summary}.rds
#'   reporting{Results,Groups,Bounds,Metrics}{,_country,_study}.rds
#' ```
#'
#' @section Snapshot:
#' `snapshot.json` uses the generic "refresh" snapshot format (version 1),
#' written by `write_snapshot_json()`: `refresh_snapshot`, `key` (the release
#' tag), `created`, `objects` (the 15 `.rds` files with their sha256), and
#' `packages` (installed `version` and `remote_sha` of each pipeline package,
#' including gsm.core). gsm-specific details live under `metadata`: `channel`,
#' `fingerprint`, `simulation`, `built_by`, `workflow_run`, `r_version`, and
#' `size`.
#'
#' `fingerprint` is a SHA-256 hash over the per-file checksums of the bundle
#' files (excluding `snapshot.json`), so two builds with identical data share a
#' fingerprint regardless of build time or origin.
#'
#' @param output_dir Directory to write the bundle into. Created if needed.
#' @param config A config list or path to a YAML file; see
#'   [read_testdata_config()]. Defaults to the checked-in config.
#' @param overwrite If `FALSE` (default), error when `output_dir` exists and is
#'   not empty. If `TRUE`, replace `output_dir` and its contents. The bundle is
#'   built in [tempdir()] and copied into `output_dir` only once complete, so a
#'   failed build leaves the existing bundle untouched.
#' @param channel Release channel (lowercase letters and digits), e.g. `"dev"`
#'   or `"prod"`.
#' @param key Release tag recorded as the snapshot `key`. Defaults to
#'   `testdata-<channel>-<YYYYMMDD>T<HHMMSS>Z` (UTC) at build time; it must
#'   match the tag the bundle is published under.
#' @param allow_local If `FALSE` (default), error when any pipeline package
#'   lacks a `RemoteSha` (a local or uncommitted install). Set `TRUE` only for
#'   local builds that won't be published.
#'
#' @returns The snapshot description (a list), invisibly.
#'
#' @examples
#' \dontrun{
#' build_testdata_bundle("bundle")
#' }
#' @export
build_testdata_bundle <- function(
  output_dir,
  config = read_testdata_config(),
  overwrite = FALSE,
  channel = "dev",
  key = NULL,
  allow_local = FALSE
) {
  if (
    missing(output_dir) || !is.character(output_dir) || length(output_dir) != 1
  ) {
    cli::cli_abort("{.arg output_dir} must be a single directory path.")
  }
  if (!isTRUE(overwrite) && !isFALSE(overwrite)) {
    cli::cli_abort("{.arg overwrite} must be {.code TRUE} or {.code FALSE}.")
  }
  if (!rlang::is_string(channel) || !grepl("^[a-z0-9]+$", channel)) {
    cli::cli_abort(
      "{.arg channel} must be a single string of lowercase letters and digits."
    )
  }
  if (!is.null(key) && !rlang::is_string(key)) {
    cli::cli_abort("{.arg key} must be {.code NULL} or a single string.")
  }
  if (!isTRUE(allow_local) && !isFALSE(allow_local)) {
    cli::cli_abort("{.arg allow_local} must be {.code TRUE} or {.code FALSE}.")
  }
  rlang::check_installed(
    pkg = setdiff(.bundle_packages, "gsm.datasim"),
    reason = "to build the test data bundle."
  )
  config <- read_testdata_config(config)
  packages <- .check_package_versions(allow_local)
  .check_output_dir(output_dir, overwrite)
  # Build in tempdir() and only replace output_dir once the bundle is
  # complete, so any failure leaves the existing bundle intact.
  target <- output_dir
  output_dir <- tempfile("bundle-staging-")
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)

  snapshots <- .simulate_bundle_snapshots(config)
  workflows <- .bundle_workflows(config)

  pipeline <- lapply(seq_along(snapshots), function(i) {
    .run_bundle_pipeline(snapshots[[i]], workflows, config$snapshot_dates[[i]])
  })
  final <- length(pipeline)

  analyzed <- pipeline[[final]]$analyzed
  analytics <- list(
    analyticsSummary = analyzed$Analysis_kri0001$Analysis_Summary,
    analyticsInput = analyzed$Analysis_kri0001$Analysis_Input,
    analyticsSummary_country = analyzed$Analysis_cou0001$Analysis_Summary,
    analyticsInput_country = analyzed$Analysis_cou0001$Analysis_Input
  )
  combined <- .combine_reporting(lapply(pipeline, `[[`, "reporting"))
  .write_csvs(
    c(
      lapply(analytics, .sort_reporting, name = "analytics"),
      .split_reporting(combined)
    ),
    output_dir
  )

  data_objects <- c(
    list(lSource = snapshots[[final]]),
    lapply(
      stats::setNames(nm = .bundle_data_names),
      .read_bundle_csv,
      dir = output_dir
    )
  )
  .write_rds(data_objects, output_dir)

  args <- .build_snapshot_args(
    output_dir,
    config,
    channel,
    key,
    allow_local,
    packages
  )
  snapshot <- do.call(write_snapshot_json, args)
  .publish_bundle(output_dir, target)
  invisible(snapshot)
}

#' Check the bundle output directory
#'
#' @param output_dir Directory to write the bundle into.
#' @param overwrite If `FALSE`, error when `output_dir` is not empty.
#' @returns `output_dir`, invisibly.
#' @keywords internal
.check_output_dir <- function(output_dir, overwrite) {
  contents <- list.files(output_dir, all.files = TRUE, no.. = TRUE)
  if (length(contents) > 0 && !overwrite) {
    cli::cli_abort(c(
      "{.arg output_dir} is not empty: {.path {output_dir}}.",
      i = "Use {.code overwrite = TRUE} to replace its contents."
    ))
  }
  invisible(output_dir)
}

#' Replace the bundle output directory with a completed staging directory
#'
#' Copies (rather than renames, since `tempdir()` may be on another volume)
#' the staged files into `output_dir`. Existing contents are backed up to
#' `tempdir()` first and restored if the copy fails.
#'
#' @param staging Completed staging directory.
#' @param output_dir Final bundle directory.
#' @returns `output_dir`, invisibly.
#' @keywords internal
.publish_bundle <- function(staging, output_dir) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  contents <- function(dir) {
    list.files(dir, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  }
  copy_into <- function(from, to) {
    all(file.copy(from, to, recursive = TRUE, copy.date = TRUE))
  }
  old <- contents(output_dir)
  backup <- tempfile("bundle-backup-")
  dir.create(backup)
  on.exit(unlink(backup, recursive = TRUE), add = TRUE)
  if (length(old) && !copy_into(old, backup)) {
    cli::cli_abort("Could not back up the existing bundle at {.path {output_dir}}.")
  }
  unlink(old, recursive = TRUE)
  if (!copy_into(contents(staging), output_dir)) {
    unlink(contents(output_dir), recursive = TRUE)
    copy_into(contents(backup), output_dir)
    cli::cli_abort("Could not write the new bundle to {.path {output_dir}}.")
  }
  invisible(output_dir)
}

#' Read a test data bundle snapshot description
#'
#' Reads the `snapshot.json` written by [build_testdata_bundle()].
#'
#' @param path A bundle directory (`output_dir`) or a path to
#'   `snapshot.json`.
#' @returns The parsed snapshot list.
#' @export
read_testdata_snapshot <- function(path) {
  if (dir.exists(path)) {
    path <- file.path(path, "snapshot.json")
  }
  jsonlite::read_json(path, simplifyVector = TRUE)
}

#' Read one object from a test data bundle
#'
#' Looks `name` up in the bundle's `snapshot.json`, verifies its checksum, and
#' reads it: `.rds` files with [readRDS()], `.csv` files with the bundle's
#' standard column types (`GroupID` character, `SnapshotDate` Date).
#'
#' @param path A bundle directory.
#' @param name An `objects` key: an object name such as `"reportingResults"`,
#'   or a CSV file name such as `"reportingResults.csv"`.
#' @returns The object.
#' @export
read_testdata_object <- function(path, name) {
  snapshot <- read_testdata_snapshot(path)
  entry <- snapshot$objects[[name]]
  if (is.null(entry)) {
    cli::cli_abort("{.val {name}} is not in the snapshot at {.path {path}}.")
  }
  file <- file.path(path, entry$file)
  actual <- digest::digest(file = file, algo = "sha256")
  if (!identical(tolower(actual), tolower(entry$sha256))) {
    cli::cli_abort(c(
      "Checksum mismatch for {.path {file}}.",
      i = "Expected {.val {entry$sha256}}, got {.val {actual}}."
    ))
  }
  if (grepl("[.]csv$", entry$file)) {
    return(.read_bundle_csv(
      sub("[.]csv$", "", basename(entry$file)),
      dirname(file)
    ))
  }
  readRDS(file)
}

# ---- pipeline ---------------------------------------------------------------

.bundle_packages <- c(
  "gsm.datasim",
  "gsm.core",
  "gsm.mapping",
  "gsm.kri",
  "gsm.qtl",
  "gsm.reporting",
  "workr"
)

#' Simulate raw snapshots for the bundle
#'
#' Simulates raw data under the config seed without changing the caller's RNG
#' state, then marks every site Active.
#'
#' @param config A validated config from [read_testdata_config()].
#' @returns Unnamed list of raw-data snapshots.
#' @keywords internal
.simulate_bundle_snapshots <- function(config) {
  rlang::check_installed("withr", reason = "to simulate the test data bundle.")
  withr::local_seed(config$seed)
  study <- create_longitudinal_study(
    study_id = config$study_id,
    participants = config$participants,
    sites = config$sites,
    snapshots = config$snapshots,
    interval = config$interval,
    domains = config$domains,
    run_analytics = FALSE,
    run_reporting = FALSE
  )
  snapshots <- unname(study$raw_data)
  # mark every site Active so all sites appear in reports.
  lapply(snapshots, function(snap) {
    snap$Raw_SITE$site_status <- "Active"
    snap
  })
}

#' Assemble the bundle workflows
#'
#' Loads mapping, metric (including inactive gsm.kri metrics), and reporting
#' workflows, with step names namespace-qualified.
#'
#' @param config A validated config from [read_testdata_config()].
#' @returns List with `mappings`, `spec`, `metrics`, and `reporting`.
#' @keywords internal
.bundle_workflows <- function(config) {
  mappings <- workr::MakeWorkflowList(
    strNames = c(config$domains, config$extra_mappings),
    strPath = "workflow/1_mappings",
    strPackage = "gsm.mapping"
  )
  # Inactive metrics (e.g. kri0007 / cou0007) are included so the example
  # reports show them.
  metrics <- c(
    workr::MakeWorkflowList(
      strPath = "workflow/2_metrics",
      strPackage = "gsm.kri",
      bActiveOnly = FALSE
    ),
    workr::MakeWorkflowList(
      strPath = "workflow/2_metrics",
      strPackage = "gsm.qtl"
    )
  )
  list(
    mappings = .qualify_workflow_steps(mappings, "gsm.mapping"),
    spec = CombineSpecs(mappings),
    metrics = .qualify_workflow_steps(metrics, c("gsm.kri", "gsm.qtl")),
    reporting = .qualify_workflow_steps(
      workr::MakeWorkflowList(
        strPath = "workflow/3_reporting",
        strPackage = "gsm.reporting"
      ),
      "gsm.reporting"
    )
  )
}

#' Namespace-qualify workflow step names
#'
#' Workflow YAML may reference step functions without a namespace (e.g.
#' `Analyze_NormalApprox`), which workr resolves from the search path. Rather
#' than attaching packages, each bare name is resolved through the namespaces
#' of `packages` and rewritten as `owner::fn`.
#'
#' @param workflows List of workflows.
#' @param packages Packages whose namespaces are searched for step functions.
#' @returns `workflows` with qualified step names.
#' @keywords internal
.qualify_workflow_steps <- function(workflows, packages) {
  qualify <- function(name) {
    if (grepl("::", name, fixed = TRUE)) {
      return(name)
    }
    for (pkg in packages) {
      ns <- asNamespace(pkg)
      if (!exists(name, envir = ns, mode = "function")) {
        next
      }
      fn_env <- environment(get(name, envir = ns, mode = "function"))
      owner <- if (is.null(fn_env)) "base" else environmentName(fn_env)
      if (owner %in% c("base", "R_GlobalEnv", "")) {
        return(name)
      }
      return(paste0(owner, "::", name))
    }
    name
  }
  lapply(workflows, function(wf) {
    wf$steps <- lapply(wf$steps, function(step) {
      step$name <- qualify(step$name)
      step
    })
    wf
  })
}

#' Run the bundle pipeline for one snapshot
#'
#' Ingests raw data, then runs mapping, metric, and reporting workflows and
#' stamps `SnapshotDate` on reporting Results and Bounds.
#'
#' @param raw A raw-data snapshot.
#' @param workflows Output of `.bundle_workflows()`.
#' @param snapshot_date Snapshot `Date`.
#' @returns List with `mapped`, `analyzed`, and `reporting`.
#' @keywords internal
.run_bundle_pipeline <- function(raw, workflows, snapshot_date) {
  lRaw <- gsm.mapping::Ingest(raw, workflows$spec)
  mapped <- workr::RunWorkflows(workflows$mappings, lRaw)
  analyzed <- workr::RunWorkflows(
    workflows$metrics,
    c(mapped, list(lWorkflows = workflows$metrics))
  )
  reporting <- workr::RunWorkflows(
    workflows$reporting,
    c(mapped, list(lAnalyzed = analyzed, lWorkflows = workflows$metrics))
  )
  reporting$Reporting_Results$SnapshotDate <- snapshot_date
  reporting$Reporting_Bounds$SnapshotDate <- snapshot_date
  list(mapped = mapped, analyzed = analyzed, reporting = reporting)
}

#' Combine reporting tables across snapshots
#'
#' Results and Bounds accumulate across snapshots; Groups and Metrics describe
#' the final snapshot.
#'
#' @param reporting List of per-snapshot reporting outputs.
#' @returns List with the four combined `Reporting_*` tables.
#' @keywords internal
.combine_reporting <- function(reporting) {
  final <- reporting[[length(reporting)]]
  list(
    Reporting_Results = dplyr::bind_rows(lapply(
      reporting,
      `[[`,
      "Reporting_Results"
    )),
    Reporting_Groups = final$Reporting_Groups,
    Reporting_Bounds = dplyr::bind_rows(lapply(
      reporting,
      `[[`,
      "Reporting_Bounds"
    )),
    Reporting_Metrics = final$Reporting_Metrics
  )
}

#' Split combined reporting tables by group level
#'
#' Results and Metrics split by `GroupLevel`; Groups keep Study rows plus the
#' rows for each level; Bounds split by `MetricID` prefix (`Analysis_kri`,
#' `Analysis_cou`, `Analysis_qtl`). Type rules are applied (`GroupID` character,
#' `SnapshotDate` Date) and each table is sorted with `.sort_reporting()`.
#'
#' @param combined List with `Reporting_Results`, `Reporting_Groups`,
#'   `Reporting_Bounds`, and `Reporting_Metrics`.
#' @returns Named list of the 12 `reporting*` objects.
#' @keywords internal
.split_reporting <- function(combined) {
  levels <- list(
    list(suffix = "", level = "Site", prefix = "Analysis_kri"),
    list(suffix = "_country", level = "Country", prefix = "Analysis_cou"),
    list(suffix = "_study", level = "Study", prefix = "Analysis_qtl")
  )
  out <- list()
  for (lv in levels) {
    tables <- list(
      reportingResults = combined$Reporting_Results[
        combined$Reporting_Results$GroupLevel == lv$level,
        ,
        drop = FALSE
      ],
      reportingGroups = combined$Reporting_Groups[
        combined$Reporting_Groups$GroupLevel %in% unique(c("Study", lv$level)),
        ,
        drop = FALSE
      ],
      reportingBounds = combined$Reporting_Bounds[
        startsWith(combined$Reporting_Bounds$MetricID, lv$prefix),
        ,
        drop = FALSE
      ],
      reportingMetrics = combined$Reporting_Metrics[
        combined$Reporting_Metrics$GroupLevel == lv$level,
        ,
        drop = FALSE
      ]
    )
    names(tables) <- paste0(names(tables), lv$suffix)
    for (nm in names(tables)) {
      out[[nm]] <- .sort_reporting(.apply_reporting_types(tables[[nm]]), nm)
    }
  }
  out
}

#' Apply reporting column types
#'
#' Coerces `GroupID` to character and `SnapshotDate` to `Date` when present.
#'
#' @param x A data frame.
#' @returns `x` with coerced types and reset row names.
#' @keywords internal
.apply_reporting_types <- function(x) {
  if ("GroupID" %in% names(x)) {
    x$GroupID <- as.character(x$GroupID)
  }
  if ("SnapshotDate" %in% names(x)) {
    x$SnapshotDate <- as.Date(x$SnapshotDate)
  }
  rownames(x) <- NULL
  x
}

# ---- sorting ---------------

.study_param_order <- c(
  "studyid",
  "nickname",
  "protocol_title",
  "status",
  "num_plan_site",
  "num_plan_subj",
  "act_fpfv",
  "est_fpfv",
  "est_lplv",
  "est_lpfv",
  "db_lock_dt",
  "therapeutic_area",
  "protocol_indication",
  "phase",
  "product",
  "SiteTarget",
  "ParticipantTarget",
  "ParticipantCount",
  "SiteCount",
  "PercentSitesActivated",
  "SiteActivation",
  "PercentParticipantsEnrolled",
  "ParticipantEnrollment"
)

#' Rank MetricIDs naturally
#'
#' Orders e.g. kri0001 < kri0007 < kri0007-2 < kri0020a, with srs metrics last.
#'
#' @param id Character vector of MetricIDs.
#' @returns Integer rank for each element of `id`.
#' @keywords internal
.metric_rank <- function(id) {
  ids <- unique(id)
  core <- sub("^.*_", "", ids)
  prefix <- sub("[0-9].*$", "", core)
  num <- suppressWarnings(as.numeric(sub("^[^0-9]*([0-9]+).*$", "\\1", core)))
  suffix <- sub("^[^0-9]*[0-9]+", "", core)
  ord <- order(prefix == "srs", prefix, num, suffix, ids, method = "radix")
  match(id, ids[ord])
}

#' Sort a bundle table on explicit keys
#'
#' Sort a table so reruns never produce row-order-only diffs. Reporting
#' Metrics/Results/Bounds/Groups use curated keys (natural `MetricID` rank with
#' `srs` last, `SnapshotDate` descending, curated Study `Param` order); anything
#' else is sorted on every column.
#'
#' @param x A data frame.
#' @param name Table name, e.g. `"reportingResults_country"` or
#'   `"Reporting_Results"`.
#' @returns `x`, reordered.
#' @keywords internal
.sort_reporting <- function(x, name) {
  # "Reporting_Results" and "reportingResults_country" both map to "results".
  kind <- sub("^reporting_?([a-z]+).*$", "\\1", tolower(name))
  ord <- switch(
    kind,
    metrics = order(.metric_rank(x$MetricID), method = "radix"),
    results = order(
      .metric_rank(x$MetricID),
      -as.numeric(as.Date(x$SnapshotDate)),
      x$GroupLevel,
      x$GroupID,
      method = "radix"
    ),
    bounds = order(
      .metric_rank(x$MetricID),
      -as.numeric(as.Date(x$SnapshotDate)),
      x$Threshold,
      x$Denominator,
      method = "radix"
    ),
    groups = order(
      x$GroupLevel,
      x$GroupID,
      ifelse(
        x$GroupLevel == "Study",
        match(x$Param, .study_param_order),
        NA_integer_
      ),
      tolower(x$Param),
      x$Param,
      method = "radix"
    ),
    if (ncol(x) > 0) {
      do.call(order, c(unname(as.list(x)), method = "radix"))
    } else {
      seq_len(nrow(x))
    }
  )
  out <- x[ord, , drop = FALSE]
  rownames(out) <- NULL
  out
}

# ---- writing ----------------------------------------------------------------

# Tables read back from CSV and written as RDS.
.bundle_data_names <- c(
  "analyticsInput",
  "analyticsSummary",
  paste0(
    rep(
      c("reportingResults", "reportingGroups", "reportingBounds", "reportingMetrics"),
      3
    ),
    rep(c("", "_country", "_study"), each = 4)
  )
)

#' Write tables as CSV
#'
#' @param tables Named list of data frames.
#' @param output_dir Output directory.
#' @returns `output_dir`, invisibly.
#' @keywords internal
.write_csvs <- function(tables, output_dir) {
  for (nm in names(tables)) {
    df <- as.data.frame(tables[[nm]])
    rownames(df) <- NULL
    utils::write.csv(
      df,
      file.path(output_dir, paste0(nm, ".csv")),
      row.names = FALSE,
      fileEncoding = "UTF-8"
    )
  }
  invisible(output_dir)
}

#' Write objects as RDS
#'
#' @param objects Named list of R objects.
#' @param output_dir Output directory.
#' @returns `output_dir`, invisibly.
#' @keywords internal
.write_rds <- function(objects, output_dir) {
  for (nm in names(objects)) {
    saveRDS(objects[[nm]], file.path(output_dir, paste0(nm, ".rds")), version = 3)
  }
  invisible(output_dir)
}

#' Read a bundle CSV back with standard column types
#'
#' `GroupID` is read as character wherever present, so IDs such as `"001"`
#' keep leading zeros; `SnapshotDate` is converted to `Date`. Other columns
#' keep [utils::read.csv()] types.
#'
#' @param name Table name, e.g. `"reportingResults_country"`.
#' @param dir Directory containing `<name>.csv`.
#' @returns A data frame.
#' @keywords internal
.read_bundle_csv <- function(name, dir) {
  path <- file.path(dir, paste0(name, ".csv"))
  header <- names(utils::read.csv(path, nrows = 0, check.names = FALSE))
  col_classes <- if ("GroupID" %in% header) c(GroupID = "character") else NA
  x <- utils::read.csv(path, colClasses = col_classes)
  if ("SnapshotDate" %in% names(x)) {
    x$SnapshotDate <- as.Date(x$SnapshotDate)
  }
  x
}

# ---- snapshot ---------------------------------------------------------------

#' Build the snapshot.json contents
#'
#' Collects the release key, objects, package versions, and gsm-specific
#' metadata for `write_snapshot_json()`. The fingerprint is a sha256 over the
#' per-file checksums of every bundle file except `snapshot.json`, so identical
#' data yields an identical fingerprint regardless of build metadata.
#'
#' @param output_dir Bundle directory.
#' @param config A validated config from [read_testdata_config()].
#' @param channel Release channel.
#' @param key Release tag, or `NULL` to generate one.
#' @param allow_local Allow packages without a `RemoteSha`.
#' @param packages Package versions from [.check_package_versions()].
#' @returns List of arguments for `write_snapshot_json()`.
#' @keywords internal
.build_snapshot_args <- function(
  output_dir,
  config,
  channel,
  key,
  allow_local,
  packages = .check_package_versions(allow_local)
) {
  files <- list.files(output_dir, recursive = TRUE)
  # Radix ordering is locale-independent, keeping the fingerprint stable.
  files <- files[order(files, method = "radix")]
  files <- files[files != "snapshot.json"]
  sha <- vapply(
    file.path(output_dir, files),
    function(f) digest::digest(file = f, algo = "sha256"),
    character(1),
    USE.NAMES = FALSE
  )
  fingerprint <- digest::digest(
    paste(files, sha, sep = "  ", collapse = "
"),
    algo = "sha256",
    serialize = FALSE
  )
  sim <- config
  sim$snapshot_dates <- format(sim$snapshot_dates)

  rds <- c("lSource", .bundle_data_names)
  csv <- files[grepl("[.]csv$", files)]
  list(
    dir = output_dir,
    # RDS objects are keyed by name; CSVs by file name so keys never collide.
    objects = c(stats::setNames(paste0(rds, ".rds"), rds), stats::setNames(csv, csv)),
    key = key %||% .testdata_key(channel),
    packages = packages,
    metadata = list(
      channel = channel,
      fingerprint = fingerprint,
      simulation = sim,
      built_by = .build_origin(),
      workflow_run = .workflow_run_url(),
      r_version = paste(R.version$major, R.version$minor, sep = "."),
      size = list(
        files = length(files),
        bytes = sum(file.size(file.path(output_dir, files)))
      )
    )
  )
}

#' Generate a release tag
#'
#' @param channel Release channel.
#' @param time Build time.
#' @returns `"testdata-<channel>-<YYYYMMDD>T<HHMMSS>Z"` (UTC).
#' @keywords internal
.testdata_key <- function(channel, time = Sys.time()) {
  paste0("testdata-", channel, "-", format(time, "%Y%m%dT%H%M%SZ", tz = "UTC"))
}

#' Identify the build origin
#'
#' @returns `"<GITHUB_REPOSITORY>@<GITHUB_SHA>"` when run in GitHub Actions,
#'   otherwise `NULL`.
#' @keywords internal
.build_origin <- function() {
  repo <- Sys.getenv("GITHUB_REPOSITORY")
  sha <- Sys.getenv("GITHUB_SHA")
  if (nzchar(repo) && nzchar(sha)) {
    return(paste0(repo, "@", sha))
  }
  NULL
}

#' Get the GitHub Actions run URL
#'
#' @returns The workflow run URL when run in GitHub Actions, otherwise `NULL`.
#' @keywords internal
.workflow_run_url <- function() {
  server <- Sys.getenv("GITHUB_SERVER_URL")
  repo <- Sys.getenv("GITHUB_REPOSITORY")
  run <- Sys.getenv("GITHUB_RUN_ID")
  if (nzchar(server) && nzchar(repo) && nzchar(run)) {
    return(paste0(server, "/", repo, "/actions/runs/", run))
  }
  NULL
}

#' Read an installed package's DESCRIPTION
#'
#' Thin wrapper around [utils::packageDescription()] so installed metadata can
#' be mocked in tests.
#'
#' @param pkg Package name.
#' @returns A `packageDescription` object.
#' @keywords internal
.pkg_description <- function(pkg) utils::packageDescription(pkg)

#' Record pipeline package versions, rejecting local installs
#'
#' @param allow_local Allow packages without a `RemoteSha`.
#' @returns Named list from [.installed_package_versions()].
#' @keywords internal
.check_package_versions <- function(allow_local) {
  packages <- .installed_package_versions(.bundle_packages)
  local <- names(packages)[vapply(
    packages,
    function(p) is.null(p$remote_sha),
    logical(1)
  )]
  if (length(local) && !allow_local) {
    cli::cli_abort(c(
      "No {.field RemoteSha} for {.pkg {local}}.",
      i = "Install from GitHub to publish, or use {.code allow_local = TRUE}."
    ))
  }
  packages
}

#' Record installed package versions
#'
#' @param packages Package names.
#' @returns Named list of `version` and `remote_sha` (`NULL` when unknown) per
#'   package.
#' @keywords internal
.installed_package_versions <- function(packages) {
  out <- lapply(packages, function(pkg) {
    desc <- .pkg_description(pkg)
    list(version = desc$Version, remote_sha = desc$RemoteSha %||% NULL)
  })
  names(out) <- packages
  out
}
