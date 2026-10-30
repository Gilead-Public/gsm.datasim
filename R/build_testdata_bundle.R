#' Build the shared gsm test data bundle
#'
#' Generate shared example data and write it to disk with a `manifest.json`
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
#'   manifest.json
#'   analytics{Input,Summary}{,_country}.csv
#'   reporting{Results,Groups,Bounds,Metrics}{,_country,_study}.csv
#'   lSource.rds
#'   analytics{Input,Summary}.rds
#'   reporting{Results,Groups,Bounds,Metrics}{,_country,_study}.rds
#' ```
#'
#' @section Manifest:
#' `fingerprint` is a SHA-256 hash over the per-file checksums of the data files
#' only, so two builds with identical data share a fingerprint regardless of
#' `built_at`, `built_by`, or `workflow_run`. `package_versions` records the
#' *installed* version (and remote SHA, when available) of each pipeline
#' package.
#'
#' @param output_dir Directory to write the bundle into. Created if needed.
#' @param config A config list or path to a YAML file; see
#'   [read_testdata_config()]. Defaults to the checked-in config.
#' @param overwrite If `FALSE` (default), error when `output_dir` exists and is
#'   not empty. If `TRUE`, delete `output_dir` and its contents first.
#'
#' @returns The manifest (a list), invisibly.
#'
#' @examples
#' \dontrun{
#' build_testdata_bundle("bundle")
#' }
#' @export
build_testdata_bundle <- function(
  output_dir,
  config = read_testdata_config(),
  overwrite = FALSE
) {
  if (
    missing(output_dir) || !is.character(output_dir) || length(output_dir) != 1
  ) {
    stop("`output_dir` must be a single directory path.")
  }
  if (!isTRUE(overwrite) && !isFALSE(overwrite)) {
    stop("`overwrite` must be `TRUE` or `FALSE`.")
  }
  rlang::check_installed(
    pkg = setdiff(.bundle_packages, "gsm.datasim"),
    reason = "to build the test data bundle."
  )
  config <- read_testdata_config(config)
  .prepare_output_dir(output_dir, overwrite)

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

  manifest <- .build_manifest(output_dir, config)
  .write_manifest(manifest, file.path(output_dir, "manifest.json"))
  invisible(manifest)
}

#' Prepare the bundle output directory
#'
#' @param output_dir Directory to write the bundle into.
#' @param overwrite If `TRUE`, delete a non-empty `output_dir` first;
#'   otherwise error when it is not empty.
#' @returns `output_dir`, invisibly.
#' @keywords internal
.prepare_output_dir <- function(output_dir, overwrite) {
  contents <- list.files(output_dir, all.files = TRUE, no.. = TRUE)
  if (length(contents) > 0) {
    if (!overwrite) {
      stop(
        "`output_dir` is not empty: ",
        output_dir,
        ". Use `overwrite = TRUE` to replace its contents."
      )
    }
    unlink(output_dir, recursive = TRUE)
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  invisible(output_dir)
}

#' Read a test data bundle manifest
#'
#' Reads the `manifest.json` written by [build_testdata_bundle()], which records
#' the bundle fingerprint, simulation config, package versions, and per-file
#' checksums.
#'
#' @param path A bundle directory (`output_dir`) or a path to
#'   `manifest.json`.
#' @returns The parsed manifest list.
#' @export
read_testdata_manifest <- function(path) {
  if (dir.exists(path)) {
    path <- file.path(path, "manifest.json")
  }
  jsonlite::read_json(path, simplifyVector = TRUE)
}

# ---- pipeline ---------------------------------------------------------------

.bundle_packages <- c(
  "gsm.datasim",
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
#' `GroupID` is read as character for `analytics*` and `reportingResults`, and
#' converted to character for the other `reportingResults*` tables;
#' `SnapshotDate` is converted to `Date`. Other columns keep [utils::read.csv()]
#' types.
#'
#' @param name Table name, e.g. `"reportingResults_country"`.
#' @param dir Directory containing `<name>.csv`.
#' @returns A data frame.
#' @keywords internal
.read_bundle_csv <- function(name, dir) {
  col_classes <- if (name %in% c("analyticsInput", "analyticsSummary", "reportingResults")) {
    c(GroupID = "character")
  } else {
    NA
  }
  x <- utils::read.csv(
    file.path(dir, paste0(name, ".csv")),
    colClasses = col_classes
  )
  if (startsWith(name, "reportingResults")) {
    x$GroupID <- as.character(x$GroupID)
  }
  if ("SnapshotDate" %in% names(x)) {
    x$SnapshotDate <- as.Date(x$SnapshotDate)
  }
  x
}

# ---- manifest ---------------------------------------------------------------

#' Build the bundle manifest
#'
#' The fingerprint is a sha256 over per-file checksums of the data files only,
#' so identical data yields an identical fingerprint regardless of build
#' metadata.
#'
#' @param output_dir Bundle directory.
#' @param config A validated config from [read_testdata_config()].
#' @returns Manifest list.
#' @keywords internal
.build_manifest <- function(output_dir, config) {
  files <- sort(list.files(output_dir, recursive = TRUE))
  files <- files[files != "manifest.json"]
  sha <- vapply(
    file.path(output_dir, files),
    function(f) digest::digest(file = f, algo = "sha256"),
    character(1),
    USE.NAMES = FALSE
  )
  fingerprint <- digest::digest(
    paste(files, sha, sep = "  ", collapse = "\n"),
    algo = "sha256",
    serialize = FALSE
  )
  sim <- config
  sim$snapshot_dates <- format(sim$snapshot_dates)

  list(
    fingerprint = fingerprint,
    built_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    built_by = .build_origin(),
    workflow_run = .workflow_run_url(),
    simulation = sim,
    package_versions = .installed_package_versions(.bundle_packages),
    size = list(
      files = length(files),
      bytes = sum(file.size(file.path(output_dir, files)))
    ),
    checksums = list(sha256 = as.list(stats::setNames(sha, files)))
  )
}

#' Write a manifest as JSON
#'
#' @param manifest Manifest list from `.build_manifest()`.
#' @param path Output file path.
#' @keywords internal
.write_manifest <- function(manifest, path) {
  jsonlite::write_json(
    manifest,
    path,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null",
    digits = NA
  )
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

#' Record installed package versions
#'
#' @param packages Package names.
#' @returns Named list of `version` and `remote_sha` per package, plus `R`.
#' @keywords internal
.installed_package_versions <- function(packages) {
  out <- lapply(packages, function(pkg) {
    desc <- .pkg_description(pkg)
    list(version = desc$Version, remote_sha = desc$RemoteSha %||% NULL)
  })
  names(out) <- packages
  out$R <- list(version = paste(R.version$major, R.version$minor, sep = "."))
  out
}
