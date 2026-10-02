#' Locate the shipped test data bundle configuration
#'
#' Returns the path to `inst/testdata/bundle-config.yaml`, the configuration
#' that [build_testdata_bundle()] uses to reproduce the shared gsm test data.
#'
#' @return A file path.
#' @examples
#' testdata_config_path()
#' @export
testdata_config_path <- function() {
  path <- system.file("testdata", "bundle-config.yaml", package = "gsm.datasim")
  if (!nzchar(path)) {
    stop("bundle-config.yaml was not found in the installed gsm.datasim package.")
  }
  path
}

#' Read and validate a test data bundle configuration
#'
#' Reads a bundle configuration YAML (by default the one shipped with the
#' package, see [testdata_config_path()]), applies any overrides passed
#' through `...`, coerces types, and validates the result.
#'
#' @param path Path to a bundle configuration YAML file.
#' @param ... Named overrides for individual fields, e.g. `participants = 40`.
#'   Only fields present in the file can be overridden.
#'
#' @return A list of class `testdata_config` with (at least) `bundle_version`,
#'   `study_id`, `seed`, `participants`, `sites`, `snapshots`, `interval`,
#'   `start_date`, `snapshot_dates`, `domains`, `mapping_package`,
#'   `analytics_packages`, `reporting_package`, `attach_packages`,
#'   `post_processing`, `format`, and `package_refs`.
#' @examples
#' cfg <- read_testdata_config()
#' cfg$participants
#'
#' # a reduced configuration for local experiments
#' small <- read_testdata_config(
#'   participants = 40, sites = 4, snapshots = 2,
#'   snapshot_dates = c("2025-02-01", "2025-03-01")
#' )
#' @export
read_testdata_config <- function(path = testdata_config_path(), ...) {
  if (!is.character(path) || length(path) != 1 || !file.exists(path)) {
    stop("Config file not found: ", path)
  }
  cfg <- yaml::read_yaml(path)

  overrides <- list(...)
  if (length(overrides)) {
    if (is.null(names(overrides)) || any(!nzchar(names(overrides)))) {
      stop("Overrides passed through `...` must be named.")
    }
    unknown <- setdiff(names(overrides), names(cfg))
    if (length(unknown)) {
      stop("Unknown config field(s): ", paste(unknown, collapse = ", "))
    }
    cfg[names(overrides)] <- overrides
  }

  validate_testdata_config(cfg)
}

# Coerce and validate a raw config list; returns a `testdata_config`.
validate_testdata_config <- function(cfg) {
  required <- c(
    "bundle_version", "study_id", "seed", "participants", "sites", "snapshots",
    "interval", "start_date", "snapshot_dates", "domains", "mapping_package",
    "analytics_packages", "reporting_package", "format"
  )
  missing <- setdiff(required, names(cfg))
  if (length(missing)) {
    stop("Config is missing required field(s): ", paste(missing, collapse = ", "))
  }

  if (!is.character(cfg$bundle_version) || length(cfg$bundle_version) != 1 ||
    !grepl("^[0-9]+[.][0-9]+[.][0-9]+$", cfg$bundle_version)) {
    stop("`bundle_version` must be a semantic version such as \"1.0.0\", got: ", format(cfg$bundle_version))
  }
  if (!is.character(cfg$study_id) || length(cfg$study_id) != 1 || !nzchar(cfg$study_id)) {
    stop("`study_id` must be a single non-empty string.")
  }

  cfg$seed <- .as_count(cfg$seed, "seed", min = 0L)
  cfg$participants <- .as_count(cfg$participants, "participants")
  cfg$sites <- .as_count(cfg$sites, "sites")
  cfg$snapshots <- .as_count(cfg$snapshots, "snapshots")

  if (!is.character(cfg$interval) || length(cfg$interval) != 1) {
    stop("`interval` must be a single string such as \"1 month\".")
  }

  cfg$start_date <- .as_date(cfg$start_date, "start_date")
  cfg$snapshot_dates <- .as_date(unlist(cfg$snapshot_dates), "snapshot_dates")
  if (length(cfg$snapshot_dates) != cfg$snapshots) {
    stop(
      "`snapshot_dates` must have one entry per snapshot: expected ", cfg$snapshots,
      ", got ", length(cfg$snapshot_dates), "."
    )
  }
  if (is.unsorted(cfg$snapshot_dates, strictly = TRUE)) {
    stop("`snapshot_dates` must be strictly increasing.")
  }

  cfg$domains <- as.character(unlist(cfg$domains))
  if (!length(cfg$domains)) stop("`domains` must list at least one domain.")

  cfg$mapping_package <- .as_string(cfg$mapping_package, "mapping_package")
  cfg$reporting_package <- .as_string(cfg$reporting_package, "reporting_package")
  cfg$analytics_packages <- as.character(unlist(cfg$analytics_packages))
  if (!length(cfg$analytics_packages)) stop("`analytics_packages` must list at least one package.")
  cfg$attach_packages <- as.character(unlist(cfg$attach_packages %||% character()))

  if (!is.character(cfg$format) || length(cfg$format) != 1 || !cfg$format %in% c("csv", "parquet", "both")) {
    stop("`format` must be one of \"csv\", \"parquet\", \"both\"; got: ", format(cfg$format))
  }

  cfg$post_processing <- cfg$post_processing %||% list()
  if (!is.list(cfg$post_processing)) stop("`post_processing` must be a mapping.")
  cfg$package_refs <- cfg$package_refs %||% list()

  structure(cfg, class = c("testdata_config", "list"))
}

.as_count <- function(x, name, min = 1L) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x < min || x != floor(x)) {
    stop("`", name, "` must be a single whole number >= ", min, "; got: ", format(x))
  }
  as.integer(x)
}

.as_string <- function(x, name) {
  if (!is.character(x) || length(x) != 1 || !nzchar(x)) {
    stop("`", name, "` must be a single non-empty string.")
  }
  x
}

.as_date <- function(x, name) {
  out <- tryCatch(as.Date(x), error = function(e) NA)
  if (!length(out) || anyNA(out)) {
    stop("`", name, "` must contain valid ISO dates (YYYY-MM-DD).")
  }
  out
}

#' @export
print.testdata_config <- function(x, ...) {
  cat("<testdata_config> bundle ", x$bundle_version, "\n", sep = "")
  cat("  study:      ", x$study_id, " (seed ", x$seed, ")\n", sep = "")
  cat("  size:       ", x$participants, " participants, ", x$sites, " sites, ",
      x$snapshots, " snapshots (", paste(format(x$snapshot_dates), collapse = ", "), ")\n", sep = "")
  cat("  domains:    ", length(x$domains), "\n", sep = "")
  cat("  packages:   mapping=", x$mapping_package, " analytics=", paste(x$analytics_packages, collapse = "+"),
      " reporting=", x$reporting_package, "\n", sep = "")
  cat("  format:     ", x$format, "\n", sep = "")
  invisible(x)
}
