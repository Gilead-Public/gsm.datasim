#' Read and validate a test data bundle config
#'
#' Reads the simulation parameters used by [build_testdata_bundle()]. The
#' supplied default should almost always be used.
#'
#' @param config Path to a YAML config file, or an already-parsed config list
#'   (re-validated).
#'
#' @returns A list with elements `seed`, `study_id`, `participants`, `sites`,
#'   `snapshots`, `interval`, `snapshot_dates` (`Date`), `domains`, and
#'   `extra_mappings`.
#'
#' @examples
#' cfg <- read_testdata_config()
#' cfg$snapshot_dates
#' @export
read_testdata_config <- function(
  config = system.file(
    "testdata",
    "bundle-config.yaml",
    package = "gsm.datasim"
  )
) {
  config <- .load_config(config)
  .check_config_fields(config)
  config <- .normalize_config(config)
  .check_snapshot_dates(config)
  config[c(.config_required_fields, "extra_mappings")]
}

.config_required_fields <- c(
  "seed",
  "study_id",
  "participants",
  "sites",
  "snapshots",
  "interval",
  "snapshot_dates",
  "domains"
)

#' Load a bundle config
#'
#' @param config Path to a YAML config file, or a config list.
#' @returns The config as a list.
#' @keywords internal
.load_config <- function(config) {
  if (is.character(config)) {
    if (length(config) != 1 || !file.exists(config)) {
      stop(
        "`config` must be the path to an existing YAML file or a config list."
      )
    }
    config <- yaml::read_yaml(config)
  }
  if (!is.list(config)) {
    stop("`config` must be the path to an existing YAML file or a config list.")
  }
  config
}

#' Check required bundle config fields
#'
#' Errors if any field in `config_required_fields` is missing or `NULL`.
#'
#' @param config A config list.
#' @returns `config`, invisibly.
#' @keywords internal
.check_config_fields <- function(config) {
  missing <- setdiff(
    .config_required_fields,
    names(config)[!vapply(config, is.null, logical(1))]
  )
  if (length(missing)) {
    stop(
      "Bundle config is missing required field(s): ",
      paste(missing, collapse = ", ")
    )
  }
  invisible(config)
}

#' Normalize bundle config types
#'
#' Coerces `snapshot_dates` to `Date` and `domains`/`extra_mappings` to
#' character (`extra_mappings` defaults to empty).
#'
#' @param config A config list.
#' @returns The normalized config.
#' @keywords internal
.normalize_config <- function(config) {
  config$snapshot_dates <- as.Date(as.character(unlist(config$snapshot_dates)))
  config$domains <- as.character(unlist(config$domains))
  config$extra_mappings <- as.character(unlist(
    config$extra_mappings %||% character()
  ))
  config
}

#' Check bundle snapshot dates
#'
#' Errors unless there is one valid date per snapshot, strictly increasing.
#'
#' @param config A normalized config list.
#' @returns `config`, invisibly.
#' @keywords internal
.check_snapshot_dates <- function(config) {
  if (
    length(config$snapshot_dates) != config$snapshots ||
      anyNA(config$snapshot_dates)
  ) {
    stop(
      "`snapshot_dates` must contain one valid date per snapshot (",
      config$snapshots,
      ")."
    )
  }
  if (is.unsorted(config$snapshot_dates, strictly = TRUE)) {
    stop("`snapshot_dates` must be strictly increasing.")
  }
  invisible(config)
}
