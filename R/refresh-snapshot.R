# Vendored from gsm.core's R/refresh-snapshot.R (the would-be "refresh"
# package). Replace with refresh::write_snapshot_json() once refresh is on
# CRAN. Kept close to the original, except `tools::sha256sum()` (R >= 4.5) is
# swapped for `digest::digest()` (see Gilead-Public/gsm.core#206).
#
# snapshot.json (format version 1):
# {
#   "refresh_snapshot": 1,
#   "key": "sha256:<data fingerprint>",
#   "created": "2026-10-30T18:25:06Z",
#   "objects": {"<name>": {"file": "<path relative to dir>", "sha256": "<hex>"}},
#   "packages": {"<pkg>": {"version": "1.2.3", "remote_sha": "<sha or null>"}},
#   "metadata": {}
# }

.snapshot_format <- 1L

#' Write snapshot.json for files already in a folder
#'
#' Checksums are computed from the files on disk.
#'
#' @param dir Snapshot folder containing the files.
#' @param objects Named list or character vector: object name -> file name.
#' @param key Version key (a string).
#' @param packages Optional named list; each element has `version` and
#'   `remote_sha`.
#' @param metadata Optional named list stored as-is.
#' @returns The snapshot description, invisibly.
#' @keywords internal
write_snapshot_json <- function(
  dir,
  objects,
  key,
  packages = NULL,
  metadata = NULL
) {
  stopifnot(rlang::is_named(objects), rlang::is_string(key))
  objects <- lapply(as.list(objects), function(file) {
    path <- file.path(dir, file)
    if (!file.exists(path)) {
      cli::cli_abort("{.path {path}} does not exist.")
    }
    list(file = file, sha256 = digest::digest(file = path, algo = "sha256"))
  })
  info <- list(
    refresh_snapshot = .snapshot_format,
    key = key,
    created = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    objects = objects
  )
  if (length(packages)) {
    info$packages <- lapply(packages, function(p) {
      sha <- p$remote_sha
      list(
        version = p$version,
        remote_sha = if (is.null(sha) || is.na(sha)) NULL else sha
      )
    })
  }
  if (length(metadata)) {
    info$metadata <- metadata
  }
  .fresh_write_json(info, file.path(dir, "snapshot.json"))
  invisible(info)
}

.fresh_write_json <- function(value, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(tmpdir = dirname(path), fileext = ".tmp")
  on.exit(unlink(tmp))
  jsonlite::write_json(
    value,
    tmp,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null",
    digits = NA
  )
  .fresh_move(tmp, path)
}

.fresh_move <- function(from, to) {
  # file.rename() can't overwrite on Windows.
  if (file.exists(to)) {
    unlink(to)
  }
  if (!file.rename(from, to) && !file.copy(from, to, overwrite = TRUE)) {
    stop("Failed to write ", to, call. = FALSE)
  }
  invisible(to)
}
