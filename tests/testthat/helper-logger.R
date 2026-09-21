#' Temporarily set the logger threshold for a test
#'
#' Silences (or, with a more verbose `level`, exposes) `logger` output for the
#' duration of the calling test, restoring the previous threshold on exit.
#'
#' @param level Log threshold to set, e.g. "FATAL" (default) or "INFO".
#' @param envir Environment whose exit triggers restoration of the threshold.
#'
#' @return The previous log threshold, invisibly.
#' @noRd
test_at_log_threshold <- function(level = "FATAL", envir = rlang::caller_env()) {
  old <- logger::log_threshold()
  withr::defer(logger::log_threshold(old), envir = envir)
  logger::log_threshold(level)
  invisible(old)
}
