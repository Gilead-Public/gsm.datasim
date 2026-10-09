# Mocks and test-environment utilities.

mock_workflow_engine <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    MakeWorkflowList = function(strNames = NULL, strPath = NULL, strPackage = NULL, ...) {
      nm <- strNames %||% c("ok_wf", "bad_wf")
      stats::setNames(rep(list(list()), length(nm)), nm)
    },
    RunWorkflows = function(lWorkflow, lData) {
      nm <- names(lWorkflow)
      if (length(nm) != 1) {
        return(list(reporting = "report"))
      }
      if (nm == "bad_wf") stop("boom")
      if (nm == "COUNTRY") stop("no country")
      if (nm == "ok_wf") {
        return(list(Analysis_ok = list(x = 1)))
      }
      if (nm == "rep_wf") {
        return(list(reporting = nm))
      }
      stats::setNames(list(data.frame(a = 1)), paste0("Mapped_", nm))
    },
    .package = "workr",
    .env = env
  )
  testthat::local_mocked_bindings(
    Ingest = function(...) list(Raw_dummy = 1),
    .package = "gsm.mapping",
    .env = env
  )
  testthat::local_mocked_bindings(CombineSpecs = function(...) list(), .env = env)
}

analytics_snapshot <- function(extra = character()) {
  base <- c("Raw_SUBJ", "Raw_SITE", extra)
  stats::setNames(
    lapply(base, function(x) data.frame(a = 1:2)),
    base
  )
}

reporting_input <- function() {
  list(
    ok = list(mapped = list(m = 1), results = list(r = 1), lWorkflow = list(w = 1)),
    skipped = NULL,
    incomplete = list(mapped = list(m = 1))
  )
}

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
test_at_log_threshold <- function(
  level = "FATAL",
  envir = rlang::caller_env()
) {
  old <- logger::log_threshold()
  withr::defer(logger::log_threshold(old), envir = envir)
  logger::log_threshold(level)
  invisible(old)
}
