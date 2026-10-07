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
