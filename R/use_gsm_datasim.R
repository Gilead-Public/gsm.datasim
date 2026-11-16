#' Use gsm.datasim as gsm.core's local test data builder
#'
#' Add `options(gsm.testdata_builder = "gsm.datasim::build_testdata_bundle")`
#' to your user or project `.Rprofile`. When installed gsm packages don't match
#' a published test data build and at least one is local (e.g. loaded with
#' `devtools::load_all()`), gsm.core calls this builder to build test data from
#' your local code, caches it, and reuses it in later sessions. If the
#' `.Rprofile` already sets `gsm.testdata_builder` (to any value), it is left
#' unchanged.
#'
#' @param scope Which `.Rprofile` to edit: `"user"` (`~/.Rprofile`, the
#'   default) or `"project"` (`.Rprofile` in the working directory).
#'
#' @returns The path to the `.Rprofile`, invisibly.
#'
#' @examples
#' \dontrun{
#' use_gsm_datasim()
#' use_gsm_datasim("project")
#' }
#' @export
use_gsm_datasim <- function(scope = c("user", "project")) {
  scope <- rlang::arg_match(scope)
  path <- .rprofile_path(scope)

  lines <- if (fs::file_exists(path)) {
    readLines(path, warn = FALSE)
  } else {
    character()
  }
  if (.sets_testdata_builder(lines)) {
    cli::cli_inform(
      "{.path {path}} already sets {.code gsm.testdata_builder}; leaving it unchanged."
    )
    return(invisible(path))
  }

  writeLines(c(lines, .testdata_builder_line()), path)
  cli::cli_inform(c(
    "v" = "Added {.code options(gsm.testdata_builder = ...)} to {.path {path}}.",
    "i" = "Restart R for this to take effect."
  ))
  invisible(path)
}

#' Check whether R code sets the test data builder option
#'
#' A text search, not a parser: looks for `gsm.testdata_builder` (optionally
#' quoted) followed by `=` (not `==`) on a non-comment line.
#'
#' @param lines Lines of R code.
#' @returns `TRUE` or `FALSE`.
#' @keywords internal
.sets_testdata_builder <- function(lines) {
  code <- lines[!grepl("^\\s*#", lines)]
  any(grepl(
    "(^|[^[:alnum:]._])[\"'`]?gsm\\.testdata_builder[\"'`]?\\s*=($|[^=])",
    code
  ))
}

#' Build the `.Rprofile` line that sets the test data builder
#'
#' @returns A single string of R code.
#' @keywords internal
.testdata_builder_line <- function() {
  'options(gsm.testdata_builder = "gsm.datasim::build_testdata_bundle")'
}

#' Get the path to a scope's `.Rprofile`
#'
#' @param scope `"user"` or `"project"`.
#' @returns The path to the `.Rprofile`.
#' @keywords internal
# nocov start
.rprofile_path <- function(scope) {
  switch(
    scope,
    user = fs::path_expand_r("~/.Rprofile"),
    project = fs::path(fs::path_wd(), ".Rprofile")
  )
}
# nocov end
