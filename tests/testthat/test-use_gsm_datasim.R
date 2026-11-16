test_that("use_gsm_datasim() creates the .Rprofile with the option line (#190)", {
  for (scope in c("user", "project")) {
    path <- local_rprofile()
    expect_false(file.exists(path))
    expect_message(use_gsm_datasim(scope), "Restart R")
    expect_identical(readLines(path), .testdata_builder_line())
  }
})

test_that("use_gsm_datasim() appends to an existing .Rprofile (#190)", {
  path <- local_rprofile()
  writeLines("options(digits = 4)", path)
  use_gsm_datasim("project") |> suppressMessages()
  expect_identical(
    readLines(path),
    c("options(digits = 4)", .testdata_builder_line())
  )
})

test_that("use_gsm_datasim() handles a file without a trailing newline (#190)", {
  path <- local_rprofile()
  cat("options(digits = 4)", file = path)
  use_gsm_datasim() |> suppressMessages()
  expect_identical(
    readLines(path),
    c("options(digits = 4)", .testdata_builder_line())
  )
})

test_that("use_gsm_datasim() is idempotent (#190)", {
  path <- local_rprofile()
  use_gsm_datasim() |> suppressMessages()
  before <- readLines(path)
  expect_message(use_gsm_datasim(), "already sets")
  expect_identical(readLines(path), before)
})

test_that("use_gsm_datasim() leaves a different builder alone (#190)", {
  path <- local_rprofile()
  writeLines('options(gsm.testdata_builder = "other::builder")', path)
  expect_message(use_gsm_datasim(), "already sets")
  expect_identical(
    readLines(path),
    'options(gsm.testdata_builder = "other::builder")'
  )
})

test_that("use_gsm_datasim() returns the path invisibly (#190)", {
  path <- local_rprofile()
  expect_invisible(out <- use_gsm_datasim() |> suppressMessages())
  expect_identical(out, path)
})

test_that("use_gsm_datasim() validates scope (#190)", {
  local_rprofile()
  expect_error(use_gsm_datasim("global"))
})

test_that("the option string names build_testdata_bundle (#190)", {
  line_expr <- parse(text = .testdata_builder_line())[[1]]
  expect_identical(
    line_expr$gsm.testdata_builder,
    "gsm.datasim::build_testdata_bundle"
  )
  expect_true(is.function(gsm.datasim::build_testdata_bundle))
})

test_that(".rprofile_path() points at the scope's .Rprofile (#190)", {
  expect_identical(
    .rprofile_path("project"),
    fs::path(fs::path_wd(), ".Rprofile")
  )
  expect_identical(
    .rprofile_path("user"),
    fs::path_expand_r("~/.Rprofile")
  )
})
