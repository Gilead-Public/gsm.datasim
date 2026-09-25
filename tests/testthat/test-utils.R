test_that(".toc_quiet() keeps timing output on by default (#noissue)", {
  withr::local_envvar(GSM_SHOW_TOC = NA)
  expect_false(.toc_quiet())
})

test_that(".toc_quiet() respects truthy GSM_SHOW_TOC values (#noissue)", {
  withr::local_envvar(GSM_SHOW_TOC = "TRUE")
  expect_false(.toc_quiet())

  withr::local_envvar(GSM_SHOW_TOC = "true")
  expect_false(.toc_quiet())
})

test_that(".toc_quiet() silences timing output for explicit FALSE (#noissue)", {
  withr::local_envvar(GSM_SHOW_TOC = "FALSE")
  expect_true(.toc_quiet())

  withr::local_envvar(GSM_SHOW_TOC = "false")
  expect_true(.toc_quiet())
})

test_that(".toc_quiet() falls back to verbose for unparseable values (#noissue)", {
  # as.logical() returns NA here; isFALSE(NA) is FALSE, so timing stays on
  withr::local_envvar(GSM_SHOW_TOC = "garbage")
  expect_false(.toc_quiet())

  withr::local_envvar(GSM_SHOW_TOC = "")
  expect_false(.toc_quiet())
})

test_that("combination_var_splitter names a split var that was never generated (#113, #143)", {
  variable_data <- list(
    studyid = rep("S", 3),
    combo = list(subjid = rep("A", 3), visit = rep("V", 3))
  )

  # The split var exists: elements are spliced in at its position.
  out <- combination_var_splitter(variable_data, list("combo"))
  expect_equal(names(out), c("studyid", "subjid", "visit"))

  # It does not: previously an opaque "attempt to select less than one
  # element in get1index" from deep inside `[[`.
  expect_error(
    combination_var_splitter(variable_data, list("absent_var")),
    "'absent_var' was not generated"
  )
})
