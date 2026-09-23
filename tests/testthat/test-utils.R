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
