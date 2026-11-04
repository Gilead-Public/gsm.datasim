test_that(".fresh_move() errors when the file cannot be moved or copied (#151)", {
  skip_if_not_installed("withr")
  from <- withr::local_tempfile(lines = "x")
  to <- file.path(withr::local_tempdir(), "missing-dir", "snapshot.json")
  expect_error(suppressWarnings(.fresh_move(from, to)), "snapshot.json")
})
