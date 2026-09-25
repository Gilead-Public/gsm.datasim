test_that("explore_testdata() returns an htmlwidget carrying the bundle summary and data (#155)", {
  skip_if_not_installed("htmlwidgets")
  bundle <- local_tiny_bundle()

  w <- explore_testdata(bundle, max_rows = 5)
  expect_s3_class(w, "htmlwidget")
  expect_s3_class(w, "Widget_TestDataExplorer")

  x <- w$x
  expect_equal(x$manifest$bundle_version, "0.0.1")
  expect_true(is.data.frame(x$tables))
  expect_true(is.data.frame(x$columns))
  expect_true(is.list(x$data))
  expect_setequal(names(x$charts), c("rows_by_snapshot", "subjects_per_site", "aes_per_subject", "enrollment_over_time"))
})

test_that("explore_testdata() embeds full rows for the latest raw layer and samples the rest (#155)", {
  skip_if_not_installed("htmlwidgets")
  bundle <- local_tiny_bundle()
  x <- explore_testdata(bundle, max_rows = 5, full_layers = "raw")$x

  latest <- "2025-03-01"
  key_raw <- paste(latest, "raw", "Raw_SUBJ", sep = "/")
  expect_true(key_raw %in% names(x$data))
  subj <- x$data[[key_raw]]
  expect_false(subj$truncated)
  expect_equal(length(subj$rows), subj$total_rows)
  expect_gt(subj$total_rows, 5)
  expect_equal(as.character(subj$columns), names(read_bundle_df(bundle, latest, "raw", "Raw_SUBJ")))

  # a mapped table (not in full_layers) is capped at max_rows
  mapped_keys <- grep(paste0("^", latest, "/mapped/"), names(x$data), value = TRUE)
  expect_gt(length(mapped_keys), 0)
  big <- Filter(function(k) x$data[[k]]$total_rows > 5, mapped_keys)
  expect_gt(length(big), 0)
  expect_true(all(vapply(big, function(k) isTRUE(x$data[[k]]$truncated) && length(x$data[[k]]$rows) == 5, logical(1))))

  # the earlier snapshot's raw layer is sampled too
  early_raw <- x$data[[paste("2025-02-01", "raw", "Raw_SUBJ", sep = "/")]]
  expect_true(early_raw$truncated || early_raw$total_rows <= 5)
})

test_that("full_max_rows caps the latest raw layer so a full-size bundle stays small (#155)", {
  skip_if_not_installed("htmlwidgets")
  bundle <- local_tiny_bundle()
  x <- explore_testdata(bundle, max_rows = 5, full_layers = "raw", full_max_rows = 10)$x
  latest <- "2025-03-01"
  raw_keys <- grep(paste0("^", latest, "/raw/"), names(x$data), value = TRUE)
  big <- Filter(function(k) x$data[[k]]$total_rows > 10, raw_keys)
  expect_gt(length(big), 0)
  expect_true(all(vapply(big, function(k) isTRUE(x$data[[k]]$truncated) && length(x$data[[k]]$rows) == 10, logical(1))))
  small <- Filter(function(k) x$data[[k]]$total_rows <= 10, raw_keys)
  expect_true(all(vapply(small, function(k) !isTRUE(x$data[[k]]$truncated), logical(1))))
  expect_equal(x$options$full_max_rows, 10L)
})

test_that("explore_testdata() validates its inputs (#155)", {
  skip_if_not_installed("htmlwidgets")
  expect_error(explore_testdata(tempfile()), "manifest.json")
  bundle <- local_tiny_bundle()
  expect_error(explore_testdata(bundle, max_rows = 0), "max_rows")
  expect_error(explore_testdata(bundle, full_max_rows = 0), "full_max_rows")
})
