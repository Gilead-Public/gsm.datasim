test_that("generate_study_snapshots preserves longitudinal continuity (#165)", {
  set.seed(1234)
  snapshots <- generate_study_snapshots(
    study_id = "TEST",
    participants = 60,
    sites = 6,
    snapshots = 3,
    interval = "1 month",
    mappings = ensure_core_mappings(c("AE"))
  )

  expect_equal(length(snapshots), 3)
  expect_equal(names(snapshots), c("2012-01-31", "2012-02-29", "2012-03-31"))

  ids <- lapply(snapshots, function(x) x$Raw_SUBJ$subjid)

  # Subjects enrolled in earlier snapshots persist into later ones.
  expect_true(all(ids[[1]] %in% ids[[2]]))
  expect_true(all(ids[[2]] %in% ids[[3]]))

  # Cohort grows rather than being regenerated from scratch.
  expect_true(length(ids[[1]]) < length(ids[[3]]))

  # Exposure accumulates beyond a single snapshot width.
  max_time <- vapply(
    snapshots,
    function(x) max(x$Raw_SUBJ$timeonstudy, na.rm = TRUE),
    numeric(1)
  )
  expect_true(all(diff(max_time) > 0))
  expect_gt(max_time[[3]], 28)

  # Enrollment dates are carried forward, not reset per snapshot.
  first_enroll <- vapply(
    snapshots,
    function(x) as.character(min(x$Raw_SUBJ$enrolldt, na.rm = TRUE)),
    character(1)
  )
  expect_equal(length(unique(first_enroll)), 1)
})
