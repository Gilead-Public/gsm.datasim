# ── Tests for column_type_generator.R and generate_data_from_workflows.R ──────

# ── infer_column_type ─────────────────────────────────────────────────────────

test_that("infer_column_type uses explicit spec type when available", {
  expect_equal(infer_column_type("foo", list(type = "date")), "date")
  expect_equal(infer_column_type("foo", list(type = "numeric")), "numeric")
  expect_equal(infer_column_type("foo", list(type = "integer")), "integer")
  expect_equal(infer_column_type("foo", list(type = "logical")), "logical")
  expect_equal(infer_column_type("foo", list(type = "character")), "character")
  expect_equal(infer_column_type("foo", list(type = "timestamp")), "timestamp")
})

test_that("infer_column_type falls back to name patterns", {
  expect_equal(infer_column_type("enroll_dt"), "date")
  expect_equal(infer_column_type("start_date"), "date")
  expect_equal(infer_column_type("act_fpfv"), "date")
  expect_equal(infer_column_type("my_yn"), "yn")
  expect_equal(infer_column_type("active_flag"), "yn")
  expect_equal(infer_column_type("subject_count"), "integer")
  expect_equal(infer_column_type("num_visits"), "integer")
  expect_equal(infer_column_type("risk_score"), "numeric")
  expect_equal(infer_column_type("lab_val"), "numeric")
  expect_equal(infer_column_type("pct_change_pct"), "numeric")
  expect_equal(infer_column_type("is_enrolled"), "logical")
  expect_equal(infer_column_type("has_consent"), "logical")
})

test_that("infer_column_type defaults to character", {
  expect_equal(infer_column_type("sitename"), "character")
  expect_equal(infer_column_type("protocol"), "character")
  expect_equal(infer_column_type("unknown_column"), "character")
})

# ── generate_column_by_type ───────────────────────────────────────────────────

test_that("generate_column_by_type produces correct types and lengths", {
  set.seed(42)
  ctx <- list(
    data       = list(),
    start_date = "2012-01-01",
    end_date   = "2012-06-30"
  )

  dates <- generate_column_by_type("visit_dt", list(), 10, ctx)
  expect_length(dates, 10)
  expect_s3_class(dates, "Date")

  nums <- generate_column_by_type("lab_score", list(), 10, ctx)
  expect_length(nums, 10)
  expect_type(nums, "double")

  ints <- generate_column_by_type("visit_count", list(), 10, ctx)
  expect_length(ints, 10)
  expect_type(ints, "integer")

  yns <- generate_column_by_type("enrolled_yn", list(), 10, ctx)
  expect_length(yns, 10)
  expect_true(all(yns %in% c("Y", "N")))

  logicals <- generate_column_by_type("is_active", list(), 10, ctx)
  expect_length(logicals, 10)
  expect_type(logicals, "logical")

  chars <- generate_column_by_type("sitename", list(), 10, ctx)
  expect_length(chars, 10)
  expect_type(chars, "character")
})

test_that("generate_column_by_type respects explicit spec type over name pattern", {
  set.seed(42)
  ctx <- list(data = list(), start_date = "2012-01-01", end_date = "2012-12-31")

  # Column name suggests date (_dt), but spec says numeric
  result <- generate_column_by_type("value_dt", list(type = "numeric"), 10, ctx)
  expect_type(result, "double")
})

test_that("generate_column_by_type samples FK from parent domain", {
  set.seed(42)
  parent_subj <- data.frame(subjid = paste0("SUBJ-", 1:5), stringsAsFactors = FALSE)
  ctx <- list(
    data = list(Raw_SUBJ = parent_subj),
    start_date = "2012-01-01",
    end_date = "2012-12-31"
  )

  fk_vals <- generate_column_by_type("subjid", list(), 20, ctx)
  expect_length(fk_vals, 20)
  expect_true(all(fk_vals %in% parent_subj$subjid))
})

# ── generate_unknown_domain ───────────────────────────────────────────────────

test_that("generate_unknown_domain creates data.frame with correct shape", {
  set.seed(42)
  spec <- list(
    patient_id = list(type = "character"),
    visit_dt   = list(type = "date"),
    lab_val    = list(type = "numeric"),
    flag_yn    = list(required = TRUE)
  )
  ctx <- list(data = list(), start_date = "2012-01-01", end_date = "2012-06-30")

  result <- generate_unknown_domain("Raw_CUSTOM", spec, 15, ctx)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 15)
  expect_true(all(c("patient_id", "visit_dt", "lab_val", "flag_yn") %in% names(result)))
  expect_s3_class(result$visit_dt, "Date")
  expect_type(result$lab_val, "double")
  expect_true(all(result$flag_yn %in% c("Y", "N")))
})

test_that("generate_unknown_domain applies source_col renames", {
  set.seed(42)
  spec <- list(
    visit = list(required = TRUE, source_col = "foldername"),
    score = list(type = "numeric")
  )
  ctx <- list(data = list(), start_date = "2012-01-01", end_date = "2012-12-31")

  result <- generate_unknown_domain("Raw_TEST", spec, 5, ctx)
  expect_true("foldername" %in% names(result))
  expect_false("visit" %in% names(result))
})

test_that("generate_unknown_domain returns empty data.frame for n=0", {
  spec <- list(col1 = list(type = "character"))
  result <- generate_unknown_domain("Raw_X", spec, 0, list())
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("generate_unknown_domain uses FK from parent data", {
  set.seed(42)
  parent_subj <- data.frame(subjid = paste0("S", 1:3), stringsAsFactors = FALSE)
  spec <- list(
    subjid = list(required = TRUE),
    value  = list(type = "numeric")
  )
  ctx <- list(
    data = list(Raw_SUBJ = parent_subj),
    start_date = "2012-01-01",
    end_date = "2012-12-31"
  )

  result <- generate_unknown_domain("Raw_CUSTOM", spec, 10, ctx)
  expect_true(all(result$subjid %in% parent_subj$subjid))
})

# ── generate_data_from_workflows ──────────────────────────────────────────────

test_that("generate_data_from_workflows rejects empty input", {
  expect_error(
    generate_data_from_workflows(list()),
    "non-empty named list"
  )
})

test_that("generate_data_from_workflows handles pure-unknown workflows", {
  set.seed(42)

  # Create a minimal fake workflow with unknown domains
  fake_workflows <- list(
    custom_wf = list(
      meta = list(Description = "test"),
      spec = list(
        Raw_CUSTOM1 = list(
          col_a = list(type = "character"),
          col_dt = list(type = "date"),
          col_num = list(type = "numeric")
        ),
        Raw_CUSTOM2 = list(
          id_col  = list(type = "character"),
          val_col = list(type = "integer")
        )
      ),
      steps = list()
    )
  )

  result <- generate_data_from_workflows(
    lWorkflows     = fake_workflows,
    n_participants = 20,
    n_sites        = 5,
    study_id       = "TEST-001"
  )

  expect_type(result, "list")
  # Should contain both custom domains (Raw_STUDY/Raw_SITE/Raw_SUBJ/Raw_ENROLL
  # are auto-added by prepare_combined_specs_for_generation if Raw_VISIT
  # is missing, but the custom domains should definitely be there)
  expect_true("Raw_CUSTOM1" %in% names(result))
  expect_true("Raw_CUSTOM2" %in% names(result))

  expect_s3_class(result$Raw_CUSTOM1, "data.frame")
  expect_equal(nrow(result$Raw_CUSTOM1), 20)
  expect_true(all(c("col_a", "col_dt", "col_num") %in% names(result$Raw_CUSTOM1)))

  expect_s3_class(result$Raw_CUSTOM2, "data.frame")
  expect_equal(nrow(result$Raw_CUSTOM2), 20)
})

test_that("generate_data_from_workflows respects domain_counts", {
  set.seed(42)

  fake_workflows <- list(
    wf1 = list(
      meta = list(),
      spec = list(
        Raw_MYDOM = list(
          x = list(type = "character"),
          y = list(type = "numeric")
        )
      ),
      steps = list()
    )
  )

  result <- generate_data_from_workflows(
    lWorkflows     = fake_workflows,
    n_participants = 50,
    domain_counts  = list(Raw_MYDOM = 25)
  )

  expect_equal(nrow(result$Raw_MYDOM), 25)
})

test_that("generate_data_from_workflows respects desired_domains filter", {
  set.seed(42)

  fake_workflows <- list(
    wf1 = list(
      meta = list(),
      spec = list(
        Raw_A = list(x = list(type = "character")),
        Raw_B = list(y = list(type = "numeric"))
      ),
      steps = list()
    )
  )

  result <- generate_data_from_workflows(
    lWorkflows      = fake_workflows,
    n_participants  = 10,
    desired_domains = c("Raw_A")
  )

  expect_true("Raw_A" %in% names(result))
  # Raw_B should NOT be generated since we filtered
  expect_false("Raw_B" %in% names(result))
})

# ── .resolve_domain_counts ────────────────────────────────────────────────────

test_that(".resolve_domain_counts uses heuristics for known domains", {
  counts <- .resolve_domain_counts(
    domain_names   = c("Raw_STUDY", "Raw_SITE", "Raw_SUBJ", "Raw_AE"),
    n_participants = 100,
    n_sites        = 10
  )

  expect_equal(counts$Raw_STUDY, 1L)
  expect_equal(counts$Raw_SITE, 10L)
  expect_equal(counts$Raw_SUBJ, 100L)
  expect_equal(counts$Raw_AE, 300L)
})

test_that(".resolve_domain_counts uses participant count for unknown domains", {
  counts <- .resolve_domain_counts(
    domain_names   = c("Raw_UNKNOWN"),
    n_participants = 42,
    n_sites        = 5
  )

  expect_equal(counts$Raw_UNKNOWN, 42L)
})

test_that(".resolve_domain_counts respects user overrides", {
  counts <- .resolve_domain_counts(
    domain_names   = c("Raw_AE", "Raw_SUBJ"),
    n_participants = 100,
    n_sites        = 10,
    user_counts    = list(Raw_AE = 999)
  )

  expect_equal(counts$Raw_AE, 999L)
  expect_equal(counts$Raw_SUBJ, 100L)  # Not overridden
})

# ── Longitudinal / multi-snapshot ─────────────────────────────────────────────

test_that("generate_data_from_workflows produces multiple snapshots", {
  set.seed(42)

  fake_workflows <- list(
    wf1 = list(
      meta = list(),
      spec = list(
        Raw_CUSTOM = list(
          col_id  = list(type = "character"),
          col_dt  = list(type = "date"),
          col_val = list(type = "numeric")
        )
      ),
      steps = list()
    )
  )

  result <- generate_data_from_workflows(
    lWorkflows     = fake_workflows,
    n_participants = 30,
    snapshot_count = 3,
    snapshot_width = "months"
  )

  # Should return a named list of 3 snapshots

  expect_type(result, "list")
  expect_length(result, 3)

  # Each snapshot should be a named list containing domain data.frames
  for (snap in result) {
    expect_true("Raw_CUSTOM" %in% names(snap))
    expect_s3_class(snap$Raw_CUSTOM, "data.frame")
  }

  # Row counts should be cumulative and non-decreasing
  row_counts <- vapply(result, function(s) nrow(s$Raw_CUSTOM), integer(1))
  expect_true(all(diff(row_counts) >= 0))

  # Last snapshot should have the full target count
  expect_equal(unname(row_counts[3]), 30L)
})

test_that("longitudinal snapshots have cumulative rows for unknown domains", {
  set.seed(123)

  fake_workflows <- list(
    wf1 = list(
      meta = list(),
      spec = list(
        Raw_EVENTS = list(
          event_id = list(type = "character"),
          event_dt = list(type = "date")
        )
      ),
      steps = list()
    )
  )

  result <- generate_data_from_workflows(
    lWorkflows     = fake_workflows,
    n_participants = 20,
    snapshot_count = 4,
    snapshot_width = "months"
  )

  # Verify cumulative pattern: each snapshot should contain all prior rows
  for (i in seq_along(result)[-1]) {
    prev_n <- nrow(result[[i - 1]]$Raw_EVENTS)
    curr_n <- nrow(result[[i]]$Raw_EVENTS)
    expect_gte(curr_n, prev_n)
  }
})

test_that("snapshot names are end-date strings", {
  set.seed(42)

  fake_workflows <- list(
    wf1 = list(
      meta = list(),
      spec = list(
        Raw_X = list(a = list(type = "character"))
      ),
      steps = list()
    )
  )

  result <- generate_data_from_workflows(
    lWorkflows     = fake_workflows,
    n_participants = 10,
    start_date     = "2012-01-01",
    snapshot_count = 3,
    snapshot_width = "months"
  )

  snap_names <- names(result)
  expect_length(snap_names, 3)
  # Names should be valid date strings
  parsed <- as.Date(snap_names)
  expect_false(any(is.na(parsed)))
  # Should be 28 days after each snapshot start
  expect_true(all(diff(parsed) > 0))
})

test_that("single snapshot returns flat list (backward compatible)", {
  set.seed(42)

  fake_workflows <- list(
    wf1 = list(
      meta = list(),
      spec = list(
        Raw_FLAT = list(x = list(type = "character"))
      ),
      steps = list()
    )
  )

  result <- generate_data_from_workflows(
    lWorkflows     = fake_workflows,
    n_participants = 10,
    snapshot_count = 1
  )

  # Single snapshot returns a flat named list of data.frames, not nested
  expect_true("Raw_FLAT" %in% names(result))
  expect_s3_class(result$Raw_FLAT, "data.frame")
})

# ── generate_unknown_domain with previous_data ─────────────────────────────

test_that("generate_unknown_domain uses cumulative delta with previous_data", {
  set.seed(42)
  spec <- list(
    col_a = list(type = "character"),
    col_b = list(type = "numeric")
  )
  ctx <- list(data = list(), start_date = "2012-01-01", end_date = "2012-06-30")

  # Generate initial 5 rows
  initial <- generate_unknown_domain("Raw_T", spec, 5, ctx)
  expect_equal(nrow(initial), 5)

  # Generate cumulative up to 8 rows with previous_data
  cumulative <- generate_unknown_domain("Raw_T", spec, 8, ctx, previous_data = initial)
  expect_equal(nrow(cumulative), 8)

  # First 5 rows should be identical to initial
  expect_equal(cumulative[1:5, ], initial)
})

test_that("generate_unknown_domain returns previous_data when delta <= 0", {
  set.seed(42)
  spec <- list(col_a = list(type = "character"))
  ctx <- list(data = list(), start_date = "2012-01-01", end_date = "2012-12-31")

  initial <- generate_unknown_domain("Raw_T", spec, 10, ctx)

  # Target is smaller than existing data — should return existing as-is
  result <- generate_unknown_domain("Raw_T", spec, 5, ctx, previous_data = initial)
  expect_equal(nrow(result), 10)
  expect_equal(result, initial)
})
