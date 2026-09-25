# Tests for the domain-neutral longitudinal helpers in R/utils-longitudinal.R.
#
# These exercise the logic directly on plain vectors, with no registry or spec
# scaffolding -- they are the tests that matter most for #143, since the
# site-targeting guarantee is a property of `inject_targeted_runs()` rather
# than of the Raw_VS wiring.

# ---- count_repeat_windows() -------------------------------------------------

test_that("count_repeat_windows returns V - W + 1, floored at zero (#143)", {
  expect_equal(count_repeat_windows(5, nWindowLength = 3), 3)
  expect_equal(count_repeat_windows(3, nWindowLength = 3), 1)
  expect_equal(count_repeat_windows(2, nWindowLength = 3), 0)
  expect_equal(count_repeat_windows(0, nWindowLength = 3), 0)

  # Vectorized over a vector of measurement counts.
  expect_equal(
    count_repeat_windows(c(0, 2, 3, 5, 10), nWindowLength = 3),
    c(0, 0, 1, 3, 8)
  )

  # Window length is honored.
  expect_equal(count_repeat_windows(c(3, 5), nWindowLength = 4), c(0, 2))
})

test_that("count_repeat_windows validates its arguments (#143)", {
  expect_error(count_repeat_windows(5, nWindowLength = 1))
  expect_error(count_repeat_windows(5, nWindowLength = 2.5))
  expect_error(count_repeat_windows("five"))
})

# ---- allocate_site_risk() ---------------------------------------------------

test_that("allocate_site_risk converts percentages to the right counts (#143)", {
  sites <- sprintf("S%02d", 1:10)
  set.seed(8812)

  bands <- allocate_site_risk(sites, dPctRed = 0.1, dPctAmber = 0.2)

  expect_equal(sum(bands == "red"), 1)
  expect_equal(sum(bands == "amber"), 2)
  expect_equal(sum(bands == "normal"), 7)
})

test_that("allocate_site_risk honors explicit counts exactly (#143)", {
  sites <- sprintf("S%02d", 1:10)
  set.seed(4420)

  bands <- allocate_site_risk(sites, nRed = 3, nAmber = 4)

  expect_equal(sum(bands == "red"), 3)
  expect_equal(sum(bands == "amber"), 4)
  expect_equal(sum(bands == "normal"), 3)
})

test_that("allocate_site_risk assigns every site exactly one band (#143)", {
  sites <- sprintf("S%02d", 1:17)
  set.seed(1993)

  bands <- allocate_site_risk(sites, dPctRed = 0.2, dPctAmber = 0.3)

  expect_named(bands, sites, ignore.order = TRUE)
  expect_length(bands, length(sites))
  expect_true(all(bands %in% c("red", "amber", "normal")))
  expect_false(any(duplicated(names(bands))))
})

test_that("allocate_site_risk errors when red + amber exceeds the site count (#143)", {
  sites <- sprintf("S%02d", 1:4)

  expect_error(allocate_site_risk(sites, nRed = 3, nAmber = 3))
  expect_error(allocate_site_risk(sites, dPctRed = 0.7, dPctAmber = 0.7))
})

test_that("allocate_site_risk degrades sensibly for very few sites (#143)", {
  sites <- c("A", "B", "C")
  set.seed(6104)

  # 10% / 20% of 3 sites rounds to 0 red and 1 amber -- no error, no
  # over-allocation.
  bands <- allocate_site_risk(sites, dPctRed = 0.1, dPctAmber = 0.2)

  expect_length(bands, 3)
  expect_true(all(bands %in% c("red", "amber", "normal")))
  expect_lte(sum(bands == "red") + sum(bands == "amber"), 3)

  # A single site still works.
  expect_length(allocate_site_risk("only", dPctRed = 0, dPctAmber = 0), 1)
})

# ---- inject_targeted_runs() -------------------------------------------------

test_that("inject_targeted_runs lands within one window of the target rate (#143)", {
  input <- make_injection_input(n_groups = 12, n_per_group = 10)

  for (target in c(0.05, 0.25, 0.45)) {
    set.seed(3301)
    out <- inject_targeted_runs(
      input$values, input$groups,
      dTargetRate = target, nWindowLength = 3
    )

    realized <- site_repeat_rate(
      data.frame(
        invid = "single",
        subjid = input$groups,
        value = out,
        stringsAsFactors = FALSE
      ),
      strValueCol = "value"
    )

    expect_equal(
      realized$rate, target,
      tolerance = 1 / realized$denominator,
      info = paste("target:", target)
    )
  }
})

test_that("inject_targeted_runs with a target of zero leaves values unchanged (#143)", {
  input <- make_injection_input()
  set.seed(7781)

  out <- inject_targeted_runs(input$values, input$groups, dTargetRate = 0)

  # The realized-counts attribute is always attached, so compare the values
  # themselves rather than the attributed object.
  expect_equal(as.numeric(out), input$values)
  expect_equal(attr(out, "realized")$numerator, 0)
})

test_that("inject_targeted_runs places repeats contiguously in group-index order (#143)", {
  values <- c(1, 2, 3, 4, 5, 6, 7, 8)
  groups <- rep("G1", 8)
  set.seed(2251)

  out <- inject_targeted_runs(values, groups, dTargetRate = 0.5, nWindowLength = 3)

  # Find runs of equal adjacent values; assert they are genuine blocks rather
  # than a scattered multiset that happens to contain duplicates.
  rle_out <- rle(as.numeric(out))
  expect_true(any(rle_out$lengths >= 3))

  # Every duplicated value must appear in a contiguous block -- no value
  # should occur in two separate runs.
  out <- as.numeric(out)
  repeated_vals <- unique(out[duplicated(out)])
  for (v in repeated_vals) {
    positions <- which(out == v)
    expect_equal(
      positions, seq(min(positions), max(positions)),
      info = paste("value", v, "is not contiguous")
    )
  }
})

test_that("inject_targeted_runs leaves groups with no windows untouched (#143)", {
  # Groups of size 1 and 2 have d = 0 at W = 3.
  values <- c(10, 20, 21, 30, 31, 32, 33, 34)
  groups <- c("A", "B", "B", "C", "C", "C", "C", "C")
  set.seed(9110)

  out <- inject_targeted_runs(values, groups, dTargetRate = 0.5, nWindowLength = 3)

  expect_equal(out[groups == "A"], values[groups == "A"])
  expect_equal(out[groups == "B"], values[groups == "B"])

  # A single length-1 group on its own must not error.
  expect_no_error(
    inject_targeted_runs(c(1), c("solo"), dTargetRate = 0.5, nWindowLength = 3)
  )
})

test_that("inject_targeted_runs respects a longer window length (#143)", {
  input <- make_injection_input(n_groups = 8, n_per_group = 12)
  set.seed(5517)

  out <- inject_targeted_runs(
    input$values, input$groups,
    dTargetRate = 0.4, nWindowLength = 4
  )

  # Every injected run must be at least W long, so at least one run of >= 4
  # exists within some group.
  max_runs <- vapply(
    split(out, input$groups),
    function(x) max(rle(x)$lengths),
    numeric(1)
  )
  expect_true(any(max_runs >= 4))
})

test_that("inject_targeted_runs reports a realized numerator and denominator that match recomputation (#143)", {
  input <- make_injection_input(n_groups = 10, n_per_group = 9)
  set.seed(1471)

  out <- inject_targeted_runs(
    input$values, input$groups,
    dTargetRate = 0.3, nWindowLength = 3
  )

  realized <- attr(out, "realized")
  expect_type(realized, "list")
  expect_true(all(c("numerator", "denominator") %in% names(realized)))

  independent <- site_repeat_rate(
    data.frame(
      invid = "single",
      subjid = input$groups,
      value = as.numeric(out),
      stringsAsFactors = FALSE
    ),
    strValueCol = "value"
  )

  expect_equal(realized$numerator, independent$numerator)
  expect_equal(realized$denominator, independent$denominator)
})

test_that("inject_targeted_runs saturates cleanly at a target rate of one (#143)", {
  input <- make_injection_input(n_groups = 4, n_per_group = 5)

  set.seed(6650)
  out <- inject_targeted_runs(
    input$values, input$groups,
    dTargetRate = 1.0, nWindowLength = 3
  )

  # A group's whole window capacity is consumed by a single run covering all
  # its values, so a rate of 1 is achievable rather than a capacity overflow.
  # This pins the invariant that the loop always delivers the full numerator.
  realized <- attr(out, "realized")
  expect_equal(realized$numerator, realized$denominator)
  expect_gt(realized$denominator, 0)

  # Independently confirm: every group is now constant.
  constant <- vapply(
    split(as.numeric(out), input$groups),
    function(x) length(unique(x)) == 1,
    logical(1)
  )
  expect_true(all(constant))
})

test_that("allocate_site_risk returns an empty result for no sites (#143)", {
  bands <- allocate_site_risk(character(0))

  expect_length(bands, 0)
  expect_type(bands, "character")
})

test_that("inject_targeted_runs validates its arguments (#143)", {
  values <- c(1, 2, 3, 4, 5, 6)
  groups <- rep("G1", 6)

  expect_error(inject_targeted_runs(values, rep("G1", 5), dTargetRate = 0.3))
  expect_error(inject_targeted_runs(values, groups, dTargetRate = 0.3, nWindowLength = 1))
  expect_error(inject_targeted_runs(values, groups, dTargetRate = 0.3, nWindowLength = 2.5))
  expect_error(inject_targeted_runs(values, groups, dTargetRate = 1.5))
  expect_error(inject_targeted_runs(values, groups, dTargetRate = -0.1))
})

test_that("inject_targeted_runs never overwrites NA and excludes it from windows (#143)", {
  # Group A: 6 non-missing values among 8 rows.
  values <- c(1, 2, NA, 3, 4, NA, 5, 6, 10, 11, 12, 13, 14, 15, 16, 17)
  groups <- rep(c("A", "B"), each = 8)
  set.seed(3390)

  out <- inject_targeted_runs(values, groups, dTargetRate = 0.4, nWindowLength = 3)

  # NA positions are preserved exactly.
  expect_equal(which(is.na(out)), which(is.na(values)))

  # The denominator counts only non-missing values: A has 6 (d = 4), B has 8
  # (d = 6), so D = 10.
  expect_equal(attr(out, "realized")$denominator, 10)
})

test_that("inject_targeted_runs gives d = 0 to a group with fewer than W non-missing values (#143)", {
  # Group A has 5 rows but only 2 non-missing -- below W = 3.
  values <- c(1, NA, NA, 2, NA, 10, 11, 12, 13, 14)
  groups <- rep(c("A", "B"), each = 5)
  set.seed(4004)

  out <- inject_targeted_runs(values, groups, dTargetRate = 0.5, nWindowLength = 3)

  # Only group B (5 non-missing, d = 3) contributes.
  expect_equal(attr(out, "realized")$denominator, 3)

  # Group A is untouched.
  expect_equal(out[groups == "A"], values[groups == "A"])
})

# ---- assign_schedule_dates() ------------------------------------------------

test_that("assign_schedule_dates sorts by group then date and joins the right dates (#143)", {
  fx <- make_schedule_fixture()

  out <- assign_schedule_dates(fx$df, fx$visits, strDateCol = "vs_dt")

  expect_true("vs_dt" %in% names(out))
  expect_s3_class(out$vs_dt, "Date")

  # Sorted by subject, then date within subject.
  expect_equal(out$subjid, sort(out$subjid))
  for (s in unique(out$subjid)) {
    dates <- out$vs_dt[out$subjid == s]
    expect_false(is.unsorted(dates), info = paste("dates not sorted for", s))
  }

  # Dates match the schedule for the corresponding visit.
  expected <- fx$visits$visit_dt[
    match(
      paste(out$subjid, out$instancename),
      paste(fx$visits$subjid, fx$visits$instancename)
    )
  ]
  expect_equal(out$vs_dt, expected)

  # Chronological order implies the visit order, since VISIT 1 precedes
  # VISIT 2 precedes Screening in this fixture's dates.
  expect_equal(
    out$instancename[out$subjid == "S1"],
    c("VISIT 1", "VISIT 2", "Screening")
  )
})

test_that("assign_schedule_dates errors when a subject-visit has no scheduled date (#143)", {
  fx <- make_schedule_fixture()
  df <- rbind(
    fx$df,
    data.frame(subjid = "S1", instancename = "UNSCHEDULED", stringsAsFactors = FALSE)
  )

  expect_error(
    assign_schedule_dates(df, fx$visits, strDateCol = "vs_dt"),
    regexp = "date"
  )
})

test_that("assign_schedule_dates honors a caller-supplied date column name (#143)", {
  fx <- make_schedule_fixture()

  out <- assign_schedule_dates(fx$df, fx$visits, strDateCol = "lb_dt")

  expect_true("lb_dt" %in% names(out))
  expect_false("vs_dt" %in% names(out))
  expect_s3_class(out$lb_dt, "Date")
})

test_that("assign_schedule_dates validates its inputs (#143)", {
  fx <- make_schedule_fixture()

  expect_error(
    assign_schedule_dates(fx$df, fx$visits, strDateCol = c("a", "b")),
    "single column name"
  )
  expect_error(
    assign_schedule_dates(fx$df["subjid"], fx$visits, strDateCol = "vs_dt"),
    "missing required column"
  )
  expect_error(
    assign_schedule_dates(fx$df, fx$visits["subjid"], strDateCol = "vs_dt"),
    "must contain a `visit_dt` column"
  )
})

test_that("allocate_site_risk rejects malformed explicit counts (#143)", {
  sites <- paste0("SITE", 1:10)

  expect_error(allocate_site_risk(sites, nRed = -1), "non-negative whole number")
  expect_error(allocate_site_risk(sites, nAmber = 1.5), "non-negative whole number")
})

test_that("assign_schedule_dates ordering is stable for repeated subject-visits (#143)", {
  fx <- make_schedule_fixture()
  # Two records for the same subject-visit must keep a deterministic order.
  df <- rbind(fx$df, fx$df[2, , drop = FALSE])

  out1 <- assign_schedule_dates(df, fx$visits, strDateCol = "vs_dt")
  out2 <- assign_schedule_dates(df, fx$visits, strDateCol = "vs_dt")

  expect_equal(out1, out2)
})

test_that("inject_targeted_runs does not over-count when the run's neighbour ties (#143)", {
  # `c(1, 2, 3, 1)` at W = 3 was the reported case: overwriting the first three
  # positions with 1 leaves the trailing 1 extending the run to 2 windows.
  values <- c(1, 2, 3, 1)
  groups <- rep("G1", 4)

  out <- inject_targeted_runs(values, groups, dTargetRate = 0.5, nWindowLength = 3)
  realized <- attr(out, "realized")

  expect_equal(realized$denominator, 2)
  expect_equal(realized$numerator, 1)
  expect_equal(count_identical_windows_naive(as.numeric(out), groups, 3), 1)
})

test_that("inject_targeted_runs realized numerator matches a naive recount (#143)", {
  set.seed(7314)
  # Coarse rounding makes incidental ties common, which is the condition under
  # which the constructed count used to drift from the computed one.
  groups <- rep(sprintf("G%02d", 1:12), each = 6)
  values <- round(stats::rnorm(length(groups), mean = 10, sd = 1), 0)

  for (rate in c(0.05, 0.25, 0.45, 1)) {
    out <- inject_targeted_runs(values, groups, dTargetRate = rate, nWindowLength = 3)
    realized <- attr(out, "realized")

    expect_equal(
      realized$numerator,
      count_identical_windows_naive(as.numeric(out), groups, 3)
    )
  }
})

test_that("inject_targeted_runs recounts rather than over-reports a single-value group (#143)", {
  # No differing value exists to swap in, so the tie cannot be broken; the
  # recount must report the true (higher) numerator instead of the target.
  values <- rep(5, 5)
  groups <- rep("G1", 5)

  out <- inject_targeted_runs(values, groups, dTargetRate = 0.34, nWindowLength = 3)
  realized <- attr(out, "realized")

  expect_equal(realized$denominator, 3)
  expect_equal(realized$numerator, 3)
})
