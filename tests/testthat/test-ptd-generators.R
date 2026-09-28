# Dosed subjects S1..Sn, enrolled 2025-01-01 and dosed 0-4 days later; n = 10000
# spreads subjids evenly over every digit bucket the derivation reads.
make_dosed_subj <- function(n = 10000) {
  df <- data.frame(
    studyid = "X",
    invid = paste0("I", seq_len(n) %% 50),
    subjid = paste0("S", seq_len(n)),
    enrollyn = "Y",
    enrolldt = as.Date("2025-01-01"),
    firstdosedate = as.Date("2025-01-01") + seq_len(n) %% 5,
    stringsAsFactors = FALSE
  )
  apply_ipns_derivations(df, as.Date("2026-01-01"))
}

ptd_cols <- c(
  "drv_treatment_discontinuation_dt",
  "drv_premature_discontinuation_reason",
  "drv_days_lapsed_enrl_discontinuation"
)

test_that("non-dosed and non-enrolled subjects carry NA in all three fields (#138)", {
  subj <- apply_ipns_derivations(make_subj(), as.Date("2025-03-15"))
  res <- apply_ptd_derivations(subj, as.Date("2025-03-15"))

  expect_true(all(is.na(res[2:4, ptd_cols])))
})

test_that("days lapsed is NA exactly when the date is NA and otherwise counts from enrollment inclusively (#138)", {
  res <- apply_ptd_derivations(make_dosed_subj(), as.Date("2026-01-01"))
  dt <- res$drv_treatment_discontinuation_dt
  dated <- !is.na(dt)

  expect_s3_class(dt, "Date")
  expect_type(res$drv_days_lapsed_enrl_discontinuation, "integer")
  expect_identical(is.na(res$drv_days_lapsed_enrl_discontinuation), !dated)
  expect_equal(
    res$drv_days_lapsed_enrl_discontinuation[dated],
    as.integer(dt[dated] - res$drv_enrollment_dt[dated]) + 1L
  )
})

test_that("discontinuation falls between first dose and the snapshot date (#138)", {
  end <- as.Date("2025-01-20")
  res <- apply_ptd_derivations(make_dosed_subj(), end)
  dt <- res$drv_treatment_discontinuation_dt
  dated <- !is.na(dt)

  expect_gt(sum(dated), 0)
  expect_true(all(
    dt[dated] >= res$drv_ip_first_dose_dt[dated] & dt[dated] <= end
  ))
})

test_that("nDiscontinuedShare sets the share of dosed subjects who discontinue (#138)", {
  subj <- make_dosed_subj()
  none <- apply_ptd_derivations(
    subj,
    as.Date("2026-01-01"),
    nDiscontinuedShare = 0
  )
  some <- apply_ptd_derivations(
    subj,
    as.Date("2026-01-01"),
    nDiscontinuedShare = 0.3
  )

  expect_true(all(is.na(none$drv_treatment_discontinuation_dt)))
  expect_equal(
    mean(!is.na(some$drv_treatment_discontinuation_dt)),
    0.3,
    tolerance = 0.05
  )
})

test_that("a later snapshot keeps every date and reason already present (#138)", {
  subj <- make_dosed_subj()
  early <- apply_ptd_derivations(subj, as.Date("2025-01-15"))
  late <- apply_ptd_derivations(subj, as.Date("2025-06-01"))
  kept <- !is.na(early$drv_treatment_discontinuation_dt)

  expect_gt(sum(kept), 0)
  expect_gt(sum(!is.na(late$drv_treatment_discontinuation_dt)), sum(kept))
  expect_identical(
    late$drv_treatment_discontinuation_dt[kept],
    early$drv_treatment_discontinuation_dt[kept]
  )
  expect_identical(
    late$drv_premature_discontinuation_reason[kept],
    early$drv_premature_discontinuation_reason[kept]
  )
})

test_that("the derivation leaves drv_ip_dosed alone and adds no discontinuation flag (#138)", {
  subj <- make_dosed_subj()
  res <- apply_ptd_derivations(subj, as.Date("2026-01-01"))

  expect_identical(res$drv_ip_dosed, subj$drv_ip_dosed)
  expect_false("drv_premature_discont" %in% names(res))
})

test_that("reasons cover NA, a comma-joined pair and single vocabulary values (#138)", {
  res <- apply_ptd_derivations(make_dosed_subj(), as.Date("2026-01-01"))
  dated <- !is.na(res$drv_treatment_discontinuation_dt)
  reason <- res$drv_premature_discontinuation_reason
  pairs <- strsplit(reason[dated & grepl(", ", reason)], ", ", fixed = TRUE)

  expect_true(any(dated & is.na(reason)))
  expect_gt(length(pairs), 0)
  expect_true(all(lengths(pairs) == 2 & !vapply(pairs, anyDuplicated, 0L)))
  expect_true(all(
    unlist(strsplit(stats::na.omit(reason), ", ", fixed = TRUE)) %in%
      ptd_reason_values
  ))
})

test_that("some dosed subjects have a reason but no discontinuation date (#138)", {
  res <- apply_ptd_derivations(make_dosed_subj(), as.Date("2026-01-01"))

  expect_true(any(
    is.na(res$drv_treatment_discontinuation_dt) &
      !is.na(res$drv_premature_discontinuation_reason)
  ))
})

test_that("a subject dosed and discontinued on the enrollment day has one day lapsed (#138)", {
  # S1 falls in the discontinuing and zero-lag buckets.
  subj <- data.frame(
    studyid = "X",
    invid = "I1",
    subjid = "S1",
    enrollyn = "Y",
    enrolldt = as.Date("2025-01-01"),
    firstdosedate = as.Date("2025-01-01"),
    stringsAsFactors = FALSE
  )
  res <- apply_ptd_derivations(
    apply_ipns_derivations(subj, as.Date("2025-02-01")),
    as.Date("2025-02-01")
  )

  expect_equal(res$drv_treatment_discontinuation_dt, as.Date("2025-01-01"))
  expect_equal(res$drv_days_lapsed_enrl_discontinuation, 1L)
})

test_that("the PTD derivation and the four drv_ generators draw no random numbers (#138)", {
  subj <- apply_ipns_derivations(make_subj(), as.Date("2025-03-15"))
  set.seed(1)
  seed <- .Random.seed

  apply_ptd_derivations(subj, as.Date("2025-03-15"))
  expect_identical(.Random.seed, seed)

  for (gen in list(
    drv_kit_assigned,
    drv_treatment_discontinuation_dt,
    drv_premature_discontinuation_reason,
    drv_days_lapsed_enrl_discontinuation
  )) {
    gen(5, as.Date("2025-01-01"))
    expect_identical(.Random.seed, seed)
  }
})
