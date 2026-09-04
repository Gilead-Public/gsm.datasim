# IP non-starter scenario generators (gsm.datasim#122)
# nonstarter_subjids() is the single shared predicate ("enrolled AND never
# dosed") that seeds the simulated drv_ip_nonstarter_status (#140).

test_that("combined SUBJ generator leaves a deterministic subset of enrolled subjects never dosed (firstdosedate NA) (#122)", {
  set.seed(1234)
  res <- enrollyn_enrolldt_timeonstudy_firstparticipantdate_firstdosedate_timeontreatment(
    n = 200,
    startDate = as.Date("2020-01-01"),
    endDate = as.Date("2021-01-01")
  )
  enrolled <- res$enrollyn == "Y"
  # at least one enrolled subject is a non-starter (enrolled but no first dose)
  expect_true(any(enrolled & is.na(res$firstdosedate)))
  # non-enrolled subjects keep NA enrolldt (unchanged behaviour)
  expect_true(all(is.na(res$enrolldt[!enrolled])))
  # dosed enrolled subjects still have firstparticipantdate == enrolldt
  dosed <- enrolled & !is.na(res$firstdosedate)
  expect_equal(res$firstparticipantdate[dosed], res$enrolldt[dosed])
})

test_that("combined SUBJ generator: nonstarter_rate = 0 keeps every enrolled subject dosed (#122)", {
  set.seed(1234)
  res <- enrollyn_enrolldt_timeonstudy_firstparticipantdate_firstdosedate_timeontreatment(
    n = 200,
    startDate = as.Date("2020-01-01"),
    endDate = as.Date("2021-01-01"),
    nonstarter_rate = 0
  )
  enrolled <- res$enrollyn == "Y"
  expect_true(all(!is.na(res$firstdosedate[enrolled])))
})

test_that("nonstarter_subjids is the shared predicate: enrolled AND firstdosedate NA (#122)", {
  raw_subj <- data.frame(
    subjid = c("S1", "S2", "S3", "S4"),
    enrollyn = c("Y", "Y", "Y", "N"),
    firstdosedate = as.Date(c(NA, "2020-02-01", NA, NA)),
    stringsAsFactors = FALSE
  )
  expect_setequal(nonstarter_subjids(raw_subj), c("S1", "S3"))
})

# The simulator is gsm's only expression of the status model, so these tests
# double as the readable statement of the precedence rules.
make_subj <- function() {
  data.frame(
    subjid = c("S1", "S2", "S3", "S4"),
    enrollyn = c("Y", "Y", "Y", "N"),
    enrolldt = as.Date(c("2025-01-01", "2025-01-01", "2025-03-01", NA)),
    firstdosedate = as.Date(c("2025-01-05", NA, NA, NA)),
    stringsAsFactors = FALSE
  )
}

# The two Potential statuses are only reachable when a subject is not drawn as
# Confirmed, so each branch is driven explicitly by nConfirmedShare rather than
# left to whichever bucket the fixture's subjids happen to land in.
test_that("undosed subjects split on the window when none are Confirmed (#140)", {
  res <- apply_ipns_derivations(
    make_subj(),
    endDate = as.Date("2025-03-15"),
    nConfirmedShare = 0
  )

  expect_equal(res$drv_ip_dosed, c("Y", "N", "N", NA))
  expect_equal(
    res$drv_ip_nonstarter_status,
    c(
      "Dosed", # S1 dosed
      "Potential Non-Starter outside window", # S2 undosed 74 days > 30
      "Potential Non-Starter within window", # S3 undosed 15 days <= 30
      NA_character_ # S4 not enrolled
    )
  )
})

test_that("Confirmed outranks either window status (#140)", {
  res <- apply_ipns_derivations(
    make_subj(),
    endDate = as.Date("2025-03-15"),
    nConfirmedShare = 1
  )

  # The same two undosed subjects, one either side of the window.
  expect_equal(res$drv_ip_nonstarter_status[[1]], "Dosed")
  expect_equal(
    res$drv_ip_nonstarter_status[2:3],
    rep("Confirmed Non-Starter", 2)
  )
})

test_that("non-enrolled subjects carry NA in every drv_ field (#140)", {
  res <- apply_ipns_derivations(make_subj(), endDate = as.Date("2025-03-15"))
  drv <- grep("^drv_", names(res), value = TRUE)

  expect_length(drv, 6)
  expect_true(all(vapply(
    drv,
    function(col) is.na(res[[col]][[4]]),
    logical(1)
  )))
})

test_that("day counts are inclusive and only undosed subjects accrue days (#140)", {
  res <- apply_ipns_derivations(make_subj(), endDate = as.Date("2025-03-15"))

  expect_equal(res$drv_enrl_first_dose_days[[1]], 5L)
  expect_true(is.na(res$drv_enrl_first_dose_days[[2]]))
  expect_true(is.na(res$drv_days_lapsed_since_enrl[[1]]))
  expect_equal(res$drv_days_lapsed_since_enrl[[2]], 74L)
})

test_that("a subject advances within -> outside as snapshots accrue (#140)", {
  df <- make_subj()
  early <- apply_ipns_derivations(
    df,
    as.Date("2025-03-10"),
    nConfirmedShare = 0
  )
  late <- apply_ipns_derivations(df, as.Date("2025-04-30"), nConfirmedShare = 0)

  # S3 enrolled 2025-03-01: inside the window at the earlier snapshot, outside
  # at the later one, with no longitudinal state carried between the two.
  expect_equal(
    early$drv_ip_nonstarter_status[[3]],
    "Potential Non-Starter within window"
  )
  expect_equal(
    late$drv_ip_nonstarter_status[[3]],
    "Potential Non-Starter outside window"
  )
})

test_that("Confirmed does not flip back on a later snapshot (#140)", {
  df <- make_subj()
  early <- apply_ipns_derivations(
    df,
    as.Date("2025-03-10"),
    nConfirmedShare = 1
  )
  late <- apply_ipns_derivations(df, as.Date("2025-04-30"), nConfirmedShare = 1)

  # Confirmed is a function of subjid, not of days lapsed, so crossing the
  # window boundary between snapshots must not change it.
  expect_equal(early$drv_ip_nonstarter_status[[3]], "Confirmed Non-Starter")
  expect_equal(late$drv_ip_nonstarter_status[[3]], "Confirmed Non-Starter")
})

test_that("the Confirmed draw honours nConfirmedShare (#140)", {
  # Guards the hash rather than the status rules: a poorly distributed one
  # makes the realised share a function of subjid length instead of the
  # argument, and nothing else here would notice.
  df <- data.frame(
    subjid = paste0("S", sprintf("%03d", seq_len(2000))),
    enrollyn = "Y",
    enrolldt = as.Date("2025-01-01"),
    firstdosedate = as.Date(NA),
    stringsAsFactors = FALSE
  )

  res <- apply_ipns_derivations(
    df,
    endDate = as.Date("2025-06-01"),
    nConfirmedShare = 0.4
  )

  expect_equal(
    mean(res$drv_ip_nonstarter_status == "Confirmed Non-Starter"),
    0.4,
    tolerance = 0.05
  )
})
