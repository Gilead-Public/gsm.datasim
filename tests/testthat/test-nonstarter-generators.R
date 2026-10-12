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
# double as the readable statement of the precedence rules. make_subj() lives
# in helper-study-fixtures.R.

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

test_that("non-enrolled subjects carry NA in every drv_ field (#140, #157)", {
  res <- apply_ipns_derivations(make_subj(), endDate = as.Date("2025-03-15"))
  drv <- grep("^drv_", names(res), value = TRUE)

  expect_true("drv_kit_assigned" %in% drv)
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
  # Guards the subjid bucketing rather than the status rules: a skewed bucket
  # makes the realised share drift from the argument, and nothing else here
  # would notice.
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

test_that("kit assignment is Y for every dosed subject and NA when not enrolled (#157)", {
  none <- apply_ipns_derivations(make_subj(), as.Date("2025-03-15"), nKitAssignedShare = 0)
  every <- apply_ipns_derivations(make_subj(), as.Date("2025-03-15"), nKitAssignedShare = 1)

  expect_equal(none$drv_kit_assigned, c("Y", "N", "N", NA))
  expect_equal(every$drv_kit_assigned, c("Y", "Y", "Y", NA))
})

test_that("kit assignment does not change with the snapshot date (#157)", {
  expect_identical(
    apply_ipns_derivations(make_subj(), as.Date("2025-03-15"))$drv_kit_assigned,
    apply_ipns_derivations(make_subj(), as.Date("2025-09-15"))$drv_kit_assigned
  )
})

test_that("the kit share among undosed subjects honours nKitAssignedShare (#157)", {
  df <- data.frame(
    subjid = paste0("S", sprintf("%03d", seq_len(10000))),
    enrollyn = "Y",
    enrolldt = as.Date("2025-01-01"),
    firstdosedate = as.Date(NA),
    stringsAsFactors = FALSE
  )

  res <- apply_ipns_derivations(df, as.Date("2025-06-01"), nKitAssignedShare = 0.5)

  expect_equal(mean(res$drv_kit_assigned == "Y"), 0.5, tolerance = 0.05)
})

test_that("each Confirmed non-starter gets a completion record with compyn N and a non-Death reason (#157)", {
  subj <- apply_ipns_derivations(make_subj(), as.Date("2025-03-15"), nConfirmedShare = 1)
  res <- apply_ipns_studcomp(make_studcomp(), subj, as.Date("2025-03-15"))
  conf <- res[res$subjid %in% c("S2", "S3"), ]

  expect_setequal(conf$subjid, c("S2", "S3"))
  expect_equal(conf$compyn, c("N", "N"))
  expect_true(all(conf$compreas %in% c("Withdrew Consent", "Lost to Follow-Up")))
  expect_equal(conf$invid, c("I1", "I1"))
})

test_that("Potential non-starters keep a blank completion value and reason (#157)", {
  subj <- apply_ipns_derivations(make_subj(), as.Date("2025-03-15"), nConfirmedShare = 0)
  studcomp <- make_studcomp()
  studcomp$compyn[2] <- "Y"
  res <- apply_ipns_studcomp(studcomp, subj, as.Date("2025-03-15"))

  expect_true(is.na(res$compyn[res$subjid == "S2"]))
  expect_equal(res$compreas[res$subjid == "S2"], "")
  expect_false("S3" %in% res$subjid)
})

test_that("a dosed subject has a reason exactly when compyn is N, NA reasons included (#157)", {
  subj <- apply_ipns_derivations(make_subj(), as.Date("2025-03-15"))
  completed <- apply_ipns_studcomp(make_studcomp(), subj, as.Date("2025-03-15"))
  studcomp <- make_studcomp()
  studcomp$compyn[1] <- "N"
  studcomp$compreas[1] <- NA
  discontinued <- apply_ipns_studcomp(studcomp, subj, as.Date("2025-03-15"))

  expect_equal(completed$compreas[completed$subjid == "S1"], "")
  expect_true(discontinued$compreas[discontinued$subjid == "S1"] %in%
    c("Lost to Follow-Up", "Death", "Withdrew Consent"))
})

test_that("completion records of non-enrolled subjects are left untouched (#157)", {
  subj <- apply_ipns_derivations(make_subj(), as.Date("2025-03-15"))
  res <- apply_ipns_studcomp(make_studcomp(), subj, as.Date("2025-03-15"))

  expect_equal(res[res$subjid == "S4", ], make_studcomp()[3, ], ignore_attr = TRUE)
})

test_that("completion records fall between the subject's anchor and the snapshot date (#157)", {
  subj <- apply_ipns_derivations(make_subj(), as.Date("2025-03-15"), nConfirmedShare = 1)
  res <- apply_ipns_studcomp(make_studcomp(), subj, as.Date("2025-03-15"))
  created <- setNames(as.Date(res$mincreated_dts), res$subjid)

  expect_equal(created[["S1"]], as.Date("2025-01-06")) # before first dose: moved
  expect_equal(created[["S2"]], as.Date("2025-01-01")) # in range: kept
  expect_equal(created[["S3"]], as.Date("2025-03-04")) # appended
})

test_that("a second pass changes nothing (#157)", {
  subj <- apply_ipns_derivations(make_subj(), as.Date("2025-03-15"), nConfirmedShare = 1)
  once <- apply_ipns_studcomp(make_studcomp(), subj, as.Date("2025-03-15"))

  expect_identical(apply_ipns_studcomp(once, subj, as.Date("2025-03-15")), once)
})

test_that("nConsentWithdrawnShare decides the Confirmed reason (#157)", {
  subj <- apply_ipns_derivations(make_subj(), as.Date("2025-03-15"), nConfirmedShare = 1)
  reasons <- function(share) {
    res <- apply_ipns_studcomp(make_studcomp(), subj, as.Date("2025-03-15"), nConsentWithdrawnShare = share)
    res$compreas[res$subjid %in% c("S2", "S3")]
  }

  expect_equal(reasons(1), rep("Withdrew Consent", 2))
  expect_equal(reasons(0), rep("Lost to Follow-Up", 2))
})

test_that("a study without completion records passes through (#157)", {
  subj <- apply_ipns_derivations(make_subj(), as.Date("2025-03-15"))

  expect_null(apply_ipns_studcomp(NULL, subj, as.Date("2025-03-15")))
})

test_that("completion records with source_col-renamed columns pass through unchanged (#157)", {
  subj <- apply_ipns_derivations(make_subj(), as.Date("2025-03-15"), nConfirmedShare = 1)
  for (col in c("subjid", "compyn", "compreas", "mincreated_dts")) {
    studcomp <- make_studcomp()
    names(studcomp)[names(studcomp) == col] <- toupper(col)

    expect_identical(apply_ipns_studcomp(studcomp, subj, as.Date("2025-03-15")), studcomp)
  }
})

test_that("apply_ipns_studcomp draws no random numbers (#157)", {
  set.seed(1)
  seed <- .Random.seed

  apply_ipns_studcomp(
    make_studcomp(),
    apply_ipns_derivations(make_subj(), as.Date("2025-03-15"), nConfirmedShare = 1),
    as.Date("2025-03-15")
  )
  expect_identical(.Random.seed, seed)
})
