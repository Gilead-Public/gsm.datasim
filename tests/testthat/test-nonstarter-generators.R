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
