# IP non-starter scenario generators (gsm.datasim#122)
# The confirmed-non-starter markers must be cross-domain consistent: the same
# subjid set carries firstdosedate = NA (SUBJ), sdrgreas = the coded reason
# (SDRGCOMP), and a present colendat (STUDCOMP), keyed off one shared predicate.

NEVER_DOSED <- "Subject Never Dosed with Study Drug"

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

test_that("apply_nonstarter_sdrgreas marks non-starters with the coded reason, others benign (#122)", {
  raw_subj <- data.frame(
    subjid = c("S1", "S2", "S3"),
    enrollyn = c("Y", "Y", "Y"),
    firstdosedate = as.Date(c(NA, "2020-02-01", NA)),
    stringsAsFactors = FALSE
  )
  df <- data.frame(subjid = c("S1", "S2", "S3"), stringsAsFactors = FALSE)
  set.seed(1)
  out <- apply_nonstarter_sdrgreas(df, raw_subj)
  expect_true("sdrgreas" %in% names(out))
  expect_type(out$sdrgreas, "character")
  expect_equal(out$sdrgreas[out$subjid == "S1"], NEVER_DOSED)
  expect_equal(out$sdrgreas[out$subjid == "S3"], NEVER_DOSED)
  expect_false(out$sdrgreas[out$subjid == "S2"] == NEVER_DOSED)
})

test_that("apply_nonstarter_colendat gives non-starters a present Date and others NA (#122)", {
  raw_subj <- data.frame(
    subjid = c("S1", "S2", "S3"),
    enrollyn = c("Y", "Y", "Y"),
    firstdosedate = as.Date(c(NA, "2020-02-01", NA)),
    stringsAsFactors = FALSE
  )
  df <- data.frame(subjid = c("S1", "S2", "S3"), stringsAsFactors = FALSE)
  out <- apply_nonstarter_colendat(df, raw_subj)
  expect_s3_class(out$colendat, "Date")
  expect_false(is.na(out$colendat[out$subjid == "S1"]))
  expect_false(is.na(out$colendat[out$subjid == "S3"]))
  expect_true(is.na(out$colendat[out$subjid == "S2"]))
})

test_that("cross-domain consistency: sdrgreas 'never dosed' and colendat-present agree on the same subjids (#122)", {
  raw_subj <- data.frame(
    subjid = c("S1", "S2", "S3", "S4"),
    enrollyn = c("Y", "Y", "Y", "N"),
    firstdosedate = as.Date(c(NA, "2020-02-01", NA, NA)),
    stringsAsFactors = FALSE
  )
  df_sdrg <- data.frame(subjid = c("S1", "S2", "S3"), stringsAsFactors = FALSE)
  df_stud <- data.frame(subjid = c("S1", "S2", "S3"), stringsAsFactors = FALSE)
  set.seed(1)
  sdrg <- apply_nonstarter_sdrgreas(df_sdrg, raw_subj)
  stud <- apply_nonstarter_colendat(df_stud, raw_subj)
  never <- sort(sdrg$subjid[sdrg$sdrgreas == NEVER_DOSED])
  colend <- sort(stud$subjid[!is.na(stud$colendat)])
  expect_equal(never, colend)
  expect_equal(never, c("S1", "S3"))
})
