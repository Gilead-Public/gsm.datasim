# Fatal-AE scenarios (gsm.datasim#142). The premature-death report
# (gsm.kri#304) needs simulated AE data carrying outcome (aeout), derived
# relatedness (aereldrv) and serious-death flag (aesdth), plus deterministic
# qualifying and non-qualifying fatal-AE cases linked to Raw_Death.

is_fatal_ae <- function(ae) {
  ae$aeout %in% "Fatal" | as.character(ae$aetoxgr) %in% "5" | ae$aesdth %in% "Yes"
}

make_fatal_ae_fixture <- function() {
  subjids <- sprintf("S%03d", 1:8)
  list(
    Raw_AE = data.frame(
      studyid = "PROT-001",
      subjid = rep(subjids, each = 2),
      aeser = "N",
      aest_dt = as.Date("2020-01-01"),
      aeen_dt = as.Date("2020-01-02"),
      mdrpt_nsv = "term1",
      mdrsoc_nsv = "soc1",
      # S003 carries a random grade-5 row that the "none" scenario must clear.
      aetoxgr = c(1L, 2L, 1L, 2L, 5L, 2L, rep(1L, 10)),
      aeongo = "N",
      aerel = "N",
      aeout = c(rep("Recovered/Resolved", 4), "Fatal", rep("Recovered/Resolved", 11)),
      aereldrv = "No",
      aesdth = c(rep("No", 4), "Yes", rep("No", 11)),
      stringsAsFactors = FALSE
    ),
    Raw_Death = data.frame(
      studyid = "PROT-001",
      subjid = subjids[1:5],
      death_dt = as.Date("2020-03-01") + 0:4,
      deathcls = "Adverse Event",
      stringsAsFactors = FALSE
    )
  )
}

test_that("aeout, aereldrv and aesdth generators sample plausible values (#142)", {
  set.seed(2286)

  out <- aeout(500)
  expect_length(out, 500)
  expect_type(out, "character")
  # Fatal outcomes come only from the deterministic death-linked scenarios.
  expect_false("Fatal" %in% out)

  expect_setequal(unique(aereldrv(500)), c("Yes", "No"))
  expect_equal(aesdth(10), rep("No", 10))
  expect_length(aeout(0), 0)
})

test_that("Raw_AE generates the fatal-AE columns when the spec declares them (#142)", {
  set.seed(2286)
  spec <- make_ae_spec()
  spec$Raw_AE$aeout <- list(required = TRUE, type = "character")
  spec$Raw_AE$aereldrv <- list(required = TRUE, type = "character")
  spec$Raw_AE$aesdth <- list(required = TRUE, type = "character")

  res <- Raw_AE(
    make_ae_data(),
    previous_data = list(),
    spec = spec,
    startDate = as.Date("2012-01-01"),
    endDate = as.Date("2012-12-31"),
    n = 20,
    split_vars = ae_split
  )

  expect_contains(names(res), c("aeout", "aereldrv", "aesdth"))
  expect_in(res$aereldrv, c("Yes", "No"))
  expect_true(all(res$aesdth == "No"))
})

test_that("Raw_AE returns previous data when it already exceeds the target count (#142)", {
  set.seed(2286)
  data <- make_ae_data()
  spec <- make_ae_spec()
  first <- Raw_AE(
    data, list(), spec,
    startDate = as.Date("2012-01-01"), endDate = as.Date("2012-12-31"),
    n = 6, split_vars = ae_split
  )
  # Seeded scenario rows can push the carried-forward frame past the next
  # snapshot's target; that must not request a negative row count.
  again <- Raw_AE(
    data, previous_data = list(Raw_AE = first), spec = spec,
    startDate = as.Date("2012-01-01"), endDate = as.Date("2012-12-31"),
    n = 4, split_vars = ae_split
  )
  expect_identical(again, first)
})

test_that("apply_fatal_ae_scenarios seeds deterministic death-linked cases (#142)", {
  fx <- make_fatal_ae_fixture()
  res <- apply_fatal_ae_scenarios(fx$Raw_AE, fx$Raw_Death)
  fatal <- res[is_fatal_ae(res), ]
  n_fatal <- function(id) sum(fatal$subjid == id)

  # S001: one qualifying fatal AE, related, with a MedDRA term.
  expect_equal(n_fatal("S001"), 1L)
  expect_equal(fatal$aereldrv[fatal$subjid == "S001"], "Yes")
  expect_false(anyNA(fatal$mdrpt_nsv[fatal$subjid == "S001"]))

  # S002: multiple qualifying fatal AEs, one related and one not.
  expect_equal(n_fatal("S002"), 2L)
  expect_setequal(fatal$aereldrv[fatal$subjid == "S002"], c("Yes", "No"))

  # S003: death with no qualifying fatal AE (its random grade-5 row is cleared).
  expect_equal(n_fatal("S003"), 0L)
  expect_true(any(res$subjid == "S003"))

  # S004: one qualifying fatal AE, not related.
  expect_equal(n_fatal("S004"), 1L)
  expect_equal(fatal$aereldrv[fatal$subjid == "S004"], "No")

  # S005: qualifying by grade only; outcome and relatedness missing.
  s005 <- fatal[fatal$subjid == "S005", ]
  expect_equal(nrow(s005), 1L)
  expect_true(is.na(s005$aereldrv))
  expect_true(is.na(s005$aeout))
  expect_equal(s005$aetoxgr, 5L)

  # A participant without a death record still has a fatal AE, so the report
  # can prove it filters to the premature-death population.
  expect_true(any(!fatal$subjid %in% fx$Raw_Death$subjid))
})

test_that("apply_fatal_ae_scenarios dates fatal AEs on or before the death date (#142)", {
  fx <- make_fatal_ae_fixture()
  res <- apply_fatal_ae_scenarios(fx$Raw_AE, fx$Raw_Death)
  seeded <- merge(
    res[is_fatal_ae(res) & res$subjid %in% fx$Raw_Death$subjid, ],
    fx$Raw_Death[, c("subjid", "death_dt")],
    by = "subjid"
  )
  expect_true(all(seeded$aest_dt <= seeded$death_dt))
  expect_true(all(seeded$aeen_dt <= seeded$death_dt))
  expect_true(all(seeded$aeen_dt >= seeded$aest_dt))
})

test_that("apply_fatal_ae_scenarios is idempotent across snapshots (#142)", {
  fx <- make_fatal_ae_fixture()
  once <- apply_fatal_ae_scenarios(fx$Raw_AE, fx$Raw_Death)
  twice <- apply_fatal_ae_scenarios(once, fx$Raw_Death)
  expect_equal(nrow(twice), nrow(once))
  expect_equal(twice, once, ignore_attr = TRUE)
})

test_that("apply_fatal_ae_scenarios preserves column types (#142)", {
  fx <- make_fatal_ae_fixture()
  res <- apply_fatal_ae_scenarios(fx$Raw_AE, fx$Raw_Death)
  expect_equal(lapply(res, class), lapply(fx$Raw_AE, class))

  fx$Raw_AE$aetoxgr <- as.character(fx$Raw_AE$aetoxgr)
  res_chr <- apply_fatal_ae_scenarios(fx$Raw_AE, fx$Raw_Death)
  expect_type(res_chr$aetoxgr, "character")
})

test_that("apply_fatal_ae_scenarios leaves legacy or incomplete inputs unchanged (#142)", {
  fx <- make_fatal_ae_fixture()
  legacy <- fx$Raw_AE[, setdiff(names(fx$Raw_AE), c("aeout", "aereldrv", "aesdth"))]

  expect_identical(apply_fatal_ae_scenarios(legacy, fx$Raw_Death), legacy)
  expect_identical(apply_fatal_ae_scenarios(fx$Raw_AE, NULL), fx$Raw_AE)
  expect_null(apply_fatal_ae_scenarios(NULL, fx$Raw_Death))
})
