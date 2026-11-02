test_that("tiver returns a constant and ietestcd samples the incl/excl codes", {
  set.seed(5140)

  expect_equal(tiver(4), rep("A1", 4))

  codes <- ietestcd(500)
  expect_length(codes, 500)
  expect_in(codes, c(paste0("incl", 1:10), paste0("excl", 1:10)))
})

test_that("ieorres samples Yes/No and iecat classifies on the INC prefix", {
  set.seed(5140)

  expect_setequal(unique(ieorres(500, iecode = NULL)), c("Yes", "No"))

  # iecat keys off an uppercase "INC" substring; the lowercase codes that
  # ietestcd actually emits therefore all fall through to "Exclusion".
  expect_equal(
    iecat(3, iecode = c("INC1", "EXC1", "incl1")),
    c("Inclusion", "Exclusion", "Exclusion")
  )
})

test_that("ietest expands the INC/EXC prefixes into words", {
  expect_equal(
    ietest(2, iecode = c("INC1", "EXC2")),
    c("Inclusion 1", "Exclusion 2")
  )
})

test_that("tiver_ietestcd_ietest_ieorres_iecat returns all five aligned fields", {
  set.seed(5140)

  res <- tiver_ietestcd_ietest_ieorres_iecat(10)

  expect_named(res, c("tiver", "ietestcd", "ietest", "ieorres", "iecat"))
  expect_length(res$ietestcd, 10)
  expect_in(res$iecat, c("Inclusion", "Exclusion"))
})

test_that("subject_to_ie returns the full subject pool when no previous data exists", {
  data <- make_ie_data(10)

  res <- subject_to_ie(5, data, previous_data = character(0))

  expect_named(res, "subjid")
  expect_equal(nrow(res), 10)
})

test_that("subject_to_ie excludes subjects already assessed", {
  data <- make_ie_data(10)

  res <- subject_to_ie(5, data, previous_data = c("S0001", "S0002"))

  expect_named(res, "subjid")
  expect_equal(nrow(res), 8)
  expect_disjoint(res$subjid, c("S0001", "S0002"))
})

test_that("Raw_IE generates a complete dataset from scratch", {
  set.seed(5140)
  data <- make_ie_data()

  res <- Raw_IE(
    data,
    previous_data = list(),
    spec = make_ie_spec(),
    n_IE = 30,
    split_vars = ie_split
  )

  expect_s3_class(res, "data.frame")
  expect_setequal(
    names(res),
    c("studyid", "subjid", "tiver", "ietestcd", "ietest", "ieorres", "iecat")
  )
  expect_in(res$subjid, data$Raw_SUBJ$subjid)
  expect_true(all(res$studyid == "PROT-001"))
  expect_true(all(res$tiver == "A1"))
})

test_that("Raw_IE returns previous data unchanged when the target count is met", {
  set.seed(5140)
  data <- make_ie_data()
  spec <- make_ie_spec()

  first <- Raw_IE(data, list(), spec, n_IE = 30, split_vars = ie_split)
  again <- Raw_IE(
    data,
    previous_data = list(Raw_IE = first),
    spec = spec,
    n_IE = nrow(first),
    split_vars = ie_split
  )

  expect_identical(again, first)
})

test_that("Raw_IE renames columns per source_col in the spec", {
  set.seed(5140)
  spec <- make_ie_spec()
  spec$Raw_IE$ieorres$source_col <- "IEORRES"

  res <- Raw_IE(
    make_ie_data(),
    previous_data = list(),
    spec = spec,
    n_IE = 30,
    split_vars = ie_split
  )

  expect_contains(names(res), "IEORRES")
  expect_disjoint(names(res), "ieorres")
})
