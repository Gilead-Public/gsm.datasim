test_that("subjid_repeated and invid_repeated repeat each value n times", {
  expect_equal(
    subjid_repeated(3, c("S1", "S2"))$subjid,
    rep(c("S1", "S2"), each = 3)
  )
  expect_equal(
    invid_repeated(2, c("0X001", "0X002"))$invid,
    rep(c("0X001", "0X002"), each = 2)
  )
})

test_that("foldername and instancename tile the visit grid across subjects", {
  possible_visits <- data.frame(
    foldername = c("Screening", "VISIT 1"),
    instancename = c("Screening", "VISIT 1"),
    stringsAsFactors = FALSE
  )
  subjs <- c("S1", "S2", "S3")

  expect_equal(
    foldername(1, subjs, possible_visits),
    rep(possible_visits$foldername, 3)
  )
  expect_equal(
    instancename(1, subjs, possible_visits),
    rep(possible_visits$instancename, 3)
  )
  # `visit` is an alias for foldername.
  expect_identical(visit, foldername)
})

# generate_consecutive_random_dates() formats its output with format(), so
# visit_dt yields character strings rather than Date objects.
test_that("visit_dt produces consecutive date strings repeated per subject", {
  set.seed(2731)
  possible_visits <- data.frame(
    foldername = c("Screening", "VISIT 1", "VISIT 2")
  )

  d <- visit_dt(2, as.Date("2012-01-01"), possible_visits, "months")

  expect_type(d, "character")
  expect_length(d, 6)
  # Dates ascend within each subject's block of visits.
  expect_true(all(diff(as.Date(d[1:3])) > 0))
  # The same visit schedule repeats for the second subject.
  expect_equal(d[1:3], d[4:6])
})

test_that("Raw_VISIT generates a complete dataset from scratch", {
  set.seed(2731)
  data <- make_visit_data()

  res <- Raw_VISIT(
    data,
    previous_data = list(),
    spec = make_visit_spec(),
    startDate = as.Date("2012-01-01"),
    SnapshotWidth = "months",
    n = 3,
    split_vars = visit_split
  )

  expect_s3_class(res, "data.frame")
  expect_true(all(
    c(
      "subjid",
      "invid",
      "studyid",
      "foldername",
      "instancename",
      "visit_dt"
    ) %in%
      names(res)
  ))
  expect_true(all(res$subjid %in% data$Raw_SUBJ$subjid))
  expect_true(all(res$studyid == "PROT-001"))
  # Eight visits are generated per subject.
  expect_equal(nrow(res), 3 * 8)
})

# When the spec omits these columns the generator injects them itself, a branch
# the fully-populated spec never reaches.
test_that("Raw_VISIT injects subjid, studyid, invid and the visit columns when absent", {
  set.seed(2731)
  spec <- list(
    Raw_VISIT = list(some_other_col = list(required = TRUE, type = "character"))
  )

  res <- Raw_VISIT(
    make_visit_data(),
    previous_data = list(),
    spec = spec,
    startDate = as.Date("2012-01-01"),
    SnapshotWidth = "months",
    n = 2,
    split_vars = visit_split
  )

  expect_true(all(
    c(
      "subjid",
      "invid",
      "studyid",
      "foldername",
      "instancename",
      "visit_dt"
    ) %in%
      names(res)
  ))
})

# A spec column carrying source_col = "foldername" is renamed to foldername on
# output, so the generator must not also generate a foldername of its own --
# otherwise the domain would end up with two columns competing for the name.
test_that("Raw_VISIT does not double-generate foldername when a source_col supplies it", {
  set.seed(2731)
  spec <- list(
    Raw_VISIT = list(
      subjid = list(required = TRUE, type = "character"),
      invid = list(required = TRUE, type = "character"),
      studyid = list(required = TRUE, type = "character"),
      visit_dt = list(required = TRUE, type = "Date"),
      VISNAM = list(
        required = TRUE,
        type = "character",
        source_col = "foldername"
      ),
      VISINST = list(
        required = TRUE,
        type = "character",
        source_col = "instancename"
      )
    )
  )

  res <- Raw_VISIT(
    make_visit_data(),
    previous_data = list(),
    spec = spec,
    startDate = as.Date("2012-01-01"),
    SnapshotWidth = "months",
    n = 2,
    split_vars = visit_split
  )

  # The renamed columns land under their source_col names, exactly once each.
  expect_equal(sum(names(res) == "foldername"), 1)
  expect_equal(sum(names(res) == "instancename"), 1)
  expect_false("VISNAM" %in% names(res))
  expect_false("VISINST" %in% names(res))
})

test_that("Raw_VISIT draws new subjects when appending to previous data", {
  set.seed(2731)
  data <- make_visit_data()
  spec <- make_visit_spec()

  first <- Raw_VISIT(
    data,
    list(),
    spec,
    startDate = as.Date("2012-01-01"),
    SnapshotWidth = "months",
    n = 3,
    split_vars = visit_split
  )
  second <- Raw_VISIT(
    data,
    previous_data = list(Raw_VISIT = first),
    spec = spec,
    startDate = as.Date("2012-01-01"),
    SnapshotWidth = "months",
    n = 5,
    split_vars = visit_split
  )

  expect_equal(nrow(second), 5 * 8)
  expect_equal(second[seq_len(nrow(first)), ], first)
  expect_length(unique(second$subjid), 5)
})

test_that("Raw_VISIT returns previous data unchanged when the target count is met", {
  set.seed(2731)
  data <- make_visit_data()
  spec <- make_visit_spec()

  first <- Raw_VISIT(
    data,
    list(),
    spec,
    startDate = as.Date("2012-01-01"),
    SnapshotWidth = "months",
    n = 3,
    split_vars = visit_split
  )
  again <- Raw_VISIT(
    data,
    previous_data = list(Raw_VISIT = first),
    spec = spec,
    startDate = as.Date("2012-01-01"),
    SnapshotWidth = "months",
    n = length(unique(first$subjid)),
    split_vars = visit_split
  )

  expect_identical(again, first)
})
