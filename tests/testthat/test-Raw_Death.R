death_classes <- c(
  "Progressive Disease", "Adverse Event", "Disease Recurrence",
  "Not related to disease",
  "Related to long-term follow-up and not related to study drug"
)

make_death_test_spec <- function() {
  list(
    Raw_Death = list(
      subjid = list(required = TRUE, type = "character"),
      studyid = list(required = TRUE, type = "character"),
      death_dt = list(required = TRUE, type = "Date"),
      deathcls = list(required = TRUE, type = "character")
    )
  )
}

make_death_test_data <- function(n_subjects = 30) {
  list(
    Raw_SUBJ = data.frame(subjid = sprintf("S%04d", seq_len(n_subjects))),
    Raw_STUDY = data.frame(protocol_number = "PROT-001")
  )
}

test_that("death_dt generates dates within the 28-day window from start (#120)", {
  set.seed(101)
  start <- as.Date("2020-06-15")

  d <- death_dt(50, start)

  expect_s3_class(d, "Date")
  expect_length(d, 50)
  expect_true(all(d >= start & d <= start + 27))
})

test_that("deathcls samples only the expected death classifications (#120)", {
  set.seed(101)

  x <- deathcls(1000)

  expect_type(x, "character")
  expect_length(x, 1000)
  expect_setequal(unique(x), death_classes)
  expect_equal(names(which.max(table(x))), "Progressive Disease")
})

test_that("deathcls handles edge-case sizes (#120)", {
  expect_length(deathcls(0), 0)

  single <- deathcls(1)
  expect_length(single, 1)
  expect_true(single %in% death_classes)
})

test_that("Raw_Death generates a complete dataset from scratch (#120)", {
  set.seed(42)
  spec <- make_death_test_spec()
  data <- make_death_test_data()
  start_date <- as.Date("2012-01-01")

  res <- Raw_Death(data, previous_data = list(), spec = spec, startDate = start_date, n = 5)

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 5)
  expect_named(res, c("subjid", "studyid", "death_dt", "deathcls"))
  expect_true(all(res$subjid %in% data$Raw_SUBJ$subjid))
  expect_equal(anyDuplicated(res$subjid), 0)
  expect_true(all(res$studyid == "PROT-001"))
  expect_s3_class(res$death_dt, "Date")
  expect_true(all(res$death_dt >= start_date & res$death_dt <= start_date + 27))
  expect_type(res$deathcls, "character")
  expect_true(all(res$deathcls %in% death_classes))
})

test_that("Raw_Death appends only the delta rows to previous data (#120)", {
  set.seed(42)
  spec <- make_death_test_spec()
  data <- make_death_test_data()
  start_date <- as.Date("2012-01-01")

  first <- Raw_Death(data, previous_data = list(), spec = spec, startDate = start_date, n = 3)
  second <- Raw_Death(
    data,
    previous_data = list(Raw_Death = first),
    spec = spec,
    startDate = start_date,
    n = 7
  )

  expect_equal(nrow(second), 7)
  expect_equal(second[seq_len(3), ], first)
  expect_true(all(second$deathcls %in% death_classes))
})

test_that("Raw_Death returns previous data unchanged when target count is met (#120)", {
  set.seed(42)
  spec <- make_death_test_spec()
  data <- make_death_test_data()
  start_date <- as.Date("2012-01-01")

  first <- Raw_Death(data, previous_data = list(), spec = spec, startDate = start_date, n = 4)
  again <- Raw_Death(
    data,
    previous_data = list(Raw_Death = first),
    spec = spec,
    startDate = start_date,
    n = 4
  )

  expect_identical(again, first)
})

test_that("Raw_Death renames columns per source_col in the spec (#120)", {
  set.seed(42)
  spec <- make_death_test_spec()
  spec$Raw_Death$deathcls$source_col <- "DEATHCLS"
  data <- make_death_test_data()

  res <- Raw_Death(data, previous_data = list(), spec = spec, startDate = as.Date("2012-01-01"), n = 5)

  expect_true("DEATHCLS" %in% names(res))
  expect_false("deathcls" %in% names(res))
  expect_true(all(res$DEATHCLS %in% death_classes))
})

test_that("domain registry exposes Raw_Death with expected schema and count function (#120)", {
  registry <- get_domain_registry()

  expect_true("Raw_Death" %in% names(registry))

  entry <- registry$Raw_Death
  expect_equal(sort(names(entry)), sort(c("dataset", "required_inputs", "count_fn", "generate_fn")))
  expect_equal(entry$dataset, "Raw_Death")
  expect_true(is.function(entry$count_fn))
  expect_true(is.function(entry$generate_fn))

  counts <- list(subject_count = c(85, 170, 200))
  expect_equal(entry$count_fn(counts, 1), 1)
  expect_equal(entry$count_fn(counts, 2), 2)
  expect_equal(entry$count_fn(counts, 3), 3)
})

test_that("Raw_Death registry adapter generates and increments data (#120)", {
  set.seed(42)
  context <- list(
    data = make_death_test_data(),
    previous_data = list(),
    combined_specs = make_death_test_spec(),
    n = 5,
    start_date = as.Date("2012-01-01")
  )

  df <- generate_domain_from_registry("Raw_Death", context)

  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 5)
  expect_named(df, c("subjid", "studyid", "death_dt", "deathcls"))
  expect_true(all(df$deathcls %in% death_classes))

  context$previous_data <- list(Raw_Death = df)
  context$n <- 8
  df2 <- generate_domain_from_registry("Raw_Death", context)

  expect_equal(nrow(df2), 8)
  expect_equal(df2[seq_len(5), ], df)

  context$previous_data <- list(Raw_Death = df2)
  df3 <- generate_domain_from_registry("Raw_Death", context)

  expect_equal(df3, df2)
})
