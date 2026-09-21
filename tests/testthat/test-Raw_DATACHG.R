test_that("subject_nsv_visit_repeated repeats each visit row n times", {
  visits <- data.frame(
    subject_nsv = c("S1-XXXX", "S2-XXXX"),
    instancename = c("Visit 1", "Visit 2"),
    stringsAsFactors = FALSE
  )

  res <- subject_nsv_visit_repeated(3, visits)

  expect_named(res, c("subject_nsv", "visnam"))
  expect_equal(res$subject_nsv, rep(c("S1-XXXX", "S2-XXXX"), each = 3))
  expect_equal(res$visnam, rep(c("Visit 1", "Visit 2"), each = 3))
})

test_that("form and field tile the form grid across visits", {
  forms <- generate_form_df(8)
  subject_nsv_visits <- data.frame(subject_nsv = c("A", "B"))

  expect_equal(form(1, subject_nsv_visits, forms), rep(forms$form, 2))
  expect_equal(field(1, subject_nsv_visits, forms), rep(forms$field, 2))
})

test_that("n_changes returns integers inside the clamped 0-20 range", {
  set.seed(7238)

  x <- n_changes(500)

  expect_type(x, "integer")
  expect_length(x, 500)
  expect_true(all(x >= 0 & x <= 20))
})

test_that("n_changes accepts a visit key map for site hotspotting", {
  set.seed(7238)
  data <- make_datachg_data()
  visits <- data.frame(
    subject_nsv = data$Raw_SUBJ$subject_nsv,
    instancename = "Visit 1",
    stringsAsFactors = FALSE
  )

  x <- n_changes(
    nrow(visits),
    subject_nsv_visits = visits,
    Raw_SUBJ_data = data$Raw_SUBJ
  )

  expect_type(x, "integer")
  expect_true(all(x >= 0 & x <= 20))
})

test_that("Raw_DATACHG generates a complete dataset from scratch", {
  set.seed(7238)
  data <- make_datachg_data()

  res <- Raw_DATACHG(
    data,
    previous_data = list(),
    spec = make_datachg_spec(),
    startDate = as.Date("2012-01-01"),
    n = 3,
    split_vars = datachg_split
  )

  expect_s3_class(res, "data.frame")
  expect_setequal(
    names(res),
    c("subject_nsv", "visnam", "studyid", "form", "field", "n_changes")
  )
  expect_true(all(res$subject_nsv %in% data$Raw_SUBJ$subject_nsv))
  expect_true(all(res$studyid == "PROT-001"))
  # 32 form/field rows are generated per subject-visit pair.
  expect_equal(nrow(res) %% 32, 0)
})

test_that("Raw_DATACHG returns previous data unchanged when the target count is met", {
  set.seed(7238)
  data <- make_datachg_data()
  spec <- make_datachg_spec()

  first <- Raw_DATACHG(
    data,
    list(),
    spec,
    startDate = as.Date("2012-01-01"),
    n = 3,
    split_vars = datachg_split
  )
  again <- Raw_DATACHG(
    data,
    previous_data = list(Raw_DATACHG = first),
    spec = spec,
    startDate = as.Date("2012-01-01"),
    n = length(unique(first$subject_nsv)),
    split_vars = datachg_split
  )

  expect_identical(again, first)
})

test_that("Raw_DATACHG renames columns per source_col in the spec", {
  set.seed(7238)
  spec <- make_datachg_spec()
  spec$Raw_DATACHG$n_changes$source_col <- "N_CHANGES"

  res <- Raw_DATACHG(
    make_datachg_data(),
    previous_data = list(),
    spec = spec,
    startDate = as.Date("2012-01-01"),
    n = 3,
    split_vars = datachg_split
  )

  expect_true("N_CHANGES" %in% names(res))
  expect_false("n_changes" %in% names(res))
})

# visnam, form and field are injected by the generator when the spec omits
# them, which the fully-populated spec above never exercises.
test_that("Raw_DATACHG injects visnam, form and field when absent from the spec", {
  set.seed(7238)
  spec <- list(
    Raw_DATACHG = list(
      subject_nsv = list(required = TRUE, type = "character"),
      studyid = list(required = TRUE, type = "character"),
      n_changes = list(required = TRUE, type = "integer")
    )
  )

  res <- Raw_DATACHG(
    make_datachg_data(),
    previous_data = list(),
    spec = spec,
    startDate = as.Date("2012-01-01"),
    n = 3,
    split_vars = datachg_split
  )

  expect_true(all(c("visnam", "form", "field") %in% names(res)))
  expect_true(all(grepl("^form", res$form)))
  expect_true(all(grepl("^field", res$field)))
})
