test_that("update_column adopts the new column when none exists yet", {
  new_col <- list(required = TRUE, type = "character")

  expect_equal(update_column(list(), new_col, "subjid"), new_col)
})

test_that("update_column keeps the existing column when types agree", {
  existing <- list(required = TRUE, type = "character")
  new_col <- list(required = FALSE, type = "character")

  expect_silent(res <- update_column(existing, new_col, "subjid"))
  expect_equal(res, existing)
})

# The only uncovered branch in this file: conflicting declared types warn and
# the first type wins.
test_that("update_column warns on a type mismatch and keeps the first type", {
  existing <- list(required = TRUE, type = "character")
  new_col <- list(required = TRUE, type = "integer")

  expect_warning(
    res <- update_column(existing, new_col, "subjid"),
    "Type mismatch for 'subjid'"
  )
  expect_equal(res$type, "character")
})

test_that("combine_domain unions columns across per-workflow specs", {
  domain_specs <- list(
    list(subjid = list(type = "character")),
    list(invid = list(type = "character"), subjid = list(type = "character"))
  )

  res <- combine_domain(domain_specs)

  expect_setequal(names(res), c("subjid", "invid"))
  expect_equal(res$subjid$type, "character")
})

test_that("CombineSpecs merges multiple domains from workflow objects", {
  workflows <- list(
    wf1 = list(spec = list(
      Raw_SUBJ = list(subjid = list(type = "character")),
      Raw_AE = list(aeser = list(type = "character"))
    )),
    wf2 = list(spec = list(
      Raw_SUBJ = list(invid = list(type = "character"))
    ))
  )

  res <- CombineSpecs(workflows, bIsWorkflow = TRUE)

  expect_setequal(names(res), c("Raw_SUBJ", "Raw_AE"))
  expect_setequal(names(res$Raw_SUBJ), c("subjid", "invid"))
  expect_named(res$Raw_AE, "aeser")
})

# bIsWorkflow = FALSE skips the $spec extraction and treats the input as
# already-unwrapped specs.
test_that("CombineSpecs accepts plain specs when bIsWorkflow is FALSE", {
  specs <- list(
    list(Raw_SUBJ = list(subjid = list(type = "character"))),
    list(Raw_SUBJ = list(invid = list(type = "character")))
  )

  res <- CombineSpecs(specs, bIsWorkflow = FALSE)

  expect_named(res, "Raw_SUBJ")
  expect_setequal(names(res$Raw_SUBJ), c("subjid", "invid"))
})
