make_vs_test_spec <- function() {
  list(
    subjid = list(required = TRUE),
    invid = list(required = TRUE),
    studyid = list(required = TRUE),
    instancename = list(required = TRUE),
    vs_dt = list(required = TRUE),
    vsperf_std = list(required = TRUE),
    weight = list(required = TRUE),
    sysbp = list(required = TRUE),
    diabp = list(required = TRUE)
  )
}

make_vs_test_data <- function(n_subjects = 20, n_visits = 6) {
  subjid <- sprintf("S%04d", seq_len(n_subjects))
  invid <- sprintf("0X%04d", (seq_len(n_subjects) %% 3) + 1)

  visits <- c("Screening", paste0("VISIT ", 1:5))[seq_len(n_visits)]
  raw_visit <- do.call(rbind, lapply(subjid, function(s) {
    data.frame(subjid = s, instancename = visits, stringsAsFactors = FALSE)
  }))

  list(
    Raw_SUBJ = data.frame(subjid = subjid, invid = invid, stringsAsFactors = FALSE),
    Raw_STUDY = data.frame(protocol_number = "PROT-VS"),
    Raw_VISIT = raw_visit
  )
}

test_that("Raw_VS is registered in the domain registry (#113)", {
  registry <- get_domain_registry()

  expect_true("Raw_VS" %in% names(registry))
  expect_equal(registry$Raw_VS$dataset, "Raw_VS")
  expect_true(is.function(registry$Raw_VS$count_fn))
  expect_true(is.function(registry$Raw_VS$generate_fn))
  expect_true(is.character(registry$Raw_VS$required_inputs))
})

test_that("Raw_VS migrated domain adapter generates a data frame with expected columns (#113)", {
  set.seed(3841)

  data <- make_vs_test_data(n_subjects = 20, n_visits = 6)

  context <- list(
    data = data,
    previous_data = list(),
    combined_specs = list(Raw_VS = make_vs_test_spec()),
    n = 20,
    start_date = as.Date("2012-01-01")
  )

  vs_df <- generate_domain_from_registry("Raw_VS", context)

  expect_s3_class(vs_df, "data.frame")
  expect_true(nrow(vs_df) > 0)
  expect_true(all(c(
    "subjid", "invid", "studyid", "instancename",
    "vs_dt", "vsperf_std", "weight", "sysbp", "diabp"
  ) %in% names(vs_df)))

  # One row per subject x visit -- no dot-flattened list columns from
  # the split_vars processing.
  expect_false(any(grepl("\\.", names(vs_df))))
  expect_equal(nrow(vs_df), 20 * 6)
})

test_that("Raw_VS invid is correctly attributed to each subject via Raw_SUBJ lookup (#113)", {
  set.seed(514)

  data <- make_vs_test_data(n_subjects = 10, n_visits = 3)

  context <- list(
    data = data,
    previous_data = list(),
    combined_specs = list(Raw_VS = make_vs_test_spec()),
    n = 10,
    start_date = as.Date("2012-01-01")
  )

  vs_df <- generate_domain_from_registry("Raw_VS", context)

  expected_invid <- data$Raw_SUBJ[match(vs_df$subjid, data$Raw_SUBJ$subjid), "invid"]
  expect_equal(vs_df$invid, expected_invid)
})

test_that("Raw_VS respects the cumulative snapshot pattern via previous_data (#113)", {
  set.seed(9042)

  data <- make_vs_test_data(n_subjects = 20, n_visits = 4)

  context1 <- list(
    data = data,
    previous_data = list(),
    combined_specs = list(Raw_VS = make_vs_test_spec()),
    n = 20,
    start_date = as.Date("2012-01-01")
  )
  vs_snapshot1 <- generate_domain_from_registry("Raw_VS", context1)

  # No new subjects -- second snapshot should return the same rows unchanged.
  context2 <- list(
    data = data,
    previous_data = list(Raw_VS = vs_snapshot1),
    combined_specs = list(Raw_VS = make_vs_test_spec()),
    n = 20,
    start_date = as.Date("2012-02-01")
  )
  vs_snapshot2 <- generate_domain_from_registry("Raw_VS", context2)

  expect_equal(nrow(vs_snapshot2), nrow(vs_snapshot1))
  expect_equal(length(unique(vs_snapshot2$subjid)), 20)
})

test_that(".generate_vital_with_duplicates injects duplicate values per subject (#113)", {
  set.seed(2077)

  n_visits <- 8
  subjects <- rep(sprintf("S%02d", 1:15), each = n_visits)

  values <- gsm.datasim:::.generate_vital_with_duplicates(
    n = length(subjects), subjects = subjects, mean = 75, sd = 10, digits = 1
  )

  expect_length(values, length(subjects))
  expect_true(is.numeric(values))

  # Every subject should have at least one exact-duplicate value among
  # their subsequent (non-first) records.
  has_duplicate <- vapply(unique(subjects), function(subj) {
    subj_values <- values[subjects == subj]
    any(duplicated(subj_values))
  }, logical(1))

  expect_true(all(has_duplicate))
})

test_that(".generate_vital_with_duplicates never marks a subject's first record as the duplicate source of itself (#113)", {
  set.seed(6613)

  subjects <- rep("S01", 5)
  values <- gsm.datasim:::.generate_vital_with_duplicates(
    n = length(subjects), subjects = subjects, mean = 100, sd = 5, digits = 0,
    dDuplicateRate = 0.5
  )

  # Every duplicated value must match a strictly earlier value for the
  # same subject (i.e. duplicates copy history, they don't invent it).
  for (i in seq_along(values)[-1]) {
    if (values[i] %in% values[seq_len(i - 1)]) {
      expect_true(values[i] %in% values[seq_len(i - 1)])
    }
  }
})

test_that(".generate_vital_with_duplicates handles subjects with a single record (#113)", {
  set.seed(777)

  subjects <- c("S01", "S02", "S03")
  values <- gsm.datasim:::.generate_vital_with_duplicates(
    n = length(subjects), subjects = subjects, mean = 75, sd = 10, digits = 1
  )

  expect_length(values, 3)
  expect_true(is.numeric(values))
})

test_that("Raw_VS generates realistic vitals distributions with duplicate injection (#113)", {
  set.seed(1120)

  data <- make_vs_test_data(n_subjects = 40, n_visits = 6)

  context <- list(
    data = data,
    previous_data = list(),
    combined_specs = list(Raw_VS = make_vs_test_spec()),
    n = 40,
    start_date = as.Date("2012-01-01")
  )

  vs_df <- generate_domain_from_registry("Raw_VS", context)

  # Weight, sysbp, diabp should each show some subject-level duplication,
  # consistent with the ~10% duplicate-injection rate.
  for (col in c("weight", "sysbp", "diabp")) {
    dup_present <- vapply(split(vs_df[[col]], vs_df$subjid), function(x) {
      any(duplicated(x))
    }, logical(1))
    expect_true(any(dup_present), info = paste(col, "should show duplicates for at least one subject"))
  }
})

test_that("Raw_VS generates the full 8-vitals superset (height/bmi/pulse/temp/resp) when spec'd (#113)", {
  set.seed(4471)

  data <- make_vs_test_data(n_subjects = 40, n_visits = 6)

  full_spec <- make_vs_test_spec()
  full_spec$height <- list(required = TRUE)
  full_spec$bmi <- list(required = TRUE)
  full_spec$pulse <- list(required = TRUE)
  full_spec$temp <- list(required = TRUE)
  full_spec$resp <- list(required = TRUE)

  context <- list(
    data = data,
    previous_data = list(),
    combined_specs = list(Raw_VS = full_spec),
    n = 40,
    start_date = as.Date("2012-01-01")
  )

  vs_df <- generate_domain_from_registry("Raw_VS", context)

  expect_true(all(c("height", "bmi", "pulse", "temp", "resp") %in% names(vs_df)))
  for (col in c("height", "bmi", "pulse", "temp", "resp")) {
    expect_true(is.numeric(vs_df[[col]]))
    dup_present <- vapply(split(vs_df[[col]], vs_df$subjid), function(x) {
      any(duplicated(x))
    }, logical(1))
    expect_true(any(dup_present), info = paste(col, "should show duplicates for at least one subject"))
  }
})

test_that("prepare_combined_specs_for_generation injects a fallback Raw_VS spec when Raw_VS is requested without a full spec (#113)", {
  combined_specs <- list(
    Raw_STUDY = list(),
    Raw_SITE = list(),
    Raw_SUBJ = list(subjid = list(required = TRUE)),
    Raw_VS = list(weight = list(required = TRUE))
  )

  prepared <- prepare_combined_specs_for_generation(combined_specs)

  expect_true("Raw_VS" %in% names(prepared))
  expect_true(all(c(
    "subjid", "invid", "studyid", "instancename",
    "vs_dt", "vsperf_std", "weight", "sysbp", "diabp"
  ) %in% names(prepared$Raw_VS)))
  # The user-supplied weight entry should be preserved by modifyList(),
  # not silently overwritten with the fallback default.
  expect_equal(prepared$Raw_VS$weight, list(required = TRUE))
})

test_that("prepare_combined_specs_for_generation leaves an already-complete Raw_VS spec untouched (#113)", {
  complete_spec <- list(
    subjid = list(required = TRUE),
    invid = list(required = TRUE),
    studyid = list(required = TRUE),
    instancename = list(required = TRUE),
    vs_dt = list(required = TRUE),
    vsperf_std = list(required = TRUE),
    weight = list(required = TRUE),
    sysbp = list(required = TRUE),
    diabp = list(required = TRUE)
  )
  combined_specs <- list(
    Raw_STUDY = list(),
    Raw_VS = complete_spec
  )

  prepared <- prepare_combined_specs_for_generation(combined_specs)

  expect_identical(prepared$Raw_VS, complete_spec)
})

test_that("prepare_combined_specs_for_generation does not add a Raw_VS spec when Raw_VS is not requested (#113)", {
  combined_specs <- list(
    Raw_STUDY = list(),
    Raw_SUBJ = list(subjid = list(required = TRUE))
  )

  prepared <- prepare_combined_specs_for_generation(combined_specs)

  expect_false("Raw_VS" %in% names(prepared))
})

test_that("Raw_VS registry adapter falls back to a default instancename spec entry when the caller-supplied spec omits it (#113)", {
  set.seed(6284)

  data <- make_vs_test_data(n_subjects = 15, n_visits = 4)

  # Spec deliberately omits `instancename` -- the registry entry should
  # inject its own `required = TRUE` default rather than erroring.
  spec_without_instancename <- make_vs_test_spec()
  spec_without_instancename$instancename <- NULL

  context <- list(
    data = data,
    previous_data = list(),
    combined_specs = list(Raw_VS = spec_without_instancename),
    n = 15,
    start_date = as.Date("2012-01-01")
  )

  vs_df <- generate_domain_from_registry("Raw_VS", context)

  expect_s3_class(vs_df, "data.frame")
  expect_true("instancename" %in% names(vs_df))
  expect_equal(nrow(vs_df), 15 * 4)
})
