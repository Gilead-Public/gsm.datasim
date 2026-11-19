disposition_source_fixture <- function() {
  ids <- sprintf("P-%02d", 1:12)
  list(
    Raw_SUBJ = data.frame(
      studyid = "DISPOSITION",
      subjid = ids,
      subject_nsv = ids,
      invid = rep(c("I1", "I2"), each = 6),
      country = "US",
      enrollyn = c(rep("Y", 4), "N", "Y", "N", NA, "Y", "Y", "Y", "Y"),
      enrolldt = as.Date(rep("2026-01-01", 12))
    ),
    Raw_VISIT = data.frame(
      subjid = ids,
      visit_dt = as.Date(c(
        "2026-08-14", "2026-05-02", "2026-03-01", "2026-03-01",
        "2026-02-01", "2026-03-01", "2026-03-01", "2026-03-01",
        "2026-03-01", "2026-03-01", "2026-03-01", "2026-03-01"
      ))
    ),
    Raw_LB = data.frame(
      subjid = "P-03",
      lbdt = as.Date("2026-03-20")
    ),
    Raw_Death = data.frame(
      subjid = c("P-03", "P-04", "P-03"),
      death_dt = as.Date(c("2026-03-10", "2026-03-10", "2026-03-12"))
    ),
    Raw_Consents = data.frame(
      subjid = "P-05",
      constype = "WITHDRAWAL OF CONSENT",
      conscat = "MAIN STUDY CONSENT",
      cons_dt = as.Date("2026-02-01")
    ),
    Raw_STUDCOMP = data.frame(
      subjid = ids,
      compyn = c(NA, "N", "N", "N", NA, "N", NA, NA, "Y", "N", NA, NA),
      compreas = c(
        NA, rep("Lost to Follow-Up", 3), NA, "Withdrawal by Subject",
        NA, NA, NA, "Physician Decision", NA, NA
      )
    ),
    Raw_AE = data.frame(
      subjid = c("P-11", "P-12"),
      aetoxgr = c(5L, 3L),
      aeout = c(NA, "Fatal"),
      aeen_dt = as.Date(c("2026-02-10", "2026-02-15"))
    )
  )
}

test_that("disposition reproduces P-01 to P-06 and all nine draft statuses (#169)", {
  data <- disposition_source_fixture()
  result <- apply_disposition_derivations(data, as.Date("2026-12-31"))

  expect_equal(result$drv_study_disposition, c(
    "Ongoing", "Lost to Follow-Up", "Death", "Lost to Follow-Up",
    "Withdrawal by Participant", "Blank", "Screen Failure", "In Screening",
    "Study Completion", "Study Discontinuation: Other", "Death", "Death"
  ))
  expect_equal(result$subjid, data$Raw_SUBJ$subjid)
  expect_equal(result$drv_last_contact_dt[3:4], as.Date(c("2026-03-20", "2026-03-10")))
  expect_equal(result$drv_death_dt[3:4], as.Date(rep("2026-03-10", 2)))
  expect_equal(result$drv_withdrawn_dt[5], as.Date("2026-02-01"))
  expect_equal(result$drv_ae_death_dt[11:12], as.Date(c("2026-02-10", "2026-02-15")))
  expect_true(all(is.na(result$rbqm_flags)))
  for (field in c(
    "drv_last_contact_dt", "drv_withdrawn_dt", "drv_death_dt", "drv_ae_death_dt"
  )) {
    expect_s3_class(result[[field]], "Date")
  }
})

test_that("disposition advances on later snapshots without new participants (#169)", {
  data <- disposition_source_fixture()
  early <- apply_disposition_derivations(data, as.Date("2026-01-31"))
  late <- apply_disposition_derivations(
    utils::modifyList(data, list(Raw_SUBJ = early)),
    as.Date("2026-12-31")
  )
  expect_equal(early$subjid, late$subjid)
  expect_equal(early$drv_study_disposition[5], "Screen Failure")
  expect_equal(late$drv_study_disposition[5], "Withdrawal by Participant")
  expect_true(all(is.na(early$drv_withdrawn_dt)))
  expect_true(all(is.na(early$drv_death_dt)))
  expect_true(all(is.na(early$drv_ae_death_dt)))
  expect_true(all(early$drv_last_contact_dt <= as.Date("2026-01-31")))
  expect_true(all(late$drv_last_contact_dt >= early$drv_last_contact_dt))
})

test_that("disposition excludes optional consent and flags duplicated screening IDs (#169)", {
  data <- disposition_source_fixture()
  data$Raw_Consents$conscat <- "OPTIONAL CONSENT"
  data$Raw_SUBJ$subject_nsv[1:2] <- "DUPLICATE"
  result <- apply_disposition_derivations(data, as.Date("2026-12-31"))
  expect_true(all(is.na(result$drv_withdrawn_dt)))
  expect_equal(result$drv_study_disposition[5], "Screen Failure")
  expect_equal(result$rbqm_flags[1:2], rep("Duplicate screening ID", 2))
  expect_true(all(is.na(result$rbqm_flags[-(1:2)])))
})

test_that("disposition handles absent optional domains and empty subjects (#169)", {
  data <- disposition_source_fixture()["Raw_SUBJ"]
  result <- apply_disposition_derivations(data, as.Date("2026-12-31"))
  expect_equal(nrow(result), 12)
  expect_true(all(result$drv_study_disposition %in% c(
    "Ongoing", "Screen Failure", "In Screening"
  )))
  data$Raw_SUBJ <- data$Raw_SUBJ[FALSE, ]
  empty <- apply_disposition_derivations(data, as.Date("2026-12-31"))
  expect_equal(nrow(empty), 0)
  expect_type(empty$drv_study_disposition, "character")
  expect_s3_class(empty$drv_last_contact_dt, "Date")
})

test_that("disposition keeps metadata out of contact dates and uses consent precedence (#169)", {
  data <- disposition_source_fixture()
  data$Raw_Death$mincreated_dts <- as.POSIXct("2026-11-01", tz = "UTC")
  result <- apply_disposition_derivations(data, as.Date("2026-12-31"))
  expect_equal(result$drv_last_contact_dt[4], as.Date("2026-03-10"))
  expect_equal(result$drv_study_disposition[4], "Lost to Follow-Up")

  data$Raw_Consents$subjid <- "P-04"
  data$Raw_Consents$cons_dt <- as.Date("2026-03-10")
  result <- apply_disposition_derivations(data, as.Date("2026-12-31"))
  expect_equal(result$drv_study_disposition[4], "Withdrawal by Participant")
})

test_that("disposition is repeatable and leaves source records and the RNG untouched (#169)", {
  data <- disposition_source_fixture()
  original <- data
  set.seed(169)
  seed <- .Random.seed
  result <- apply_disposition_derivations(data, as.Date("2026-12-31"))
  expect_identical(.Random.seed, seed)
  expect_identical(data, original)
  data$Raw_SUBJ <- result
  expect_equal(
    apply_disposition_derivations(data, as.Date("2026-12-31")),
    result
  )

  data$Raw_STUDCOMP$compreas[6] <- "Death"
  expect_equal(
    apply_disposition_derivations(data, as.Date("2026-12-31"))$drv_study_disposition[6],
    "Blank"
  )
})
