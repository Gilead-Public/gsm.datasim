# Helper to build mock visit data for all subjects × all visit types
mock_visits <- function(subjids, visit_types = c("Screening", paste0("VISIT ", 1:5),
                                                  "End of Treatment", "Follow-up")) {
  data.frame(
    subjid       = rep(subjids, each = length(visit_types)),
    foldername   = rep(visit_types, times = length(subjids)),
    instancename = rep(visit_types, times = length(subjids)),
    visit_dt     = as.character(
      seq(as.Date("2024-01-01"), by = 1, length.out = length(subjids) * length(visit_types))
    ),
    stringsAsFactors = FALSE
  )
}

test_that("Screening visits are kept for every subject", {
  data <- list(
    Raw_VISIT = mock_visits(c("S1", "S2", "S3")),
    Raw_SUBJ  = data.frame(subjid = c("S1", "S2", "S3"),
                           enrollyn = c("Y", "N", "N"),
                           stringsAsFactors = FALSE)
  )

  result <- filter_visits_by_status(data)
  screening <- result[result$foldername == "Screening", ]

  expect_equal(sort(screening$subjid), c("S1", "S2", "S3"))
})

test_that("VISIT 1-5 are only kept for enrolled subjects", {
  data <- list(
    Raw_VISIT = mock_visits(c("S1", "S2", "S3")),
    Raw_SUBJ  = data.frame(subjid = c("S1", "S2", "S3"),
                           enrollyn = c("Y", "N", "Y"),
                           stringsAsFactors = FALSE)
  )

  result <- filter_visits_by_status(data)
  treatment <- result[grepl("^VISIT ", result$foldername), ]

  expect_true(all(treatment$subjid %in% c("S1", "S3")))
  expect_false("S2" %in% treatment$subjid)
})

test_that("End of Treatment is only kept for discontinued subjects (sdrgyn == 'N') (#87, #91)", {
  data <- list(
    Raw_VISIT    = mock_visits(c("S1", "S2", "S3")),
    Raw_SUBJ     = data.frame(subjid = c("S1", "S2", "S3"),
                              enrollyn = c("Y", "Y", "Y"),
                              stringsAsFactors = FALSE),
    Raw_SDRGCOMP = data.frame(subjid = c("S1", "S2", "S3"),
                              sdrgyn = c("Y", "N", "Y"),
                              stringsAsFactors = FALSE)
  )

  result <- filter_visits_by_status(data)
  eot <- result[result$foldername == "End of Treatment", ]

  expect_equal(eot$subjid, "S2")
})

test_that("Follow-up is only kept for study-completed subjects (compyn == 'Y')  (#87, #91)", {
  data <- list(
    Raw_VISIT     = mock_visits(c("S1", "S2", "S3")),
    Raw_SUBJ      = data.frame(subjid = c("S1", "S2", "S3"),
                               enrollyn = c("Y", "Y", "Y"),
                               stringsAsFactors = FALSE),
    Raw_STUDCOMP  = data.frame(subjid = c("S1", "S2", "S3"),
                               compyn = c("N", "Y", NA),
                               stringsAsFactors = FALSE)
  )

  result <- filter_visits_by_status(data)
  followup <- result[result$foldername == "Follow-up", ]

  expect_equal(followup$subjid, "S2")
})

test_that("EoT and Follow-up are removed when SDRGCOMP/STUDCOMP are absent (#87, #91)", {
  data <- list(
    Raw_VISIT = mock_visits(c("S1", "S2")),
    Raw_SUBJ  = data.frame(subjid = c("S1", "S2"),
                           enrollyn = c("Y", "Y"),
                           stringsAsFactors = FALSE)
  )

  result <- filter_visits_by_status(data)

  expect_false("End of Treatment" %in% result$foldername)
  expect_false("Follow-up" %in% result$foldername)
})

test_that("unenrolled subjects only get Screening", {
  data <- list(
    Raw_VISIT    = mock_visits(c("S1")),
    Raw_SUBJ     = data.frame(subjid = "S1", enrollyn = "N",
                              stringsAsFactors = FALSE),
    Raw_SDRGCOMP = data.frame(subjid = "S1", sdrgyn = "N",
                              stringsAsFactors = FALSE),
    Raw_STUDCOMP = data.frame(subjid = "S1", compyn = "Y",
                              stringsAsFactors = FALSE)
  )

  result <- filter_visits_by_status(data)

  # Unenrolled → no treatment visits, but EoT/Follow-up could still apply
  # per the rules (EoT depends on sdrgyn, Follow-up on compyn, not enrollyn)
  expect_true("Screening" %in% result$foldername)
  expect_false(any(grepl("^VISIT ", result$foldername)))
})

test_that("duplicate SDRGCOMP rows do not produce duplicate visits (#87, #91)", {
  data <- list(
    Raw_VISIT    = mock_visits(c("S1")),
    Raw_SUBJ     = data.frame(subjid = "S1", enrollyn = "Y",
                              stringsAsFactors = FALSE),
    Raw_SDRGCOMP = data.frame(subjid = c("S1", "S1"),
                              sdrgyn = c("N", "N"),
                              stringsAsFactors = FALSE)
  )

  result <- filter_visits_by_status(data)
  eot <- result[result$foldername == "End of Treatment", ]

  expect_equal(nrow(eot), 1)
})

test_that("visit dates are unique per subject  (#87, #91)", {
  data <- list(
    Raw_VISIT = mock_visits(c("S1", "S2")),
    Raw_SUBJ  = data.frame(subjid = c("S1", "S2"),
                           enrollyn = c("Y", "Y"),
                           stringsAsFactors = FALSE)
  )

  result <- filter_visits_by_status(data)

  # Check no duplicate dates within each subject
  dupes <- result %>%
    dplyr::group_by(subjid) %>%
    dplyr::filter(duplicated(visit_dt)) %>%
    dplyr::ungroup()

  expect_equal(nrow(dupes), 0)
})
