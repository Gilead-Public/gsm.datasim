disposition_generation_specs <- function() {
  study <- make_study_spec()
  study$Raw_STUDY$studyid$source_col <- "protocol_number"
  study$Raw_STUDY$protocol_number <- NULL
  site <- make_site_spec()
  site$Raw_SITE$invid$source_col <- "pi_number"
  site$Raw_SITE$Country$source_col <- "country"
  c(
    study,
    site,
    list(Raw_SUBJ = list(
      studyid = list(type = "character"),
      subjid = list(type = "character"),
      subject_nsv = list(type = "character"),
      invid = list(type = "character"),
      country = list(type = "character"),
      enrollyn = list(type = "character"),
      timeonstudy = list(type = "integer"),
      firstparticipantdate = list(type = "Date"),
      firstdosedate = list(type = "Date"),
      timeontreatment = list(type = "integer")
    )),
    make_enroll_spec(),
    make_visit_spec(),
    make_studcomp_spec(),
    make_consents_spec()
  )
}

test_that("both generation loops emit disposition on every snapshot (#169)", {
  test_at_log_threshold()
  specs <- disposition_generation_specs()
  set.seed(42)
  legacy <- generate_snapshots_from_combined_specs(
    SnapshotCount = 2,
    SnapshotWidth = "months",
    ParticipantCount = 20,
    SiteCount = 2,
    StudyID = "DISPOSITION",
    combined_specs = specs,
    mappings = c("SUBJ", "ENROLL", "VISIT", "STUDCOMP", "Consents"),
    strStartDate = "2026-01-01"
  )
  config <- list(
    temporal_config = list(
      start_date = "2026-01-01",
      snapshot_count = 2,
      snapshot_width = "months"
    ),
    study_params = list(
      study_id = "DISPOSITION", participant_count = 20, site_count = 2
    )
  )
  set.seed(42)
  native <- run_domain_generation_loop(specs, config, source_domains = "SUBJ")
  for (snapshots in list(legacy, native)) {
    expect_length(snapshots, 2)
    for (date in names(snapshots)) {
      data <- snapshots[[date]]
      result <- data$Raw_SUBJ
      expect_true(all(c(
        "drv_last_contact_dt", "drv_withdrawn_dt", "drv_death_dt",
        "drv_ae_death_dt", "drv_study_disposition", "rbqm_flags"
      ) %in% names(result)))
      expect_false(anyNA(result$drv_study_disposition))
      expect_true(all(result$drv_last_contact_dt <= as.Date(date), na.rm = TRUE))
      expect_equal(
        result,
        apply_disposition_derivations(data, as.Date(date))
      )
    }
    expect_true(any(snapshots[[2]]$Raw_SUBJ$enrollyn %in% "N"))
  }
})

test_that("workflow-driven generation also adds disposition fields (#169)", {
  test_at_log_threshold()
  specs <- disposition_generation_specs()
  set.seed(42)
  data <- .generate_single_snapshot(
    combined_specs = specs,
    domain_n = .resolve_domain_counts(names(specs), 20, 2),
    registry = get_domain_registry(),
    start_date = as.Date("2026-01-01"),
    end_date = as.Date("2026-01-29"),
    snapshot_idx = 1,
    snapshot_count = 1,
    snapshot_width = "months",
    study_id = "DISPOSITION",
    previous_data = list()
  )
  expect_false(anyNA(data$Raw_SUBJ$drv_study_disposition))
  expect_equal(
    data$Raw_SUBJ,
    apply_disposition_derivations(data, as.Date("2026-01-29"))
  )
})
