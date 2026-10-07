# Fixtures for test-ipns-snapshot-integration.R.

subj_seed_config <- function(participant_count = 1, snapshot_count = 2) {
  list(
    SnapshotCount = snapshot_count,
    SnapshotWidth = "months",
    ParticipantCount = participant_count,
    SiteCount = 2,
    StudyID = "IPNS-SNAP",
    workflow_path = "workflow/1_mappings",
    mappings = "AE",
    package = "gsm.mapping",
    strStartDate = "2012-01-01",
    desired_specs = NULL
  )
}

native_subj_config <- function(study_id, participant_count, snapshot_count) {
  config <- create_standard_study_config(
    study_id,
    participant_count = participant_count,
    site_count = 2,
    adverse_events = FALSE, protocol_deviations = FALSE, lab_data = FALSE,
    subject_visits = FALSE, visit_schedule = FALSE, enrollment = TRUE,
    data_changes = FALSE, data_entry = FALSE, queries = FALSE,
    pharmacokinetics = FALSE, study_drug_completion = FALSE,
    study_completion = FALSE, inclusion_exclusion = FALSE, country = FALSE,
    death = FALSE, randomization = FALSE, overall_response = FALSE
  )
  set_temporal_config(
    config,
    start_date = "2012-01-01",
    snapshot_count = snapshot_count,
    snapshot_width = "months"
  )
}

# Generated on first call and reused, so the tests sharing it pay for one run.
.core_run_cache <- new.env(parent = emptyenv())

# Same seed, size and snapshots as gsm.core's data-raw/simulate_longitudinal_data.R.
core_shaped_run <- function() {
  if (is.null(.core_run_cache$run)) {
    set.seed(1234)
    .core_run_cache$run <- suppressWarnings(generate_rawdata_for_single_study(
      SnapshotCount = 3,
      SnapshotWidth = "months",
      ParticipantCount = 1000,
      SiteCount = 150,
      StudyID = "AA-AA-000-0000",
      workflow_path = "workflow/1_mappings",
      mappings = c("SUBJ", "ENROLL", "STUDCOMP", "SITE", "STUDY"),
      package = "gsm.mapping",
      desired_specs = NULL
    ))
  }
  .core_run_cache$run
}

is_blank <- function(x) is.na(x) | x == ""
