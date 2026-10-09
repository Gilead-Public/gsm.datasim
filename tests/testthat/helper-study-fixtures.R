# Fixtures for study-level tests: longitudinal studies, workflow mocks,
# and small hand-built Raw_SUBJ / workflow frames.

fake_raw_data <- function(n = 2) {
  out <- lapply(seq_len(n), function(i) {
    list(Raw_SUBJ = data.frame(subjid = 1:3), Raw_SITE = data.frame(invid = 1:2))
  })
  names(out) <- paste0("2023-0", seq_len(n), "-28")
  out
}

fake_study <- function(analytics = NULL, reporting = NULL, domains = c("AE", "LB")) {
  study <- create_longitudinal_study_data(
    "FAKE", fake_raw_data(2),
    list(participants = 10, sites = 2, domains = domains)
  )
  study$analytics <- analytics
  study$reporting <- reporting
  study
}

make_test_study <- function(n = 4) {
  raw <- lapply(seq_len(n), function(i) list(Raw_AE = data.frame(i = i), Raw_LB = data.frame(i = i)))
  names(raw) <- paste0("2023-0", seq_len(n), "-01")
  create_longitudinal_study_data(
    "ST", raw,
    list(participants = 10, sites = 2, snapshots = n, interval = "1 month", domains = c("AE", "LB"))
  )
}

# Fixture shared by the column_overrides integration tests in
# test-generate_data_from_workflows.R: a minimal fake workflow exposing a
# single Raw_CUSTOM domain with a numeric and a character column.
make_override_workflows <- function() {
  list(
    wf1 = list(
      meta = list(),
      spec = list(
        Raw_CUSTOM = list(
          base_val = list(type = "numeric"),
          label = list(type = "character")
        )
      ),
      steps = list()
    )
  )
}

# ---- IP non-starter / premature treatment discontinuation fixtures ----
# (test-nonstarter-generators.R, test-ptd-generators.R, test-Raw_SUBJ.R,
# test-Raw_STUDCOMP.R)

# A small, deterministic Raw_SUBJ frame whose rows are hand-picked to exercise
# the dosed/undosed and enrolled/not-enrolled branches of
# apply_ipns_derivations().
make_subj <- function() {
  data.frame(
    studyid = "X",
    invid = "I1",
    subjid = c("S1", "S2", "S3", "S4"),
    enrollyn = c("Y", "Y", "Y", "N"),
    enrolldt = as.Date(c("2025-01-01", "2025-01-01", "2025-03-01", NA)),
    firstdosedate = as.Date(c("2025-01-05", NA, NA, NA)),
    stringsAsFactors = FALSE
  )
}

# The default 10,000 sequential subject IDs cover every digit bucket used by
# apply_ptd_derivations() (test-ptd-generators.R).
make_dosed_subj <- function(n = 10000) {
  df <- data.frame(
    studyid = "X",
    invid = paste0("I", seq_len(n) %% 50),
    subjid = paste0("S", seq_len(n)),
    enrollyn = "Y",
    enrolldt = as.Date("2025-01-01"),
    firstdosedate = as.Date("2025-01-01") + seq_len(n) %% 5,
    stringsAsFactors = FALSE
  )
  apply_ipns_derivations(df, as.Date("2026-01-01"))
}

ptd_cols <- c(
  "drv_treatment_discontinuation_dt",
  "drv_premature_discontinuation_reason",
  "drv_days_lapsed_enrl_discontinuation"
)

# Includes inconsistent completion values, a pre-dose timestamp, and a missing record.
make_studcomp <- function() {
  data.frame(
    studyid = "X",
    invid = "I1",
    subjid = c("S1", "S2", "S4"),
    compyn = c("Y", NA, "N"),
    compreas = c("Death", "", "Death"),
    mincreated_dts = as.POSIXct(
      c("2024-12-01", "2025-01-01", "2024-12-01"),
      tz = "UTC"
    ),
    stringsAsFactors = FALSE
  )
}

# ---- Domain-neutral longitudinal helper fixtures (R/utils-longitudinal.R) ----

# Build a well-sized group structure for injection tests.
make_injection_input <- function(n_groups = 10, n_per_group = 8, seed = 505) {
  set.seed(seed)
  groups <- rep(sprintf("G%02d", seq_len(n_groups)), each = n_per_group)
  values <- round(stats::rnorm(length(groups), mean = 100, sd = 15), 1)
  list(values = values, groups = groups)
}

# A visit schedule plus a matching subject-visit frame for
# `assign_schedule_dates()` tests.
make_schedule_fixture <- function() {
  visits <- data.frame(
    subjid = rep(c("S1", "S2"), each = 3),
    instancename = rep(c("Screening", "VISIT 1", "VISIT 2"), 2),
    visit_dt = as.Date(c(
      "2012-03-01", "2012-01-01", "2012-02-01",
      "2012-03-01", "2012-01-01", "2012-02-01"
    )),
    stringsAsFactors = FALSE
  )

  # Deliberately shuffled relative to chronological order, so a correct
  # implementation must actually sort.
  df <- data.frame(
    subjid = c("S2", "S1", "S1", "S2", "S1", "S2"),
    instancename = c("VISIT 2", "Screening", "VISIT 1", "Screening", "VISIT 2", "VISIT 1"),
    stringsAsFactors = FALSE
  )

  list(df = df, visits = visits)
}

# ---- IP non-starter snapshot integration fixtures ----
# (test-ipns-snapshot-integration.R)

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

native_subj_config <- function(study_id, participant_count, snapshot_count,
                               study_completion = FALSE) {
  config <- create_standard_study_config(
    study_id,
    participant_count = participant_count,
    site_count = 2,
    adverse_events = FALSE, protocol_deviations = FALSE, lab_data = FALSE,
    subject_visits = FALSE, visit_schedule = FALSE, enrollment = TRUE,
    data_changes = FALSE, data_entry = FALSE, queries = FALSE,
    pharmacokinetics = FALSE, study_drug_completion = FALSE,
    study_completion = study_completion, inclusion_exclusion = FALSE, country = FALSE,
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
