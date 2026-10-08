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

# Fixture shared by the IP non-starter derivation tests
# (test-nonstarter-generators.R): a small, deterministic Raw_SUBJ frame whose
# rows are hand-picked to exercise the dosed/undosed and enrolled/not-enrolled
# branches of apply_ipns_derivations().
make_subj <- function() {
  data.frame(
    subjid = c("S1", "S2", "S3", "S4"),
    enrollyn = c("Y", "Y", "Y", "N"),
    enrolldt = as.Date(c("2025-01-01", "2025-01-01", "2025-03-01", NA)),
    firstdosedate = as.Date(c("2025-01-05", NA, NA, NA)),
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
