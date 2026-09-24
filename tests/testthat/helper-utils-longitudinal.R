# Test data builders for the domain-neutral longitudinal helpers in
# R/utils-longitudinal.R. Used by test-utils-longitudinal.R.

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
