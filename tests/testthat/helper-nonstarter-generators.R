# Fixture shared by the IP non-starter derivation tests
# (test-nonstarter-generators.R): a small, deterministic Raw_SUBJ frame whose
# rows are hand-picked to exercise the dosed/undosed and enrolled/not-enrolled
# branches of apply_ipns_derivations().
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
