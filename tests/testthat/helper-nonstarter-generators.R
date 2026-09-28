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

# Completion records for make_subj(): S1 (dosed) completed with a Death reason on
# a date before its first dose; S2 has a blank record; S4 is not enrolled; S3 has none.
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
