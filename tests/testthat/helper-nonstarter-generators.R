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
