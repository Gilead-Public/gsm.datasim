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
