make_test_study <- function(n = 4) {
  raw <- lapply(seq_len(n), function(i) list(Raw_AE = data.frame(i = i), Raw_LB = data.frame(i = i)))
  names(raw) <- paste0("2023-0", seq_len(n), "-01")
  create_longitudinal_study_data(
    "ST", raw,
    list(participants = 10, sites = 2, snapshots = n, interval = "1 month", domains = c("AE", "LB"))
  )
}
