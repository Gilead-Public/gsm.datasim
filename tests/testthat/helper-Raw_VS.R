make_vs_test_spec <- function() {
  list(
    subjid = list(required = TRUE),
    invid = list(required = TRUE),
    studyid = list(required = TRUE),
    instancename = list(required = TRUE),
    vs_dt = list(required = TRUE),
    vsperf_std = list(required = TRUE),
    weight = list(required = TRUE),
    sysbp = list(required = TRUE),
    diabp = list(required = TRUE)
  )
}

make_vs_test_data <- function(n_subjects = 20, n_visits = 6) {
  subjid <- sprintf("S%04d", seq_len(n_subjects))
  invid <- sprintf("0X%04d", (seq_len(n_subjects) %% 3) + 1)

  visits <- c("Screening", paste0("VISIT ", 1:5))[seq_len(n_visits)]
  raw_visit <- do.call(
    rbind,
    lapply(subjid, function(s) {
      data.frame(subjid = s, instancename = visits, stringsAsFactors = FALSE)
    })
  )

  list(
    Raw_SUBJ = data.frame(
      subjid = subjid,
      invid = invid,
      stringsAsFactors = FALSE
    ),
    Raw_STUDY = data.frame(protocol_number = "PROT-VS"),
    Raw_VISIT = raw_visit
  )
}
