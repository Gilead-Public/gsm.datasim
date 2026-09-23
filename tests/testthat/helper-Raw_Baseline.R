make_baseline_spec <- function() {
  list(
    Raw_Baseline = list(
      subjid = list(required = TRUE, type = "character"),
      studyid = list(required = TRUE, type = "character"),
      scan_dt = list(required = TRUE, type = "Date")
    )
  )
}

make_baseline_data <- function(n_subjects = 30) {
  make_subj_study_data(n_subjects)
}
