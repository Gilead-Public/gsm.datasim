make_anticancer_spec <- function() {
  list(
    Raw_AntiCancer = list(
      subjid = list(required = TRUE, type = "character"),
      studyid = list(required = TRUE, type = "character"),
      cmtrt = list(required = TRUE, type = "character"),
      cmst_dt = list(required = TRUE, type = "Date")
    )
  )
}

make_anticancer_data <- function(n_subjects = 30) {
  make_subj_study_data(n_subjects)
}
