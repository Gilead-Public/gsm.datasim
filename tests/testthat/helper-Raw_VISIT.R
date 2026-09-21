visit_split <- list("subjid_repeated", "invid_repeated")

make_visit_spec <- function() {
  list(
    Raw_VISIT = list(
      subjid = list(required = TRUE, type = "character"),
      invid = list(required = TRUE, type = "character"),
      studyid = list(required = TRUE, type = "character"),
      foldername = list(required = TRUE, type = "character"),
      instancename = list(required = TRUE, type = "character"),
      visit_dt = list(required = TRUE, type = "Date")
    )
  )
}

make_visit_data <- function(n_subjects = 10) {
  make_subj_study_data(n_subjects, invid_prefix = "0X%03d")
}
