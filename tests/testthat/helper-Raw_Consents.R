make_consents_spec <- function() {
  list(
    Raw_Consents = list(
      subjid = list(required = TRUE, type = "character"),
      studyid = list(required = TRUE, type = "character"),
      cons_dt = list(required = TRUE, type = "Date"),
      constype = list(required = TRUE, type = "character"),
      conscat = list(required = TRUE, type = "character")
    )
  )
}

make_consents_data <- function(n_subjects = 30) {
  make_subj_study_data(n_subjects)
}
