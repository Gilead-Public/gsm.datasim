enroll_split <- list("subject_to_enrollment")

make_enroll_spec <- function() {
  list(
    Raw_ENROLL = list(
      studyid = list(required = TRUE, type = "character"),
      invid = list(required = TRUE, type = "character"),
      country = list(required = TRUE, type = "character"),
      subjid = list(required = TRUE, type = "character"),
      subjectid = list(required = TRUE, type = "character"),
      enrollyn = list(required = TRUE, type = "character")
    )
  )
}

make_enroll_data <- function(n_subjects = 10) {
  make_subj_study_data(
    n_subjects,
    invid_prefix = "SITE%02d",
    extra_subj_cols = list(
      country = rep(c("USA", "CAN"), length.out = n_subjects),
      enrollyn = rep("Y", n_subjects)
    )
  )
}
