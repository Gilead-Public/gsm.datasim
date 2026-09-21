randomization_split <- list("subjid_invid_country")

make_randomization_spec <- function() {
  list(
    Raw_Randomization = list(
      studyid = list(required = TRUE, type = "character"),
      subjid = list(required = TRUE, type = "character"),
      invid = list(required = TRUE, type = "character"),
      country = list(required = TRUE, type = "character"),
      rgmn_dt = list(required = TRUE, type = "Date")
    )
  )
}

make_randomization_data <- function(n_subjects = 30) {
  make_subj_study_data(
    n_subjects,
    invid_prefix = "SITE%02d",
    extra_subj_cols = list(
      country = rep(c("USA", "CAN", "MEX"), length.out = n_subjects)
    )
  )
}
