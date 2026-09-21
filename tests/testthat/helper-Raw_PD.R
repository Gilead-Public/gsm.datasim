pd_dvdecod_values <- c(
  "Informed Consent",
  "Missing Data",
  "Study Procedures",
  "Inclusion Criteria",
  "Exclusion Criteria"
)

make_pd_spec <- function() {
  list(
    Raw_PD = list(
      subjid = list(required = TRUE, type = "character"),
      studyid = list(required = TRUE, type = "character"),
      dvdecod = list(required = TRUE, type = "character"),
      dvterm = list(required = TRUE, type = "character"),
      deemedimportant = list(required = TRUE, type = "character"),
      category = list(required = TRUE, type = "character")
    )
  )
}

make_pd_data <- function(n_subjects = 30) {
  make_subj_study_data(n_subjects, invid_prefix = "SITE%02d")
}
