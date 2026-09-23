studcomp_split <- list("subjid_invid_unique")

studcomp_compreas_values <- c(
  "",
  "Lost to Follow-Up",
  "Death",
  "Withdrew Consent"
)

make_studcomp_spec <- function() {
  list(
    Raw_STUDCOMP = list(
      subjid = list(required = TRUE, type = "character"),
      invid = list(required = TRUE, type = "character"),
      studyid = list(required = TRUE, type = "character"),
      compyn = list(required = TRUE, type = "character"),
      compreas = list(required = TRUE, type = "character")
    )
  )
}

make_studcomp_data <- function(n_subjects = 30) {
  make_subj_study_data(n_subjects, invid_prefix = "SITE%02d")
}
