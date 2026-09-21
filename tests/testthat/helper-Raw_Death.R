death_classes <- c(
  "Progressive Disease",
  "Adverse Event",
  "Disease Recurrence",
  "Not related to disease",
  "Related to long-term follow-up and not related to study drug"
)

make_death_test_spec <- function() {
  list(
    Raw_Death = list(
      subjid = list(required = TRUE, type = "character"),
      studyid = list(required = TRUE, type = "character"),
      death_dt = list(required = TRUE, type = "Date"),
      deathcls = list(required = TRUE, type = "character")
    )
  )
}

make_death_test_data <- function(n_subjects = 30) {
  make_subj_study_data(n_subjects)
}
