ie_split <- list("subject_to_ie", "tiver_ietestcd_ietest_ieorres_iecat")

make_ie_spec <- function() {
  list(
    Raw_IE = list(
      subjid = list(required = TRUE, type = "character"),
      studyid = list(required = TRUE, type = "character"),
      tiver = list(required = TRUE, type = "character"),
      ietestcd = list(required = TRUE, type = "character"),
      ietest = list(required = TRUE, type = "character"),
      ieorres = list(required = TRUE, type = "character"),
      iecat = list(required = TRUE, type = "character")
    )
  )
}

make_ie_data <- function(n_subjects = 30) {
  # Raw_SUBJ is deliberately multi-column: subject_to_ie subsets it with
  # `[i, ]`, which would drop a single-column frame to a vector.
  make_subj_study_data(n_subjects, invid_prefix = "SITE%02d")
}
