ae_split <- list("aest_dt_aeen_dt")

make_ae_spec <- function() {
  list(
    Raw_AE = list(
      subjid = list(required = TRUE, type = "character"),
      studyid = list(required = TRUE, type = "character"),
      aeser = list(required = TRUE, type = "character"),
      aest_dt = list(required = TRUE, type = "Date"),
      aeen_dt = list(required = TRUE, type = "Date"),
      mdrpt_nsv = list(required = TRUE, type = "character"),
      mdrsoc_nsv = list(required = TRUE, type = "character"),
      aetoxgr = list(required = TRUE, type = "character"),
      aeongo = list(required = TRUE, type = "character"),
      aerel = list(required = TRUE, type = "character")
    )
  )
}

make_ae_data <- function(n_subjects = 30) {
  make_subj_study_data(n_subjects, invid_prefix = "SITE%02d")
}
