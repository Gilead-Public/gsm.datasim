datachg_split <- list("subject_nsv_visit_repeated")

make_datachg_spec <- function() {
  list(
    Raw_DATACHG = list(
      subject_nsv = list(required = TRUE, type = "character"),
      visnam = list(required = TRUE, type = "character"),
      studyid = list(required = TRUE, type = "character"),
      form = list(required = TRUE, type = "character"),
      field = list(required = TRUE, type = "character"),
      n_changes = list(required = TRUE, type = "integer")
    )
  )
}

make_datachg_data <- function(n_subjects = 6) {
  subjids <- sprintf("S%04d", seq_len(n_subjects))
  make_subj_study_data(
    n_subjects,
    invid_prefix = "SITE%02d",
    extra_subj_cols = list(subject_nsv = paste0(subjids, "-XXXX")),
    extra_domains = list(Raw_VISIT = make_two_visit_frame(subjids))
  )
}
