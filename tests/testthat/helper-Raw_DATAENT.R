dataent_split <- list("subject_nsv_visit_repeated")

make_dataent_spec <- function() {
  list(
    Raw_DATAENT = list(
      subject_nsv = list(required = TRUE, type = "character"),
      visnam = list(required = TRUE, type = "character"),
      studyid = list(required = TRUE, type = "character"),
      form = list(required = TRUE, type = "character"),
      visit_date = list(required = TRUE, type = "Date"),
      data_entry_lag = list(required = TRUE, type = "integer")
    )
  )
}

make_dataent_data <- function(n_subjects = 6) {
  subjids <- sprintf("S%04d", seq_len(n_subjects))
  make_subj_study_data(
    n_subjects,
    invid_prefix = "SITE%02d",
    extra_subj_cols = list(subject_nsv = paste0(subjids, "-XXXX")),
    extra_domains = list(Raw_VISIT = make_two_visit_frame(subjids))
  )
}
