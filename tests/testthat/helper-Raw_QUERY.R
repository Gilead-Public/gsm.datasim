query_split <- list("subject_nsv_visit_repeated")

make_query_spec <- function() {
  list(
    Raw_QUERY = list(
      subject_nsv = list(required = TRUE, type = "character"),
      visnam = list(required = TRUE, type = "character"),
      studyid = list(required = TRUE, type = "character"),
      querystatus = list(required = TRUE, type = "character"),
      queryage = list(required = TRUE, type = "integer")
    )
  )
}

make_query_data <- function(n_subjects = 10) {
  subjids <- sprintf("S%04d", seq_len(n_subjects))
  nsvs <- paste0(subjids, "-XXXX")
  make_subj_study_data(
    n_subjects,
    invid_prefix = "SITE%02d",
    extra_subj_cols = list(subject_nsv = nsvs),
    extra_domains = list(Raw_VISIT = make_two_visit_frame(subjids))
  )
}
