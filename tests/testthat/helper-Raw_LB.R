lb_split <- list("subj_visit_repeated")

make_lb_spec <- function() {
  list(
    Raw_LB = list(
      subjid = list(required = TRUE, type = "character"),
      visnam = list(required = TRUE, type = "character"),
      studyid = list(required = TRUE, type = "character"),
      battrnam = list(required = TRUE, type = "character"),
      lbtstnam = list(required = TRUE, type = "character"),
      lb_dt = list(required = TRUE, type = "Date"),
      toxgrg_nsv = list(required = TRUE, type = "character")
    )
  )
}

make_lb_data <- function(n_subjects = 6) {
  subjids <- sprintf("S%04d", seq_len(n_subjects))
  make_subj_study_data(
    n_subjects,
    invid_prefix = "SITE%02d",
    extra_domains = list(Raw_VISIT = make_two_visit_frame(subjids))
  )
}
