pk_split <- list("subjid_visit_pkdat")

make_pk_spec <- function() {
  list(
    Raw_PK = list(
      subjid = list(required = TRUE, type = "character"),
      visit = list(required = TRUE, type = "character"),
      pkdat = list(required = TRUE, type = "Date"),
      studyid = list(required = TRUE, type = "character"),
      pktpt = list(required = TRUE, type = "character"),
      pkperf = list(required = TRUE, type = "character")
    )
  )
}

make_pk_data <- function(n_subjects = 20) {
  subjids <- sprintf("S%04d", seq_len(n_subjects))
  make_subj_study_data(
    n_subjects,
    invid_prefix = "SITE%02d",
    extra_domains = list(
      Raw_VISIT = data.frame(
        subjid = rep(subjids, each = 3),
        foldername = rep(
          c("Cycle 1", "Cycle 2", "Cycle 3"),
          times = n_subjects
        ),
        visit_dt = as.Date("2012-01-01") + seq_len(n_subjects * 3),
        stringsAsFactors = FALSE
      )
    )
  )
}
