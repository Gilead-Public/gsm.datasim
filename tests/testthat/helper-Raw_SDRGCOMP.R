make_sdrgcomp_spec <- function() {
  list(
    Raw_SDRGCOMP = list(
      subjid = list(required = TRUE, type = "character"),
      studyid = list(required = TRUE, type = "character"),
      sdrgyn = list(required = TRUE, type = "character")
    )
  )
}

make_sdrgcomp_data <- function(n_subjects = 30) {
  subjids <- sprintf("S%04d", seq_len(n_subjects))
  # Raw_SDRGCOMP draws its subject pool from the visit data, not Raw_SUBJ.
  make_subj_study_data(
    n_subjects,
    extra_domains = list(
      Raw_VISIT = data.frame(
        subjid = rep(subjids, each = 2),
        stringsAsFactors = FALSE
      )
    )
  )
}
