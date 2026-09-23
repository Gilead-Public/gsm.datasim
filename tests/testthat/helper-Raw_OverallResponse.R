overallresponse_split <- list("subjid_rs_dt")
overallresponse_values <- c("NE", "PD", "SD", "PR", "CR")

make_overallresponse_spec <- function() {
  list(
    Raw_OverallResponse = list(
      subjid = list(required = TRUE, type = "character"),
      rs_dt = list(required = TRUE, type = "Date"),
      studyid = list(required = TRUE, type = "character"),
      ovrlresp = list(required = TRUE, type = "character"),
      response_folder = list(required = TRUE, type = "character")
    )
  )
}

make_overallresponse_data <- function(n_subjects = 20) {
  subjids <- sprintf("S%04d", seq_len(n_subjects))
  make_subj_study_data(
    n_subjects,
    invid_prefix = "SITE%02d",
    extra_domains = list(
      Raw_VISIT = data.frame(
        subjid = rep(subjids, each = 3),
        visit_dt = as.Date("2012-01-01") + seq_len(n_subjects * 3),
        stringsAsFactors = FALSE
      )
    )
  )
}
