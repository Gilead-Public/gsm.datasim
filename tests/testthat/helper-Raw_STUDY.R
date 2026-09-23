make_study_spec <- function() {
  list(
    Raw_STUDY = list(
      studyid = list(required = TRUE, type = "character"),
      protocol_number = list(required = TRUE, type = "character"),
      nickname = list(required = TRUE, type = "character"),
      protocol_title = list(required = TRUE, type = "character"),
      phase = list(required = TRUE, type = "character"),
      num_plan_site = list(required = TRUE, type = "integer"),
      num_plan_subj = list(required = TRUE, type = "integer"),
      act_fpfv = list(required = TRUE, type = "Date"),
      est_fpfv = list(required = TRUE, type = "Date"),
      est_lpfv = list(required = TRUE, type = "Date"),
      est_lplv = list(required = TRUE, type = "Date"),
      db_lock_dt = list(required = TRUE, type = "Date")
    )
  )
}

study_inputs <- function(...) {
  defaults <- list(
    StudyID = "STUDY-001",
    SiteCount = 10,
    ParticipantCount = 100,
    MinDate = as.Date("2012-01-01"),
    MaxDate = as.Date("2012-03-01"),
    GlobalMaxDate = as.Date("2012-12-31")
  )
  utils::modifyList(defaults, list(...))
}
