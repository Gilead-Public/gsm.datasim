make_gilda_study_spec <- function() {
  list(
    raw_gilda_study_data = list(
      protocol = list(required = TRUE, type = "character"),
      num_plan_subj = list(required = TRUE, type = "integer"),
      num_plan_site = list(required = TRUE, type = "integer"),
      act_lplv = list(required = TRUE, type = "Date"),
      act_fpfv = list(required = TRUE, type = "Date"),
      est_fpfv = list(required = TRUE, type = "Date"),
      est_lplv = list(required = TRUE, type = "Date"),
      phase = list(required = TRUE, type = "character")
    )
  )
}
