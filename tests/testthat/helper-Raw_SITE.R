site_split <- list("Country_State_City")

make_site_spec <- function() {
  list(
    Raw_SITE = list(
      studyid = list(required = TRUE, type = "character"),
      invid = list(required = TRUE, type = "character"),
      Country = list(required = TRUE, type = "character"),
      State = list(required = TRUE, type = "character"),
      City = list(required = TRUE, type = "character"),
      site_status = list(required = TRUE, type = "character"),
      InvestigatorFirstName = list(required = TRUE, type = "character"),
      InvestigatorLastName = list(required = TRUE, type = "character")
    )
  )
}

make_site_data <- function() {
  list(Raw_STUDY = data.frame(protocol_number = "PROT-001"))
}
