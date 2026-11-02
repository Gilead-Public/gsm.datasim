# Spec builders and constants for the Raw_* domain generator tests.
# Grouped here (rather than one file per domain) so shared shapes are visible.

# ---- Split-key and value constants ----

ae_split <- list("aest_dt_aeen_dt")

datachg_split <- list("subject_nsv_visit_repeated")

dataent_split <- list("subject_nsv_visit_repeated")

death_classes <- c(
  "Progressive Disease",
  "Adverse Event",
  "Disease Recurrence",
  "Not related to disease",
  "Related to long-term follow-up and not related to study drug"
)

enroll_split <- list("subject_to_enrollment")

ie_split <- list("subject_to_ie", "tiver_ietestcd_ietest_ieorres_iecat")

lb_split <- list("subj_visit_repeated")

overallresponse_split <- list("subjid_rs_dt")

overallresponse_values <- c("NE", "PD", "SD", "PR", "CR")

pd_dvdecod_values <- c(
  "Informed Consent",
  "Missing Data",
  "Study Procedures",
  "Inclusion Criteria",
  "Exclusion Criteria"
)

pk_split <- list("subjid_visit_pkdat")

query_split <- list("subject_nsv_visit_repeated")

randomization_split <- list("subjid_invid_country")

site_split <- list("Country_State_City")

studcomp_split <- list("subjid_invid_unique")

studcomp_compreas_values <- c(
  "",
  "Lost to Follow-Up",
  "Death",
  "Withdrew Consent"
)

visit_split <- list("subjid_repeated", "invid_repeated")

# ---- Domain specs ----

make_ae_spec <- function() {
  list(
    Raw_AE = list(
      subjid = list(required = TRUE, type = "character"),
      studyid = list(required = TRUE, type = "character"),
      aeser = list(required = TRUE, type = "character"),
      aest_dt = list(required = TRUE, type = "Date"),
      aeen_dt = list(required = TRUE, type = "Date"),
      mdrpt_nsv = list(required = TRUE, type = "character"),
      mdrsoc_nsv = list(required = TRUE, type = "character"),
      aetoxgr = list(required = TRUE, type = "character"),
      aeongo = list(required = TRUE, type = "character"),
      aerel = list(required = TRUE, type = "character")
    )
  )
}

make_anticancer_spec <- function() {
  list(
    Raw_AntiCancer = list(
      subjid = list(required = TRUE, type = "character"),
      studyid = list(required = TRUE, type = "character"),
      cmtrt = list(required = TRUE, type = "character"),
      cmst_dt = list(required = TRUE, type = "Date")
    )
  )
}

make_baseline_spec <- function() {
  list(
    Raw_Baseline = list(
      subjid = list(required = TRUE, type = "character"),
      studyid = list(required = TRUE, type = "character"),
      scan_dt = list(required = TRUE, type = "Date")
    )
  )
}

make_consents_spec <- function() {
  list(
    Raw_Consents = list(
      subjid = list(required = TRUE, type = "character"),
      studyid = list(required = TRUE, type = "character"),
      cons_dt = list(required = TRUE, type = "Date"),
      constype = list(required = TRUE, type = "character"),
      conscat = list(required = TRUE, type = "character")
    )
  )
}

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

make_death_test_spec <- function() {
  list(
    Raw_Death = list(
      subjid = list(required = TRUE, type = "character"),
      studyid = list(required = TRUE, type = "character"),
      death_dt = list(required = TRUE, type = "Date"),
      deathcls = list(required = TRUE, type = "character")
    )
  )
}

make_enroll_spec <- function() {
  list(
    Raw_ENROLL = list(
      studyid = list(required = TRUE, type = "character"),
      invid = list(required = TRUE, type = "character"),
      country = list(required = TRUE, type = "character"),
      subjid = list(required = TRUE, type = "character"),
      subjectid = list(required = TRUE, type = "character"),
      enrollyn = list(required = TRUE, type = "character")
    )
  )
}

make_ie_spec <- function() {
  list(
    Raw_IE = list(
      subjid = list(required = TRUE, type = "character"),
      studyid = list(required = TRUE, type = "character"),
      tiver = list(required = TRUE, type = "character"),
      ietestcd = list(required = TRUE, type = "character"),
      ietest = list(required = TRUE, type = "character"),
      ieorres = list(required = TRUE, type = "character"),
      iecat = list(required = TRUE, type = "character")
    )
  )
}

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

make_pd_spec <- function() {
  list(
    Raw_PD = list(
      subjid = list(required = TRUE, type = "character"),
      studyid = list(required = TRUE, type = "character"),
      dvdecod = list(required = TRUE, type = "character"),
      dvterm = list(required = TRUE, type = "character"),
      deemedimportant = list(required = TRUE, type = "character"),
      category = list(required = TRUE, type = "character")
    )
  )
}

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

make_randomization_spec <- function() {
  list(
    Raw_Randomization = list(
      studyid = list(required = TRUE, type = "character"),
      subjid = list(required = TRUE, type = "character"),
      invid = list(required = TRUE, type = "character"),
      country = list(required = TRUE, type = "character"),
      rgmn_dt = list(required = TRUE, type = "Date")
    )
  )
}

make_sdrgcomp_spec <- function() {
  list(
    Raw_SDRGCOMP = list(
      subjid = list(required = TRUE, type = "character"),
      studyid = list(required = TRUE, type = "character"),
      sdrgyn = list(required = TRUE, type = "character")
    )
  )
}

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

make_studcomp_spec <- function() {
  list(
    Raw_STUDCOMP = list(
      subjid = list(required = TRUE, type = "character"),
      invid = list(required = TRUE, type = "character"),
      studyid = list(required = TRUE, type = "character"),
      compyn = list(required = TRUE, type = "character"),
      compreas = list(required = TRUE, type = "character")
    )
  )
}

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

make_visit_spec <- function() {
  list(
    Raw_VISIT = list(
      subjid = list(required = TRUE, type = "character"),
      invid = list(required = TRUE, type = "character"),
      studyid = list(required = TRUE, type = "character"),
      foldername = list(required = TRUE, type = "character"),
      instancename = list(required = TRUE, type = "character"),
      visit_dt = list(required = TRUE, type = "Date")
    )
  )
}

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

# Mirrors the authoritative `Raw_VS` spec in gsm.mapping's VS.yaml
# (Gilead-Public/gsm.mapping#165). Two things drive the column names below:
#
#   * `Raw_VS` is the RAW extract, so `source_col` entries mean the emitted
#     columns are the source names -- `project`, `foldername`, `bsaentry` --
#     which `rename_raw_data_vars_per_spec()` applies on the way out.
#   * There is no `invid`; site is joined from `Mapped_SUBJ` when `Mapped_VS`
#     is built. Use `attach_vs_site()` in tests that need it.
make_vs_test_spec <- function() {
  list(
    studyid = list(required = TRUE, type = "character", source_col = "project"),
    subjid = list(required = TRUE, type = "character"),
    visit = list(required = TRUE, type = "character", source_col = "foldername"),
    vs_dt = list(required = TRUE, type = "Date"),
    vsperf_std = list(required = TRUE, type = "character"),
    weight = list(required = TRUE, type = "numeric"),
    sysbp = list(required = TRUE, type = "numeric"),
    diabp = list(required = TRUE, type = "numeric")
  )
}

make_vs_full_spec <- function() {
  spec <- make_vs_test_spec()
  spec$height <- list(required = TRUE, type = "numeric")
  spec$bsa <- list(required = TRUE, type = "numeric", source_col = "bsaentry")
  spec$pulse <- list(required = TRUE, type = "numeric")
  spec$temp <- list(required = TRUE, type = "numeric")
  spec$resp <- list(required = TRUE, type = "numeric")
  spec
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
