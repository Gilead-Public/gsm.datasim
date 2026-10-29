# Input-data builders for the Raw_* domain generator tests.
#
# Most Raw_* test files only need a subject pool (optionally spread across
# sites, with a few extra columns) plus a single-row Raw_STUDY record. This
# factors that repeated shape out so each test-Raw_*.R file only has to
# describe how it differs from the common case.

#' Build a minimal Raw_SUBJ + Raw_STUDY fixture pair
#'
#' @param n_subjects Number of subjects to generate.
#' @param invid_prefix `sprintf()` template (e.g. `"SITE%02d"`) used to build
#'   site identifiers for `invid`, recycled across subjects. `NULL` (default)
#'   omits the `invid` column entirely.
#' @param invid_sites Number of distinct site ids to cycle through.
#' @param extra_subj_cols Named list of additional Raw_SUBJ columns, each
#'   already the right length (`n_subjects`), appended after `invid`.
#' @param protocol_number Value for `Raw_STUDY$protocol_number`.
#' @param extra_domains Named list of additional top-level domains (e.g.
#'   `Raw_VISIT`) merged into the returned list.
#'
#' @return A named list with `Raw_SUBJ`, `Raw_STUDY`, and any `extra_domains`.
#' @noRd
make_subj_study_data <- function(
  n_subjects,
  invid_prefix = NULL,
  invid_sites = 3,
  extra_subj_cols = list(),
  protocol_number = "PROT-001",
  extra_domains = list()
) {
  subj_cols <- list(subjid = sprintf("S%04d", seq_len(n_subjects)))

  if (!is.null(invid_prefix)) {
    subj_cols$invid <- rep(
      sprintf(invid_prefix, seq_len(invid_sites)),
      length.out = n_subjects
    )
  }
  subj_cols <- c(subj_cols, extra_subj_cols)

  c(
    list(
      Raw_SUBJ = do.call(
        data.frame,
        c(subj_cols, list(stringsAsFactors = FALSE))
      ),
      Raw_STUDY = data.frame(protocol_number = protocol_number)
    ),
    extra_domains
  )
}

#' Build a two-visits-per-subject Raw_VISIT fixture
#'
#' Shared by the domains (Raw_DATACHG, Raw_LB, Raw_QUERY) whose fixtures
#' repeat "Visit 1" / "Visit 2" once per subject/subject_nsv id.
#'
#' @param ids Vector of subject (or subject_nsv) identifiers.
#' @param id_col Name of the id column in the returned data frame.
#'
#' @return A data frame with `id_col` and `instancename`.
#' @noRd
make_two_visit_frame <- function(ids, id_col = "subjid") {
  visits <- data.frame(
    id = rep(ids, each = 2),
    instancename = rep(c("Visit 1", "Visit 2"), times = length(ids)),
    stringsAsFactors = FALSE
  )
  names(visits)[1] <- id_col
  visits
}

# ---- Shared shapes ----

#' Build the Raw_SUBJ/Raw_STUDY/Raw_VISIT fixture used by the subject_nsv domains
#'
#' Shared by Raw_DATACHG, Raw_DATAENT and Raw_QUERY, which all need a
#' `subject_nsv` column on Raw_SUBJ and two visits per subject.
#' @noRd
make_nsv_visit_data <- function(n_subjects) {
  subjids <- sprintf("S%04d", seq_len(n_subjects))
  make_subj_study_data(
    n_subjects,
    invid_prefix = "SITE%02d",
    extra_subj_cols = list(subject_nsv = paste0(subjids, "-XXXX")),
    extra_domains = list(Raw_VISIT = make_two_visit_frame(subjids))
  )
}

# ---- Per-domain data ----

make_ae_data <- function(n_subjects = 30) {
  make_subj_study_data(n_subjects, invid_prefix = "SITE%02d")
}

make_anticancer_data <- function(n_subjects = 30) {
  make_subj_study_data(n_subjects)
}

make_baseline_data <- function(n_subjects = 30) {
  make_subj_study_data(n_subjects)
}

make_consents_data <- function(n_subjects = 30) {
  make_subj_study_data(n_subjects)
}

make_datachg_data <- function(n_subjects = 6) {
  make_nsv_visit_data(n_subjects)
}

make_dataent_data <- function(n_subjects = 6) {
  make_nsv_visit_data(n_subjects)
}

make_query_data <- function(n_subjects = 10) {
  make_nsv_visit_data(n_subjects)
}

make_death_test_data <- function(n_subjects = 30) {
  make_subj_study_data(n_subjects)
}

make_enroll_data <- function(n_subjects = 10) {
  make_subj_study_data(
    n_subjects,
    invid_prefix = "SITE%02d",
    extra_subj_cols = list(
      country = rep(c("USA", "CAN"), length.out = n_subjects),
      enrollyn = rep("Y", n_subjects)
    )
  )
}

make_ie_data <- function(n_subjects = 30) {
  # Raw_SUBJ is deliberately multi-column: subject_to_ie subsets it with
  # `[i, ]`, which would drop a single-column frame to a vector.
  make_subj_study_data(n_subjects, invid_prefix = "SITE%02d")
}

make_lb_data <- function(n_subjects = 6) {
  subjids <- sprintf("S%04d", seq_len(n_subjects))
  make_subj_study_data(
    n_subjects,
    invid_prefix = "SITE%02d",
    extra_domains = list(Raw_VISIT = make_two_visit_frame(subjids))
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

make_pd_data <- function(n_subjects = 30) {
  make_subj_study_data(n_subjects, invid_prefix = "SITE%02d")
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

make_randomization_data <- function(n_subjects = 30) {
  make_subj_study_data(
    n_subjects,
    invid_prefix = "SITE%02d",
    extra_subj_cols = list(
      country = rep(c("USA", "CAN", "MEX"), length.out = n_subjects)
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

make_site_data <- function() {
  list(Raw_STUDY = data.frame(protocol_number = "PROT-001"))
}

make_studcomp_data <- function(n_subjects = 30) {
  make_subj_study_data(n_subjects, invid_prefix = "SITE%02d")
}

make_visit_data <- function(n_subjects = 10) {
  make_subj_study_data(n_subjects, invid_prefix = "0X%03d")
}

# ---- Raw_VS ----

# Emitted (post-rename) vital column names, so `bsa` appears as `bsaentry`.
VS_VITAL_COLS <- c(
  "weight", "height", "bsaentry", "sysbp",
  "diabp", "pulse", "temp", "resp"
)

# The emitted visit column name, per `source_col: foldername`.
VS_VISIT_COL <- "foldername"

# `Raw_VISIT` now matters to `Raw_VS` generation: `vs_dt` is taken from
# `visit_dt` rather than being a constant, so the test fixture must carry a
# real per-visit schedule.
# `strDateClass` mirrors the real `Raw_VISIT` generator, which emits
# "%Y-%m-%d" character dates rather than `Date`s (see `visit_dt()` in
# R/Raw_VISIT.R). Tests use "character" to exercise the production path.
make_vs_test_data <- function(n_subjects = 20, n_visits = 6, n_sites = 3,
                              start_date = as.Date("2012-01-01"),
                              strDateClass = c("Date", "character")) {
  strDateClass <- match.arg(strDateClass)
  subjid <- sprintf("S%04d", seq_len(n_subjects))
  invid <- sprintf("0X%04d", (seq_len(n_subjects) %% n_sites) + 1)

  visits <- c("Screening", paste0("VISIT ", seq_len(max(n_visits - 1, 1))))[seq_len(n_visits)]
  visit_dates <- start_date + seq(0, by = 28, length.out = n_visits)
  if (strDateClass == "character") {
    visit_dates <- format(visit_dates, "%Y-%m-%d")
  }

  raw_visit <- do.call(
    rbind,
    lapply(subjid, function(s) {
      data.frame(
        subjid = s,
        instancename = visits,
        visit_dt = visit_dates,
        stringsAsFactors = FALSE
      )
    })
  )

  list(
    Raw_SUBJ = data.frame(
      subjid = subjid,
      invid = invid,
      stringsAsFactors = FALSE
    ),
    Raw_STUDY = data.frame(protocol_number = "PROT-VS"),
    Raw_VISIT = raw_visit
  )
}

make_vs_context <- function(data, spec = make_vs_test_spec(), n = NULL,
                            risk_profile = NULL,
                            start_date = as.Date("2012-01-01")) {
  list(
    data = data,
    previous_data = list(),
    combined_specs = list(Raw_VS = spec),
    n = n %||% nrow(data$Raw_SUBJ),
    start_date = start_date,
    risk_profile = risk_profile
  )
}

# `Raw_VS` deliberately carries no `invid` (gsm.mapping#165 joins site on from
# `Mapped_SUBJ` when building `Mapped_VS`). Tests that assert per-site
# behaviour must therefore do that join themselves, exactly as the mapping
# does, rather than reading a column off the raw domain.
attach_vs_site <- function(vs_df, data) {
  vs_df$invid <- data$Raw_SUBJ$invid[match(vs_df$subjid, data$Raw_SUBJ$subjid)]
  vs_df
}
