# Shared fixture builders for the Raw_* domain generator tests.
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
