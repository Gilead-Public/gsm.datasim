#' Generate Raw SDRGCOMP Data
#'
#' Generate Raw SDRGCOMP based on `SDRGCOMP.yaml` from `gsm.mapping`.
#'
#' @inheritParams Raw_STUDY
#' @returns a data.frame pertaining to the raw dataset plugged into `SDRGCOMP.yaml`
#' @family internal
#' @keywords internal
#' @noRd

Raw_SDRGCOMP <- function(data, previous_data, spec, startDate, ...) {
  # Function body for Raw_SDRGCOMP
  inps <- list(...)

  curr_spec <- spec$Raw_SDRGCOMP

  if ("Raw_SDRGCOMP" %in% names(previous_data)) {
    dataset <- previous_data$Raw_SDRGCOMP
    previous_row_num <- nrow(dataset)
  } else {
    dataset <- NULL
    previous_row_num <- 0
  }

  n <- inps$n - previous_row_num
  if (n == 0) {
    return(dataset)
  }

  existing_subjs <- if (is.null(dataset)) unique(character(0)) else unique(dataset$subjid)
  available_subjs <- setdiff(unique(data$Raw_VISIT$subjid), existing_subjs)

  args <- list(
    subjid = list(n, available_subjs, replace = FALSE),
    studyid = list(n, data$Raw_STUDY$protocol_number[[1]]),
    default = list(n, startDate)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_SDRGCOMP, ...)

  res <- apply_nonstarter_sdrgreas(res, data$Raw_SUBJ)

  return(res)
}

sdrgyn <- function(n, ...) {
  # Function body for sdrgyn
  sample(c("Y", "N"),
    prob = c(0.70, 0.30),
    n,
    replace = TRUE
  )
}

#' Subjects that are enrolled but never dosed (IP non-starters)
#'
#' The single shared predicate keeping the confirmed-non-starter markers
#' consistent across SUBJ (`firstdosedate` NA), SDRGCOMP (`sdrgreas`) and
#' STUDCOMP (`compreas`).
#'
#' @param raw_subj a `Raw_SUBJ` data.frame carrying `subjid`, `enrollyn`,
#'   `firstdosedate`.
#' @returns a character vector of non-starter `subjid`s.
#' @family internal
#' @keywords internal
#' @noRd
nonstarter_subjids <- function(raw_subj) {
  if (is.null(raw_subj) ||
    !all(c("subjid", "enrollyn", "firstdosedate") %in% names(raw_subj))) {
    return(character(0))
  }
  as.character(raw_subj$subjid[raw_subj$enrollyn %in% "Y" & is.na(raw_subj$firstdosedate)])
}

#' Mark study-drug-completion reason for IP non-starters
#'
#' Sets `sdrgreas` to the coded "never dosed" reason for the non-starter subset
#' (see `nonstarter_subjids()`) and a benign completion reason otherwise.
#'
#' The benign reason is a deterministic function of `subjid`, not a random draw,
#' so the value is stable each time the helper runs. The cumulative-delta
#' generators carry previously generated rows forward and re-run this helper over
#' the full frame on every snapshot; a per-subject deterministic reason keeps
#' those carried-forward rows idempotent instead of silently re-randomising them.
#'
#' @param df a generated `Raw_SDRGCOMP` data.frame (must carry `subjid`).
#' @param raw_subj the `Raw_SUBJ` frame used to identify non-starters.
#' @param never_dosed_reason the coded reason pinned across datasim/mapping.
#' @returns `df` with an `sdrgreas` column.
#' @family internal
#' @keywords internal
#' @noRd
apply_nonstarter_sdrgreas <- function(df, raw_subj,
                                      never_dosed_reason = "Subject Never Dosed with Study Drug") {
  if (is.null(df) || nrow(df) == 0 || !("subjid" %in% names(df))) {
    return(df)
  }
  subj <- as.character(df$subjid)
  ns <- subj %in% nonstarter_subjids(raw_subj)
  benign_reasons <- c("Study Drug Completed", "Study Drug Discontinued")
  benign_idx <- vapply(subj, function(s) sum(utf8ToInt(s)) %% 2L, integer(1))
  df$sdrgreas <- ifelse(ns, never_dosed_reason, benign_reasons[benign_idx + 1L])
  df
}
