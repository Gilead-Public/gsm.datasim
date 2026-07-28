#' Generate Raw LB Data
#'
#' Generate Raw LB based on `LB.yaml` from `gsm.mapping`.
#'
#' @inheritParams Raw_STUDY
#' @returns a data.frame pertaining to the raw dataset plugged into `LB.yaml`
#' @family internal
#' @keywords internal
#' @noRd

Raw_LB <- function(data, previous_data, spec, startDate, ...) {
  # Function body for Raw_LB
  inps <- list(...)

  curr_spec <- spec$Raw_LB


  if ("Raw_LB" %in% names(previous_data)) {
    dataset <- previous_data$Raw_LB
    previous_row_num <- length(unique(dataset$subjid))
  } else {
    dataset <- NULL
    previous_row_num <- 0
  }


  n <- inps$n - previous_row_num
  if (n == 0) {
    return(dataset)
  }

  tests <- data.frame(
    battrnam = c(
      "CHEMISTRY PANEL", "CHEMISTRY PANEL", "CHEMISTRY PANEL", "CHEMISTRY PANEL",
      "HEMATOLOGY&DIFFERENTIAL PANEL", "HEMATOLOGY&DIFFERENTIAL PANEL",
      "CHEMISTRY PANEL", "CHEMISTRY PANEL", "CHEMISTRY PANEL", "CHEMISTRY PANEL",
      "CHEMISTRY PANEL", "HEMATOLOGY&DIFFERENTIAL PANEL", "HEMATOLOGY&DIFFERENTIAL PANEL",
      "CHEMISTRY PANEL", "CHEMISTRY PANEL", "CHEMISTRY PANEL",
      "HEMATOLOGY&DIFFERENTIAL PANEL", "HEMATOLOGY&DIFFERENTIAL PANEL", "CHEMISTRY PANEL",
      "CHEMISTRY PANEL", "HEMATOLOGY&DIFFERENTIAL PANEL", "HEMATOLOGY&DIFFERENTIAL PANEL",
      "HEMATOLOGY&DIFFERENTIAL PANEL", "HEMATOLOGY&DIFFERENTIAL PANEL",
      "HEMATOLOGY&DIFFERENTIAL PANEL", "CHEMISTRY PANEL", "HEMATOLOGY&DIFFERENTIAL PANEL",
      "HEMATOLOGY&DIFFERENTIAL PANEL", "HEMATOLOGY&DIFFERENTIAL PANEL",
      "HEMATOLOGY&DIFFERENTIAL PANEL", "CHEMISTRY PANEL", "HEMATOLOGY&DIFFERENTIAL PANEL",
      "HEMATOLOGY&DIFFERENTIAL PANEL", "CHEMISTRY PANEL", "CHEMISTRY PANEL",
      "CHEMISTRY PANEL", "CHEMISTRY PANEL", "CHEMISTRY PANEL", "CHEMISTRY PANEL",
      "CHEMISTRY PANEL", "CHEMISTRY PANEL", "CHEMISTRY PANEL",
      "HEMATOLOGY&DIFFERENTIAL PANEL", "CHEMISTRY PANEL", "CHEMISTRY PANEL"
    ),
    lbtstnam = c(
      "ALT (SGPT)", "AST (SGOT)", "Albumin-QT", "Alkaline Phosphatase", "Basophils",
      "Basophils (%)", "Calcium (EDTA)", "Calcium Corrected for Albumin",
      "Cholesterol (High Performance)", "Creatine Kinase", "Direct Bilirubin",
      "Eosinophils", "Eosinophils (%)", "GGT", "Globulin-QT", "Glucose (2dp SI)",
      "Hematocrit", "Hemoglobin", "Indirect Bili", "LDH", "Lymphocytes",
      "Lymphocytes (%)", "MCH", "MCHC", "MCV", "Magnesium-PS", "Monocytes",
      "Monocytes (%)", "Neutrophils", "Neutrophils (%)", "Phosphorus", "Platelets",
      "RBC", "Serum Bicarbonate", "Serum Chloride", "Serum Potassium", "Serum Sodium",
      "Serum Uric Acid", "Total Bilirubin", "Total Protein", "Triglycerides (GPO)",
      "Urea Nitrogen", "WBC", "Creatinine(Rate Blanked)-2dp", "CHM.CCA.00.00"
    )
  )


  if (!("battrnam" %in% names(curr_spec))) {
    curr_spec$battrnam <- list(required = TRUE)
  }

  if (!("lbtstnam" %in% names(curr_spec))) {
    curr_spec$lbtstnam <- list(required = TRUE)
  }

  if (!("visnam" %in% names(curr_spec))) {
    curr_spec$visnam <- list(required = TRUE)
  }

  if (!("rptresn" %in% names(curr_spec))) {
    curr_spec$rptresn <- list(required = TRUE)
  }

  if (all(c("subjid", "visnam") %in% names(curr_spec))) {
    curr_spec$subj_visit_repeated <- list(required = TRUE)
    curr_spec$subjid <- NULL
    curr_spec$visnam <- NULL
  }

  subjs <- subjid(n, external_subjid = data$Raw_SUBJ$subjid, replace = FALSE)
  subj_visits <- data$Raw_VISIT |>
    dplyr::filter(subjid %in% subjs) |>
    dplyr::select(subjid, instancename)

  all_n <- nrow(subj_visits) * nrow(tests)

  args <- list(
    subj_visit_repeated = list(nrow(tests), subj_visits),
    studyid = list(all_n, data$Raw_STUDY$protocol_number[[1]]),
    lb_dt = list(all_n, startDate),
    toxgrg_nsv = list(all_n, subj_visits, data$Raw_SUBJ, nrow(tests)),
    rptresn = list(all_n, subj_visits, tests),
    default = list(all_n, subj_visits, tests)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_LB, ...)

  return(res)
}


subj_visit_repeated <- function(n, data, ...) {
  res <- repeat_rows(n, data)
  return(list(
    subjid = res$subjid,
    visnam = res$instancename
  ))
}

battrnam <- function(n, subj_visits, tests, ...) {
  rep(tests$battrnam, nrow(subj_visits))
}


lbtstnam <- function(n, subj_visits, tests, ...) {
  rep(tests$lbtstnam, nrow(subj_visits))
}

#' Lab test-specific normal-distribution parameters for `rptresn`
#'
#' Mean/SD pairs chosen so ~95% of values fall within the plausible clinical
#' ranges noted in issue #114 (e.g. ALT ~7-56 U/L, Hemoglobin ~11.5-17.5 g/dL,
#' Platelets ~150-400 x10^3/uL, Creatinine ~0.6-1.2 mg/dL). Tests not listed
#' explicitly fall back to a generic default range.
#' @keywords internal
#' @noRd
.lb_rptresn_params <- list(
  "ALT (SGPT)" = list(mean = 31.5, sd = 12.25, digits = 0),
  "AST (SGOT)" = list(mean = 27, sd = 9, digits = 0),
  "Albumin-QT" = list(mean = 4.3, sd = 0.35, digits = 1),
  "Alkaline Phosphatase" = list(mean = 80, sd = 20, digits = 0),
  "Basophils" = list(mean = 0.05, sd = 0.02, digits = 2),
  "Basophils (%)" = list(mean = 0.6, sd = 0.3, digits = 1),
  "Calcium (EDTA)" = list(mean = 9.5, sd = 0.4, digits = 1),
  "Calcium Corrected for Albumin" = list(mean = 9.5, sd = 0.4, digits = 1),
  "Cholesterol (High Performance)" = list(mean = 180, sd = 35, digits = 0),
  "Creatine Kinase" = list(mean = 120, sd = 60, digits = 0),
  "Direct Bilirubin" = list(mean = 0.15, sd = 0.05, digits = 2),
  "Eosinophils" = list(mean = 0.15, sd = 0.07, digits = 2),
  "Eosinophils (%)" = list(mean = 2.5, sd = 1, digits = 1),
  "GGT" = list(mean = 30, sd = 15, digits = 0),
  "Globulin-QT" = list(mean = 2.7, sd = 0.4, digits = 1),
  "Glucose (2dp SI)" = list(mean = 5.2, sd = 0.8, digits = 2),
  "Hematocrit" = list(mean = 42, sd = 4, digits = 1),
  "Hemoglobin" = list(mean = 14.5, sd = 1.5, digits = 1),
  "Indirect Bili" = list(mean = 0.5, sd = 0.15, digits = 2),
  "LDH" = list(mean = 180, sd = 40, digits = 0),
  "Lymphocytes" = list(mean = 2, sd = 0.6, digits = 2),
  "Lymphocytes (%)" = list(mean = 30, sd = 8, digits = 1),
  "MCH" = list(mean = 30, sd = 2, digits = 1),
  "MCHC" = list(mean = 34, sd = 1.2, digits = 1),
  "MCV" = list(mean = 90, sd = 6, digits = 1),
  "Magnesium-PS" = list(mean = 2, sd = 0.2, digits = 2),
  "Monocytes" = list(mean = 0.5, sd = 0.15, digits = 2),
  "Monocytes (%)" = list(mean = 7, sd = 2, digits = 1),
  "Neutrophils" = list(mean = 4, sd = 1.2, digits = 2),
  "Neutrophils (%)" = list(mean = 60, sd = 8, digits = 1),
  "Phosphorus" = list(mean = 3.5, sd = 0.5, digits = 1),
  "Platelets" = list(mean = 275, sd = 45, digits = 0),
  "RBC" = list(mean = 4.8, sd = 0.5, digits = 2),
  "Serum Bicarbonate" = list(mean = 25, sd = 2.5, digits = 0),
  "Serum Chloride" = list(mean = 102, sd = 3, digits = 0),
  "Serum Potassium" = list(mean = 4.2, sd = 0.4, digits = 1),
  "Serum Sodium" = list(mean = 140, sd = 3, digits = 0),
  "Serum Uric Acid" = list(mean = 5, sd = 1.2, digits = 1),
  "Total Bilirubin" = list(mean = 0.7, sd = 0.25, digits = 2),
  "Total Protein" = list(mean = 7, sd = 0.5, digits = 1),
  "Triglycerides (GPO)" = list(mean = 120, sd = 45, digits = 0),
  "Urea Nitrogen" = list(mean = 14, sd = 4, digits = 0),
  "WBC" = list(mean = 7, sd = 1.8, digits = 1),
  "Creatinine(Rate Blanked)-2dp" = list(mean = 0.9, sd = 0.15, digits = 2),
  "CHM.CCA.00.00" = list(mean = 1, sd = 0.3, digits = 2)
)

#' Generate reported numeric lab results (`rptresn`)
#'
#' Produces test-specific numeric distributions for each `lbtstnam`, with a
#' configurable probability of injecting a duplicate value matching a prior
#' visit's `rptresn` for the same subject/test, consistent with the
#' vitals-duplication approach in `Raw_VS.R`.
#'
#' @param n Total number of values to generate (`nrow(subj_visits) * nrow(tests)`)
#' @param subj_visits Data frame of subject/visit rows (repeated once per test),
#'   with columns `subjid` and `instancename`.
#' @param tests Data frame of lab tests with a `lbtstnam` column.
#' @param dDuplicateRate Proportion of subsequent subject/test records to
#'   duplicate from a prior visit.
#' @returns Numeric vector of length `n`.
#' @keywords internal
#' @noRd
rptresn <- function(n, subj_visits, tests, dDuplicateRate = 0.10, ...) {
  n_tests <- nrow(tests)
  test_col <- rep(tests$lbtstnam, nrow(subj_visits))
  subj_col <- rep(subj_visits$subjid, each = n_tests)

  default_params <- list(mean = 5, sd = 1, digits = 2)

  values <- vapply(test_col, function(test_name) {
    params <- .lb_rptresn_params[[test_name]]
    if (is.null(params)) params <- default_params
    round(stats::rnorm(1, mean = params$mean, sd = params$sd), digits = params$digits)
  }, numeric(1))

  # Inject duplicates per subject x test combination, mirroring
  # `.generate_vital_with_duplicates()` in Raw_VS.R.
  keys <- paste(subj_col, test_col, sep = "\u001f")
  unique_keys <- unique(keys)
  for (key in unique_keys) {
    idx <- which(keys == key)
    if (length(idx) <= 1) next

    subsequent_idx <- idx[-1]
    n_dups <- max(1, round(length(subsequent_idx) * dDuplicateRate))
    dup_positions <- sample(subsequent_idx, size = min(n_dups, length(subsequent_idx)))

    for (pos in dup_positions) {
      prior_idx <- idx[idx < pos]
      chosen_idx <- if (length(prior_idx) == 1) prior_idx else sample(prior_idx, 1)
      values[pos] <- values[chosen_idx]
    }
  }

  unname(values)
}

toxgrg_nsv <- function(n, subj_visits = NULL, Raw_SUBJ_data = NULL, tests_n = 1, ...) {
  # Increase higher lab tox grades in hotspot sites while preserving baseline mix.
  row_keys <- NULL
  if (!is.null(subj_visits) && "subjid" %in% names(subj_visits) && !is.null(tests_n)) {
    row_keys <- rep(subj_visits$subjid, each = tests_n)
    row_keys <- row_keys[seq_len(min(length(row_keys), n))]
    if (length(row_keys) < n) {
      row_keys <- c(row_keys, rep(row_keys[length(row_keys)], n - length(row_keys)))
    }
  }

  sample_categorical_with_hotspots(
    values = c("", "0", "1", "2", "3", "4"),
    n = n,
    base_prob = c(0.49, 0.4875, 0.01, 0.005, 0.005, 0.0025),
    outlier_idx = 3:6,
    row_keys = row_keys,
    key_map = Raw_SUBJ_data,
    key_col = "subjid",
    site_col = "invid"
  )
}
