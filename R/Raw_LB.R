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

  if (all(c("subjid", "visnam") %in% names(curr_spec))) {
    curr_spec$subj_visit_repeated <- list(required = TRUE)
    curr_spec$subjid <- NULL
    curr_spec$visnam <- NULL
  }

  subjs <- subjid(n, external_subjid = data$Raw_SUBJ$subjid, replace = FALSE)
  subj_visits <- data$Raw_SV %>%
    dplyr::filter(subjid %in% subjs) %>%
    dplyr::select(subjid, instancename)

  all_n <- nrow(subj_visits) * nrow(tests)

  args <- list(
    subj_visit_repeated = list(nrow(tests), subj_visits),
    studyid = list(all_n, data$Raw_STUDY$protocol_number[[1]]),
    lb_dt = list(all_n, startDate),
    toxgrg_nsv = list(all_n, subj_visits, data$Raw_SUBJ, nrow(tests)),
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
