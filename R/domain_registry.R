#' Domain Registry (draft)
#'
#' Returns a named list that defines per-domain generation behavior for the
#' registry-based migration path.
#'
#' @return Named list of domain registry entries.
#' @export
get_domain_registry <- function() {
  # ---------------------------------------------------------------------------
  # Shared helper: standard cumulative-delta pattern used by most domains.
  # Returns (dataset, n) where dataset is the existing rows from previous_data
  # and n is the number of NEW rows to generate (target - already present).
  # ---------------------------------------------------------------------------
  .delta <- function(key, previous_data, target_n, unique_col = NULL) {
    if (key %in% names(previous_data)) {
      dataset <- previous_data[[key]]
      prev_n  <- if (is.null(unique_col)) nrow(dataset)
                 else length(unique(dataset[[unique_col]]))
    } else {
      dataset <- NULL
      prev_n  <- 0L
    }
    list(dataset = dataset, n = target_n - prev_n)
  }

  list(
    # ── Core administrative datasets ─────────────────────────────────────────
    Raw_SITE = list(
      dataset         = "Raw_SITE",
      required_inputs = c("data", "previous_data", "combined_specs", "n", "start_date"),
      count_fn        = function(counts, snapshot_idx) counts$site_count[snapshot_idx],
      generate_fn     = function(context) {
        Raw_SITE(
          data          = context$data,
          previous_data = context$previous_data,
          spec          = context$combined_specs,
          startDate     = context$start_date,
          n_sites       = context$n,
          split_vars    = list("Country_State_City")
        )
      }
    ),

    Raw_SUBJ = list(
      dataset         = "Raw_SUBJ",
      required_inputs = c("data", "previous_data", "combined_specs", "n",
                          "start_date", "end_date"),
      count_fn        = function(counts, snapshot_idx) counts$subject_count[snapshot_idx],
      generate_fn     = function(context) {
        Raw_SUBJ(
          data          = context$data,
          previous_data = context$previous_data,
          spec          = context$combined_specs,
          startDate     = context$start_date,
          endDate       = context$end_date,
          n_subj        = context$n,
          split_vars    = list(
            "subject_site_synq",
            "subjid_subject_nsv",
            "enrollyn_enrolldt_timeonstudy_firstparticipantdate_firstdosedate_timeontreatment"
          )
        )
      }
    ),

    Raw_ENROLL = list(
      dataset         = "Raw_ENROLL",
      required_inputs = c("data", "previous_data", "combined_specs", "n", "start_date"),
      count_fn        = function(counts, snapshot_idx) counts$subject_count[snapshot_idx],
      generate_fn     = function(context) {
        Raw_ENROLL(
          data          = context$data,
          previous_data = context$previous_data,
          spec          = context$combined_specs,
          startDate     = context$start_date,
          n_enroll      = context$n,
          split_vars    = list("subject_to_enrollment")
        )
      }
    ),

    Raw_IE = list(
      dataset         = "Raw_IE",
      required_inputs = c("data", "previous_data", "combined_specs", "n"),
      count_fn        = function(counts, snapshot_idx) counts$subject_count[snapshot_idx],
      generate_fn     = function(context) {
        Raw_IE(
          data          = context$data,
          previous_data = context$previous_data,
          spec          = context$combined_specs,
          n_IE          = context$n,
          split_vars    = list("subject_to_ie", "tiver_ietestcd_ietest_ieorres_iecat")
        )
      }
    ),

    Raw_EXCLUSION = list(
      dataset         = "Raw_EXCLUSION",
      required_inputs = c("data", "previous_data", "combined_specs", "n"),
      count_fn        = function(counts, snapshot_idx) counts$subject_count[snapshot_idx],
      generate_fn     = function(context) {
        Raw_EXCLUSION(
          data          = context$data,
          previous_data = context$previous_data,
          spec          = context$combined_specs,
          n_EXCLUSION   = context$n
        )
      }
    ),

    # ── Visit / time-on-study datasets ───────────────────────────────────────
    Raw_VISIT = list(
      dataset         = "Raw_VISIT",
      required_inputs = c("data", "previous_data", "combined_specs", "n",
                          "start_date", "snapshot_count", "snapshot_width"),
      count_fn        = function(counts, snapshot_idx) counts$subject_count[snapshot_idx],
      generate_fn     = function(context) {
        Raw_VISIT(
          data           = context$data,
          previous_data  = context$previous_data,
          spec           = context$combined_specs,
          startDate      = context$start_date,
          n              = context$n,
          SnapshotCount  = context$snapshot_count,
          SnapshotWidth  = context$snapshot_width,
          split_vars     = list("subjid_invid")
        )
      }
    ),

    Raw_STUDCOMP = list(
      dataset         = "Raw_STUDCOMP",
      required_inputs = c("data", "previous_data", "combined_specs", "n", "start_date"),
      count_fn        = function(counts, snapshot_idx)
                          ceiling(counts$subject_count[snapshot_idx] / 10),
      generate_fn     = function(context) {
        Raw_STUDCOMP(
          data          = context$data,
          previous_data = context$previous_data,
          spec          = context$combined_specs,
          startDate     = context$start_date,
          n             = context$n,
          split_vars    = list("subjid_invid_unique")
        )
      }
    ),

    # ── EDC / data management datasets ───────────────────────────────────────
    Raw_DATACHG = list(
      dataset         = "Raw_DATACHG",
      required_inputs = c("data", "previous_data", "combined_specs", "n", "start_date"),
      count_fn        = function(counts, snapshot_idx) counts$subject_count[snapshot_idx],
      generate_fn     = function(context) {
        Raw_DATACHG(
          data          = context$data,
          previous_data = context$previous_data,
          spec          = context$combined_specs,
          startDate     = context$start_date,
          n             = context$n,
          split_vars    = list("subject_nsv_visit_repeated")
        )
      }
    ),

    Raw_DATAENT = list(
      dataset         = "Raw_DATAENT",
      required_inputs = c("data", "previous_data", "combined_specs", "n", "start_date"),
      count_fn        = function(counts, snapshot_idx) counts$subject_count[snapshot_idx],
      generate_fn     = function(context) {
        Raw_DATAENT(
          data          = context$data,
          previous_data = context$previous_data,
          spec          = context$combined_specs,
          startDate     = context$start_date,
          n             = context$n,
          split_vars    = list("subject_nsv_visit_repeated")
        )
      }
    ),

    Raw_QUERY = list(
      dataset         = "Raw_QUERY",
      required_inputs = c("data", "previous_data", "combined_specs", "n", "start_date"),
      count_fn        = function(counts, snapshot_idx) counts$subject_count[snapshot_idx],
      generate_fn     = function(context) {
        Raw_QUERY(
          data          = context$data,
          previous_data = context$previous_data,
          spec          = context$combined_specs,
          startDate     = context$start_date,
          n             = context$n,
          split_vars    = list("subject_nsv_visit_repeated")
        )
      }
    ),

    # ── Clinical outcome datasets ─────────────────────────────────────────────
    Raw_AE = list(
      dataset = "Raw_AE",
      required_inputs = c("data", "previous_data", "combined_specs", "n", "start_date", "end_date"),
      count_fn = function(counts, snapshot_idx) counts$ae_count[snapshot_idx],
      generate_fn = function(context) {
        spec      <- context$combined_specs
        curr_spec <- spec$Raw_AE
        data          <- context$data
        previous_data <- context$previous_data

        if ("Raw_AE" %in% names(previous_data)) {
          dataset          <- previous_data$Raw_AE
          previous_row_num <- nrow(dataset)
        } else {
          dataset          <- NULL
          previous_row_num <- 0
        }

        n <- context$n - previous_row_num
        if (n <= 0) return(dataset)

        if (all(c("aeser", "aest_dt", "aeen_dt", "mdrpt_nsv", "mdrsoc_nsv", "aetoxgr") %in% names(curr_spec))) {
          curr_spec$aeser           <- list(required = TRUE)
          curr_spec$aest_dt_aeen_dt <- list(required = TRUE)
          curr_spec$aest_dt         <- NULL
          curr_spec$aeen_dt         <- NULL
          curr_spec$mdrpt_nsv       <- list(required = TRUE)
          curr_spec$mdrsoc_nsv      <- list(required = TRUE)
          curr_spec$aetoxgr         <- list(required = TRUE)
        }

        args <- list(
          subjid          = list(n, external_subjid = data$Raw_SUBJ$subjid),
          aest_dt_aeen_dt = list(n, context$start_date, context$end_date),
          studyid         = list(n, data$Raw_STUDY$protocol_number[[1]]),
          default         = list(n, context$start_date)
        )

        as.data.frame(add_new_var_data(dataset, curr_spec, args, spec$Raw_AE,
                                        split_vars = list("aest_dt_aeen_dt")))
      }
    ),
    Raw_LB = list(
      dataset = "Raw_LB",
      required_inputs = c("data", "previous_data", "combined_specs", "n", "start_date"),
      count_fn = function(counts, snapshot_idx) counts$subject_count[snapshot_idx],
      generate_fn = function(context) {
        spec      <- context$combined_specs
        curr_spec <- spec$Raw_LB
        data          <- context$data
        previous_data <- context$previous_data

        if ("Raw_LB" %in% names(previous_data)) {
          dataset          <- previous_data$Raw_LB
          previous_row_num <- length(unique(dataset$subjid))
        } else {
          dataset          <- NULL
          previous_row_num <- 0
        }

        n <- context$n - previous_row_num
        if (n <= 0) return(dataset)

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

        if (!("battrnam" %in% names(curr_spec))) curr_spec$battrnam <- list(required = TRUE)
        if (!("lbtstnam" %in% names(curr_spec))) curr_spec$lbtstnam <- list(required = TRUE)
        if (!("visnam"   %in% names(curr_spec))) curr_spec$visnam   <- list(required = TRUE)

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
          studyid             = list(all_n, data$Raw_STUDY$protocol_number[[1]]),
          lb_dt               = list(all_n, context$start_date),
          default             = list(all_n, subj_visits, tests)
        )

        as.data.frame(add_new_var_data(dataset, curr_spec, args, spec$Raw_LB,
                                        split_vars = list("subj_visit_repeated")))
      }
    ),

    # ── Protocol deviation / study drug datasets ─────────────────────────────
    Raw_PD = list(
      dataset         = "Raw_PD",
      required_inputs = c("data", "previous_data", "combined_specs", "n", "start_date"),
      count_fn        = function(counts, snapshot_idx)
                          counts$subject_count[snapshot_idx] * 3L,
      generate_fn     = function(context) {
        d         <- .delta("Raw_PD", context$previous_data, context$n)
        if (d$n <= 0) return(d$dataset)
        spec      <- context$combined_specs
        curr_spec <- spec$Raw_PD
        args <- list(
          studyid = list(d$n, context$data$Raw_STUDY$protocol_number[[1]]),
          subjid  = list(d$n, external_subjid = context$data$Raw_SUBJ$subjid),
          default = list(d$n, context$start_date)
        )
        as.data.frame(add_new_var_data(d$dataset, curr_spec, args, spec$Raw_PD))
      }
    ),

    Raw_SDRGCOMP = list(
      dataset         = "Raw_SDRGCOMP",
      required_inputs = c("data", "previous_data", "combined_specs", "n", "start_date"),
      count_fn        = function(counts, snapshot_idx)
                          ceiling(counts$subject_count[snapshot_idx]/2),
      generate_fn     = function(context) {
        d         <- .delta("Raw_SDRGCOMP", context$previous_data, context$n)
        if (d$n <= 0) return(d$dataset)
        spec      <- context$combined_specs
        curr_spec <- spec$Raw_SDRGCOMP
        args <- list(
          subjid  = list(d$n, unique(context$data$Raw_VISIT$subjid), replace = FALSE),
          studyid = list(d$n, context$data$Raw_STUDY$protocol_number[[1]]),
          default = list(d$n, context$start_date)
        )
        as.data.frame(add_new_var_data(d$dataset, curr_spec, args, spec$Raw_SDRGCOMP))
      }
    ),

    # ── Specialty clinical datasets ───────────────────────────────────────────
    Raw_Consents = list(
      dataset         = "Raw_Consents",
      required_inputs = c("data", "previous_data", "combined_specs", "n", "start_date"),
      count_fn        = function(counts, snapshot_idx)
                          ceiling(counts$subject_count[snapshot_idx] / 75),
      generate_fn     = function(context) {
        d         <- .delta("Raw_Consents", context$previous_data, context$n)
        if (d$n <= 0) return(d$dataset)
        spec      <- context$combined_specs
        curr_spec <- spec$Raw_Consents
        if (all(c("cons_dt", "constype", "conscat") %in% names(curr_spec))) {
          curr_spec$cons_dt  <- list(required = TRUE)
          curr_spec$constype <- list(required = TRUE)
          curr_spec$conscat  <- list(required = TRUE)
        }
        args <- list(
          subjid  = list(d$n, external_subjid = context$data$Raw_SUBJ$subjid,
                         replace = FALSE),
          studyid = list(d$n, context$data$Raw_STUDY$protocol_number[[1]]),
          cons_dt = list(d$n, context$start_date),
          default = list(d$n)
        )
        as.data.frame(add_new_var_data(d$dataset, curr_spec, args, spec$Raw_Consents))
      }
    ),

    Raw_Death = list(
      dataset         = "Raw_Death",
      required_inputs = c("data", "previous_data", "combined_specs", "n", "start_date"),
      count_fn        = function(counts, snapshot_idx)
                          ceiling(counts$subject_count[snapshot_idx] / 85),
      generate_fn     = function(context) {
        d         <- .delta("Raw_Death", context$previous_data, context$n)
        if (d$n <= 0) return(d$dataset)
        spec      <- context$combined_specs
        curr_spec <- spec$Raw_Death
        if ("death_dt" %in% names(curr_spec)) {
          curr_spec$death_dt <- list(required = TRUE)
        }
        args <- list(
          subjid   = list(d$n, external_subjid = context$data$Raw_SUBJ$subjid,
                          replace = FALSE),
          studyid  = list(d$n, context$data$Raw_STUDY$protocol_number[[1]]),
          death_dt = list(d$n, context$start_date),
          default  = list(d$n)
        )
        as.data.frame(add_new_var_data(d$dataset, curr_spec, args, spec$Raw_Death))
      }
    ),

    Raw_AntiCancer = list(
      dataset         = "Raw_AntiCancer",
      required_inputs = c("data", "previous_data", "combined_specs", "n", "start_date"),
      count_fn        = function(counts, snapshot_idx)
                          ceiling(counts$subject_count[snapshot_idx] / 10),
      generate_fn     = function(context) {
        d         <- .delta("Raw_AntiCancer", context$previous_data, context$n)
        if (d$n <= 0) return(d$dataset)
        spec      <- context$combined_specs
        curr_spec <- spec$Raw_AntiCancer
        if (all(c("cmtrt", "cmst_dt") %in% names(curr_spec))) {
          curr_spec$cmtrt   <- list(required = TRUE)
          curr_spec$cmst_dt <- list(required = TRUE)
        }
        args <- list(
          subjid  = list(d$n, external_subjid = context$data$Raw_SUBJ$subjid,
                         replace = FALSE),
          studyid = list(d$n, context$data$Raw_STUDY$protocol_number[[1]]),
          cmst_dt = list(d$n, context$start_date),
          default = list(d$n)
        )
        as.data.frame(add_new_var_data(d$dataset, curr_spec, args, spec$Raw_AntiCancer))
      }
    ),

    Raw_Randomization = list(
      dataset         = "Raw_Randomization",
      required_inputs = c("data", "previous_data", "combined_specs", "n", "start_date"),
      count_fn        = function(counts, snapshot_idx) counts$subject_count[snapshot_idx],
      generate_fn     = function(context) {
        Raw_Randomization(
          data          = context$data,
          previous_data = context$previous_data,
          spec          = context$combined_specs,
          startDate     = context$start_date,
          n             = context$n,
          split_vars    = list("subjid_invid_country")
        )
      }
    ),

    Raw_OverallResponse = list(
      dataset         = "Raw_OverallResponse",
      required_inputs = c("data", "previous_data", "combined_specs", "n"),
      count_fn        = function(counts, snapshot_idx) counts$subject_count[snapshot_idx],
      generate_fn     = function(context) {
        Raw_OverallResponse(
          data          = context$data,
          previous_data = context$previous_data,
          spec          = context$combined_specs,
          n             = context$n,
          split_vars    = list("subjid_rs_dt")
        )
      }
    ),

    Raw_PK = list(
      dataset         = "Raw_PK",
      required_inputs = c("data", "previous_data", "combined_specs", "n"),
      count_fn        = function(counts, snapshot_idx) counts$subject_count[snapshot_idx],
      generate_fn     = function(context) {
        Raw_PK(
          data          = context$data,
          previous_data = context$previous_data,
          spec          = context$combined_specs,
          n             = context$n,
          split_vars    = list("subjid_visit_pkdat")
        )
      }
    ),

    Raw_Baseline = list(
      dataset         = "Raw_Baseline",
      required_inputs = c("data", "previous_data", "combined_specs", "n", "start_date"),
      count_fn        = function(counts, snapshot_idx) counts$subject_count[snapshot_idx],
      generate_fn     = function(context) {
        d         <- .delta("Raw_Baseline", context$previous_data, context$n)
        if (d$n <= 0) return(d$dataset)
        spec      <- context$combined_specs
        curr_spec <- spec$Raw_Baseline
        if ("scan_dt" %in% names(curr_spec)) {
          curr_spec$scan_dt <- list(required = TRUE)
        }
        args <- list(
          subjid  = list(d$n, external_subjid = context$data$Raw_SUBJ$subjid,
                         replace = FALSE),
          studyid = list(d$n, context$data$Raw_STUDY$protocol_number[[1]]),
          scan_dt = list(d$n, context$start_date),
          default = list(d$n)
        )
        as.data.frame(add_new_var_data(d$dataset, curr_spec, args, spec$Raw_Baseline))
      }
    )
  )
}

validate_domain_registry_entry <- function(entry) {
  required_fields <- c("dataset", "required_inputs", "count_fn", "generate_fn")
  missing_fields <- setdiff(required_fields, names(entry))
  if (length(missing_fields) > 0) {
    stop("Invalid domain registry entry. Missing fields: ", paste(missing_fields, collapse = ", "))
  }

  if (!is.character(entry$dataset) || length(entry$dataset) != 1) {
    stop("Invalid domain registry entry: 'dataset' must be a single character value")
  }
  if (!is.character(entry$required_inputs) || length(entry$required_inputs) == 0) {
    stop("Invalid domain registry entry: 'required_inputs' must be a non-empty character vector")
  }
  if (!is.function(entry$count_fn) || !is.function(entry$generate_fn)) {
    stop("Invalid domain registry entry: 'count_fn' and 'generate_fn' must be functions")
  }

  invisible(TRUE)
}

generate_domain_from_registry <- function(data_type, context, registry = get_domain_registry()) {
  if (!data_type %in% names(registry)) {
    return(NULL)
  }

  entry <- registry[[data_type]]
  validate_domain_registry_entry(entry)

  missing_inputs <- setdiff(entry$required_inputs, names(context))
  if (length(missing_inputs) > 0) {
    stop("Missing required context fields for ", data_type, ": ", paste(missing_inputs, collapse = ", "))
  }

  as.data.frame(entry$generate_fn(context))
}
