# AE severity grading simulation.
#
# Raw_AE records get a MedDRA-style preferred term (`mdrpt_nsv`) and system
# organ class (`mdrsoc_nsv`), then a CTCAE grade (`aetoxgr`) drawn from a
# term-specific distribution shifted by the grading tendency of the site the
# record's subject belongs to. A few sites systematically over-grade and a few
# under-grade, so the two-sided AE grading KRIs (kri0016 / kri0017 in gsm.kri)
# have true positives in both directions.

#' Catalog of simulated AE preferred terms
#'
#' Common preferred terms with their SOC, a relative frequency weight, and a
#' term-specific CTCAE grade 1-5 distribution. The weighted mix gives roughly
#' 21% Grade 3+ study-wide; mild terms (e.g. Headache) rarely reach Grade 3
#' while serious terms (e.g. Sepsis) start at Grade 3.
#'
#' @returns a data.frame with columns `mdrpt_nsv`, `mdrsoc_nsv`, `weight`, and
#'   `g1`-`g5`.
#' @family internal
#' @keywords internal
#' @noRd
ae_term_catalog <- function() {
  gi <- "Gastrointestinal disorders"
  gen <- "General disorders and administration site conditions"
  nerv <- "Nervous system disorders"
  skin <- "Skin and subcutaneous tissue disorders"
  resp <- "Respiratory, thoracic and mediastinal disorders"
  inf <- "Infections and infestations"
  blood <- "Blood and lymphatic system disorders"
  metab <- "Metabolism and nutrition disorders"
  musc <- "Musculoskeletal and connective tissue disorders"

  rows <- list(
    list("Nausea", gi, 10, c(0.62, 0.30, 0.08, 0, 0)),
    list("Diarrhoea", gi, 8, c(0.55, 0.30, 0.14, 0.01, 0)),
    list("Vomiting", gi, 5, c(0.60, 0.30, 0.10, 0, 0)),
    list("Constipation", gi, 5, c(0.70, 0.26, 0.04, 0, 0)),
    list("Fatigue", gen, 10, c(0.50, 0.36, 0.14, 0, 0)),
    list("Pyrexia", gen, 5, c(0.62, 0.30, 0.07, 0.01, 0)),
    list("Headache", nerv, 6, c(0.72, 0.24, 0.04, 0, 0)),
    list("Dizziness", nerv, 3, c(0.78, 0.19, 0.03, 0, 0)),
    list("Peripheral sensory neuropathy", nerv, 3, c(0.50, 0.38, 0.12, 0, 0)),
    list("Rash", skin, 5, c(0.58, 0.30, 0.11, 0.01, 0)),
    list("Pruritus", skin, 3, c(0.74, 0.23, 0.03, 0, 0)),
    list("Cough", resp, 4, c(0.74, 0.23, 0.03, 0, 0)),
    list("Dyspnoea", resp, 3, c(0.40, 0.35, 0.19, 0.04, 0.02)),
    list("Pulmonary embolism", resp, 1, c(0, 0.15, 0.55, 0.20, 0.10)),
    list("Pneumonia", inf, 4, c(0.04, 0.24, 0.50, 0.13, 0.09)),
    list("Urinary tract infection", inf, 3, c(0.28, 0.50, 0.20, 0.02, 0)),
    list("Sepsis", inf, 1, c(0, 0, 0.40, 0.40, 0.20)),
    list("Anaemia", blood, 6, c(0.28, 0.35, 0.32, 0.05, 0)),
    list("Neutropenia", blood, 6, c(0.10, 0.25, 0.40, 0.25, 0)),
    list("Thrombocytopenia", blood, 2, c(0.25, 0.30, 0.30, 0.15, 0)),
    list("Febrile neutropenia", blood, 1, c(0, 0, 0.80, 0.17, 0.03)),
    list("Alanine aminotransferase increased", "Investigations", 3, c(0.55, 0.25, 0.15, 0.05, 0)),
    list("Decreased appetite", metab, 4, c(0.58, 0.30, 0.12, 0, 0)),
    list("Hypokalaemia", metab, 2, c(0.45, 0.25, 0.22, 0.08, 0)),
    list("Arthralgia", musc, 3, c(0.64, 0.30, 0.06, 0, 0)),
    list("Back pain", musc, 3, c(0.58, 0.32, 0.10, 0, 0)),
    list("Hypertension", "Vascular disorders", 4, c(0.28, 0.40, 0.29, 0.03, 0)),
    list("Acute kidney injury", "Renal and urinary disorders", 1, c(0.15, 0.30, 0.35, 0.12, 0.08)),
    list("Insomnia", "Psychiatric disorders", 2, c(0.74, 0.24, 0.02, 0, 0))
  )

  grade_probs <- do.call(rbind, lapply(rows, `[[`, 4))
  colnames(grade_probs) <- paste0("g", 1:5)

  data.frame(
    mdrpt_nsv = vapply(rows, `[[`, character(1), 1),
    mdrsoc_nsv = vapply(rows, `[[`, character(1), 2),
    weight = vapply(rows, `[[`, numeric(1), 3),
    grade_probs,
    stringsAsFactors = FALSE
  )
}

sample_ae_term_idx <- function(n) {
  catalog <- ae_term_catalog()
  sample.int(nrow(catalog), n, replace = TRUE, prob = catalog$weight)
}

#' Draw CTCAE grades for AE records
#'
#' Each record's grade comes from its term's distribution in
#' [ae_term_catalog()], shifted on the cumulative-logit scale by `site_shift`:
#' positive shifts move mass toward higher grades (over-grading), negative
#' shifts toward Grade 1 (under-grading). Terms not in the catalog use the
#' study-wide weighted mix.
#'
#' @param terms Character vector of preferred terms, one per record.
#' @param site_shift Numeric shift per record (recycled).
#'
#' @returns an integer vector of grades 1-5.
#' @family internal
#' @keywords internal
#' @noRd
draw_ae_grades <- function(terms, site_shift = 0) {
  n <- length(terms)
  if (n == 0) {
    return(integer(0))
  }

  catalog <- ae_term_catalog()
  catalog_probs <- as.matrix(catalog[paste0("g", 1:5)])
  study_mix <- colSums(catalog_probs * catalog$weight) / sum(catalog$weight)

  probs <- catalog_probs[match(terms, catalog$mdrpt_nsv), , drop = FALSE]
  unknown <- is.na(probs[, 1])
  probs[unknown, ] <- rep(study_mix, each = sum(unknown))

  # Cumulative P(grade <= k) for k = 1..4, shifted on the logit scale.
  cum <- pmin(t(apply(probs, 1, cumsum))[, 1:4, drop = FALSE], 1)
  shift <- rep_len(site_shift, n)
  cum_shifted <- stats::plogis(stats::qlogis(cum) - shift)

  u <- stats::runif(n)
  as.integer(1L + rowSums(u > cum_shifted))
}

# Deterministic pseudo-uniform in [0, 1) per string. Each string seeds its
# own draw and the global RNG state is restored afterwards, so a site keeps the
# same grading tendency across snapshots without shifting other generators.
hash_unif <- function(x) {
  modulus <- 2147483647
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (had_seed) {
    old_seed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
  }
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  })

  vapply(as.character(x), function(s) {
    h <- 0
    for (code in utf8ToInt(s)) {
      h <- (h * 31 + code) %% modulus
    }
    set.seed(as.integer(h), kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
    stats::runif(1)
  }, numeric(1), USE.NAMES = FALSE)
}

#' Site-level AE grading tendencies
#'
#' Assigns each site a shift on the cumulative-logit grade scale. Most sites
#' get small random variation; about 5% of sites systematically over-grade
#' and about 5% under-grade (at least one of each whenever there are two or
#' more sites). The assignment is a deterministic function of the study and
#' site ids, so it is stable across snapshots and does not consume RNG draws.
#'
#' @param invid Character vector of site ids.
#' @param studyid Study id, so different studies seed different sites.
#' @param intensity Outlier intensity (see `get_outlier_intensity()`). Scales
#'   the over/under-grading shift; `0` turns site effects off.
#' @param hotspot_frac Fraction of sites seeded in each direction.
#' @param hotspot_shift Logit shift for seeded sites at intensity 1.
#' @param noise_sd SD of the background site-to-site variation.
#'
#' @returns a data.frame with columns `invid`, `grading` (`"over"`,
#'   `"under"`, or `"typical"`), and `shift`, one row per unique site.
#' @family internal
#' @keywords internal
#' @noRd
ae_site_grading_profile <- function(invid,
                                    studyid = "",
                                    intensity = get_outlier_intensity(),
                                    hotspot_frac = 0.05,
                                    hotspot_shift = 2,
                                    noise_sd = 0.2) {
  sites <- sort(unique(as.character(invid[!is.na(invid)])))
  if (is.null(studyid) || length(studyid) == 0 || is.na(studyid[[1]])) {
    studyid <- ""
  }
  key <- paste(studyid[[1]], sites, sep = "::")

  u <- hash_unif(key)
  grading <- ifelse(u < hotspot_frac, "over", ifelse(u >= 1 - hotspot_frac, "under", "typical"))

  # Guarantee a true positive in each direction for small studies.
  if (length(sites) >= 2) {
    if (!any(grading == "over")) {
      grading[which.min(ifelse(grading == "under", Inf, u))] <- "over"
    }
    if (!any(grading == "under")) {
      grading[which.max(ifelse(grading == "over", -Inf, u))] <- "under"
    }
  }

  noise <- stats::qnorm(pmin(pmax(hash_unif(paste0(key, "::noise")), 1e-6), 1 - 1e-6)) * noise_sd
  shift <- noise + hotspot_shift * intensity * ((grading == "over") - (grading == "under"))
  if (intensity <= 0) {
    grading <- rep("typical", length(sites))
    shift <- rep(0, length(sites))
  }

  data.frame(invid = sites, grading = grading, shift = shift, stringsAsFactors = FALSE)
}

#' Simulate AE terms, grades, and seriousness keyed on each record's site
#'
#' Post-processes freshly generated Raw_AE rows. Per-column generators run
#' independently of the record's `subjid`, so this step re-draws the
#' term/SOC pair, `aetoxgr`, and `aeser` using the site of the subject each
#' record actually belongs to.
#'
#' @param dataset Raw_AE data.frame (already renamed per spec).
#' @param new_rows Row indices generated in this snapshot; earlier rows are
#'   left untouched.
#' @param Raw_SUBJ_data Raw_SUBJ data.frame with `subjid` and `invid`.
#' @param studyid Study id used to seed site grading tendencies.
#' @param spec The Raw_AE spec, used to resolve `source_col` renames.
#' @param intensity Outlier intensity.
#'
#' @returns `dataset` with updated AE term, SOC, grade, and seriousness.
#' @family internal
#' @keywords internal
#' @noRd
simulate_ae_grading <- function(dataset,
                                new_rows,
                                Raw_SUBJ_data = NULL,
                                studyid = "",
                                spec = NULL,
                                intensity = get_outlier_intensity()) {
  new_rows <- new_rows[new_rows >= 1 & new_rows <= NROW(dataset)]
  if (!is.data.frame(dataset) || length(new_rows) == 0) {
    return(dataset)
  }

  col_name <- function(var) {
    source_col <- spec[[var]]$source_col
    if (is.null(source_col)) var else source_col
  }
  subjid_col <- col_name("subjid")
  term_col <- col_name("mdrpt_nsv")
  soc_col <- col_name("mdrsoc_nsv")
  grade_col <- col_name("aetoxgr")
  ser_col <- col_name("aeser")

  subjids <- if (subjid_col %in% names(dataset)) dataset[[subjid_col]][new_rows] else NULL
  has_sites <- !is.null(subjids) && is.data.frame(Raw_SUBJ_data) &&
    all(c("subjid", "invid") %in% names(Raw_SUBJ_data))

  catalog <- ae_term_catalog()
  term_idx <- sample_ae_term_idx(length(new_rows))
  if (term_col %in% names(dataset)) {
    dataset[[term_col]][new_rows] <- catalog$mdrpt_nsv[term_idx]
  }
  if (soc_col %in% names(dataset)) {
    dataset[[soc_col]][new_rows] <- catalog$mdrsoc_nsv[term_idx]
  }

  if (grade_col %in% names(dataset)) {
    site_shift <- 0
    if (has_sites) {
      row_sites <- Raw_SUBJ_data$invid[match(subjids, Raw_SUBJ_data$subjid)]
      profile <- ae_site_grading_profile(Raw_SUBJ_data$invid, studyid, intensity = intensity)
      site_shift <- profile$shift[match(row_sites, profile$invid)]
      site_shift[is.na(site_shift)] <- 0
    }
    grades <- draw_ae_grades(catalog$mdrpt_nsv[term_idx], site_shift = site_shift)
    if (is.character(dataset[[grade_col]])) {
      grades <- as.character(grades)
    }
    dataset[[grade_col]][new_rows] <- grades
  }

  if (ser_col %in% names(dataset) && has_sites) {
    dataset[[ser_col]][new_rows] <- aeser(
      length(new_rows),
      Raw_SUBJ_data = Raw_SUBJ_data,
      row_keys = subjids
    )
  }

  dataset
}
