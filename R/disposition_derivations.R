#' Add simulated upstream study disposition fields
#'
#' Follows the draft analytics specification for gsm.roadmap#61, section 2.1.
#' Uses the snapshot's source records without changing them or defining metric
#' denominators. The provisional withdrawal column matches gsm.mapping#169.
#'
#' @param data A named list of raw snapshot domains.
#' @param endDate The snapshot cutoff date.
#' @return Raw_SUBJ with six upstream disposition fields.
#' @keywords internal
#' @noRd
apply_disposition_derivations <- function(data, endDate) {
  subj <- data$Raw_SUBJ
  if (is.null(subj)) {
    return(NULL)
  }
  stopifnot(
    all(c("subjid", "enrollyn") %in% names(subj)),
    length(endDate) == 1L,
    !is.na(as.Date(endDate))
  )
  endDate <- as.Date(endDate)
  n <- nrow(subj)
  missing_dates <- rep(as.Date(NA), n)

  subject_dates <- function(df, columns, latest = FALSE) {
    result <- missing_dates
    if (is.null(df) || !("subjid" %in% names(df))) {
      return(result)
    }
    for (column in intersect(columns, names(df))) {
      dates <- as.Date(df[[column]])
      eligible <- !is.na(dates) & dates <= endDate & !is.na(df$subjid)
      if (any(eligible)) {
        by_subject <- tapply(
          as.numeric(dates[eligible]), df$subjid[eligible],
          if (latest) max else min
        )
        result <- as.Date(
          as.numeric(by_subject[match(subj$subjid, names(by_subject))]),
          origin = "1970-01-01"
        )
      }
    }
    result
  }

  # Include only clinical event domains; metadata and prior drv_ values are not
  # new contact events. Source schemas may use Date or POSIXct event columns.
  contact_domains <- c(
    "Raw_SUBJ", "Raw_ENROLL", "Raw_Consents", "Raw_VISIT",
    "Raw_Randomization", "Raw_LB", "Raw_AE", "Raw_Death", "Raw_DEATH",
    "Raw_OverallResponse", "Raw_AntiCancer"
  )
  last_contact <- missing_dates
  for (domain in intersect(contact_domains, names(data))) {
    df <- data[[domain]]
    columns <- names(df)[vapply(df, function(x) {
      inherits(x, "Date") || inherits(x, "POSIXt")
    }, logical(1))]
    columns <- setdiff(columns, "mincreated_dts")
    if (domain == "Raw_SUBJ") {
      columns <- intersect(
        columns, c("enrolldt", "firstparticipantdate", "firstdosedate")
      )
    }
    for (column in columns) {
      dates <- subject_dates(df, column, latest = TRUE)
      replace <- !is.na(dates) & (is.na(last_contact) | dates > last_contact)
      last_contact[replace] <- dates[replace]
    }
  }

  consent <- data$Raw_Consents
  if (!is.null(consent)) {
    stopifnot(all(c("subjid", "constype", "conscat", "cons_dt") %in% names(consent)))
    consent <- consent[
      toupper(consent$constype) %in% "WITHDRAWAL OF CONSENT" &
        toupper(consent$conscat) %in% "MAIN STUDY CONSENT",
    ]
  }
  withdrawn <- subject_dates(consent, "cons_dt")
  death <- subject_dates(data$Raw_Death, "death_dt")
  other_death <- subject_dates(data$Raw_DEATH, "death_dt")
  replace <- !is.na(other_death) & (is.na(death) | other_death < death)
  death[replace] <- other_death[replace]

  ae <- data$Raw_AE
  if (!is.null(ae)) {
    fatal <- rep(FALSE, nrow(ae))
    if ("aetoxgr" %in% names(ae)) {
      fatal <- fatal | toupper(as.character(ae$aetoxgr)) %in% c("5", "GRADE 5")
    }
    if ("aeout" %in% names(ae)) {
      fatal <- fatal | toupper(ae$aeout) %in% "FATAL"
    }
    ae <- ae[fatal, ]
  }
  ae_death <- subject_dates(ae, "aeen_dt")

  compyn <- compreas <- rep(NA_character_, n)
  completion <- data$Raw_STUDCOMP
  if (!is.null(completion)) {
    stopifnot(all(c("subjid", "compyn", "compreas") %in% names(completion)))
    index <- match(subj$subjid, completion$subjid)
    compyn <- completion$compyn[index]
    compreas <- completion$compreas[index]
  }
  reason <- toupper(compreas)
  has_reason <- !is.na(reason) & nzchar(trimws(reason))
  base_status <- rep("Blank", n)
  base_status[subj$enrollyn %in% "Y" & (is.na(compyn) | compyn == "")] <- "Ongoing"
  base_status[is.na(subj$enrollyn) | subj$enrollyn == ""] <- "In Screening"
  base_status[subj$enrollyn %in% "N"] <- "Screen Failure"
  base_status[compyn %in% "Y"] <- "Study Completion"
  other <- has_reason & !(reason %in% c(
    "WITHDRAWAL BY SUBJECT", "WITHDREW CONSENT", "LOST TO FOLLOW-UP", "DEATH"
  ))
  base_status[other] <- "Study Discontinuation: Other"
  base_status[reason %in% "LOST TO FOLLOW-UP"] <- "Lost to Follow-Up"
  unsupported <- reason %in% c("WITHDRAWAL BY SUBJECT", "WITHDREW CONSENT", "DEATH") |
    (compyn %in% "N" & !has_reason)
  base_status[unsupported] <- "Blank"

  # Earlier dates win; ordered candidates implement only the documented
  # tie-breaking precedence, including LTFU ahead of a same-day death.
  status <- base_status
  selected_date <- missing_dates
  candidates <- list(
    list(date = withdrawn, status = rep("Withdrawal by Participant", n)),
    list(date = last_contact, status = base_status,
         eligible = base_status %in% c("Lost to Follow-Up", "Study Discontinuation: Other")),
    list(date = death, status = rep("Death", n)),
    list(date = ae_death, status = rep("Death", n)),
    list(date = last_contact, status = base_status,
         eligible = !(base_status %in% c("Blank", "Lost to Follow-Up", "Study Discontinuation: Other")))
  )
  for (candidate in candidates) {
    eligible <- candidate$eligible
    if (is.null(eligible)) {
      eligible <- rep(TRUE, n)
    }
    replace <- eligible & !is.na(candidate$date) &
      (is.na(selected_date) | candidate$date < selected_date)
    selected_date[replace] <- candidate$date[replace]
    status[replace] <- candidate$status[replace]
  }

  subj$drv_last_contact_dt <- last_contact
  subj$drv_withdrawn_dt <- withdrawn
  subj$drv_death_dt <- death
  subj$drv_ae_death_dt <- ae_death
  subj$drv_study_disposition <- status
  subj$rbqm_flags <- rep(NA_character_, n)
  if ("subject_nsv" %in% names(subj)) {
    screening_id <- paste(subj$studyid, subj$subject_nsv, sep = ":")
    duplicate <- duplicated(screening_id) | duplicated(screening_id, fromLast = TRUE)
    duplicate <- duplicate & !is.na(subj$subject_nsv) & nzchar(subj$subject_nsv)
    subj$rbqm_flags[duplicate] <- "Duplicate screening ID"
  }
  subj
}
