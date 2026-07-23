create_snapshot_reports <- function(study_data) {
  previous_snapshot <- NULL
  for (snapshot_names in names(study_data)) {
    snapshot_raw_data <- study_data[[snapshot_names]]
    current_snapshot <- generate_risk_signals_report(snapshot_raw_data)

    if (!is.null(previous_snapshot)) {
      lAnalysis_site <- purrr::imap(previous_snapshot$lAnalysis_site, \(this, metric)
      purrr::imap(this[grep("Analysis", names(this))], \(x, idx)
      dplyr::bind_rows(x, current_snapshot$lAnalysis_site[[metric]][[idx]])))
      lAnalysis_country <- purrr::imap(previous_snapshot$lAnalysis_country, \(this, metric)
      purrr::imap(this[grep("Analysis", names(this))], \(x, idx)
      dplyr::bind_rows(x, current_snapshot$lAnalysis_country[[metric]][[idx]])))

      lReporting_site <- purrr::imap(previous_snapshot$lReporting_site[grep("Reporting", names(previous_snapshot$lReporting_site))], \(x, idx)
      dplyr::bind_rows(x, current_snapshot$lReporting_site[[idx]]))
      lReporting_country <- purrr::imap(previous_snapshot$lReporting_country[grep("Reporting", names(previous_snapshot$lReporting_country))], \(x, idx)
      dplyr::bind_rows(x, current_snapshot$lReporting_country[[idx]]))

      current_snapshot <- list(
        lAnalysis_site = lAnalysis_site,
        lAnalysis_country = lAnalysis_country,
        lReporting_site = lReporting_site,
        lReporting_country = lReporting_country
      )
    }

    study_data[[snapshot_names]]$snapshot <- current_snapshot
    previous_snapshot <- current_snapshot
  }
  return(study_data)
}
