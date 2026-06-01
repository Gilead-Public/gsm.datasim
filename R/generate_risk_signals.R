generate_risk_signals_report <- function(lData) {
  mapping_wf <- workr::MakeWorkflowList(strPath = "workflow/1_mappings")
  mapped <- workr::RunWorkflows(mapping_wf, lData, bKeepInputData = TRUE)

  # Step 2 - Create Analysis Data - Generate 12 KRIs
  kri_wf <- workr::MakeWorkflowList(strPath = "workflow/2_metrics", strNames = "kri")
  kris <- workr::RunWorkflows(kri_wf, mapped)

  cou_wf <- workr::MakeWorkflowList(strPath = "workflow/2_metrics", strNames = "cou")
  cous <- workr::RunWorkflows(cou_wf, mapped)

  # Step 3 - Create Reporting Data - Import Metadata and stack KRI Results

  reporting_wf_site <- workr::MakeWorkflowList(strPath = "workflow/3_reporting")
  reporting_site <- workr::RunWorkflows(
    reporting_wf_site,
    c(mapped, list(lAnalyzed = kris, lWorkflows = kri_wf))
  )

  reporting_wf_country <- workr::MakeWorkflowList(strPath = "workflow/3_reporting")
  reporting_country <- workr::RunWorkflows(
    reporting_wf_country,
    c(mapped, list(lAnalyzed = cous, lWorkflows = cou_wf))
  )

  return(
    list(
      lAnalysis_site = kris,
      lAnalysis_country = cous,
      lReporting_site = reporting_site,
      lReporting_country = reporting_country
    )
  )
}
