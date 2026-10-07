# execute_analytics_pipeline runs workflows and tolerates failures

    Code
      res <- execute_analytics_pipeline(raw, list(verbose = TRUE, analytics_package = "gsm.kri"))
    Output
      Running analytics pipeline on 2 snapshots...
      
      Processing snapshot 1/2 (snap1)...
      Available datasets: Raw_SUBJ, Raw_SITE, Raw_IE, Raw_ENROLL, Raw_PD 
    Condition
      Warning in `value[[3L]]()`:
      Skipping workflow bad_wf for snapshot snap1: boom
    Output
      
      Processing snapshot 2/2 (snap2)...
      Available datasets: Raw_SUBJ 
    Condition
      Warning in `doTryCatch()`:
      Skipping snapshot snap2: missing required datasets (Raw_SITE, Raw_SUBJ)
    Output
      
      Analytics pipeline completed for 1 of 1 snapshots.

# execute_analytics_pipeline converts unexpected errors to warnings

    Code
      res <- execute_analytics_pipeline(list(), list(verbose = TRUE,
        analytics_package = "gsm.kri"))
    Output
      Running analytics pipeline on 0 snapshots...
    Condition
      Warning in `value[[3L]]()`:
      GSM analytics pipeline failed: kaboom
    Output
      Analytics pipeline skipped due to error.

# execute_reporting_pipeline runs per-snapshot reporting and tolerates problems

    Code
      res <- execute_reporting_pipeline(input, list(verbose = TRUE))
    Output
      Running reporting pipeline for snapshot: ok
    Condition
      Warning in `doTryCatch()`:
      Skipping reporting for snapshot incomplete: analytics pipeline output is missing mapped data, results, or workflow list.
    Output
      Running reporting pipeline for snapshot: ok2
      Reporting pipeline completed for 2 of 2 snapshots.

