# quick_longitudinal_study() standard type normalizes the study ID

    Code
      study <- quick_longitudinal_study("s", participants = 10, sites = 2,
        months_duration = 2, include_pipeline = TRUE, verbose = TRUE)
    Output
      Creating standard longitudinal study: s 
      Parameters: 10 participants, 2 sites, 2 months
      Running analytics pipeline...
      Running reporting pipeline...
      Study creation completed successfully!
      Generated 2 snapshots with 14 domains
      Analytics pipeline completed with results for 1 metrics
      Reporting pipeline completed for 1 of 2 snapshots

# quick_longitudinal_study() summarizes multi-snapshot analytics when verbose

    Code
      study <- quick_longitudinal_study("s", months_duration = 2, include_pipeline = TRUE,
        verbose = TRUE)
    Output
      Creating standard longitudinal study: s 
      Parameters: 1000 participants, 150 sites, 2 months
      Running analytics pipeline...
      Running reporting pipeline...
      Study creation completed successfully!
      Generated 2 snapshots with 14 domains
      Analytics pipeline completed with results for 2 metrics
      Reporting pipeline completed for 1 of 1 snapshots

# quick_longitudinal_study() handles empty analytics when verbose

    Code
      study <- quick_longitudinal_study("s", months_duration = 2, include_pipeline = TRUE,
        verbose = TRUE)
    Output
      Creating standard longitudinal study: s 
      Parameters: 1000 participants, 150 sites, 2 months
      Running analytics pipeline...
      Running reporting pipeline...
      Study creation completed successfully!
      Generated 2 snapshots with 14 domains
      Analytics pipeline completed with results for 0 metrics
      Reporting pipeline completed for 0 of 0 snapshots

# quick_longitudinal_study() endpoints type builds config from endpoint domains

    Code
      piped <- quick_longitudinal_study("e", study_type = "endpoints",
        months_duration = 2, include_pipeline = TRUE, verbose = TRUE)
    Output
      Creating endpoints longitudinal study: e 
      Parameters: 1000 participants, 150 sites, 2 months
      Running analytics pipeline...
      Running reporting pipeline...
      Study creation completed successfully!
      Generated 2 snapshots with 2 domains
      Analytics pipeline completed with results for 0 metrics
      Reporting pipeline completed for 1 of 1 snapshots

# create_multiple_longitudinal_studies() generates sequentially and verbosely

    Code
      studies <- create_multiple_longitudinal_studies(c("A", "B"), run_analytics = TRUE,
      run_reporting = TRUE, verbose = TRUE)
    Output
      Creating 2 longitudinal studies...
      Study names: A, B 
      Generating studies sequentially...
      Creating study 1/2: A
        - Study A completed successfully
      Creating study 2/2: B
        - Study B completed successfully
      
      === Study Generation Summary ===
      Total studies created: 2 
        A: 5 participants, 1 sites, 2 snapshots, 1 domains
        B: 5 participants, 1 sites, 2 snapshots, 1 domains
      Analytics completed for 0 studies
      Reporting completed for 0 studies

# create_multiple_longitudinal_studies() exports studies when requested

    Code
      invisible(create_multiple_longitudinal_studies(c("A"), export_studies = TRUE,
      verbose = TRUE))
    Output
      Creating 1 longitudinal studies...
      Study names: A 
      Generating studies sequentially...
      Creating study 1/1: A
        - Study A completed successfully
      Exporting 1 studies to disk...
        - Exporting A ...
      All studies exported to: . 
      
      === Study Generation Summary ===
      Total studies created: 1 
        A: 1 participants, 1 sites, 0 snapshots, 1 domains

# create_multiple_longitudinal_studies() supports parallel generation

    Code
      studies <- create_multiple_longitudinal_studies(c("A", "B"), parallel = TRUE,
      verbose = TRUE)
    Output
      Creating 2 longitudinal studies...
      Study names: A, B 
      Generating studies in parallel...
      
      === Study Generation Summary ===
      Total studies created: 2 
        A: 1 participants, 1 sites, 0 snapshots, 1 domains
        B: 1 participants, 1 sites, 0 snapshots, 1 domains

# print and summary methods handle optional fields and results

    Code
      print(studies)
    Output
      Multiple Longitudinal Studies Collection
      =======================================
      Number of studies: 2 
      
      Study: A 
        - Participants: 10 
        - Sites: 2 
        - Snapshots: 2 
        - Domains: 2 (AE, LB) 
        - Analytics: Yes 
        - Reporting: Yes 
      
      Study: B 
        - Participants: unknown 
        - Sites: unknown 
        - Snapshots: 0 
        - Domains: 0 () 
        - Analytics: No 
        - Reporting: No 
      

---

    Code
      print(summ)
    Output
      Summary: Multiple Longitudinal Studies
      =====================================
      
      Collection Overview:
        - Number of studies: 2 
        - Total participants: 10 
        - Total sites: 2 
        - Total snapshots: 2 
        - Analytics completed: 1 of 2 studies
        - Reporting completed: 1 of 2 studies
        - Unique domains: 2 (AE, LB) 
      
      Individual Study Details:
         A :
          - Participants: 10 | Sites: 2 | Snapshots: 2 
          - Domains: AE, LB 
          - Analytics: Yes | Reporting: Yes 
         B :
          - Participants: | Sites: | Snapshots: 0 
          - Domains:  
          - Analytics: No | Reporting: No 

