# ── Workflow-Driven Data Generation Example ───────────────────────────────────
#
# This example demonstrates how to generate simulated raw data directly from
# an lWorkflows list — the same structure returned by
# gsm.core::MakeWorkflowList(). Domains with existing generators (Raw_AE,
# Raw_SUBJ, etc.) use the curated logic; unknown domains fall back to
# type-based column generation driven by the spec metadata.

library(gsm.datasim)

# ── 1. Load workflows from gsm.mapping ──────────────────────────────────────
lWorkflows <- gsm.core::MakeWorkflowList(
  strPath    = "workflow/1_mappings",
  strPackage = "gsm.mapping"
)

# ── 2. Generate raw data for all domains in the spec ─────────────────────────
raw_data <- generate_data_from_workflows(
  lWorkflows     = lWorkflows,
  n_participants = 200,
  n_sites        = 20,
  study_id       = "WF-DEMO-001",
  start_date     = "2012-01-01",
  end_date       = "2012-12-31"
)

cat("Generated domains:\n")
for (nm in names(raw_data)) {
  cat(sprintf("  %-25s %d rows x %d cols\n", nm, nrow(raw_data[[nm]]), ncol(raw_data[[nm]])))
}

# ── 3. Generate with custom domain counts ────────────────────────────────────
raw_data_custom <- generate_data_from_workflows(
  lWorkflows     = lWorkflows,
  n_participants = 100,
  domain_counts  = list(Raw_AE = 500, Raw_PD = 150)
)

# ── 4. Longitudinal multi-snapshot generation ────────────────────────────────
#
# Generate 6 monthly snapshots. Row counts ramp up via count_gen() so early
# snapshots have fewer participants than later ones. The final snapshot
# reaches the full n_participants target. Each snapshot's data is cumulative:
# rows from snapshot N are preserved in snapshot N+1.

snapshots <- generate_data_from_workflows(
  lWorkflows     = lWorkflows,
  n_participants = 200,
  n_sites        = 20,
  study_id       = "WF-LONG-001",
  start_date     = "2012-01-01",
  snapshot_count = 6,
  snapshot_width = "months"
)

cat("\nLongitudinal snapshots:\n")
for (snap_name in names(snapshots)) {
  snap <- snapshots[[snap_name]]
  n_subj <- if ("Raw_SUBJ" %in% names(snap)) nrow(snap$Raw_SUBJ) else NA
  cat(sprintf("  %s: %d domains, Raw_SUBJ = %s rows\n",
              snap_name, length(snap), n_subj))
}

# ── 5. Generate only selected domains ────────────────────────────────────────
raw_data_subset <- generate_data_from_workflows(
  lWorkflows      = lWorkflows,
  n_participants  = 50,
  desired_domains = c("Raw_SUBJ", "Raw_AE", "Raw_SITE")
)

# ── 6. Use a custom workflow with an unknown domain ──────────────────────────
#
# This shows the type-based fallback: columns without a dedicated generator are
# produced based on their spec type or name pattern.

custom_workflows <- list(
  custom_disease = list(
    meta  = list(Description = "Custom disease assessment"),
    spec  = list(
      Raw_DISEASE = list(
        subjid      = list(required = TRUE),
        assess_dt   = list(type = "date"),
        score_val   = list(type = "numeric"),
        category    = list(type = "character"),
        resolved_yn = list(required = TRUE)
      )
    ),
    steps = list()
  )
)

raw_custom <- generate_data_from_workflows(
  lWorkflows     = custom_workflows,
  n_participants = 30,
  n_sites        = 5,
  study_id       = "CUSTOM-001"
)

cat("\nCustom domain columns:\n")
str(raw_custom$Raw_DISEASE)

# ── 7. Integration via raw_data_generator() ──────────────────────────────────
#
# The existing raw_data_generator() also supports the lWorkflows parameter:

result <- raw_data_generator(
  lWorkflows       = lWorkflows,
  ParticipantCount = 100,
  SiteCount        = 15,
  StudyID          = "WF-INTEGRATED-001"
)
