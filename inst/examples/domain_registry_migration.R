# Example: DomainRegistry migration path
# This example demonstrates how to inspect the registry and run generation
# where migrated domains (currently Raw_AE and Raw_LB) are handled through
# the registry-backed adapter path.

library(gsm.datasim)

# Inspect current registry entries
registry <- get_domain_registry()
cat("Registry-backed domains:", paste(names(registry), collapse = ", "), "\n")

# Show the key contract fields for one entry
cat("Raw_AE entry fields:", paste(names(registry$Raw_AE), collapse = ", "), "\n")

# Build study config using study-builder helpers (non-legacy path)
config <- create_study_config(
  study_id = "REGISTRY-EXAMPLE-001",
  participant_count = 80,
  site_count = 8
)

config <- set_temporal_config(
  config,
  start_date = "2012-01-01",
  snapshot_count = 2,
  snapshot_width = "months"
)

for (dataset_name in c("Raw_STUDY", "Raw_SITE", "Raw_SUBJ", "Raw_ENROLL", "Raw_VISIT", "Raw_AE", "Raw_LB")) {
  config <- add_dataset_config(config, dataset_name, enabled = TRUE)
}

# Generate from study-builder core
# (Raw_AE + Raw_LB are currently routed through DomainRegistry first,
# then legacy switch logic is used only for domains not yet migrated.)
raw_data <- generate_study_data(config)

# Equivalent variant using the explicit config helper:
raw_data_via_config <- generate_raw_data_from_config(config)
cat(
  "Equivalent generate_raw_data_from_config() snapshots:",
  length(raw_data_via_config),
  "\n"
)

# Explore generated snapshots
cat("Generated snapshots:", length(raw_data), "\n")
cat("Snapshot date keys:", paste(names(raw_data), collapse = ", "), "\n")

snapshot_1 <- raw_data[[1]]
cat("Datasets in snapshot 1:", paste(names(snapshot_1), collapse = ", "), "\n")

# Verify migrated domain datasets are present
cat("Raw_AE rows in snapshot 1:", nrow(snapshot_1$Raw_AE), "\n")
cat("Raw_LB rows in snapshot 1:", nrow(snapshot_1$Raw_LB), "\n")

# Optional: summarize via high-level helper
study <- create_longitudinal_study_data(
  study_id = "REGISTRY-EXAMPLE-001",
  raw_data = raw_data,
  config = list(
    participants = 80,
    sites = 8,
    snapshots = 2,
    interval = "1 month",
    domains = c("AE", "LB", "VISIT"),
    study_type = "standard",
    verbose = FALSE
  )
)

summarize_longitudinal_study(study)
