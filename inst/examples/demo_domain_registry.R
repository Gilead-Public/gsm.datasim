# =============================================================================
# gsm.datasim — Domain Registry Developer Demo
# =============================================================================
# The Domain Registry is the new extensible system that replaces the legacy
# switch() block in generate_snapshots_from_combined_specs(). It defines,
# per domain, exactly how data should be generated: what counts to use, what
# arguments to pass to the generator function, and which function to call.
#
# Migrating a domain to the registry makes it:
#   - self-describing (all logic lives in one place per domain)
#   - independently testable
#   - overridable without touching core generation code
#
# This script covers:
#   1. Inspecting the current registry
#   2. Understanding the entry contract
#   3. How the registry fits into the generation pipeline
#   4. Adding a new domain to the registry
#   5. Generating data that exercises your new domain entry
# =============================================================================

library(gsm.datasim)


# ── 1. INSPECTING THE CURRENT REGISTRY ───────────────────────────────────────
# `get_domain_registry()` returns a named list.  Each name is a Raw_* dataset.
# Raw_AE and Raw_LB are currently migrated; all other domains still use the
# legacy switch() path.

registry <- get_domain_registry()

cat("Registry-backed domains:", paste(names(registry), collapse = ", "), "\n")
# > Registry-backed domains: Raw_AE, Raw_LB

# Each entry is itself a named list — the "contract" for generation.
cat("\nRaw_AE entry fields:\n")
print(names(registry$Raw_AE))
# > "dataset"         "required_inputs" "count_fn"        "generate_fn"


# ── 2. UNDERSTANDING THE ENTRY CONTRACT ──────────────────────────────────────
# Every valid entry must have exactly these four fields:
#
#   $dataset         character(1)   — the Raw_* name ("Raw_AE")
#   $required_inputs character(n)   — context keys the entry reads
#   $count_fn        function(counts, snapshot_idx)
#                                   — picks the right element from the pre-
#                                     computed counts list for this snapshot
#   $generate_fn     function(context)
#                                   — contains ALL generation logic; takes the
#                                     full context list and returns a data.frame
#
# That's it — no separate Raw_*() function, no args_builder, no package/path.
# The registry entry IS the generator.

ae_entry <- registry$Raw_AE

# Walk through the contract for Raw_AE:
cat("\n--- Raw_AE entry ---\n")
cat("dataset        :", ae_entry$dataset, "\n")
cat("required_inputs:", paste(ae_entry$required_inputs, collapse = ", "), "\n")

# count_fn: receives the 'counts' environment and picks ae_count
cat("\ncount_fn body:\n")
print(body(ae_entry$count_fn))

# generate_fn: takes the full context and contains all logic inline
cat("\ngenerate_fn body (first 10 lines):\n")
bdy <- deparse(body(ae_entry$generate_fn))
cat(paste(head(bdy, 10), collapse = "\n"), "\n  ...\n")


# ── 3. HOW THE REGISTRY FITS INTO THE PIPELINE ───────────────────────────────
# Inside generate_snapshots_from_combined_specs() the per-domain loop is:
#
#   migrated_data <- generate_domain_from_registry(data_type, context)
#   if (!is.null(migrated_data)) {
#     data[[data_type]] <- migrated_data
#     next          # ← skip the legacy switch()
#   }
#   # ... legacy switch() path for domains not yet in the registry
#
# `context` is a list with:
#   data, previous_data, combined_specs,
#   n, start_date, end_date,
#   snapshot_idx, snapshot_count, snapshot_width, study_id
#
# generate_domain_from_registry():
#   1. Looks up the entry in the registry
#   2. Calls validate_domain_registry_entry(entry)
#   3. Calls entry$generate_fn(context)  ← no args_builder step
#
# You can verify this end-to-end by generating a small study:

config <- create_study_config(
  study_id          = "REG-DEMO-001",
  participant_count = 50,
  site_count        = 5
) |>
  set_temporal_config(start_date = "2023-01-01", snapshot_count = 3,
                      snapshot_width = "months") |>
  add_dataset_config("Raw_AE",   enabled = TRUE) |>
  add_dataset_config("Raw_LB",   enabled = TRUE) |>
  add_dataset_config("Raw_VISIT", enabled = TRUE)

raw_data <- generate_raw_data_from_config(config, verbose = TRUE)
# Look for "[registry]" vs "[legacy]" in the verbose output (once logging is added)

# Registry-generated AE and LB are present alongside legacy VISIT:
cat("\nDatasets in snapshot 1:", paste(names(raw_data[[1]]), collapse = ", "), "\n")
cat("Raw_AE rows :", nrow(raw_data[[1]]$Raw_AE),  "\n")
cat("Raw_LB rows :", nrow(raw_data[[1]]$Raw_LB),  "\n")
cat("Raw_VISIT rows:", nrow(raw_data[[1]]$Raw_VISIT), "\n")


# ── 4. ADDING A NEW DOMAIN TO THE REGISTRY ───────────────────────────────────
# To migrate a domain from the legacy switch() to the registry:
#   a) Write a well-typed entry list
#   b) Merge it into the registry (or pass it as an override)
#   c) Remove the corresponding case from the switch()
#
# Here we add Raw_QUERY — currently handled by the legacy path.

raw_query_entry <- list(
  dataset = "Raw_QUERY",

  # Context keys this entry will read
  required_inputs = c("data", "previous_data", "combined_specs", "n", "start_date"),

  # count_fn: same rule as the legacy switch — use subject_count
  count_fn = function(counts, snapshot_idx) counts$subject_count[snapshot_idx],

  # generate_fn: ALL logic inline — no separate Raw_QUERY() function needed.
  # Mirrors the Raw_QUERY arm of the legacy switch() plus the shared cumulative
  # pattern used by Raw_AE and Raw_LB.
  generate_fn = function(context) {
    spec      <- context$combined_specs
    curr_spec <- spec$Raw_QUERY
    data          <- context$data
    previous_data <- context$previous_data

    if ("Raw_QUERY" %in% names(previous_data)) {
      dataset          <- previous_data$Raw_QUERY
      previous_row_num <- nrow(dataset)
    } else {
      dataset          <- NULL
      previous_row_num <- 0
    }

    n <- context$n - previous_row_num
    if (n <= 0) return(dataset)

    args <- list(
      default    = list(n, context$start_date),
      split_vars = list("subject_nsv_visit_repeated")
    )

    as.data.frame(add_new_var_data(dataset, curr_spec, args, spec$Raw_QUERY))
  }
)

# Merge into the existing registry
extended_registry <- c(get_domain_registry(), list(Raw_QUERY = raw_query_entry))

cat("\nExtended registry domains:", paste(names(extended_registry), collapse = ", "), "\n")

# Validate the new entry manually (the same check generate_domain_from_registry runs)
required_contract_fields <- c("dataset", "required_inputs", "count_fn", "generate_fn")
missing <- setdiff(required_contract_fields, names(raw_query_entry))
if (length(missing) == 0) {
  cat("Entry contract: OK\n")
} else {
  cat("Entry missing fields:", paste(missing, collapse = ", "), "\n")
}


# ── 5. GENERATING DATA WITH A CUSTOM REGISTRY ENTRY ─────────────────────────
# The cleanest way to exercise a custom entry end-to-end today is to generate
# a full study and confirm the domain appears — the registry dispatch happens
# inside generate_snapshots_from_combined_specs automatically for any domain
# whose name is in get_domain_registry().
#
# Once Raw_QUERY is fully migrated (entry merged into get_domain_registry()
# and the legacy switch case removed), the call below will route it through
# your entry with NO other code changes needed.

config_with_query <- create_study_config(
  study_id          = "REG-DEMO-002",
  participant_count = 60,
  site_count        = 6
) |>
  set_temporal_config(start_date = "2023-01-01", snapshot_count = 4,
                      snapshot_width = "months") |>
  add_dataset_config("Raw_AE",    enabled = TRUE) |>
  add_dataset_config("Raw_LB",    enabled = TRUE) |>
  add_dataset_config("Raw_QUERY", enabled = TRUE)

raw_query_data <- generate_raw_data_from_config(config_with_query, verbose = FALSE)

# QUERY still goes through the legacy switch today — confirm it generates:
cat("\nRaw_QUERY rows per snapshot:\n")
print(sapply(raw_query_data, function(s) nrow(s$Raw_QUERY)))

# When you are ready to fully migrate, the two steps are:
#   1.  Add raw_query_entry to get_domain_registry() in R/domain_registry.R
#   2.  Remove the `Raw_QUERY = list(...)` arm from the switch() in
#       generate_snapshots_from_combined_specs() (R/generate_rawdata_for_single_study.R)
#
# The output above should be identical before and after the migration.


# ── QUICK REFERENCE: entry template ──────────────────────────────────────────
# Copy-paste this skeleton when migrating a new domain.
#
# my_entry <- list(
#   dataset         = "Raw_XXXXX",
#   required_inputs = c("data", "previous_data", "combined_specs",
#                        "n", "start_date"),
#
#   count_fn    = function(counts, snapshot_idx) counts$subject_count[snapshot_idx],
#
#   generate_fn = function(context) {
#     spec      <- context$combined_specs
#     curr_spec <- spec$Raw_XXXXX
#     data          <- context$data
#     previous_data <- context$previous_data
#
#     # Cumulative pattern: only generate the delta from the previous snapshot
#     if ("Raw_XXXXX" %in% names(previous_data)) {
#       dataset          <- previous_data$Raw_XXXXX
#       previous_row_num <- nrow(dataset)
#     } else {
#       dataset          <- NULL
#       previous_row_num <- 0
#     }
#     n <- context$n - previous_row_num
#     if (n <= 0) return(dataset)
#
#     args <- list(
#       default    = list(n, context$start_date),
#       split_vars = list("your_split_var_name")
#     )
#     as.data.frame(add_new_var_data(dataset, curr_spec, args, spec$Raw_XXXXX))
#   }
# )
