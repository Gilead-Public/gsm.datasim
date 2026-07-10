#' Generate Raw Data from Workflow Specifications
#'
#' Takes a list of workflows (as returned by `workr::MakeWorkflowList()`) and
#' generates simulated raw data for every `Raw_*` domain found in the combined
#' specification. Domains that already have a dedicated generator in the domain
#' registry or a legacy `Raw_*()` function are produced with those generators;
#' all other domains fall back to type-based column generation via
#' [generate_unknown_domain()].
#'
#' When `snapshot_count > 1`, the function produces cumulative longitudinal
#' snapshots using the same delta-accumulation pattern as the core pipeline:
#' each snapshot's `previous_data` is the prior snapshot, row counts ramp up
#' via `count_gen()`, and dates advance by `snapshot_width`.
#'
#' @param lWorkflows A named list of workflow objects, each containing a `$spec`
#'   element (e.g. from `workr::MakeWorkflowList()`).
#' @param n_participants Integer. Target number of participants (default 100).
#' @param n_sites Integer. Target number of sites (default 10).
#' @param study_id Character. Study identifier (default `"STUDY-001"`).
#' @param start_date Character or Date. First date of simulated data
#'   (default `"2012-01-01"`).
#' @param end_date Character or Date. Last date of simulated data. Only used in
#'   single-snapshot mode; for multi-snapshot mode the end date of each snapshot
#'   is derived from `start_date` + `snapshot_width`. Defaults to
#'   `"2012-12-31"`.
#' @param snapshot_count Integer. Number of longitudinal snapshots to generate
#'   (default 1). When \code{> 1} the return value is a named list of snapshots,
#'   each itself a named list of domain data.frames.
#' @param snapshot_width Character. Time step between snapshots -- passed to
#'   [seq.Date()] as `by` (e.g. `"months"`, `"weeks"`, `"3 months"`). Default
#'   `"months"`.
#' @param domain_counts Optional named list mapping domain names to desired
#'   *final* row counts (e.g. `list(Raw_AE = 300, Raw_LB = 500)`). In
#'   multi-snapshot mode these are the targets for the *last* snapshot; earlier
#'   snapshots ramp up via `count_gen()`. Domains not listed here receive a
#'   default based on heuristic multipliers of `n_participants`.
#' @param desired_domains Optional character vector of domain names to generate.
#'   `NULL` (default) generates all `Raw_*` domains found in the spec.
#' @param column_overrides Optional named list for specifying or overriding
#'   individual columns in already-generated domains. The top-level names are
#'   domain names (e.g. `"Raw_LB"`); each element is itself a named list whose
#'   names are column names. Each column value can be:
#'   \describe{
#'     \item{A function `function(n, df)`}{Called with the row count and the
#'       fully-generated domain `data.frame`. Use this to derive a column from
#'       other columns in the same domain (e.g. computing a ratio).}
#'     \item{A function `function(n)`}{Called with just the row count. Useful
#'       for custom distributions or categorical values.}
#'     \item{A vector}{Sampled with replacement to fill `n` rows.}
#'     \item{A scalar}{Repeated to fill all `n` rows.}
#'   }
#'   Example:
#'   \preformatted{
#'   column_overrides = list(
#'     Raw_LB = list(
#'       score_val  = function(n)    round(runif(n, 0, 10), 1),
#'       lbstresu   = c("mg/dL", "mmol/L", "g/L"),
#'       visit_flag = function(n, df) ifelse(df$visnam == "SCREENING", "S", "F")
#'     )
#'   )
#'   }
#'
#' @return When `snapshot_count == 1`, a named list of `data.frame`s (one per
#'   domain). When `snapshot_count > 1`, a named list of snapshots keyed by
#'   snapshot end-date, each containing a named list of domain `data.frame`s.
#'
#' @details
#' The generation follows a three-tier fallback strategy for each domain:
#' \enumerate{
#'   \item **Domain registry** -- `generate_domain_from_registry()` is tried first.
#'         This covers all domains with dedicated, curated generation logic.
#'   \item **Legacy Raw_*() function** -- if the domain is not in the registry but a
#'         function with the domain name exists (e.g. `Raw_AE()`), it is called.
#'   \item **Type-based fallback** -- [generate_unknown_domain()] generates each
#'         column using spec metadata (type, FK detection, name pattern heuristics).
#' }
#'
#' Domains are generated in dependency order (Raw_STUDY -> Raw_SITE -> Raw_SUBJ ->
#' Raw_ENROLL first) so that downstream domains can reference foreign key columns
#' from previously generated domains.
#'
#' @examples
#' \dontrun{
#' # Load workflows from gsm.mapping
#' lWorkflows <- workr::MakeWorkflowList(
#'   strPath = "workflow/1_mappings",
#'   strPackage = "gsm.mapping"
#' )
#'
#' # Generate raw data for all domains in the spec (single snapshot)
#' raw_data <- generate_data_from_workflows(lWorkflows, n_participants = 200)
#'
#' # Generate 6 monthly snapshots (longitudinal)
#' snapshots <- generate_data_from_workflows(
#'   lWorkflows,
#'   n_participants = 200,
#'   snapshot_count = 6,
#'   snapshot_width = "months"
#' )
#'
#' # Generate only specific domains with custom row counts
#' raw_data <- generate_data_from_workflows(
#'   lWorkflows,
#'   desired_domains = c("Raw_SUBJ", "Raw_AE", "Raw_SITE"),
#'   domain_counts = list(Raw_AE = 600)
#' )
#'
#' # --- column_overrides examples -------------------------------------------
#'
#' # Add a new numeric column to Raw_LB using a custom distribution.
#' # Workflows that reference a column with no named generator (e.g. score_val
#' # from a preexisting LB workflow) are auto-filled via type inference; use
#' # column_overrides when you need a specific distribution instead.
#' raw_data <- generate_data_from_workflows(
#'   lWorkflows,
#'   column_overrides = list(
#'     Raw_LB = list(
#'       score_val = function(n) round(runif(n, 0, 10), 1)
#'     )
#'   )
#' )
#'
#' # Sample from a fixed set of values (sampled with replacement)
#' raw_data <- generate_data_from_workflows(
#'   lWorkflows,
#'   column_overrides = list(
#'     Raw_LB = list(
#'       lbstresu = c("mg/dL", "mmol/L", "g/L")
#'     )
#'   )
#' )
#'
#' # Derive a column from other columns in the same domain using function(n, df).
#' # The second argument receives the fully-generated domain data.frame.
#' raw_data <- generate_data_from_workflows(
#'   lWorkflows,
#'   column_overrides = list(
#'     Raw_LB = list(
#'       lbstnrhi = function(n, df) round(df$lbstresn * 1.2, 2),
#'       visit_flag = function(n, df) ifelse(df$visnam == "SCREENING", "S", "F")
#'     )
#'   )
#' )
#'
#' # Broadcast a scalar to every row, and combine overrides across domains
#' raw_data <- generate_data_from_workflows(
#'   lWorkflows,
#'   column_overrides = list(
#'     Raw_LB = list(
#'       lbcat     = "CHEMISTRY",
#'       score_val = function(n) round(runif(n, 0, 10), 1)
#'     ),
#'     Raw_AE = list(
#'       severity_score = function(n) sample(1:5, n, replace = TRUE)
#'     )
#'   )
#' )
#'
#' # column_overrides also apply on every snapshot in multi-snapshot mode
#' snapshots <- generate_data_from_workflows(
#'   lWorkflows,
#'   snapshot_count = 6,
#'   column_overrides = list(
#'     Raw_LB = list(
#'       score_val = function(n) round(runif(n, 0, 10), 1)
#'     )
#'   )
#' )
#' }
#'
#' @export
generate_data_from_workflows <- function(
    lWorkflows,
    n_participants = 100,
    n_sites = 10,
    study_id = "STUDY-001",
    start_date = "2012-01-01",
    end_date = "2012-12-31",
    snapshot_count = 1L,
    snapshot_width = "months",
    domain_counts = NULL,
    desired_domains = NULL,
    column_overrides = NULL) {
  # -- Validate inputs -------------------------------------------------------
  workflow_names <- names(lWorkflows)
  if (
    !is.list(lWorkflows) ||
      length(lWorkflows) == 0 ||
      is.null(workflow_names) ||
      length(workflow_names) != length(lWorkflows) ||
      any(is.na(workflow_names)) ||
      any(!nzchar(workflow_names))
  ) {
    stop("`lWorkflows` must be a non-empty named list of workflow objects.", call. = FALSE)
  }

  snapshot_count <- as.integer(snapshot_count)
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  # -- Combine specs from all workflows -------------------------------------
  combined_specs <- CombineSpecs(lWorkflows, bIsWorkflow = TRUE)
  had_raw_visit_spec <- "Raw_VISIT" %in% names(combined_specs)
  raw_visit_spec <- if (had_raw_visit_spec) combined_specs$Raw_VISIT else NULL

  combined_specs <- prepare_combined_specs_for_generation(combined_specs, desired_specs = desired_domains)

  if (had_raw_visit_spec && "Raw_VISIT" %in% names(combined_specs)) {
    combined_specs$Raw_VISIT <- raw_visit_spec
  }

  if (length(combined_specs) == 0) {
    warning("No Raw_* domains found in the combined workflow specs.", call. = FALSE)
    return(list())
  }

  logger::log_info("Generating data for {length(combined_specs)} domains: {paste(names(combined_specs), collapse = ', ')}")

  # -- Build the domain registry for known-domain lookups -------------------
  registry <- get_domain_registry()

  # -- Determine final (max) row counts per domain ----------------------------
  domain_max_n <- .resolve_domain_counts(
    domain_names   = names(combined_specs),
    n_participants = n_participants,
    n_sites        = n_sites,
    user_counts    = domain_counts
  )

  # -- Single-snapshot shortcut -----------------------------------------------
  if (snapshot_count <= 1L) {
    return(
      .generate_single_snapshot(
        combined_specs   = combined_specs,
        domain_n         = domain_max_n,
        registry         = registry,
        start_date       = start_date,
        end_date         = end_date,
        snapshot_idx     = 1L,
        snapshot_count   = 1L,
        snapshot_width   = snapshot_width,
        study_id         = study_id,
        previous_data    = list(),
        column_overrides = column_overrides
      )
    )
  }

  # -- Multi-snapshot longitudinal generation ---------------------------------
  snapshot_start_dates <- seq(start_date, length.out = snapshot_count, by = snapshot_width)
  snapshot_end_dates <- c(snapshot_start_dates[-1] - 1, end_date)
  snapshot_end_dates <- pmin(snapshot_end_dates, end_date)

  # Build per-snapshot counts using count_gen() for realistic ramp-up
  domain_count_vectors <- stats::setNames(
    lapply(names(domain_max_n), function(d) {
      max_n <- domain_max_n[[d]]
      if (max_n <= 1L) {
        rep(max_n, snapshot_count)
      } else {
        count_gen(max_n, snapshot_count)
      }
    }),
    names(domain_max_n)
  )

  snapshots <- list()
  previous_data <- list()

  for (snapshot_idx in seq_len(snapshot_count)) {
    logger::log_info("-- Snapshot {snapshot_idx}/{snapshot_count} ({snapshot_end_dates[snapshot_idx]}) --")

    domain_n_this <- stats::setNames(
      lapply(names(domain_count_vectors), function(d) domain_count_vectors[[d]][snapshot_idx]),
      names(domain_count_vectors)
    )

    snapshot_data <- .generate_single_snapshot(
      combined_specs   = combined_specs,
      domain_n         = domain_n_this,
      registry         = registry,
      start_date       = snapshot_start_dates[snapshot_idx],
      end_date         = snapshot_end_dates[snapshot_idx],
      snapshot_idx     = snapshot_idx,
      snapshot_count   = snapshot_count,
      snapshot_width   = snapshot_width,
      study_id         = study_id,
      previous_data    = previous_data,
      column_overrides = column_overrides
    )

    snapshots[[snapshot_idx]] <- snapshot_data
    previous_data <- snapshot_data
    logger::log_info("-- Snapshot {snapshot_idx} complete --")
  }

  names(snapshots) <- as.character(snapshot_end_dates)
  snapshots
}


# -- Internal helpers ---------------------------------------------------------

#' Generate a Single Snapshot of Domain Data
#'
#' Iterates over all domains in `combined_specs` using the three-tier fallback
#' (registry -> legacy -> type-based). Supports cumulative generation via
#' `previous_data`.
#'
#' @keywords internal
.generate_single_snapshot <- function(combined_specs, domain_n, registry,
                                      start_date, end_date,
                                      snapshot_idx, snapshot_count, snapshot_width,
                                      study_id, previous_data,
                                      column_overrides = NULL) {
  data <- list()

  for (domain in names(combined_specs)) {
    n <- domain_n[[domain]]
    domain_spec <- combined_specs[[domain]]

    logger::log_info("Generating {domain} (target n = {n})...")

    # -- Tier 1: Domain registry ------------------------------------------------
    registry_context <- list(
      data           = data,
      previous_data  = previous_data,
      combined_specs = combined_specs,
      n              = n,
      start_date     = start_date,
      end_date       = end_date,
      snapshot_idx   = snapshot_idx,
      snapshot_count = snapshot_count,
      snapshot_width = snapshot_width,
      study_id       = study_id
    )

    registry_result <- tryCatch(
      generate_domain_from_registry(
        data_type = domain,
        context   = registry_context,
        registry  = registry
      ),
      error = function(e) {
        logger::log_debug("Registry generation failed for {domain}: {conditionMessage(e)}")
        NULL
      }
    )

    if (!is.null(registry_result)) {
      data[[domain]] <- as.data.frame(registry_result)
      data[[domain]] <- .apply_column_overrides(data[[domain]], domain, column_overrides)
      logger::log_info("{domain} generated via domain registry ({nrow(data[[domain]])} rows)")
      next
    }

    # -- Tier 2: Legacy Raw_*() function ----------------------------------------
    legacy_fn <- tryCatch(match.fun(domain), error = function(e) NULL)
    if (!is.null(legacy_fn)) {
      legacy_result <- tryCatch(
        {
          legacy_fn(
            data          = data,
            previous_data = previous_data,
            spec          = combined_specs,
            n             = n,
            startDate     = start_date,
            endDate       = end_date
          )
        },
        error = function(e) {
          logger::log_debug("Legacy function failed for {domain}: {conditionMessage(e)}")
          NULL
        }
      )

      if (!is.null(legacy_result)) {
        data[[domain]] <- as.data.frame(legacy_result)
        data[[domain]] <- .apply_column_overrides(data[[domain]], domain, column_overrides)
        logger::log_info("{domain} generated via legacy function ({nrow(data[[domain]])} rows)")
        next
      }
    }

    # -- Tier 3: Type-based fallback --------------------------------------------
    fallback_context <- list(
      data       = data,
      start_date = start_date,
      end_date   = end_date
    )

    data[[domain]] <- generate_unknown_domain(
      domain_name   = domain,
      domain_spec   = domain_spec,
      n             = n,
      context       = fallback_context,
      previous_data = previous_data[[domain]]
    )
    data[[domain]] <- .apply_column_overrides(data[[domain]], domain, column_overrides)
    logger::log_info("{domain} generated via type-based fallback ({nrow(data[[domain]])} rows)")
  }

  data
}

#' Apply Column Overrides to a Generated Domain Data Frame
#'
#' Post-processes a domain `data.frame` by applying user-supplied column
#' specifications from `column_overrides`. Columns may be added (if new) or
#' replaced (if already present).
#'
#' Each column value in the override list can be:
#' * A **function** with signature `function(n, df)` — receives row count and the
#'   full domain `data.frame`; useful for deriving values from other columns.
#' * A **function** with signature `function(n)` — receives only the row count.
#' * A **vector** — sampled with replacement to `n` rows.
#' * A **scalar** — repeated to fill all `n` rows.
#'
#' @keywords internal
.apply_column_overrides <- function(df, domain, column_overrides) {
  if (is.null(column_overrides) || !domain %in% names(column_overrides)) {
    return(df)
  }

  overrides <- column_overrides[[domain]]
  n <- nrow(df)

  for (col_name in names(overrides)) {
    spec <- overrides[[col_name]]

    col_vals <- if (is.function(spec)) {
      params <- names(formals(spec))
      if (length(params) >= 2) {
        spec(n, df)
      } else {
        spec(n)
      }
    } else if (length(spec) == 1) {
      rep(spec, n)
    } else {
      sample(spec, n, replace = TRUE)
    }

    df[[col_name]] <- col_vals
    logger::log_debug("Column override applied: {domain}${col_name}")
  }

  df
}

#' Resolve Row Counts for Each Domain
#'
#' Applies heuristic multipliers for known domain patterns, user overrides,
#' and falls back to `n_participants` for unknown domains.
#'
#' @keywords internal
.resolve_domain_counts <- function(domain_names, n_participants, n_sites, user_counts = NULL) {
  # Heuristic: map domain name patterns to multipliers of n_participants
  multipliers <- list(
    Raw_STUDY = function(np, ns) 1L,
    Raw_SITE = function(np, ns) ns,
    Raw_SUBJ = function(np, ns) np,
    Raw_ENROLL = function(np, ns) np,
    Raw_IE = function(np, ns) np,
    Raw_VISIT = function(np, ns) np,
    Raw_STUDCOMP = function(np, ns) ceiling(np / 10),
    Raw_AE = function(np, ns) np * 3L,
    Raw_PD = function(np, ns) np * 3L,
    Raw_LB = function(np, ns) np,
    Raw_SDRGCOMP = function(np, ns) ceiling(np / 2),
    Raw_DATACHG = function(np, ns) np,
    Raw_DATAENT = function(np, ns) np,
    Raw_QUERY = function(np, ns) np,
    Raw_Consents = function(np, ns) ceiling(np / 75),
    Raw_Death = function(np, ns) ceiling(np / 85),
    Raw_AntiCancer = function(np, ns) ceiling(np / 10),
    Raw_Randomization = function(np, ns) np,
    Raw_OverallResponse = function(np, ns) np,
    Raw_PK = function(np, ns) np,
    Raw_Baseline = function(np, ns) np
  )

  counts <- stats::setNames(
    lapply(domain_names, function(d) {
      # User override first
      if (!is.null(user_counts) && d %in% names(user_counts)) {
        return(as.integer(user_counts[[d]]))
      }
      # Known heuristic
      if (d %in% names(multipliers)) {
        return(as.integer(multipliers[[d]](n_participants, n_sites)))
      }
      # Default: participant count
      as.integer(n_participants)
    }),
    domain_names
  )

  counts
}
