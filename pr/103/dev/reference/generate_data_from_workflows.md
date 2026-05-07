# Generate Raw Data from Workflow Specifications

Takes a list of workflows (as returned by
[`gsm.core::MakeWorkflowList()`](https://gilead-biostats.github.io/gsm.core/reference/MakeWorkflowList.html))
and generates simulated raw data for every `Raw_*` domain found in the
combined specification. Domains that already have a dedicated generator
in the domain registry or a legacy `Raw_*()` function are produced with
those generators; all other domains fall back to type-based column
generation via
[`generate_unknown_domain()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/generate_unknown_domain.md).

## Usage

``` r
generate_data_from_workflows(
  lWorkflows,
  n_participants = 100,
  n_sites = 10,
  study_id = "STUDY-001",
  start_date = "2012-01-01",
  end_date = "2012-12-31",
  snapshot_count = 1L,
  snapshot_width = "months",
  domain_counts = NULL,
  desired_domains = NULL
)
```

## Arguments

- lWorkflows:

  A named list of workflow objects, each containing a `$spec` element
  (e.g. from
  [`gsm.core::MakeWorkflowList()`](https://gilead-biostats.github.io/gsm.core/reference/MakeWorkflowList.html)).

- n_participants:

  Integer. Target number of participants (default 100).

- n_sites:

  Integer. Target number of sites (default 10).

- study_id:

  Character. Study identifier (default `"STUDY-001"`).

- start_date:

  Character or Date. First date of simulated data (default
  `"2012-01-01"`).

- end_date:

  Character or Date. Last date of simulated data. Only used in
  single-snapshot mode; for multi-snapshot mode the end date of each
  snapshot is derived from `start_date` + `snapshot_width`. Defaults to
  `"2012-12-31"`.

- snapshot_count:

  Integer. Number of longitudinal snapshots to generate (default 1).
  When `> 1` the return value is a named list of snapshots, each itself
  a named list of domain data.frames.

- snapshot_width:

  Character. Time step between snapshots – passed to
  [`seq.Date()`](https://rdrr.io/r/base/seq.Date.html) as `by` (e.g.
  `"months"`, `"weeks"`, `"3 months"`). Default `"months"`.

- domain_counts:

  Optional named list mapping domain names to desired *final* row counts
  (e.g. `list(Raw_AE = 300, Raw_LB = 500)`). In multi-snapshot mode
  these are the targets for the *last* snapshot; earlier snapshots ramp
  up via `count_gen()`. Domains not listed here receive a default based
  on heuristic multipliers of `n_participants`.

- desired_domains:

  Optional character vector of domain names to generate. `NULL`
  (default) generates all `Raw_*` domains found in the spec.

## Value

When `snapshot_count == 1`, a named list of `data.frame`s (one per
domain). When `snapshot_count > 1`, a named list of snapshots keyed by
snapshot end-date, each containing a named list of domain `data.frame`s.

## Details

When `snapshot_count > 1`, the function produces cumulative longitudinal
snapshots using the same delta-accumulation pattern as the core
pipeline: each snapshot's `previous_data` is the prior snapshot, row
counts ramp up via `count_gen()`, and dates advance by `snapshot_width`.

The generation follows a three-tier fallback strategy for each domain:

1.  **Domain registry** – `generate_domain_from_registry()` is tried
    first. This covers all domains with dedicated, curated generation
    logic.

2.  **Legacy Raw\_\*() function** – if the domain is not in the registry
    but a function with the domain name exists (e.g. `Raw_AE()`), it is
    called.

3.  **Type-based fallback** –
    [`generate_unknown_domain()`](https://gilead-biostats.github.io/gsm.datasim/dev/reference/generate_unknown_domain.md)
    generates each column using spec metadata (type, FK detection, name
    pattern heuristics).

Domains are generated in dependency order (Raw_STUDY -\> Raw_SITE -\>
Raw_SUBJ -\> Raw_ENROLL first) so that downstream domains can reference
foreign key columns from previously generated domains.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load workflows from gsm.mapping
lWorkflows <- gsm.core::MakeWorkflowList(
  strPath = "workflow/1_mappings",
  strPackage = "gsm.mapping"
)

# Generate raw data for all domains in the spec (single snapshot)
raw_data <- generate_data_from_workflows(lWorkflows, n_participants = 200)

# Generate 6 monthly snapshots (longitudinal)
snapshots <- generate_data_from_workflows(
  lWorkflows,
  n_participants = 200,
  snapshot_count = 6,
  snapshot_width = "months"
)

# Generate only specific domains with custom row counts
raw_data <- generate_data_from_workflows(
  lWorkflows,
  desired_domains = c("Raw_SUBJ", "Raw_AE", "Raw_SITE"),
  domain_counts = list(Raw_AE = 600)
)
} # }
```
