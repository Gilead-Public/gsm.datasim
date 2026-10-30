# Fixtures for build_testdata_bundle() tests.
#
# Simulation and workflow execution are slow, so build tests mock them with
# tiny fixed outputs. That exercises orchestration, post-processing, sorting,
# splitting, writing, and the manifest in well under a second. The real
# simulation and pipeline wiring are tested separately with mocks at the
# workr / gsm.mapping boundary.

skip_if_no_bundle_pipeline <- function() {
  for (pkg in c(
    "workr",
    "gsm.mapping",
    "gsm.kri",
    "gsm.qtl",
    "gsm.reporting"
  )) {
    testthat::skip_if_not_installed(pkg)
  }
}

.bundle_cache <- new.env(parent = emptyenv())

small_bundle_config <- function() {
  # system.file() is slow under load_all(), so read the shipped config once.
  if (is.null(.bundle_cache$config)) {
    .bundle_cache$config <- read_testdata_config()
  }
  cfg <- .bundle_cache$config
  # Pinned so tests don't depend on the shipped config's values.
  cfg$seed <- 1235
  cfg$study_id <- "AA-AA-000-0000"
  cfg$participants <- 4
  cfg$sites <- 2
  cfg$snapshots <- 2
  cfg$snapshot_dates <- as.Date(c("2025-02-01", "2025-03-01"))
  cfg
}

fake_raw_snapshot <- function(i) {
  list(
    Raw_SITE = data.frame(invid = c("S1", "S2"), site_status = "Pending"),
    Raw_SUBJ = data.frame(
      studyid = "AA-AA-000-0000",
      subjid = paste0("P", seq_len(2 * i)),
      invid = rep(c("S1", "S2"), i)
    )
  )
}

fake_pipeline <- function(raw, workflows, snapshot_date) {
  results <- data.frame(
    GroupID = c(2, 1, 1, 10, 100),
    GroupLevel = c("Site", "Site", "Site", "Country", "Study"),
    MetricID = c(
      "Analysis_kri0001",
      "Analysis_kri0001",
      "Analysis_kri0007",
      "Analysis_cou0007",
      "Analysis_qtl0001"
    ),
    Score = c(1.5, 0.5, 2, 3, 4)
  )
  bounds <- data.frame(
    MetricID = c("Analysis_kri0001", "Analysis_cou0007", "Analysis_qtl0001"),
    Threshold = c(-2, 2, 0.1),
    Denominator = 1
  )
  results$SnapshotDate <- snapshot_date
  bounds$SnapshotDate <- snapshot_date
  list(
    mapped = list(Mapped_SUBJ = data.frame(subjid = rev(raw$Raw_SUBJ$subjid))),
    analyzed = list(
      Analysis_kri0001 = list(
        ID = "kri0001",
        Analysis_Input = data.frame(
          GroupID = c("S2", "S1"),
          Numerator = c(1, 0)
        ),
        Analysis_Summary = data.frame(
          GroupID = c("S2", "S1"),
          Score = c(1.5, 0.5)
        )
      ),
      Analysis_cou0001 = list(
        Analysis_Input = data.frame(GroupID = "US", Numerator = 1),
        Analysis_Summary = data.frame(GroupID = "US", Score = 1)
      )
    ),
    reporting = list(
      Reporting_Results = results,
      Reporting_Groups = data.frame(
        GroupLevel = c("Site", "Study", "Country"),
        GroupID = c("S1", "AA", "US"),
        Param = "status"
      ),
      Reporting_Bounds = bounds,
      Reporting_Metrics = data.frame(
        MetricID = unique(results$MetricID),
        GroupLevel = c("Site", "Site", "Country", "Study")
      )
    )
  )
}

# Replaces the slow stages of build_testdata_bundle() for the calling test.
local_fake_bundle_stages <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    create_longitudinal_study = function(..., snapshots) {
      list(
        raw_data = stats::setNames(
          lapply(seq_len(snapshots), fake_raw_snapshot),
          paste0("snap", seq_len(snapshots))
        )
      )
    },
    .bundle_workflows = function(config) list(),
    .run_bundle_pipeline = fake_pipeline,
    .env = env
  )
}

build_fake_bundle <- function(env = parent.frame()) {
  testthat::skip_if_not_installed("withr")
  local_fake_bundle_stages(env)
  out <- withr::local_tempdir(.local_envir = env)
  build_testdata_bundle(out, small_bundle_config())
  out
}
