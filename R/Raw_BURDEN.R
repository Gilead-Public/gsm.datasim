#' Generate Raw BURDEN Data
#'
#' Simulates the DQS active-protocol count per GISMO site, the input to
#' `BURDEN.yaml` in `gsm.mapping`. Counts are right-skewed (most PIs run 1-5
#' studies, a few run many), and about 8% of sites have no DQS row so the
#' missing-coverage path is exercised. Rows from earlier snapshots carry over,
#' with an occasional +/-1 change, because DQS is an as-of-refresh source.
#'
#' @param data Current snapshot data; uses `Raw_SITE`.
#' @param previous_data Previous snapshot data; uses `Raw_SITE` and `Raw_BURDEN`.
#' @param missing_rate Share of new sites without a DQS row.
#' @param change_rate Share of carried-over sites whose count changes by 1.
#' @returns A data.frame with `edc_site_id` and integer `active_protocol_count`.
#' @family internal
#' @keywords internal
#' @noRd
Raw_BURDEN <- function(
  data,
  previous_data,
  missing_rate = 0.08,
  change_rate = 0.1
) {
  # Restore the RNG so adding this domain leaves every other simulated domain unchanged.
  old_seed <- get0(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (!is.null(old_seed)) {
    on.exit(assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
  }

  sites <- unique(as.character(data$Raw_SITE$pi_number))
  prev_sites <- unique(as.character(previous_data$Raw_SITE$pi_number))
  dataset <- previous_data$Raw_BURDEN %||%
    data.frame(edc_site_id = character(), active_protocol_count = integer())

  n_prev <- nrow(dataset)
  if (n_prev > 0) {
    step <- sample(c(-1L, 1L), n_prev, replace = TRUE) *
      (stats::runif(n_prev) < change_rate)
    dataset$active_protocol_count <- pmax(
      1L,
      dataset$active_protocol_count + as.integer(step)
    )
  }

  # Sites seen earlier without a DQS row stay uncovered.
  new_sites <- setdiff(sites, prev_sites)
  covered <- new_sites[stats::runif(length(new_sites)) >= missing_rate]
  new_rows <- data.frame(
    edc_site_id = covered,
    active_protocol_count = 1L +
      as.integer(stats::rnbinom(length(covered), size = 1.5, mu = 2.5))
  )

  as.data.frame(rbind(dataset, new_rows))
}
