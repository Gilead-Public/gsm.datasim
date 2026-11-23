burden_snapshot <- function(sites) {
  list(Raw_SITE = data.frame(pi_number = sites, stringsAsFactors = FALSE))
}

test_that("Raw_BURDEN has positive integer counts for a subset of Raw_SITE sites (#203)", {
  set.seed(1)
  sites <- sprintf("0X%03d", 1:200)
  res <- Raw_BURDEN(burden_snapshot(sites), list())

  expect_named(res, c("edc_site_id", "active_protocol_count"))
  expect_type(res$active_protocol_count, "integer")
  expect_true(all(res$active_protocol_count >= 1L))
  expect_true(all(res$edc_site_id %in% sites))
  expect_false(anyDuplicated(res$edc_site_id) > 0)
  # Some sites have no DQS row, and the tail reaches high-burden PIs.
  expect_lt(nrow(res), length(sites))
  expect_gt(max(res$active_protocol_count), 5L)
})

test_that("Raw_BURDEN carries sites forward and keeps uncovered sites uncovered (#203)", {
  set.seed(2)
  first_sites <- sprintf("0X%03d", 1:100)
  first <- burden_snapshot(first_sites)
  first$Raw_BURDEN <- Raw_BURDEN(first, list())
  second <- burden_snapshot(c(first_sites, sprintf("0X%03d", 101:150)))
  res <- Raw_BURDEN(second, first)

  uncovered <- setdiff(first_sites, first$Raw_BURDEN$edc_site_id)
  expect_false(any(uncovered %in% res$edc_site_id))
  expect_true(all(first$Raw_BURDEN$edc_site_id %in% res$edc_site_id))

  prev <- first$Raw_BURDEN$active_protocol_count
  curr <- res$active_protocol_count[match(first$Raw_BURDEN$edc_site_id, res$edc_site_id)]
  expect_true(all(abs(curr - prev) <= 1L))
})

test_that("Raw_BURDEN leaves the caller's RNG state unchanged (#203)", {
  set.seed(3)
  before <- .Random.seed
  Raw_BURDEN(burden_snapshot(sprintf("0X%03d", 1:10)), list())
  expect_identical(.Random.seed, before)
})

test_that("the registry and bundle config include Raw_BURDEN (#203)", {
  expect_contains(names(get_domain_registry()), "Raw_BURDEN")
  expect_contains(read_testdata_config()$domains, "BURDEN")
})

test_that("simulated Raw_BURDEN maps onto Raw_SITE with some sites uncovered (#203)", {
  testthat::skip_if_not_installed("gsm.mapping")
  testthat::skip_if(
    !nzchar(system.file("workflow", "1_mappings", "BURDEN.yaml", package = "gsm.mapping")),
    "installed gsm.mapping has no BURDEN mapping"
  )
  test_at_log_threshold()
  set.seed(1235)
  study <- create_longitudinal_study(
    study_id = "BURDEN-TEST",
    participants = 80,
    sites = 60,
    snapshots = 2,
    interval = "1 month",
    domains = c("SITE", "SUBJ", "BURDEN"),
    run_analytics = FALSE,
    run_reporting = FALSE
  )
  snap <- study$raw_data[[length(study$raw_data)]]

  expect_contains(names(snap$Raw_BURDEN), c("edc_site_id", "active_protocol_count"))
  expect_true(all(snap$Raw_BURDEN$edc_site_id %in% snap$Raw_SITE$pi_number))
  expect_true(any(!snap$Raw_SITE$pi_number %in% snap$Raw_BURDEN$edc_site_id))
})
