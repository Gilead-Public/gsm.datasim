test_that("organize_analytics_results respects verbose flag and organizes Analysis_* results", {
  pipeline_results <- list(
    results = list(
      Analysis_cou0001 = list(
        Analysis_Summary = data.frame(
          GroupID = "US",
          GroupLevel = "Country",
          Numerator = 10,
          Denominator = 100,
          Metric = 0.1,
          Score = 0,
          Flag = 0,
          stringsAsFactors = FALSE
        )
      )
    )
  )

  captured <- capture.output({
    organized <- organize_analytics_results(pipeline_results, verbose = FALSE)
  })
  expect_equal(length(captured), 0)

  # Test new flat structure
  expect_true(is.list(organized))
  expect_equal(length(organized), 1)
  expect_true("cou0001" %in% names(organized))
  
  metric_output <- organized$cou0001
  expect_true(is.list(metric_output))
  expect_equal(metric_output$metric_id, "cou0001")
  expect_true(is.list(metric_output$data_frames))
  expect_true("Analysis_Summary" %in% names(metric_output$data_frames))
  expect_s3_class(metric_output$data_frames$Analysis_Summary, "data.frame")
  expect_equal(nrow(metric_output$data_frames$Analysis_Summary), 1)
  expect_equal(metric_output$data_frames$Analysis_Summary$GroupLevel[[1]], "Country")
  expect_true("Metric_ID" %in% names(metric_output$data_frames$Analysis_Summary))
  expect_equal(metric_output$data_frames$Analysis_Summary$Metric_ID[[1]], "cou0001")
})

test_that("organize_analytics_results preserves snapshot date keys for per-snapshot analytics", {
  pipeline_results <- list(
    "2012-01-31" = list(
      results = list(
        Analysis_cou0001 = list(
          Analysis_Summary = data.frame(
            GroupID = "US",
            GroupLevel = "Country",
            Numerator = 10,
            Denominator = 100,
            Metric = 0.1,
            Score = 0,
            Flag = 0,
            stringsAsFactors = FALSE
          )
        )
      )
    ),
    "2012-02-29" = list(
      results = list(
        Analysis_cou0002 = list(
          Analysis_Summary = data.frame(
            GroupID = "UK",
            GroupLevel = "Country",
            Numerator = 20,
            Denominator = 200,
            Metric = 0.1,
            Score = 0,
            Flag = 0,
            stringsAsFactors = FALSE
          )
        )
      )
    )
  )

  organized <- organize_analytics_results(pipeline_results, verbose = FALSE)

  expect_equal(names(organized), c("2012-01-31", "2012-02-29"))
  expect_equal(length(organized[["2012-01-31"]]), 1)
  expect_equal(length(organized[["2012-02-29"]]), 1)
  expect_true("cou0001" %in% names(organized[["2012-01-31"]]))
  expect_true("cou0002" %in% names(organized[["2012-02-29"]]))
  expect_equal(organized[["2012-01-31"]]$cou0001$metric_id, "cou0001")
  expect_equal(organized[["2012-02-29"]]$cou0002$metric_id, "cou0002")
})

test_that("generate_analytics_layers returns organized analytics for all snapshots", {
  skip_if_not_installed("testthat", minimum_version = "3.1.0")

  raw_data <- list("2012-01-31" = list(), "2012-02-29" = list())
  config <- list()

  analytics <- testthat::with_mocked_bindings(
    execute_analytics_pipeline = function(raw_data, config) {
      list(
        "2012-01-31" = list(
          results = list(
            Analysis_cou0001 = list(
              Analysis_Summary = data.frame(
                GroupID = "US",
                GroupLevel = "Country",
                Numerator = 10,
                Denominator = 100,
                Metric = 0.1,
                Score = 0,
                Flag = 0,
                stringsAsFactors = FALSE
              )
            )
          )
        ),
        "2012-02-29" = list(
          results = list(
            Analysis_cou0002 = list(
              Analysis_Summary = data.frame(
                GroupID = "UK",
                GroupLevel = "Country",
                Numerator = 20,
                Denominator = 200,
                Metric = 0.1,
                Score = 0,
                Flag = 0,
                stringsAsFactors = FALSE
              )
            )
          )
        )
      )
    },
    generate_analytics_layers(raw_data, config, verbose = FALSE)
  )

  expect_true(is.list(analytics))
  expect_equal(names(analytics), c("2012-01-31", "2012-02-29"))
  expect_equal(length(analytics[["2012-01-31"]]), 1)
  expect_equal(length(analytics[["2012-02-29"]]), 1)
})
