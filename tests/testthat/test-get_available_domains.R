test_that("get_available_domains() with no args returns all registry domain names (#123)", {
  domains <- get_available_domains()

  expect_type(domains, "character")
  expect_setequal(domains, names(get_domain_registry()))
})

test_that("get_available_domains(NULL) matches the no-arg registry lookup (#123)", {
  expect_identical(get_available_domains(NULL), get_available_domains())
})

test_that("get_available_domains(study) returns the union of snapshot domain names (#123)", {
  study <- list(
    raw_data = list(
      "2012-01-31" = list(Raw_AE = data.frame(), Raw_LB = data.frame()),
      "2012-02-29" = list(Raw_AE = data.frame(), Raw_VISIT = data.frame())
    )
  )

  expect_setequal(
    get_available_domains(study),
    c("Raw_AE", "Raw_LB", "Raw_VISIT")
  )
})

test_that("get_available_domains(study) returns character(0) for an empty study (#123)", {
  study <- list(raw_data = list())

  expect_identical(get_available_domains(study), character(0))
})
