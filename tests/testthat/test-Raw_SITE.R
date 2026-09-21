test_that("invid generates unique site identifiers in the 0X prefix space", {
  set.seed(8409)

  x <- invid(10, previous_data = NULL)

  expect_length(x, 10)
  expect_equal(anyDuplicated(x), 0)
  expect_true(all(grepl("^0X", x)))
})

# The previous_data branch is what keeps site IDs unique across snapshots.
test_that("invid excludes identifiers already present in previous data", {
  set.seed(8409)
  previous <- data.frame(invid = c("0X000", "0X001"), stringsAsFactors = FALSE)

  x <- invid(50, previous_data = previous)

  expect_length(x, 50)
  expect_false(any(x %in% previous$invid))
})

test_that("invid errors when the identifier pool is exhausted", {
  expect_error(
    invid(10001, previous_data = NULL),
    "Not enough unique strings available"
  )
})

test_that("country, site_status and investigator name generators sample their value sets", {
  set.seed(8409)

  expect_setequal(unique(country(500)), c("US", "China", "Japan"))
  expect_setequal(unique(site_status(500)), c("Active", "", "Closed"))
  expect_setequal(
    unique(InvestigatorFirstName(500)),
    c("John", "Joanne", "Fred")
  )
  expect_setequal(unique(InvestigatorLastName(500)), c("Doe", "Deer", "Smith"))
})

test_that("City samples from the reference city list", {
  set.seed(8409)

  x <- City(200)

  expect_length(x, 200)
  expect_true(all(x %in% Country_State_City_data$city))
})

# State and Country each have two modes: keyed to supplied cities, or sampled
# independently when no cities are passed.
test_that("State and Country resolve values from supplied cities", {
  cities <- c("London", "Foster City", "Tokyo")

  expect_equal(State(3, cities = cities), c("Greater London", "CA", "Kanto"))
  expect_equal(Country(3, cities = cities), c("UK", "US", "Japan"))
})

test_that("State and Country sample independently when no cities are supplied", {
  set.seed(8409)

  states <- State(50)
  countries <- Country(50)

  expect_length(states, 50)
  expect_length(countries, 50)
  expect_true(all(states %in% Country_State_City_data$state))
  expect_true(all(countries %in% Country_State_City_data$country))
})

test_that("Country_State_City returns a geographically consistent triple", {
  set.seed(8409)

  res <- Country_State_City(20)

  expect_named(res, c("City", "State", "Country"))
  expect_length(res$City, 20)

  # Each city must map back to its own row in the reference table.
  idx <- match(res$City, Country_State_City_data$city)
  expect_equal(res$State, Country_State_City_data$state[idx])
  expect_equal(res$Country, Country_State_City_data$country[idx])
})

test_that("Raw_SITE generates a complete dataset from scratch", {
  set.seed(8409)

  res <- Raw_SITE(
    make_site_data(),
    previous_data = list(),
    spec = make_site_spec(),
    startDate = as.Date("2012-01-01"),
    n_sites = 5,
    split_vars = site_split
  )

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 5)
  expect_true(all(
    c("studyid", "invid", "Country", "State", "City") %in% names(res)
  ))
  expect_true(all(res$studyid == "PROT-001"))
  expect_equal(anyDuplicated(res$invid), 0)
})

test_that("Raw_SITE appends only the delta rows to previous data", {
  set.seed(8409)
  spec <- make_site_spec()
  data <- make_site_data()

  first <- Raw_SITE(
    data,
    list(),
    spec,
    startDate = as.Date("2012-01-01"),
    n_sites = 3,
    split_vars = site_split
  )
  second <- Raw_SITE(
    data,
    previous_data = list(Raw_SITE = first),
    spec = spec,
    startDate = as.Date("2012-01-01"),
    n_sites = 7,
    split_vars = site_split
  )

  expect_equal(nrow(second), 7)
  expect_equal(second[seq_len(3), ], first)
  expect_equal(anyDuplicated(second$invid), 0)
})

test_that("Raw_SITE returns previous data unchanged when the target count is met", {
  set.seed(8409)
  spec <- make_site_spec()
  data <- make_site_data()

  first <- Raw_SITE(
    data,
    list(),
    spec,
    startDate = as.Date("2012-01-01"),
    n_sites = 4,
    split_vars = site_split
  )
  again <- Raw_SITE(
    data,
    previous_data = list(Raw_SITE = first),
    spec = spec,
    startDate = as.Date("2012-01-01"),
    n_sites = 4,
    split_vars = site_split
  )

  expect_identical(again, first)
})

test_that("Raw_SITE renames columns per source_col in the spec", {
  set.seed(8409)
  spec <- make_site_spec()
  spec$Raw_SITE$site_status$source_col <- "SITE_STATUS"

  res <- Raw_SITE(
    make_site_data(),
    previous_data = list(),
    spec = spec,
    startDate = as.Date("2012-01-01"),
    n_sites = 5,
    split_vars = site_split
  )

  expect_true("SITE_STATUS" %in% names(res))
  expect_false("site_status" %in% names(res))
})
