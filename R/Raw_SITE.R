#' Generate Raw SITE Data
#'
#' Generate Raw SITE based on `SITE.yaml` from `gsm.mapping`.
#'
#' @inheritParams Raw_STUDY
#' @returns a data.frame pertaining to the raw dataset plugged into `SITE.yaml`
#' @family internal
#' @keywords internal
#' @noRd

Raw_SITE <- function(data, previous_data, spec, startDate, ...) {
  inps <- list(...)

  curr_spec <- spec$Raw_SITE

  if ("Raw_SITE" %in% names(previous_data)) {
    dataset <- previous_data$Raw_SITE
    previous_row_num <- nrow(dataset)
  } else {
    dataset <- NULL
    previous_row_num <- 0
  }

  n <- inps$n_sites - previous_row_num
  if (n == 0) {
    return(dataset)
  }


  if (all(c("Country", "State", "City") %in% names(curr_spec))) {
    curr_spec$Country_State_City <- list(required = TRUE)
    curr_spec$Country <- NULL
    curr_spec$State <- NULL
    curr_spec$City <- NULL
  }

  if (all(c("invid") %in% names(curr_spec))) {
    curr_spec$invid <- list(required = TRUE)
  }


  # Function body for Raw_SITE
  args <- list(
    studyid = list(n, data$Raw_STUDY$protocol_number[[1]]),
    invid = list(n, dataset),
    default = list(n, startDate)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_SITE, ...)


  return(res)
}

invid <- function(n, previous_data, ...) {
  # Function body for invid
  args <- list(...)

  if ("invid" %in% names(previous_data)) {
    already_generated_invids <- previous_data$invid
  } else {
    already_generated_invids <- c()
  }


  # Generate all possible 3-digit numbers as strings with leading zeros
  possible_numbers <- sprintf("%03d", 0:9999)

  # Create all possible strings starting with "0X" and ending with the 3-digit numbers
  possible_strings_invids <- paste0("0X", possible_numbers)


  # Exclude the old strings to avoid duplication
  new_strings_invids <- setdiff(possible_strings_invids, already_generated_invids)

  # Check if there are enough unique strings to generate
  if (length(new_strings_invids) < n) {
    stop("Not enough unique strings available to generate ", n, " new strings.")
  }

  # Randomly sample 'n' unique strings from the available strings
  sample(new_strings_invids, n)
}

country <- function(n, ...) {
  # Function body for country
  sample(c("US", "China", "Japan"), n, replace = TRUE)
}

InvestigatorFirstName <- function(n, ...) {
  # Function body for InvestigatorFirstName
  sample(c("John", "Joanne", "Fred"),
    n,
    replace = TRUE
  )
}

InvestigatorLastName <- function(n, ...) {
  # Function body for InvestigatorLastName
  sample(c("Doe", "Deer", "Smith"),
    n,
    replace = TRUE
  )
}
site_status <- function(n, ...) {
  # Function body for site_status
  sample(c("Active", "", "Closed"),
    n,
    replace = TRUE
  )
}

Country_State_City_data <- data.frame(
  country = c("UK", "UK", "US", "US", "Japan", "Japan"),
  state = c("Greater London", "Buckinghamshire", "CA", "PA", "Kanto", "Kumamoto Prefecture"),
  city = c("London", "Milton Keynes", "Foster City", "Newtown Square", "Tokyo", "Kumamoto")
)

City <- function(n, ...) {
  cities <- unique(Country_State_City_data$city)
  # Function body for City
  sample(cities,
    n,
    replace = TRUE
  )
}

State <- function(n, ...) {
  args <- list(...)
  if ("cities" %in% names(args)) {
    indices <- match(args$cities, Country_State_City_data$city)
    states <- Country_State_City_data$state[indices]
  } else {
    states <- Country_State_City_data$state %>%
      sample(n,
        replace = TRUE
      )
  }
  return(states)
}

Country <- function(n, ...) {
  args <- list(...)
  if ("cities" %in% names(args)) {
    indices <- match(args$cities, Country_State_City_data$city)
    countries <- Country_State_City_data$country[indices]
  } else {
    countries <- Country_State_City_data$country %>%
      sample(n, replace = TRUE)
  }
  return(countries)
}

Country_State_City <- function(n, ...) {
  cities <- City(n)
  states <- State(n, cities = cities)
  countries <- Country(n, cities = cities)
  return(list(
    City = cities,
    State = states,
    Country = countries
  ))
}
