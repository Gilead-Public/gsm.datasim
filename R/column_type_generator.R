#' Infer Column Type from Spec and Name Patterns
#'
#' Resolves the data type for a column by checking (1) the explicit `type` field
#' in the spec, (2) column name patterns, then (3) falling back to `"character"`.
#'
#' @param col_name Character. The column name.
#' @param col_spec List. Spec metadata for the column (may contain `type`,
#'   `source_col`, `required`).
#'
#' @return A single character string: one of `"date"`, `"numeric"`, `"integer"`,
#'   `"logical"`, `"yn"`, `"character"`, or `"timestamp"`.
#'
#' @keywords internal
infer_column_type <- function(col_name, col_spec = list()) {
  # 1. Explicit type from spec

  if (!is.null(col_spec$type)) {
    type <- tolower(col_spec$type)
    if (type %in% c("date", "numeric", "integer", "logical", "character", "timestamp")) {
      return(type)
    }
  }

  # 2. Infer from column name patterns
  name <- tolower(col_name)

  # Date patterns: _dt, _date, date suffix, fpfv/lpfv date abbreviations
  if (grepl("(_dt|_date|date)$", name) || grepl("^(dt_|date_)", name)) {
    return("date")
  }
  if (grepl("(fpfv|lpfv|lplv|fplv)$", name)) {
    return("date")
  }

  # Y/N flag patterns
  if (grepl("(_yn|yn)$", name) || grepl("(_flag|flag)$", name)) {
    return("yn")
  }

  # Integer/count patterns
  if (grepl("(_count|count|_num|_n)$", name) || grepl("^(num_|n_)", name)) {
    return("integer")
  }

  # Numeric/score patterns
  if (grepl("(_score|_val|_result|_pct|_rate|_ratio)$", name)) {
    return("numeric")
  }

  # Logical patterns
  if (grepl("^(is_|has_|was_|can_)", name)) {
    return("logical")
  }

  # 3. Default
  "character"
}


#' Get Foreign Key Mappings
#'
#' Returns a list mapping common FK column names to the domain and column they
#' should reference. Used by [generate_column_by_type()] to maintain referential
#' integrity when generating data for unknown domains.
#'
#' @return A named list where each element has `domain` and `column` fields.
#'
#' @keywords internal
get_fk_mappings <- function() {
  list(
    subjid = list(domain = "Raw_SUBJ", column = "subjid"),
    subject_nsv = list(domain = "Raw_SUBJ", column = "subject_nsv"),
    invid = list(domain = "Raw_SITE", column = "invid"),
    siteid = list(domain = "Raw_SITE", column = "invid"),
    studyid = list(domain = "Raw_STUDY", column = "protocol_number"),
    country = list(domain = "Raw_SITE", column = "country")
  )
}


#' Generate a Single Column by Inferred Type
#'
#' Produces a vector of length `n` with realistic simulated values based on the
#' resolved column type. For columns that look like foreign keys (e.g. `subjid`,
#' `invid`), values are sampled from existing parent domain data when available
#' in `context$data`.
#'
#' @param col_name Character. The column name.
#' @param col_spec List. Spec metadata for the column.
#' @param n Integer. Number of rows to generate.
#' @param context List with element `data` (named list of data.frames already
#'   generated for earlier domains), and `start_date`/`end_date` (Date or
#'   character coercible to Date).
#'
#' @return A vector of length `n`.
#'
#' @export
generate_column_by_type <- function(col_name, col_spec = list(), n, context = list()) {
  # ── FK lookup ──────────────────────────────────────────────────────────────
  fk_map <- get_fk_mappings()
  if (col_name %in% names(fk_map)) {
    fk <- fk_map[[col_name]]
    parent <- context$data[[fk$domain]]
    if (!is.null(parent) && fk$column %in% names(parent)) {
      pool <- unique(parent[[fk$column]])
      if (length(pool) > 0) {
        return(sample(pool, n, replace = TRUE))
      }
    }
  }

  # ── Type-based generation ──────────────────────────────────────────────────
  col_type <- infer_column_type(col_name, col_spec)

  start_date <- tryCatch(as.Date(context$start_date %||% "2012-01-01"), error = function(e) as.Date("2012-01-01"))
  end_date <- tryCatch(as.Date(context$end_date %||% "2012-12-31"), error = function(e) as.Date("2012-12-31"))

  switch(col_type,
    date = sample(seq.Date(start_date, end_date, by = "day"), n, replace = TRUE),
    timestamp = {
      dates <- sample(seq.Date(start_date, end_date, by = "day"), n, replace = TRUE)
      as.POSIXct(paste(dates, sprintf(
        "%02d:%02d:%02d",
        sample(0:23, n, replace = TRUE),
        sample(0:59, n, replace = TRUE),
        sample(0:59, n, replace = TRUE)
      )))
    },
    numeric = round(stats::rnorm(n, mean = 50, sd = 15), 2),
    integer = sample(1L:100L, n, replace = TRUE),
    logical = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.7, 0.3)),
    yn = sample(c("Y", "N"), n, replace = TRUE, prob = c(0.8, 0.2)),
    # default: character
    {
      prefix <- toupper(gsub("[^a-zA-Z]", "", substr(col_name, 1, 4)))
      paste0(prefix, "-", sprintf("%04d", seq_len(n)))
    }
  )
}


#' Generate Data for an Unknown Domain
#'
#' Creates a complete `data.frame` for a domain that has no dedicated generator
#' function in the domain registry or as a legacy `Raw_*()` function. Each column
#' in the spec is generated using [generate_column_by_type()], with an attempt to
#' reuse existing named generator functions for columns whose names match a
#' known generator (e.g. `subjid`, `studyid`).
#'
#' When `previous_data` is supplied (a `data.frame` from the prior snapshot),
#' the function uses a cumulative delta pattern: it keeps all existing rows and
#' only generates `n - nrow(previous_data)` new rows, then binds them together.
#' This mirrors the behavior of the core domain registry generators for
#' longitudinal multi-snapshot generation.
#'
#' @param domain_name Character. The domain name (e.g. `"Raw_CUSTOM"`).
#' @param domain_spec Named list. Column specifications as returned by
#'   `CombineSpecs()` for this domain.
#' @param n Integer. Target total number of rows for this snapshot (cumulative).
#' @param context List with `data`, `start_date`, `end_date` (same structure as
#'   passed to [generate_column_by_type()]).
#' @param previous_data Optional `data.frame` from the prior snapshot for this
#'   domain. When provided, existing rows are retained and only the delta
#'   (`n - nrow(previous_data)`) new rows are generated.
#'
#' @return A `data.frame` with `n` rows and one column per spec entry (or more
#'   rows if `previous_data` already exceeds `n`).
#'
#' @export
generate_unknown_domain <- function(domain_name, domain_spec, n, context = list(),
                                    previous_data = NULL) {
  if (n <= 0 && is.null(previous_data)) {
    return(data.frame())
  }
  if (length(domain_spec) == 0) {
    logger::log_warn("Domain {domain_name} has no columns in spec; returning empty data.frame")
    return(data.frame(.row = seq_len(n))[, -1, drop = FALSE])
  }

  # ── Delta calculation for cumulative snapshots ───────────────────────────
  existing_df <- NULL
  if (!is.null(previous_data) && is.data.frame(previous_data) && nrow(previous_data) > 0) {
    existing_df <- previous_data
    delta_n <- n - nrow(existing_df)
    if (delta_n <= 0) {
      return(existing_df)
    }
  } else {
    delta_n <- n
  }

  col_names <- names(domain_spec)
  columns <- stats::setNames(
    lapply(col_names, function(col_name) {
      col_spec <- domain_spec[[col_name]]

      # Try to use an existing named generator function if one exists in the
      # package namespace (e.g. subjid, studyid, enrolldt).
      gen_fn <- tryCatch(
        match.fun(col_name),
        error = function(e) NULL
      )
      if (!is.null(gen_fn)) {
        result <- tryCatch(
          {
            # Attempt to call with common argument patterns
            if ("external_subjid" %in% names(formals(gen_fn)) &&
              !is.null(context$data$Raw_SUBJ$subjid)) {
              gen_fn(delta_n, external_subjid = context$data$Raw_SUBJ$subjid)
            } else if (all(c("startDate", "endDate") %in% names(formals(gen_fn)))) {
              gen_fn(delta_n,
                startDate = context$start_date %||% "2012-01-01",
                endDate = context$end_date %||% "2012-12-31"
              )
            } else if ("startDate" %in% names(formals(gen_fn))) {
              gen_fn(delta_n, startDate = context$start_date %||% "2012-01-01")
            } else {
              gen_fn(delta_n)
            }
          },
          error = function(e) NULL
        )

        if (!is.null(result) && (is.atomic(result) && length(result) == delta_n)) {
          return(result)
        }
      }

      # Fall back to type-based generation
      generate_column_by_type(col_name, col_spec, delta_n, context)
    }),
    col_names
  )

  new_df <- as.data.frame(columns, stringsAsFactors = FALSE)
  new_df <- rename_raw_data_vars_per_spec(new_df, domain_spec)

  # ── Bind new rows onto existing data ─────────────────────────────────────
  if (!is.null(existing_df)) {
    dplyr::bind_rows(existing_df, new_df)
  } else {
    new_df
  }
}
