combination_var_splitter <- function(variable_data, split_vars) {
  for (split_var_name in split_vars) {
    # Step 1: Find the index of the sublist in the main list
    sublist_index <- which(names(variable_data) == split_var_name)

    # Step 2: Extract the elements of the sublist
    sublist_elements <- variable_data[[sublist_index]]

    # Step 3: Remove the sublist from the main list
    variable_data[[sublist_index]] <- NULL

    # Step 4: Insert the sublist elements into the main list at the original position
    variable_data <- append(variable_data, sublist_elements, after = sublist_index - 1)
  }

  return(variable_data)
}


get_outlier_intensity <- function() {
  intensity <- getOption("gsm.datasim.outlier_intensity", default = 1)
  if (!is.numeric(intensity) || length(intensity) != 1 || is.na(intensity)) {
    return(1)
  }
  max(0, intensity)
}


scale_outlier_probabilities <- function(prob, outlier_idx, intensity = get_outlier_intensity()) {
  if (length(prob) == 0) {
    return(prob)
  }

  total_prob <- sum(prob)
  if (total_prob <= 0) {
    return(prob)
  }

  # Work with normalized probabilities and reapply original total at the end.
  p <- prob / total_prob
  valid_outlier_idx <- unique(outlier_idx[outlier_idx >= 1 & outlier_idx <= length(p)])

  if (length(valid_outlier_idx) == 0 || intensity == 1) {
    return(p * total_prob)
  }

  non_outlier_idx <- setdiff(seq_along(p), valid_outlier_idx)
  outlier_mass <- sum(p[valid_outlier_idx])
  non_outlier_mass <- sum(p[non_outlier_idx])

  if (outlier_mass <= 0 || non_outlier_mass <= 0) {
    return(p * total_prob)
  }

  # Use odds scaling (with a stronger boost for intensity > 1) so low-baseline
  # outlier categories move enough to impact downstream flagging.
  effective_intensity <- if (intensity <= 1) intensity else intensity^2
  outlier_odds <- outlier_mass / non_outlier_mass
  scaled_outlier_odds <- outlier_odds * effective_intensity
  target_outlier_mass <- scaled_outlier_odds / (1 + scaled_outlier_odds)

  p[valid_outlier_idx] <- p[valid_outlier_idx] * (target_outlier_mass / outlier_mass)
  p[non_outlier_idx] <- p[non_outlier_idx] * ((1 - target_outlier_mass) / non_outlier_mass)

  p * total_prob
}


generate_zscore_outlier_values <- function(n,
                                           mean,
                                           sd,
                                           min_value = -Inf,
                                           max_value = Inf,
                                           intensity = get_outlier_intensity(),
                                           one_sided = TRUE,
                                           integer = TRUE) {
  if (n <= 0) {
    return(numeric(0))
  }

  base <- stats::rnorm(n, mean = mean, sd = sd)

  if (intensity > 1) {
    outlier_fraction <- min(0.35, 0.02 * (intensity^1.4))
    n_outliers <- max(1L, floor(n * outlier_fraction))
    outlier_idx <- sample.int(n, n_outliers, replace = FALSE)

    z_magnitude <- stats::rnorm(
      n_outliers,
      mean = 2.8 + 0.6 * log(intensity),
      sd = 0.45
    )

    z_sign <- if (isTRUE(one_sided)) {
      rep(1, n_outliers)
    } else {
      sample(c(-1, 1), n_outliers, replace = TRUE)
    }

    base[outlier_idx] <- mean + (z_magnitude * z_sign * sd)
  }

  base <- pmin(pmax(base, min_value), max_value)
  if (isTRUE(integer)) {
    return(as.integer(round(base)))
  }
  base
}


inject_site_hotspot_outliers <- function(values,
                                         row_keys,
                                         key_map,
                                         key_col = "subject_nsv",
                                         site_col = "invid",
                                         intensity = get_outlier_intensity(),
                                         min_z = 3) {
  if (length(values) == 0 || intensity <= 1) {
    return(values)
  }

  if (is.null(key_map) || !(key_col %in% names(key_map)) || !(site_col %in% names(key_map))) {
    return(values)
  }

  # Map each generated row to a site via row key (e.g., subject_nsv -> invid).
  map_idx <- match(row_keys, key_map[[key_col]])
  row_sites <- key_map[[site_col]][map_idx]
  valid_idx <- which(!is.na(row_sites))
  if (length(valid_idx) == 0) {
    return(values)
  }

  unique_sites <- unique(row_sites[valid_idx])
  n_sites <- length(unique_sites)
  if (n_sites == 0) {
    return(values)
  }

  hotspot_frac <- min(0.25, max(0.05, 0.04 * intensity))
  n_hotspots <- max(1L, floor(n_sites * hotspot_frac))
  hotspot_sites <- sample(unique_sites, n_hotspots, replace = FALSE)
  hotspot_idx <- which(row_sites %in% hotspot_sites)
  if (length(hotspot_idx) == 0) {
    return(values)
  }

  base_sd <- stats::sd(values, na.rm = TRUE)
  if (is.na(base_sd) || base_sd <= 0) {
    base_sd <- 1
  }

  # Push hotspot rows into the 3+ SD tail to trigger z-score style flaggers.
  shift_mean <- max(min_z, 2 + intensity) * base_sd
  values[hotspot_idx] <- values[hotspot_idx] + abs(stats::rnorm(
    length(hotspot_idx),
    mean = shift_mean,
    sd = 0.4 * base_sd
  ))

  values
}


sample_categorical_with_hotspots <- function(values,
                                             n,
                                             base_prob,
                                             outlier_idx,
                                             row_keys = NULL,
                                             key_map = NULL,
                                             key_col = "subjid",
                                             site_col = "invid",
                                             intensity = get_outlier_intensity()) {
  if (n <= 0) {
    return(values[integer(0)])
  }

  base_prob <- scale_outlier_probabilities(base_prob, outlier_idx = outlier_idx, intensity = intensity)
  baseline_draw <- sample(values, n, replace = TRUE, prob = base_prob)

  if (intensity <= 1 || is.null(row_keys) || is.null(key_map) ||
    !(key_col %in% names(key_map)) || !(site_col %in% names(key_map))) {
    return(baseline_draw)
  }

  map_idx <- match(row_keys, key_map[[key_col]])
  row_sites <- key_map[[site_col]][map_idx]
  valid_idx <- which(!is.na(row_sites))
  if (length(valid_idx) == 0) {
    return(baseline_draw)
  }

  unique_sites <- unique(row_sites[valid_idx])
  hotspot_frac <- min(0.25, max(0.05, 0.04 * intensity))
  n_hotspots <- max(1L, floor(length(unique_sites) * hotspot_frac))
  hotspot_sites <- sample(unique_sites, n_hotspots, replace = FALSE)
  hotspot_idx <- which(row_sites %in% hotspot_sites)
  if (length(hotspot_idx) == 0) {
    return(baseline_draw)
  }

  p <- base_prob / sum(base_prob)
  out_idx <- unique(outlier_idx[outlier_idx >= 1 & outlier_idx <= length(p)])
  non_idx <- setdiff(seq_along(p), out_idx)
  if (length(out_idx) == 0 || length(non_idx) == 0) {
    return(baseline_draw)
  }

  out_mass <- sum(p[out_idx])
  non_mass <- sum(p[non_idx])
  if (out_mass <= 0 || non_mass <= 0) {
    return(baseline_draw)
  }

  boost <- intensity^2
  out_odds <- out_mass / non_mass
  boosted_out_mass <- (out_odds * boost) / (1 + out_odds * boost)
  boosted_out_mass <- min(0.995, boosted_out_mass)

  p_hot <- p
  p_hot[out_idx] <- p_hot[out_idx] * (boosted_out_mass / out_mass)
  p_hot[non_idx] <- p_hot[non_idx] * ((1 - boosted_out_mass) / non_mass)

  baseline_draw[hotspot_idx] <- sample(values, length(hotspot_idx), replace = TRUE, prob = p_hot)
  baseline_draw
}


add_new_var_data <- function(dataset, vars, args, orig_curr_spec, ...) {
  internal_args <- list(...)

  variable_data <- lapply(names(vars), function(var_name) {
    generator_func <- var_name
    if (!(var_name %in% names(args))) {
      curr_args <- args$default
    } else {
      curr_args <- args[[var_name]]
      if (!(var_name %in% names(dataset))) {
        curr_args[[var_name]] <- NULL
      } else {
        curr_args[[var_name]] <- dataset[[var_name]]
      }
    }

    # Generate data using the generator function.
    # If no function with this name exists (e.g. a new spec column like
    # `score_val` in Raw_LB from a preexisting workflow), fall back to
    # type-based generation via generate_column_by_type so that the parent
    # generator (registry or legacy) can still produce a structurally correct
    # dataset with the extra column auto-filled.
    tryCatch(
      do.call(generator_func, curr_args),
      error = function(e) {
        if (grepl("could not find function", conditionMessage(e), fixed = TRUE)) {
          n_val <- curr_args[[1L]]
          if (!is.numeric(n_val) || length(n_val) != 1L) {
            n_val <- tryCatch(nrow(curr_args[[1L]]), error = function(e2) 1L)
          }
          logger::log_debug(
            "No generator function '{var_name}' found; using type-based fallback (n = {n_val})"
          )
          generate_column_by_type(
            var_name,
            orig_curr_spec[[var_name]] %||% list(),
            as.integer(n_val)
          )
        } else {
          stop(e)
        }
      }
    )
  })


  names(variable_data) <- names(vars)
  if ("split_vars" %in% names(internal_args)) {
    variable_data <- combination_var_splitter(variable_data, internal_args$split_vars)
  }

  variable_data <- variable_data %>%
    as.data.frame() %>%
    rename_raw_data_vars_per_spec(orig_curr_spec)


  if (!is.null(dataset)) {
    return(dplyr::bind_rows(dataset, variable_data))
  } else {
    return(variable_data)
  }
}

count_gen <- function(max_n, SnapshotCount) {
  iteration <- max_n / SnapshotCount
  counts <- c()
  for (i in seq(SnapshotCount)) {
    if (i > 1) {
      start <- counts[i - 1]
    } else {
      start <- 1
    }
    end <- i * iteration

    if (i < SnapshotCount) {
      if ((start < end) & ((floor(end) - start) > 1)) {
        new_element <- sample(floor((start + end) / 2):floor(end), size = 1)
      } else {
        new_element <- start
      }
    } else {
      new_element <- max_n
    }

    counts <- c(counts, new_element)
  }

  return(counts)
}

.gsm_datasim_spec_cache <- new.env(parent = emptyenv())

load_specs <- function(workflow_path, mappings, package, use_cache = TRUE) {
  cache_key <- paste(
    package,
    workflow_path,
    paste(mappings, collapse = "|"),
    sep = "::"
  )

  if (isTRUE(use_cache) && exists(cache_key, envir = .gsm_datasim_spec_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .gsm_datasim_spec_cache, inherits = FALSE))
  }

  wf_mapping <- workr::MakeWorkflowList(strPath = workflow_path, strNames = mappings, strPackage = package)
  wf_req <- workr::MakeWorkflowList(strPath = "workflow/1_mappings", strNames = c("SUBJ", "STUDY", "SITE", "ENROLL"), strPackage = "gsm.mapping")
  wf_all <- modifyList(wf_mapping, wf_req)
  if (any(c("OverallResponse") %in% mappings)) {
    wf_visit <- workr::MakeWorkflowList(strPath = "workflow/1_mappings", strNames = c("VISIT"), strPackage = "gsm.mapping")
    wf_all <- modifyList(wf_all, wf_visit)
  }
  if (any(c("Consents", "Death") %in% mappings)) {
    wf_studcomp <- workr::MakeWorkflowList(strPath = "workflow/1_mappings", strNames = c("STUDCOMP"), strPackage = "gsm.mapping")
    wf_all <- modifyList(wf_all, wf_studcomp)
  }
  combined_specs <- CombineSpecs(wf_all)

  if (isTRUE(use_cache)) {
    assign(cache_key, combined_specs, envir = .gsm_datasim_spec_cache)
  }

  return(combined_specs)
}

rename_raw_data_vars_per_spec <- function(variable_data, spec) {
  for (var_name in names(spec)) {
    variabale <- spec[[var_name]]

    # Check if "source_col" exists in the sublist
    if ("source_col" %in% names(variabale)) {
      # Retrieve the new name from "source_col"
      new_name <- variabale[["source_col"]]
      # Rename only when the source column is present
      if (var_name %in% names(variable_data)) {
        names(variable_data)[names(variable_data) == var_name] <- new_name
      }
    }
  }
  return(variable_data)
}

generate_unique_combinations_code <- function(data, vars, run_code = FALSE) {
  # Extract unique combinations
  unique_combinations <- unique(data[, vars, drop = FALSE])

  # Start constructing the code
  code <- "unique_combinations <- data.frame(\n"

  # Iterate over each variable to construct the code
  for (i in seq_along(vars)) {
    var_name <- vars[i]
    values <- unique_combinations[[var_name]]
    # Handle character and factor variables by quoting the values
    if (is.character(values) || is.factor(values)) {
      values_str <- paste0('"', values, '"', collapse = ", ")
    } else {
      values_str <- paste(values, collapse = ", ")
    }
    # Add the variable and its values to the code
    code <- paste0(code, "  ", var_name, " = c(", values_str, ")")
    # Add a comma if not the last variable
    if (i < length(vars)) {
      code <- paste0(code, ",\n")
    } else {
      code <- paste0(code, "\n")
    }
  }
  code <- paste0(code, ")")

  if (run_code) {
    result <- eval(parse(text = code))
  } else {
    result <- code
  }

  cat(result)

  return(result)
}


generate_consecutive_random_dates <- function(n, start_date, mean_days_between_dates = 30, ...) {
  start_date <- as.Date(start_date)
  dates <- vector("character", n)
  prev_date <- start_date

  for (i in seq_len(n)) {
    # Define the date range (start one day after previous to guarantee unique dates)
    min_date <- prev_date + 1
    max_date <- prev_date + mean_days_between_dates
    # Generate a random date within the range
    random_date <- as.Date(sample(seq(min_date, max_date, by = "day"), 1), origin = "1970-01-01")
    # Store the date as a string
    dates[i] <- format(random_date, "%Y-%m-%d")
    # Update prev_date for the next iteration
    prev_date <- random_date
  }
  return(dates)
}

repeat_rows <- function(n, data) {
  if (is.vector(data)) {
    return(rep(data, each = n))
  } else if (is.data.frame(data) || is.matrix(data)) {
    result <- data[rep(seq_len(nrow(data)), each = n), , drop = FALSE]
    rownames(result) <- NULL
    return(result)
  } else {
    stop("Unsupported data type. Input must be a vector, matrix, or data frame.")
  }
}

generate_form_df <- function(n) {
  num_forms <- ceiling(n / 4)
  forms <- rep(paste0("form", 1:num_forms), each = 4)[1:n]
  fields <- paste0("field", 1:n)
  data.frame(form = forms, field = fields)
}

enrollment_count_gen <- function(subject_count) {
  screened <- function(n, previous_screened) {
    lower_bound <- max(n %/% 3, previous_screened)
    if (lower_bound != n) {
      return(sample(lower_bound:n, size = 1))
    } else {
      return(n)
    }
  }

  previous_screened <- 0
  enrollment_count <- c()
  for (i in seq(subject_count)) {
    if (i == 1) {
      previous_screened <- 0
    } else {
      previous_screened <- subject_count[i - 1]
    }

    enrollment_count <- c(enrollment_count, screened(subject_count[i], previous_screened))
  }

  return(enrollment_count)
}


save_data_on_disk <- function(data, base_path = NULL) {
  logger::log_info(glue::glue("Saving datasets ..."))
  logger::log_info(glue::glue("Please wait, proces may take around 15 minutes due to the number of files and file sizes ..."))

  # Calculate the total number of dataframes to process
  total_dfs <- sum(
    sapply(
      data,
      function(study_data) {
        sum(
          sapply(
            study_data,
            function(snapshot_data) {
              length(snapshot_data)
            }
          )
        )
      }
    )
  )

  # Initialize the progress bar
  pb <- txtProgressBar(min = 0, max = total_dfs, style = 3)
  counter <- 0 # Initialize a counter

  tictoc::tic()
  for (study_name in names(data)) {
    study_data <- data[[study_name]]

    for (snapshot_name in names(study_data)) {
      snapshot_data <- study_data[[snapshot_name]]

      # Create the folder structure using the names
      if (is.null(base_path)) {
        base_path <- "data-raw"
      }

      dir_path <- file.path(base_path, "simulated_data", study_name, snapshot_name)

      dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)

      for (df_name in names(snapshot_data)) {
        df_value <- snapshot_data[[df_name]]
        # Define the file path for the Parquet file
        file_path <- file.path(dir_path, paste0(df_name, ".parquet"))

        # Save the dataframe to a Parquet file
        arrow::write_parquet(df_value, file_path)

        counter <- counter + 1
        setTxtProgressBar(pb, counter)
      }
    }
  }

  close(pb)

  logger::log_info(glue::glue("Saved all data successfully"))
  tictoc::toc()
}

generate_random_fpfv <- function(min_date, max_date, canBeEmpty = FALSE, previous_date = NULL) {
  # Ensure that max_date is a Date object
  max_date <- as.Date(max_date)
  min_date <- as.Date(min_date)

  # If canBeEmpty is TRUE, there is a chance that the result can be NA
  if (canBeEmpty && stats::runif(1) < 0.2 && (is.null(previous_date) || is.na(previous_date))) {
    return(NA) # Randomly decide to return NA with 20% chance
  }


  if (is.null(previous_date) || is.na(previous_date)) {
    random_date <- sample(seq(from = min_date, to = max_date, by = "day"), 1)
  } else {
    random_date <- previous_date
  }

  return(random_date)
}

period_to_days <- function(period) {
  # Convert to lowercase and trim any extra whitespace
  p <- tolower(trimws(period))

  # 1) Look for patterns like "4 weeks", "10 days", etc.
  #    This pattern means: one or more digits, followed by spaces, followed by letters.
  #    For example: "4 weeks" -> c("4", "weeks")
  if (grepl("^\\d+\\s+\\w+$", p)) {
    parts <- strsplit(p, "\\s+")[[1]]
    num <- as.numeric(parts[1])
    unit <- parts[2]

    # Map the unit to a multiplier (rough or exact, as needed)
    # Adjust as you see fit; for instance, "months" can be ~30, but is an approximation.
    day_equiv <- switch(unit,
      "day"    = 1,
      "days"   = 1,
      "week"   = 7,
      "weeks"  = 7,
      "month"  = 30,
      "months" = 30,
      "year"   = 365,
      "years"  = 365,
      stop("Unrecognized unit in '", period, "'")
    )
    return(num * day_equiv)

    # 2) Handle single-word strings like "weekly", "biweekly", etc.
  } else {
    # For single-word strings, create a small dictionary of known terms.
    # You can expand this as much as you want.
    return(switch(p,
      "days"      = 1,
      "weeks"     = 7,
      "months"    = 30,
      "years"     = 365,
      # fallback if nothing matches
      stop("Unrecognized period: ", period)
    ))
  }
}
period_to_days("months")
