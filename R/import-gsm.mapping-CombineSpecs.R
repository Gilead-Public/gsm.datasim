CombineSpecs <- function(lSpecs, bIsWorkflow = TRUE) {
  if (bIsWorkflow) {
    lSpecs <- map(lSpecs, ~ .x$spec)
  }

  # Get all unique domains across all specs
  all_domains <- unique(unlist(map(lSpecs, names)))

  # Combine specs for each domain using lapply/map
  combined_specs <- map(all_domains, function(domain) {
    domain_specs <- map(lSpecs, ~ .x[[domain]] %||% list())
    combine_domain(domain_specs)
  })

  # Set the names of combined_specs to the domain names
  names(combined_specs) <- all_domains

  return(combined_specs)
}

combine_domain <- function(domain_specs) {
  combined <- reduce(domain_specs, function(combined, spec) {
    # Ensure all columns exist in both combined and spec
    combined_cols <- union(names(combined), names(spec))

    # Fill missing columns with NULLs in both lists
    combined <- map(combined_cols, ~ combined[[.x]] %||% NULL)
    spec <- map(combined_cols, ~ spec[[.x]] %||% NULL)
    names(combined) <- combined_cols
    names(spec) <- combined_cols

    # Combine the specifications using map2
    combined <- pmap(list(combined, spec, combined_cols), function(combined_col, spec_col, col_name) {
      update_column(combined_col, spec_col, col_name)
    })

    # Ensure the output is a named list
    set_names(combined, combined_cols)
  }, .init = list())

  return(combined)
}

update_column <- function(existing_col, new_col, col_name) {
  if (length(existing_col)) {
    # Handle type conflict with a warning when available
    if (!is.null(existing_col$type) && !is.null(new_col$type)) {
      if (existing_col$type != new_col$type) {
        warning(
          sprintf("Type mismatch for '%s'. Using first type: %s", col_name, existing_col$type),
          call. = FALSE
        )
      }
    }
  } else {
    # If the column doesn't exist, use the new column
    existing_col <- new_col
  }
  return(existing_col)
}
