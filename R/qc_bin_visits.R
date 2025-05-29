#' Count visits to each bin on a given day
#'
#' This function counts the number of visits to each bin on a given day.
#' It ensures all possible bins are included in the result, with zero visits
#' for bins that weren't visited.
#'
#' @param data A dataframe containing bin visit data.
#' @param bin_col Name of the column containing bin IDs. Default from [bin_col2()].
#' @param bins_all Vector of all possible bin IDs (feed and water).
#'
#' @return A tibble with columns for bin ID and visit frequency.
#' @keywords internal
#' @noRd
count_visits_per_bin <- function(data, bin_col = bin_col2(), bins_all = NULL) {
  if (nrow(data) == 0 || is.null(data)) {
    return(tibble::tibble(!!rlang::sym(bin_col) := integer(0), visit_freq = integer(0)))
  }
  
  # Count visits for each bin in the data
  visit_counts <- data |>
    dplyr::count(!!rlang::sym(bin_col), name = "visit_freq")
  
  # If bins_all is provided, ensure all bins are represented
  if (!is.null(bins_all)) {
    all_bins_df <- tibble::tibble(!!rlang::sym(bin_col) := bins_all)
    visit_counts <- dplyr::full_join(all_bins_df, visit_counts, by = bin_col) |>
      dplyr::mutate(visit_freq = tidyr::replace_na(visit_freq, 0))
  }
  
  return(visit_counts)
}

#' Count visits per cow and bin
#'
#' This function counts the number of visits each cow makes to each bin.
#'
#' @param data A dataframe containing bin visit data.
#' @inheritParams set_global_cols
#'
#' @return A tibble with columns for cow ID, bin ID, and visit frequency.
#' @keywords internal
#' @noRd
count_visits_per_cow_bin <- function(data, id_col = id_col2(), bin_col = bin_col2()) {
  if (nrow(data) == 0 || is.null(data)) {
    return(tibble::tibble(
      !!rlang::sym(id_col) := character(0),
      !!rlang::sym(bin_col) := integer(0),
      visit_freq = integer(0)
    ))
  }
  
  count_df <- data |>
    dplyr::count(!!rlang::sym(id_col), !!rlang::sym(bin_col), name = "visit_freq")

  return(count_df)
}

#' Calculate number of unique feed and water bins visited by each cow
#'
#' This function determines how many unique feed bins and water bins each cow visited.
#'
#' @param data A dataframe recording how many times each cow visited each bin, returned from [count_visits_per_cow_bin()]
#' @inheritParams set_global_cols
#'
#' @return A tibble with columns for cow ID, number of unique feed bins visited,
#'         number of unique water bins visited, and total unique bins visited.
#' @keywords internal
#' @noRd
count_unique_feed_water_bins_visited_per_cow <- function(data, 
                              id_col = id_col2(), 
                              bin_col = bin_col2(),
                              bin_offset = bin_offset2(),
                              bins_feed = bins_feed2(),
                              bins_wat = bins_wat2()) {
  
  if (nrow(data) == 0 || is.null(data)) {
    return(tibble::tibble(
      !!rlang::sym(id_col) := character(0),
      unique_feed_bins_visited = integer(0),
      unique_water_bins_visited = integer(0),
      total_bins_visited = integer(0)
    ))
  }
  
  # Prepare water bin IDs with offset applied
  wat_bins_with_offset <- bin_offset + bins_wat
  
  # Count feed bins visited per cow
  feed_bins_per_cow <- data |>
    dplyr::filter(!!rlang::sym(bin_col) %in% bins_feed) |>
    dplyr::distinct(!!rlang::sym(id_col), !!rlang::sym(bin_col)) |>
    dplyr::count(!!rlang::sym(id_col), name = "unique_feed_bins_visited")
  
  # Count water bins visited per cow
  water_bins_per_cow <- data |>
    dplyr::filter(!!rlang::sym(bin_col) %in% wat_bins_with_offset) |>
    dplyr::distinct(!!rlang::sym(id_col), !!rlang::sym(bin_col)) |>
    dplyr::count(!!rlang::sym(id_col), name = "unique_water_bins_visited")
  
  # Combine feed and water bin counts
  feed_water_bin_count <- dplyr::full_join(feed_bins_per_cow, water_bins_per_cow, by = id_col) |>
    dplyr::mutate(
      unique_feed_bins_visited = tidyr::replace_na(unique_feed_bins_visited, 0),
      unique_water_bins_visited = tidyr::replace_na(unique_water_bins_visited, 0),
      total_bins_visited = unique_feed_bins_visited + unique_water_bins_visited
    )
  return(feed_water_bin_count)
}

#' Check bins with low or no visits
#'
#' This function analyzes bin visit patterns to identify bins with low/no visits.
#'
#' @inheritParams qc
#' @inheritParams qc_warning_skeleton
#' @param all_bins Vector of all bin IDs included in this analysis.
#' @param warn Warning dataframe to update with bin visit issues.
#' @param verbose Logical. Whether to print warning messages to console.
#'
#' @return Updated warning dataframe with bin visit warnings.
#' @keywords internal
#' @noRd
qc_bin_visits <- function(comb, warn, cfg = qc_config(), 
                          id_col = id_col2(), 
                          bin_col = bin_col2(),
                          all_bins = bins_feed2(),
                          verbose = TRUE) {
  
  # Skip if no data
  if (length(comb) == 0) {
    return(warn)
  }
  
  # Process each day's data
  for (i in seq_along(comb)) {
    date <- names(comb)[i]
    day_idx <- which(warn$date == date)
    
    # Skip if no matching date in warnings or empty data
    if (length(day_idx) == 0 || nrow(comb[[i]]) == 0) {
      next
    }
    
    # Count visits to each bin
    bin_visits <- count_visits_per_bin(comb[[i]], bin_col, all_bins)
    
    # Check for bins never visited
    never_visited <- bin_visits |>
      dplyr::filter(visit_freq == 0) |>
      dplyr::pull(!!rlang::sym(bin_col))
    
    if (length(never_visited) > 0) {
      warn$bins_never_visited[day_idx] <- paste(never_visited, collapse = "; ")
      
      if (verbose) {
        message(sprintf("Date %s: %d bins were never visited: %s", 
                        date, length(never_visited), 
                        paste(never_visited, collapse = ", ")))
      }
    }
    
    # Check for bins with low traffic
    low_traffic <- bin_visits |>
      dplyr::filter(visit_freq > 0, visit_freq < cfg$low_visit_threshold) |>
      dplyr::pull(!!rlang::sym(bin_col))
    
    if (length(low_traffic) > 0) {
      warn$bins_low_traffic[day_idx] <- paste(low_traffic, collapse = "; ")
      
      if (verbose) {
        message(sprintf("Date %s: %d bins has low cow traffic (<%d visits): %s", 
                        date, length(low_traffic), cfg$low_visit_threshold,
                        paste(low_traffic, collapse = ", ")))
      }
    }
  }
  
  return(warn)
} 