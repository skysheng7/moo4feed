
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