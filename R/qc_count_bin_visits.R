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

#' Calculate number of unique bins visited by each cow
#'
#' This function determines how many unique bins each cow visited.
#'
#' @param data A dataframe containing bin visit data
#' @param all_bins Vector of bin IDs to count visits for
#' @param bin_type String describing the type of bins ("feed", "water", or "all")
#' @inheritParams set_global_cols
#'
#' @return A tibble with columns for cow ID and number of unique bins visited
#' @keywords internal
#' @noRd
count_unique_bins_visited_per_cow <- function(data, 
                              all_bins,
                              bin_type = "all",
                              id_col = id_col2(), 
                              bin_col = bin_col2()) {
  
  if (nrow(data) == 0 || is.null(data)) {
    # Create appropriate column name based on bin_type
    if (bin_type == "feed") {
      col_name <- "unique_feed_bins_visited"
    } else if (bin_type == "water") {
      col_name <- "unique_water_bins_visited"
    } else {
      col_name <- "unique_bins_visited"
    }
    
    # Return empty tibble with appropriate column name
    return(tibble::tibble(
      !!rlang::sym(id_col) := character(0),
      !!rlang::sym(col_name) := integer(0)
    ))
  }
  
  # Determine name for the count column based on bin_type
  count_col <- if (bin_type == "feed") {
    "unique_feed_bins_visited"
  } else if (bin_type == "water") {
    "unique_water_bins_visited"
  } else {
    "unique_bins_visited"
  }
  
  # Count unique bins visited per cow
  bins_per_cow <- data |>
    dplyr::filter(!!rlang::sym(bin_col) %in% all_bins) |>
    dplyr::distinct(!!rlang::sym(id_col), !!rlang::sym(bin_col)) |>
    dplyr::count(!!rlang::sym(id_col), name = count_col)
  
  return(bins_per_cow)
}
