#' Analyze Bin Visit Patterns For Each Cow
#'
#' This function analyzes how cows interact with feed and water bins
#' across multiple days. For each day and each cow, it calculates:
#' - The number of unique feed bins visited
#' - The number of unique water bins visited 
#' - The total number of unique bins visited
#'
#' @param feed A list of daily **feed** data frames named by date, or `NULL`
#'        if you don't have feeder data
#' @param water A list of daily **water** data frames named by date, or `NULL`
#'        if you don't have water data
#' @param return_list Logical. Whether to return a list of dataframes or a single dataframe.
#' @inheritParams set_global_cols
#'
#' @return A dataframe with columns for date, cow ID, number of unique feed bins visited,
#'         number of unique water bins visited, and total unique bins visited.
#'
#' @examples
#' # With separate feed and water data lists
#' bin_visit_stats <- unique_bin_visits(feed = all_fed, 
#'                                      water = all_wat,
#'                                      id_col = "cow",
#'                                      bin_col = "bin",
#'                                      bin_offset = 100,
#'                                      bins_feed = 1:30,
#'                                      bins_wat = 1:30,
#'                                      return_list = FALSE)
#' 
#' # View the results
#' head(bin_visit_stats)
#'
#' @export
unique_bin_visits <- function(feed = NULL, 
                              water = NULL,
                              id_col = id_col2(), 
                              bin_col = bin_col2(),
                              bin_offset = bin_offset2(),
                              bins_feed = bins_feed2(),
                              bins_wat = bins_wat2(),
                              return_list = FALSE) {
  
  # Validate input - at least one of feed or water must be provided
  if (is.null(feed) && is.null(water)) {
    stop("At least one of `feed` or `water` must be provided.")
  }
  
  # Prepare lists to store results
  dates <- c()
  
  # Get all unique dates from both feed and water data
  if (!is.null(feed)) {
    dates <- c(dates, names(feed))
  }
  if (!is.null(water)) {
    dates <- c(dates, names(water))
  }
  dates <- sort(unique(dates))
  
  # Create result lists
  daily_results <- vector("list", length(dates))
  names(daily_results) <- dates
  
  # Process each day's data
  for (date_val in dates) {
    feed_data <- NULL
    water_data <- NULL
    feed_results <- NULL
    water_results <- NULL
    
    # Get feed data for this date if available
    if (!is.null(feed) && date_val %in% names(feed)) {
      feed_data <- feed[[date_val]]
      
      # Process feed data if not empty
      if (!is.null(feed_data) && nrow(feed_data) > 0) {
        feed_results <- count_unique_bins_visited_per_cow(
          data = feed_data,
          all_bins = bins_feed,
          bin_type = "feed",
          id_col = id_col,
          bin_col = bin_col
        )
      }
    }
    
    # Get water data for this date if available
    if (!is.null(water) && date_val %in% names(water)) {
      water_data <- water[[date_val]]
      
      # Process water data if not empty
      if (!is.null(water_data) && nrow(water_data) > 0) {
        water_results <- count_unique_bins_visited_per_cow(
          data = water_data,
          all_bins = bin_offset + bins_wat,
          bin_type = "water",
          id_col = id_col,
          bin_col = bin_col
        )
      }
    }
    
    # Create result for this day
    result <- NULL
    
    # Combine feed and water results if both exist
    if (!is.null(feed_results) && !is.null(water_results)) {
      result <- dplyr::full_join(feed_results, water_results, by = id_col) |>
        dplyr::mutate(
          unique_feed_bins_visited = tidyr::replace_na(unique_feed_bins_visited, 0),
          unique_water_bins_visited = tidyr::replace_na(unique_water_bins_visited, 0),
          total_bins_visited = unique_feed_bins_visited + unique_water_bins_visited
        )
    } 
    # If only feed results exist
    else if (!is.null(feed_results)) {
      result <- feed_results |>
        dplyr::mutate(
          unique_water_bins_visited = 0,
          total_bins_visited = unique_feed_bins_visited
        )
    } 
    # If only water results exist
    else if (!is.null(water_results)) {
      result <- water_results |>
        dplyr::mutate(
          unique_feed_bins_visited = 0,
          total_bins_visited = unique_water_bins_visited
        )
    } 
    # If no results for this day
    else {
      result <- tibble::tibble(
        !!rlang::sym(id_col) := character(0),
        unique_feed_bins_visited = integer(0),
        unique_water_bins_visited = integer(0),
        total_bins_visited = integer(0)
      )
    }
    
    # Add date column
    result <- result |>
      dplyr::mutate(date = date_val) |>
      dplyr::select(date, dplyr::everything())
    
    # Store in results list
    daily_results[[date_val]] <- result
  }
  
  if (return_list){
    return(daily_results)
  }
  else{
    # Merge all daily results into a single dataframe
    merged_results <- merge_list_df(daily_results)
    return(merged_results)
  }
  
}
