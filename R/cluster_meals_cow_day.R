
#' Core clustering logic for one animal-day combination
#'
#' @param animal_day_df Dataframe with visits for one animal on one day
#' @inheritParams cluster_meals
#' @inheritParams set_global_cols
#'
#' @return Dataframe with meal summaries for this animal-day
#' @keywords internal
#' @noRd
cluster_meals_cow_day <- function(animal_day_df, eps, min_pts, 
                                 id_col, start_col, end_col, bin_col, intake_col, dur_col, tz = tz2()) {
  
  # Parameter validation
  required_cols <- c(id_col, start_col, end_col, bin_col, intake_col, dur_col, "date")
  missing_cols <- setdiff(required_cols, names(animal_day_df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # If the total number of visits is less than min_pts, return empty (treated as noise)
  if (nrow(animal_day_df) < min_pts) {
    return(create_empty_meal_df(id_col, tz))
  }
  
  # Sort by start time
  animal_day_df <- animal_day_df[order(animal_day_df[[start_col]]), ]
  
  # Convert start times to minutes from midnight using helper function
  minutes_from_midnight <- convert_times_to_minutes(animal_day_df[[start_col]], tz = tz)
  
  # Check for and remove NA values in start times before clustering
  if (any(is.na(minutes_from_midnight))) {
    n_na <- sum(is.na(minutes_from_midnight))
    message("Found ", n_na, " NA values in start times for clustering. These rows will be removed.")
    valid_rows <- !is.na(minutes_from_midnight)
    animal_day_df <- animal_day_df[valid_rows, ]
    minutes_from_midnight <- minutes_from_midnight[valid_rows]
  }
  
  # Check if we still have enough data after removing NAs
  if (length(minutes_from_midnight) < min_pts) {
    return(create_empty_meal_df(id_col, tz))
  }
  
  # Eps should already be determined at a higher level based on scope
  if (is.null(eps)) {
    stop("eps should be determined before calling cluster_meals_cow_day")
  }
  
  # DBSCAN expects a matrix
  time_matrix <- matrix(minutes_from_midnight, ncol = 1)
  clusters <- dbscan::dbscan(time_matrix, eps = eps, minPts = min_pts)
  
  # Filter out noise points (cluster = 0)
  valid_clusters <- clusters$cluster[clusters$cluster > 0]
  if (length(valid_clusters) == 0) {
    # All points are noise
    return(create_empty_meal_df(id_col, tz))
  }
  
  # Add cluster assignments to dataframe
  animal_day_df$cluster <- clusters$cluster
  
  # Filter to only non-noise clusters and calculate meal statistics
  meal_data <- animal_day_df[animal_day_df$cluster > 0, ]
  
  meal_summaries <- meal_data |>
    dplyr::group_by(cluster) |>
    dplyr::summarise(
      !!id_col := dplyr::first(.data[[id_col]]),
      date = dplyr::first(date),
      meal_id = dplyr::first(cluster),
      meal_start = min(.data[[start_col]]),
      meal_end = max(.data[[end_col]]),
      meal_duration = as.numeric(lubridate::interval(min(.data[[start_col]]), 
                                                     max(.data[[end_col]])) / lubridate::seconds(1)),
      visit_count = dplyr::n(),
      total_intake = sum(.data[[intake_col]], na.rm = TRUE),
      total_feeding_duration = sum(.data[[dur_col]], na.rm = TRUE),
      unique_bins_count = length(unique(.data[[bin_col]])),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      feeding_percentage = ifelse(meal_duration > 0, (total_feeding_duration / meal_duration) * 100, 0)
    ) |>
    dplyr::select(-total_feeding_duration)
  
  # Reorder meal_id to be sequential within animal-day
  meal_summaries <- meal_summaries[order(meal_summaries$meal_start), ]
  meal_summaries$meal_id <- seq_len(nrow(meal_summaries))
  
  return(as.data.frame(meal_summaries))
}