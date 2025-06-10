
#' Core clustering logic for one cow-day combination
#'
#' @param cow_day_df Dataframe with visits for one cow on one day
#' @inheritParams cluster_meals
#'
#' @return Dataframe with meal summaries for this cow-day
#' @keywords internal
#' @noRd
cluster_meals_cow_day <- function(cow_day_df, eps, min_pts, id_col, start_col, 
                                 end_col, bin_col, intake_col, dur_col) {
  
  # If only one visit, return empty (treated as noise)
  if (nrow(cow_day_df) < min_pts) {
    return(create_empty_meal_df(id_col))
  }
  
  # Sort by start time
  cow_day_df <- cow_day_df[order(cow_day_df[[start_col]]), ]
  
  # Convert start times to minutes from midnight using lubridate
  start_times <- lubridate::as_datetime(cow_day_df[[start_col]])
  midnight <- lubridate::floor_date(start_times[1], "day")
  minutes_from_midnight <- as.numeric(lubridate::interval(midnight, start_times) / lubridate::minutes(1))
  
  # Auto-determine eps if not provided
  if (is.null(eps)) {
    # Convert end times to minutes from midnight for gap calculation
    end_times <- lubridate::as_datetime(cow_day_df[[end_col]])
    end_minutes_from_midnight <- as.numeric(lubridate::interval(midnight, end_times) / lubridate::minutes(1))
    eps <- optimal_interval(minutes_from_midnight, end_minutes_from_midnight)
  }
  
  # DBSCAN expects a matrix
  time_matrix <- matrix(minutes_from_midnight, ncol = 1)
  clusters <- dbscan::dbscan(time_matrix, eps = eps, minPts = min_pts)
  
  # Filter out noise points (cluster = 0)
  valid_clusters <- clusters$cluster[clusters$cluster > 0]
  if (length(valid_clusters) == 0) {
    # All points are noise
    return(create_empty_meal_df(id_col))
  }
  
  # Add cluster assignments to dataframe
  cow_day_df$cluster <- clusters$cluster
  
  # Filter to only non-noise clusters and calculate meal statistics
  meal_data <- cow_day_df[cow_day_df$cluster > 0, ]
  
  meal_summaries <- meal_data |>
    dplyr::group_by(cluster) |>
    dplyr::summarise(
      !!id_col := first(.data[[id_col]]),
      date = first(date),
      meal_id = first(cluster),
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
  
  # Reorder meal_id to be sequential within cow-day
  meal_summaries <- meal_summaries[order(meal_summaries$meal_start), ]
  meal_summaries$meal_id <- seq_len(nrow(meal_summaries))
  
  return(as.data.frame(meal_summaries))
}