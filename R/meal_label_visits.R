#' Cluster feeding visits into meals and label each visit
#'
#' @description
#' One-stop function to cluster feeding visits into meals and label each visit with its meal information.
#' This function first calls [cluster_meals()] to identify meals, then merges the meal information back to 
#' the original visit data using [merge_cluster_results()].
#'
#' @param data Feeding visit data (dataframe or list of dataframes)
#' @inheritParams cluster_meals
#' @inheritParams merge_cluster_results
#'
#' @return Same structure as input data (dataframe or list of dataframes) with additional columns:
#'   - meal_id: Sequential meal number within animal-day (0 for outliers)
#'   - meal_start: Start time of the meal this visit belongs to (NA for outliers)
#'   - meal_end: End time of the meal this visit belongs to (NA for outliers)
#'   - meal_duration: Total duration of the meal this visit belongs to (NA for outliers)
#'   - total_intake: Total intake of the meal this visit belongs to (NA for outliers)
#'   - visit_count: Number of visits in the meal this visit belongs to (NA for outliers)
#'
#' @details
#' This function is a convenience wrapper for [cluster_meals()] and [merge_cluster_results()].
#' It clusters feeding visits into meals and then labels each visit with its meal assignment 
#' and summary statistics.
#'
#' @examples
#' # Create a toy dataset
#' toy_data <- all_fed[[1]][which(all_fed[[1]]$cow == 5114),]
#' 
#' # Cluster and label meals
#' labeled <- meal_label_visits(toy_data, id_col = 'cow', start_col = 'start', 
#' end_col = 'end', bin_col = 'bin', intake_col = 'intake', dur_col = 'duration',
#' tz = 'America/Vancouver')
#' head(labeled)
#'
#' @export
meal_label_visits <- function(data,
                              eps = NULL,
                              min_pts = 2,
                              method = "gmm",
                              percentile = 0.93,
                              eps_scope = "all_animals",
                              lower_bound = 5,
                              upper_bound = 60,
                              use_log_transform = TRUE,
                              log_multiplier = 20,
                              log_offset = 1,
                              id_col = id_col2(),
                              start_col = start_col2(),
                              end_col = end_col2(),
                              bin_col = bin_col2(),
                              intake_col = intake_col2(),
                              dur_col = duration_col2(),
                              tz = tz2()) {
  meal_results <- cluster_meals(
    data = data,
    eps = eps,
    min_pts = min_pts,
    method = method,
    percentile = percentile,
    eps_scope = eps_scope,
    lower_bound = lower_bound,
    upper_bound = upper_bound,
    use_log_transform = use_log_transform,
    log_multiplier = log_multiplier,
    log_offset = log_offset,
    id_col = id_col,
    start_col = start_col,
    end_col = end_col,
    bin_col = bin_col,
    intake_col = intake_col,
    dur_col = dur_col,
    tz = tz
  )
  merge_cluster_results(
    visit_data = data,
    meal_results = meal_results,
    id_col = id_col,
    start_col = start_col,
    end_col = end_col,
    tz = tz
  )
} 