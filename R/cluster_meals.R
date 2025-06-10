#' Cluster feeding visits into meals using DBSCAN
#'
#' @description
#' This function clusters individual feeding visits into meals using DBSCAN (Density-Based Spatial Clustering).
#' For each cow on each day, visits that occur close together in time are grouped into meals.
#' The function automatically determines optimal clustering parameters if not specified.
#'
#' @param data A single dataframe or list of dataframes containing feeding visit data
#' @param eps DBSCAN epsilon parameter (maximum time gap in minutes between visits in same meal).
#'   If NULL (default), the parameter is automatically determined using statistical methods.
#' @param min_pts DBSCAN minimum points parameter (minimum visits to form a dense cluster). Default is 3.
#' @inheritParams set_global_cols
#'
#' @return A dataframe with meal-level summaries containing:
#' \describe{
#'   \item{[id_col2()]}{Animal ID}
#'   \item{date}{Date}
#'   \item{meal_id}{Sequential meal number within cow-day}
#'   \item{meal_start}{Start time of first visit in meal}
#'   \item{meal_end}{End time of last visit in meal}
#'   \item{meal_duration}{Total time from meal start to end (seconds)}
#'   \item{visit_count}{Number of visits in meal}
#'   \item{total_intake}{Sum of intake across all visits in meal}
#'   \item{feeding_percentage}{Percentage of meal time spent actively feeding}
#'   \item{unique_bins_count}{Number of unique bins visited in meal}
#' }
#'
#' @details
#' The function uses DBSCAN clustering on visit start times (converted to minutes from midnight).
#' Visits are clustered based on temporal proximity, with the eps parameter determining the maximum
#' time gap between visits in the same meal. Single visits or visits classified as "noise" by DBSCAN
#' are treated as noise points and excluded from meal summaries.
#'
#' When eps=NULL, the function automatically determines the optimal parameter using:
#' - 75th percentile of inter-visit gaps
#' - Gaussian mixture modeling
#' - We will pick the minimum eps of the two methods, 
#' with a minimum of 5 minutes and a maximum of 60 minutes, to be conservative
#'
#' @examples
#' 
#' # Cluster meals with automatic parameter determination (all_fed is a list of dataframes included in the package)
#' meals <- cluster_meals(all_fed[[1]], eps = 90, min_pts = 3, id_col="cow", 
#'                        start_col="start", end_col="end", bin_col="bin", 
#'                        intake_col="intake", dur_col="duration")
#' 
#' @export
cluster_meals <- function(data,
                         eps = NULL,
                         min_pts = 3,
                         id_col = id_col2(),
                         start_col = start_col2(),
                         end_col = end_col2(),
                         bin_col = bin_col2(),
                         intake_col = intake_col2(),
                         dur_col = duration_col2()) {
  
  # Input validation
  if (is.null(data)) {
    stop("data cannot be NULL")
  }
  
  # Handle single dataframe vs list of dataframes
  if (is.data.frame(data)) {
    result <- cluster_meals_single_df(data, eps, min_pts, id_col, start_col, 
                                     end_col, bin_col, intake_col, dur_col)
  } else if (is.list(data)) {
    # check if all items in the list are dataframes
    if (!all(sapply(data, is.data.frame))) {
      stop("All items in the list must be dataframes")
    }
    
    # Process each dataframe in the list
    results <- lapply(data, function(df) {
      cluster_meals_single_df(df, eps, min_pts, id_col, start_col, 
                             end_col, bin_col, intake_col, dur_col)
    })
    
    # Combine all results
    result <- do.call(rbind, results)
    rownames(result) <- NULL
  } else {
    stop("data must be a dataframe or list of dataframes")
  }
  
  return(result)
}

#' Create empty meal summary dataframe with correct structure
#'
#' @param id_col Name of the ID column
#'
#' @return Empty dataframe with meal summary structure
#' @keywords internal
#' @noRd
create_empty_meal_df <- function(id_col) {
  empty_df <- data.frame(
    date = as.Date(character(0)),
    meal_id = integer(0),
    meal_start = lubridate::as_datetime(character(0)),
    meal_end = lubridate::as_datetime(character(0)),
    meal_duration = numeric(0),
    visit_count = integer(0),
    total_intake = numeric(0),
    feeding_percentage = numeric(0),
    unique_bins_count = integer(0),
    stringsAsFactors = FALSE
  )
  # Add the id column with the correct name
  empty_df[[id_col]] <- character(0)
  # Reorder columns to put id_col first
  empty_df <- empty_df[c(id_col, setdiff(names(empty_df), id_col))]
  return(empty_df)
}


