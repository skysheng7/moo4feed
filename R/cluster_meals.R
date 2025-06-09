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
#'   \item{cow}{Animal ID}
#'   \item{date}{Date}
#'   \item{meal_id}{Sequential meal number within cow-day}
#'   \item{meal_start}{Start time of first visit in meal}
#'   \item{meal_end}{End time of last visit in meal}
#'   \item{meal_duration}{Total time from meal start to end (seconds)}
#'   \item{visit_count}{Number of visits in meal}
#'   \item{total_intake}{Sum of intake across all visits in meal}
#'   \item{feeding_duration}{Sum of visit durations in meal (active feeding time)}
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
#' - 80th percentile of inter-visit gaps
#' - Knee/elbow method on gap distributions
#' - Statistical analysis of feeding patterns
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

#' Process meal clustering for a single dataframe
#'
#' @param df Single dataframe with feeding visit data
#' @inheritParams cluster_meals
#'
#' @return Dataframe with meal summaries
#' @keywords internal
#' @noRd
cluster_meals_single_df <- function(df, eps, min_pts, id_col, start_col, 
                                   end_col, bin_col, intake_col, dur_col) {
  
  # Validate required columns
  required_cols <- c(id_col, start_col, end_col, bin_col, intake_col, dur_col)
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Add date column if not present
  if (!"date" %in% names(df)) {
    df$date <- as.Date(df[[start_col]])
  }
  
  # Group by cow and date, then process each group
  results <- df |>
    dplyr::group_by(.data[[id_col]], date) |>
    dplyr::group_split() |>
    lapply(function(group_df) {
      cluster_meals_cow_day(group_df, eps, min_pts, id_col, start_col, 
                           end_col, bin_col, intake_col, dur_col)
    })
  
  # Combine results
  if (length(results) > 0) {
    result <- do.call(rbind, results)
    rownames(result) <- NULL
    return(result)
  } else {
    # Return empty dataframe with correct structure
    return(data.frame(
      cow = character(0),
      date = as.Date(character(0)),
      meal_id = integer(0),
      meal_start = as.POSIXct(character(0)),
      meal_end = as.POSIXct(character(0)),
      meal_duration = numeric(0),
      visit_count = integer(0),
      total_intake = numeric(0),
      feeding_duration = numeric(0),
      unique_bins_count = integer(0),
      stringsAsFactors = FALSE
    ))
  }
}

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
    return(data.frame(
      cow = character(0),
      date = as.Date(character(0)),
      meal_id = integer(0),
      meal_start = as.POSIXct(character(0)),
      meal_end = as.POSIXct(character(0)),
      meal_duration = numeric(0),
      visit_count = integer(0),
      total_intake = numeric(0),
      feeding_duration = numeric(0),
      unique_bins_count = integer(0),
      stringsAsFactors = FALSE
    ))
  }
  
  # Sort by start time
  cow_day_df <- cow_day_df[order(cow_day_df[[start_col]]), ]
  
  # Convert start times to minutes from midnight
  start_times <- as.POSIXct(cow_day_df[[start_col]])
  midnight <- as.POSIXct(paste(as.Date(start_times[1]), "00:00:00"))
  minutes_from_midnight <- as.numeric(difftime(start_times, midnight, units = "mins"))
  
  # Auto-determine eps if not provided
  if (is.null(eps)) {
    eps <- determine_optimal_eps(minutes_from_midnight)
  }
  
  # Apply DBSCAN clustering
  if (!requireNamespace("dbscan", quietly = TRUE)) {
    stop("Package 'dbscan' is required but not installed. Please install it with: install.packages('dbscan')")
  }
  
  # DBSCAN expects a matrix
  time_matrix <- matrix(minutes_from_midnight, ncol = 1)
  clusters <- dbscan::dbscan(time_matrix, eps = eps, minPts = min_pts)
  
  # Filter out noise points (cluster = 0)
  valid_clusters <- clusters$cluster[clusters$cluster > 0]
  if (length(valid_clusters) == 0) {
    # All points are noise
    return(data.frame(
      cow = character(0),
      date = as.Date(character(0)),
      meal_id = integer(0),
      meal_start = as.POSIXct(character(0)),
      meal_end = as.POSIXct(character(0)),
      meal_duration = numeric(0),
      visit_count = integer(0),
      total_intake = numeric(0),
      feeding_duration = numeric(0),
      unique_bins_count = integer(0),
      stringsAsFactors = FALSE
    ))
  }
  
  # Add cluster assignments to dataframe
  cow_day_df$cluster <- clusters$cluster
  
  # Filter to only non-noise clusters and calculate meal statistics
  meal_data <- cow_day_df[cow_day_df$cluster > 0, ]
  
  meal_summaries <- meal_data |>
    dplyr::group_by(cluster) |>
    dplyr::summarise(
      cow = first(.data[[id_col]]),
      date = first(date),
      meal_id = first(cluster),
      meal_start = min(.data[[start_col]]),
      meal_end = max(.data[[end_col]]),
      meal_duration = as.numeric(difftime(max(.data[[end_col]]), min(.data[[start_col]]), units = "secs")),
      visit_count = dplyr::n(),
      total_intake = sum(.data[[intake_col]], na.rm = TRUE),
      feeding_duration = sum(.data[[dur_col]], na.rm = TRUE),
      unique_bins_count = length(unique(.data[[bin_col]])),
      .groups = "drop"
    )
  
  # Reorder meal_id to be sequential within cow-day
  meal_summaries <- meal_summaries[order(meal_summaries$meal_start), ]
  meal_summaries$meal_id <- seq_len(nrow(meal_summaries))
  
  return(as.data.frame(meal_summaries))
}

#' Determine optimal eps parameter for DBSCAN clustering
#'
#' @param time_points Numeric vector of time points (minutes from midnight)
#'
#' @return Optimal eps value in minutes
#' @keywords internal
#' @noRd
determine_optimal_eps <- function(time_points) {
  
  if (length(time_points) <= 1) {
    return(30) # Default fallback
  }
  
  # Calculate inter-visit gaps
  sorted_times <- sort(time_points)
  gaps <- diff(sorted_times)
  
  if (length(gaps) == 0) {
    return(30) # Default fallback
  }
  
  # Method 1: 80th percentile of gaps
  percentile_eps <- stats::quantile(gaps, 0.8, na.rm = TRUE)
  
  # Method 2: Knee/elbow method using gap distribution
  if (length(gaps) >= 3) {
    # Sort gaps and look for the "elbow" point
    sorted_gaps <- sort(gaps)
    n_gaps <- length(sorted_gaps)
    
    # Calculate second differences to find inflection point
    if (n_gaps >= 5) {
      # Use second derivative approach
      first_diff <- diff(sorted_gaps)
      second_diff <- diff(first_diff)
      
      if (length(second_diff) > 0) {
        # Find the point where second derivative is maximum (steepest increase)
        elbow_idx <- which.max(second_diff) + 2 # Adjust for diff operations
        elbow_eps <- sorted_gaps[min(elbow_idx, length(sorted_gaps))]
      } else {
        elbow_eps <- percentile_eps
      }
    } else {
      elbow_eps <- percentile_eps
    }
  } else {
    elbow_eps <- percentile_eps
  }
  
  # Take the minimum of the two methods to be conservative
  optimal_eps <- min(percentile_eps, elbow_eps, na.rm = TRUE)
  
  # Apply reasonable bounds
  optimal_eps <- max(5, min(optimal_eps, 120)) # Between 5 and 120 minutes
  
  return(as.numeric(optimal_eps))
} 