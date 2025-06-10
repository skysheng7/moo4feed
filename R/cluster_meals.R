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
    df$date <- lubridate::date(df[[start_col]])
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
    return(create_empty_meal_df(id_col))
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
    eps <- determine_optimal_eps(minutes_from_midnight, end_minutes_from_midnight)
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

#' Determine optimal eps parameter for DBSCAN clustering
#'
#' @param start_times Numeric vector of visit start times (minutes from midnight)
#' @param end_times Numeric vector of visit end times (minutes from midnight)
#'
#' @return Optimal eps value in minutes
#' @keywords internal
#' @noRd
determine_optimal_eps <- function(start_times, end_times) {
  
  if (length(start_times) <= 1) {
    return(30) # Default fallback
  }
  
  # Calculate inter-visit gaps (from end of previous visit to start of current visit)
  if (length(start_times) != length(end_times)) {
    stop("start_times and end_times must have the same length")
  }
  
  # Sort by start times and get corresponding end times
  sorted_indices <- order(start_times)
  sorted_start_times <- start_times[sorted_indices]
  sorted_end_times <- end_times[sorted_indices]
  
  # Calculate gaps between end of previous visit and start of current visit
  gaps <- numeric(length(sorted_start_times) - 1)
  for (i in 2:length(sorted_start_times)) {
    gaps[i - 1] <- sorted_start_times[i] - sorted_end_times[i - 1]
  }
  
  # Remove negative gaps (overlapping visits)
  gaps <- gaps[gaps >= 0]
  
  if (length(gaps) == 0) {
    return(30) # Default fallback
  }
  
  # Method 1: 75th percentile of gaps
  percentile_eps <- determine_eps_percentile(gaps)
  
  # Method 2: Gaussian mixture modeling
  gmm_eps <- determine_eps_gmm(gaps)
  
  # Take the minimum of the two methods to be conservative
  optimal_eps <- min(percentile_eps, gmm_eps, na.rm = TRUE)
  
  # Apply reasonable bounds
  optimal_eps <- max(5, min(optimal_eps, 60)) # Between 5 and 60 minutes
  
  return(as.numeric(optimal_eps))
}

#' Determine eps using percentile-based method
#'
#' @param gaps Numeric vector of inter-visit gaps
#'
#' @return Eps value based on 75th percentile of gaps
#' @keywords internal
#' @noRd
determine_eps_percentile <- function(gaps) {
  if (length(gaps) == 0) {
    return(30)
  }
  
  # Use 75th percentile of gaps
  percentile_eps <- stats::quantile(gaps, 0.75, na.rm = TRUE)
  return(as.numeric(percentile_eps))
}

#' Determine eps using Gaussian mixture modeling
#'
#' @param gaps Numeric vector of inter-visit gaps
#'
#' @return Eps value based on intersection of within-meal and between-meal distributions
#' @keywords internal
#' @noRd
determine_eps_gmm <- function(gaps) {
  
  if (length(gaps) < 10) {
    # Not enough data for GMM, fall back to percentile
    return(determine_eps_percentile(gaps))
  }
  
  # Try to fit 2-component Gaussian mixture model
  tryCatch({
    # Fit 2-component normal mixture
    mix_fit <- mixtools::normalmixEM(gaps, k = 2, verb = FALSE, 
                                    maxit = 1000, epsilon = 1e-08)
    
    # Extract parameters for the two components
    mu1 <- mix_fit$mu[1]
    mu2 <- mix_fit$mu[2]
    sigma1 <- mix_fit$sigma[1]
    sigma2 <- mix_fit$sigma[2]
    lambda1 <- mix_fit$lambda[1]
    lambda2 <- mix_fit$lambda[2]
    
    # Ensure component 1 is the within-meal (smaller mean) distribution
    if (mu1 > mu2) {
    # Swap components
    temp_mu <- mu1; mu1 <- mu2; mu2 <- temp_mu
    temp_sigma <- sigma1; sigma1 <- sigma2; sigma2 <- temp_sigma
    temp_lambda <- lambda1; lambda1 <- lambda2; lambda2 <- temp_lambda
    }
    
    # Find intersection point between the two distributions
    intersection_eps <- find_distribution_intersection(mu1, sigma1, lambda1, 
                                                    mu2, sigma2, lambda2)
    
    return(intersection_eps)
  }, error = function(e) {
    # If GMM fails, fall back to percentile method
    return(determine_eps_percentile(gaps))
  })
}

#' Find intersection point between two normal distributions in a mixture
#'
#' @param mu1 Mean of first distribution (within-meal)
#' @param sigma1 Standard deviation of first distribution
#' @param lambda1 Mixing proportion of first distribution
#' @param mu2 Mean of second distribution (between-meal)
#' @param sigma2 Standard deviation of second distribution  
#' @param lambda2 Mixing proportion of second distribution
#'
#' @return Intersection point between the two weighted distributions
#' @keywords internal
#' @noRd
find_distribution_intersection <- function(mu1, sigma1, lambda1, mu2, sigma2, lambda2) {
  
  # Define the difference function between weighted densities
  diff_func <- function(x) {
    density1 <- lambda1 * stats::dnorm(x, mean = mu1, sd = sigma1)
    density2 <- lambda2 * stats::dnorm(x, mean = mu2, sd = sigma2)
    return(density1 - density2)
  }
  
  # Find intersection point between the distributions
  # Search in the range between the two means
  search_range <- c(mu1, mu2)
  
  tryCatch({
    # Use uniroot to find where the difference is zero
    intersection <- stats::uniroot(diff_func, interval = search_range, 
                                  extendInt = "yes", tol = 1e-6)
    return(intersection$root)
  }, error = function(e) {
    # If uniroot fails, use a conservative estimate
    # Use the point closer to the within-meal distribution
    return(mu1 + 0.5 * (mu2 - mu1))
  })
} 