#' Calculate optimal interval between feeding visits for meal clustering
#'
#' @description
#' This function calculates the optimal interval between feeding visits for meal clustering
#' by analyzing the gaps between feeding visits for each animal within each day.
#' The function determines an appropriate time threshold that can be used to cluster
#' visits into meals based on temporal proximity. 
#' \itemize{
#'   \item If you wish to calculate optimal interval for a single animal across multiple days, 
#'   make sure data belongs to a single animal across multiple days.
#'   \item If you wish to calculate optimal interval for all animals across all days, 
#'   make sure data recorded all animals across all days. 
#'   \item If you wish to calculate optimal interval for a single animal on a single day, 
#'   make sure data belongs to a single animal and only 1 day.
#' }
#'
#' @param data A single dataframe or list of dataframes containing feeding visit data
#' @param method Character string specifying the method for eps determination.
#'   Options are "both" (default), "percentile", or "gmm".
#'   \itemize{
#'     \item "percentile": Uses the specified percentile of inter-visit gaps to determine 
#'     the optimal interval between feeding visits for meal clustering
#'     \item "gmm": Uses Gaussian mixture modeling to identify 
#'     the optimal interval between feeding visits for meal clustering
#'     \item "both": Uses both methods and returns the minimum (more conservative)
#'   }
#' @param percentile Numeric value between 0 and 1 specifying which percentile to use 
#'   for eps determination when method="percentile" or "both". Default is 0.9.
#' @param lower_bound Numeric value for lower bound of the optimal interval, if NULL, no lower bound is applied.
#' @param upper_bound Numeric value for upper bound of the optimal interval, if NULL, no upper bound is applied.
#' @param use_log_transform Logical indicating whether to use log transformation for GMM fitting. Default is TRUE. 
#'  Log transformation often provides better separation of within-meal and between-meal gaps.
#' @param log_multiplier Numeric value for multiplier of log transformation. Default is 20.
#' @param log_offset Numeric value for offset of log transformation. Default is 1.
#' @inheritParams set_global_cols
#'
#' @return A numeric value representing the optimal eps parameter in minutes.
#'   This value is bounded between lower_bound and upper_bound for practical meal clustering.
#'
#' @details
#' The function processes feeding visit data to calculate time gaps between consecutive
#' visits for each animal within each day. These gaps are then analyzed using statistical
#' methods to determine an optimal time threshold for clustering visits into meals.
#'
#' The analysis considers:
#' - Inter-visit gaps within each animal-day combination
#' - Statistical distribution of gap durations
#' - Conservative bounds (lower_bound-upper_bound) for practical application
#'
#' When method="both", the function uses both percentile and Gaussian mixture modeling
#' approaches and returns the minimum value to be more conservative in meal definitions.
#'
#' @examples
#' # Calculate optimal eps using default method (both percentile and GMM)
#' meal_interval(all_fed[[1]])
#' 
#' # Use only percentile method with 80th percentile
#' meal_interval(all_fed[[1]], method = "percentile", percentile = 0.8)
#' 
#' # Use only Gaussian mixture modeling
#' meal_interval(all_fed[[1]], method = "gmm")
#' 
#' # Work with list of dataframes
#' meal_interval(all_fed, method = "both", percentile = 0.9)
#'
#' @seealso [cluster_meals()] for using the eps parameter in meal clustering
#' @export
meal_interval <- function(data,
                                  method = "both",
                                  percentile = 0.9,
                                  lower_bound = 5,
                                  upper_bound = 60,
                                  use_log_transform = TRUE,
                                  log_multiplier = 20,
                                  log_offset = 1,
                                  id_col = id_col2(),
                                  start_col = start_col2(),
                                  end_col = end_col2(),
                                  tz = tz2()) {
  
  # Input validation
  if (is.null(data)) {
    stop("data cannot be NULL")
  }
  
  # Convert to unified format (always work with single combined dataframe)
  if (is.data.frame(data)) {
    combined_data <- data
  } else if (is.list(data)) {
    # Check if all items in the list are dataframes
    if (!all(sapply(data, is.data.frame))) {
      stop("All items in the list must be dataframes")
    }
    # Combine all dataframes
    combined_data <- do.call(rbind, data)
    rownames(combined_data) <- NULL
  } else {
    stop("data must be a dataframe or list of dataframes")
  }
  
  # Validate required columns
  required_cols <- c(id_col, start_col, end_col)
  missing_cols <- setdiff(required_cols, names(combined_data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Check if we have any data
  if (nrow(combined_data) == 0) {
    warning("No data provided, returning default eps value of 30 minutes")
    return(30)
  }
  
  # Calculate gaps between visits for each animal within each day
  gaps <- calculate_gaps_by_animal(combined_data, id_col, start_col, end_col, tz)
  
  # Determine optimal eps from gaps
  optimal_eps <- optimal_interval_from_gaps(gaps, method, percentile, lower_bound, upper_bound, use_log_transform, log_multiplier, log_offset)
  
  return(optimal_eps)
} 