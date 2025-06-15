#' Calculate non-nutritive visits
#'
#' This function calculates the number of non-nutritive visits for each date.
#' A non-nutritive visit occurs when a cow visits a bin that has feed available
#' (more than the calibration error) but doesn't eat anything.
#'
#' @param data A list of data frames, where each element represents a day's data.
#'   Each data frame should contain columns for cow ID, intake, and start weight.
#' @param calibration_error Numeric. The threshold for considering a weight reading
#'   as significant (default: 0.5 kg).
#'
#' @return A list of data frames, where each element contains the count of
#'   non-nutritive visits per cow for that day.
#'
#' @examples
#' # Example data
#' data <- list(
#'   "2023-01-01" = data.frame(
#'     Cow = c("1", "2", "3"),
#'     Intake = c(0.2, 0.3, 0.4),
#'     Startweight = c(10, 15, 20)
#'   )
#' )
#' result <- calculate_non_nutritive_visits(data, calibration_error = 0.5)
#'
#' @export
calculate_non_nutritive_visits <- function(data, calibration_error = 0.5) {
  # Input validation
  if (!is.list(data)) {
    stop("`data` must be a list of data frames")
  }
  if (length(data) == 0) {
    stop("`data` list is empty")
  }
  
  # Process each day's data
  result <- list()
  for (date in names(data)) {
    daily_data <- data[[date]]
    
    # Filter for non-nutritive visits
    non_nutritive <- daily_data[
      daily_data$Intake <= calibration_error & 
      daily_data$Startweight > calibration_error,
    ]
    
    # Count visits per cow
    visit_counts <- stats::aggregate(
      list(number_of_non_nutritive_visits = rep(1, nrow(non_nutritive))),
      by = list(Cow = non_nutritive$Cow),
      FUN = sum
    )
    
    result[[date]] <- visit_counts
  }
  
  return(result)
}

#' Calculate visits with no feed available
#'
#' This function calculates the number of visits when there was no feed available
#' for each date. This occurs when a cow visits a bin that has less than the
#' calibration error amount of feed left and doesn't eat anything.
#'
#' @param data A list of data frames, where each element represents a day's data.
#'   Each data frame should contain columns for cow ID, intake, and start weight.
#' @param calibration_error Numeric. The threshold for considering a weight reading
#'   as significant (default: 0.5 kg).
#'
#' @return A list of data frames, where each element contains the count of
#'   visits with no feed available per cow for that day.
#'
#' @examples
#' # Example data
#' data <- list(
#'   "2023-01-01" = data.frame(
#'     Cow = c("1", "2", "3"),
#'     Intake = c(0.2, 0.3, 0.4),
#'     Startweight = c(0.3, 0.2, 0.1)
#'   )
#' )
#' result <- calculate_no_feed_visits(data, calibration_error = 0.5)
#'
#' @export
calculate_no_feed_visits <- function(data, calibration_error = 0.5) {
  # Input validation
  if (!is.list(data)) {
    stop("`data` must be a list of data frames")
  }
  if (length(data) == 0) {
    stop("`data` list is empty")
  }
  
  # Process each day's data
  result <- list()
  for (date in names(data)) {
    daily_data <- data[[date]]
    
    # Filter for visits with no feed
    no_feed <- daily_data[
      daily_data$Intake <= calibration_error & 
      daily_data$Startweight <= calibration_error,
    ]
    
    # Count visits per cow
    visit_counts <- stats::aggregate(
      list(number_of_visits_when_no_feed = rep(1, nrow(no_feed))),
      by = list(Cow = no_feed$Cow),
      FUN = sum
    )
    
    result[[date]] <- visit_counts
  }
  
  return(result)
}

# Internal helper functions ----------------------------------------------------

#' @keywords internal
#' @noRd
.validate_calibration_error <- function(calibration_error) {
  if (!is.numeric(calibration_error) || length(calibration_error) != 1 || 
      calibration_error <= 0 || is.na(calibration_error)) {
    stop("`calibration_error` must be a positive numeric scalar", call. = FALSE)
  }
}

#' @keywords internal
#' @noRd
.validate_daily_data <- function(daily_data) {
  required_cols <- c("Cow", "Intake", "Startweight")
  missing_cols <- setdiff(required_cols, names(daily_data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", 
         paste(missing_cols, collapse = ", "), 
         call. = FALSE)
  }
} 