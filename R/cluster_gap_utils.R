#' Calculate gaps between feeding visits
#'
#' Calculates inter-visit gaps (time from end of one visit to start of the next)
#' for temporal clustering analysis.
#'
#' @param start_times Numeric vector of visit start times (minutes from midnight)
#' @param end_times Numeric vector of visit end times (minutes from midnight)
#'
#' @return Numeric vector of gaps in minutes (negative gaps removed)
#' @keywords internal
#' @noRd
calculate_gaps <- function(start_times, end_times) {
  if (length(start_times) <= 1) {
    return(numeric(0))
  }
  
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
  
  return(gaps)
}

#' Calculate gaps between feeding visits grouped by animal and date
#'
#' Calculates inter-visit gaps properly by grouping data by animal and date first,
#' preventing gaps being calculated between visits from different animals.
#'
#' @param data Dataframe containing visit data
#' @inheritParams set_global_cols
#'
#' @return Numeric vector of all gaps from all animals (negative gaps removed)
#' @keywords internal
#' @noRd
calculate_gaps_by_animal <- function(data, id_col = id_col2(), start_col = start_col2(), end_col = end_col2(), tz = tz2()) {
  if (nrow(data) <= 1) {
    return(numeric(0))
  }
  
  # Add date column if not present
  if (!"date" %in% names(data)) {
    data$date <- lubridate::date(data[[start_col]])
  }
  
  # Check for NA values in date column
  if (any(is.na(data$date))) {
    n_na <- sum(is.na(data$date))
    warning("Found ", n_na, " NA values in date column. These rows will be removed.")
    data <- data[!is.na(data$date), ]
  }
  
  # Convert times to minutes from midnight
  data$start_minutes <- convert_times_to_minutes(data[[start_col]], tz = tz)
  data$end_minutes <- convert_times_to_minutes(data[[end_col]], tz = tz)
  
  # Check for NA values in converted minutes
  if (any(is.na(data$start_minutes)) || any(is.na(data$end_minutes))) {
    n_na_start <- sum(is.na(data$start_minutes))
    n_na_end <- sum(is.na(data$end_minutes))
    if (n_na_start > 0 || n_na_end > 0) {
      warning("Found ", n_na_start, " NA values in start_minutes and ", 
              n_na_end, " NA values in end_minutes. These rows will be removed.")
    }
    # Remove rows with NA in either start or end minutes
    valid_rows <- !is.na(data$start_minutes) & !is.na(data$end_minutes)
    data <- data[valid_rows, ]
  }
  
  if (nrow(data) <= 1) {
    warning("After removing NA values, insufficient data remains")
    return(numeric(0))
  }
  
  # Group by animal and date, calculate gaps within each group
  all_gaps <- data |>
    dplyr::group_by(.data[[id_col]], date) |>
    dplyr::arrange(.data[[id_col]], date, .data$start_minutes) |>
    dplyr::summarise(
      gaps = list(calculate_gaps(.data$start_minutes, .data$end_minutes)),
      .groups = "drop"
    ) |>
    dplyr::pull(gaps) |>
    unlist()
  
  # Filter out any remaining NA or infinite values from gaps
  if (length(all_gaps) > 0) {
    valid_gaps <- !is.na(all_gaps) & is.finite(all_gaps)
    if (any(!valid_gaps)) {
      n_invalid <- sum(!valid_gaps)
      warning("Found ", n_invalid, " invalid gap values (NA or Inf). These will be removed.")
      all_gaps <- all_gaps[valid_gaps]
    }
  }
  
  return(all_gaps)
} 