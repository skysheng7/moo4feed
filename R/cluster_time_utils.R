#' Convert datetime vectors to minutes from midnight
#'
#' Converts datetime vectors to minutes from midnight for clustering analysis.
#'
#' @param datetime_vec Vector of datetime objects
#' @param reference_date Date to use as reference for midnight calculation.
#'   If NULL, uses the date of the first non-NA datetime in the vector.
#' @param tz Timezone to use for the datetime objects. Default is [tz2()].
#'
#' @return Numeric vector of minutes from midnight
#' @keywords internal
#' @noRd
convert_times_to_minutes <- function(datetime_vec, reference_date = NULL, tz = tz2()) {
  # Handle empty input
  if (length(datetime_vec) == 0) {
    return(numeric(0))
  }
  
  # Convert to datetime and handle conversion errors
  tryCatch({
    datetime_vec <- lubridate::as_datetime(datetime_vec, tz = tz)

    if (all(is.na(datetime_vec))) {
      stop("Failed to convert datetime_vec to datetime objects, returned all NA values")
    }
  }, error = function(e) {
    stop("Failed to convert datetime_vec to datetime objects: ", e$message)
  })
  
  # Determine reference midnight
  if (is.null(reference_date)) {
    # Find first non-NA datetime for reference
    first_valid_datetime <- datetime_vec[!is.na(datetime_vec)][1]
    
    if (is.na(first_valid_datetime)) {
      stop("All datetime values are NA and no reference_date provided")
    }
    
    midnight <- lubridate::floor_date(first_valid_datetime, "day")
  } else {
    # Validate and use provided reference_date
    tryCatch({
      if (inherits(reference_date, "Date")) {
        midnight <- lubridate::as_datetime(paste0(reference_date, " 00:00:00"), tz = tz)
      } else {
        # Try to parse as date string
        ref_date <- lubridate::as_date(reference_date, tz = tz)
        midnight <- lubridate::as_datetime(paste0(ref_date, " 00:00:00"), tz = tz)

        if (is.na(midnight)) {
          stop("Invalid reference_date provided, returned NA, unale to proceed")
        }
      }
    }, error = function(e) {
      stop("Invalid reference_date provided: ", e$message)
    })
  }
  
  # Ensure timezone consistency
  if (!is.na(midnight) && length(datetime_vec[!is.na(datetime_vec)]) > 0) {
    # Use timezone from the datetime_vec if available
    first_tz <- lubridate::tz(datetime_vec[!is.na(datetime_vec)][1])
    midnight <- lubridate::with_tz(midnight, first_tz)
  }
  
  # Calculate minutes from midnight
  tryCatch({
    minutes_from_midnight <- as.numeric(lubridate::interval(midnight, datetime_vec) / lubridate::minutes(1))
    return(minutes_from_midnight)
  }, error = function(e) {
    stop("Failed to calculate minutes from midnight: ", e$message)
  })
} 