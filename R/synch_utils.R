#' Create empty pair-wise matrix for synchronicity analysis
#'
#' @description
#' Creates an empty symmetric animal × animal matrix initialized to 0,
#' with animal IDs as both row and column names.
#'
#' @param animal_ids Character or numeric vector of animal IDs
#' @return Matrix with animal IDs as row/column names, initialized to 0
#' 
#' @keywords internal
#' @noRd
create_empty_pair_matrix <- function(animal_ids) {
  if (length(animal_ids) == 0) {
    stop("animal_ids cannot be empty", call. = FALSE)
  }
  
  n <- length(animal_ids)
  empty_matrix <- matrix(0, nrow = n, ncol = n)
  colnames(empty_matrix) <- animal_ids
  rownames(empty_matrix) <- animal_ids
  
  return(empty_matrix)
}

#' Calculate bout count, total time, and average duration from time vector
#'
#' @description
#' Vectorized calculation of bout statistics from a sequence of timestamps.
#' A new bout starts when the time gap between consecutive timestamps exceeds
#' the threshold (1 second for "sec", 60 seconds for "min").
#'
#' @param time_vector POSIXct vector of timestamps when activity occurred
#' @param resolution Character, either "sec" or "min" for time resolution
#' @return List with three elements:
#'   \itemize{
#'     \item \code{bout}: Integer, number of distinct bouts
#'     \item \code{total_time}: Numeric, total time in seconds or minutes
#'     \item \code{avg_duration}: Numeric, average duration per bout
#'   }
#' 
#' @keywords internal
#' @noRd
calculate_bout_duration <- function(time_vector, resolution = c("sec", "min")) {
  # Validate resolution
  resolution <- match.arg(trimws(tolower(resolution)), c("sec", "min"))
  
  # Handle empty time vector
  if (length(time_vector) == 0) {
    return(list(bout = 0, total_time = 0, avg_duration = 0))
  }
  
  # Ensure POSIXct
  if (!lubridate::is.POSIXct(time_vector)) {
    stop("time_vector must be POSIXct", call. = FALSE)
  }
  
  # Sort time vector
  time_vector <- sort(time_vector)
  
  # Single time point = 1 bout, duration depends on resolution
  if (length(time_vector) == 1) {
    time_unit <- if (resolution == "sec") 1 else 1/60
    return(list(bout = 1, total_time = time_unit, avg_duration = time_unit))
  }
  
  # Calculate time gaps in seconds
  time_gaps <- as.numeric(diff(time_vector), units = "secs")
  
  # Threshold for new bout
  threshold <- if (resolution == "sec") 1 else 60
  
  # Identify bout boundaries (gap > threshold means new bout)
  new_bout <- c(TRUE, time_gaps > threshold)
  
  # Assign bout IDs
  bout_ids <- cumsum(new_bout)
  
  # Count bouts
  n_bouts <- max(bout_ids)
  
  # Calculate total time
  # For "sec": count of time points
  # For "min": count of time points (each represents 1 minute)
  total_time <- length(time_vector)
  
  # Calculate average duration
  avg_duration <- total_time / n_bouts
  
  return(list(
    bout = n_bouts,
    total_time = total_time,
    avg_duration = avg_duration
  ))
}

#' Parse bin layout string into neighbor lookup structure
#'
#' @description
#' Parses a bin layout string (e.g., "1-2-3\n4-5-6") into a data structure
#' that allows fast lookup of spatial neighbors for any bin. Bins are only
#' neighbors if they are adjacent within the same row (left/right). Bins in
#' different rows are never neighbors, even if rows have different lengths.
#'
#' @param bin_layout Character string representing bin layout, with rows
#'   separated by "\n" and bins within rows separated by "-"
#' @return Named list where each element is a bin number (as character) and
#'   the value is a numeric vector of neighboring bin numbers (within same row)
#' 
#' @keywords internal
#' @noRd
parse_bin_layout <- function(bin_layout) {
  if (is.null(bin_layout) || length(bin_layout) == 0 || bin_layout == "") {
    stop("bin_layout cannot be NULL or empty", call. = FALSE)
  }
  
  # Split by newlines to get rows
  rows <- strsplit(bin_layout, "\n")[[1]]
  rows <- trimws(rows)
  rows <- rows[rows != ""]  # Remove empty rows
  
  if (length(rows) == 0) {
    stop("bin_layout must contain at least one row", call. = FALSE)
  }
  
  # Parse each row to get bin numbers
  bin_matrix <- lapply(rows, function(row) {
    bins <- strsplit(row, "-")[[1]]
    bins <- trimws(bins)
    as.numeric(bins[bins != ""])
  })
  
  # Create neighbor lookup list
  # Only consider left and right neighbors within the same row
  # Bins in different rows are physically separate and never neighbors
  neighbor_lookup <- list()
  
  n_rows <- length(bin_matrix)
  for (i in seq_len(n_rows)) {
    n_cols <- length(bin_matrix[[i]])
    
    for (j in seq_len(n_cols)) {
      bin_num <- bin_matrix[[i]][j]
      
      if (is.na(bin_num)) next
      
      neighbors <- numeric(0)
      
      # Left neighbor (same row)
      if (j > 1 && !is.na(bin_matrix[[i]][j - 1])) {
        neighbors <- c(neighbors, bin_matrix[[i]][j - 1])
      }
      
      # Right neighbor (same row)
      if (j < n_cols && !is.na(bin_matrix[[i]][j + 1])) {
        neighbors <- c(neighbors, bin_matrix[[i]][j + 1])
      }
      
      neighbor_lookup[[as.character(bin_num)]] <- unique(neighbors)
    }
  }
  
  return(neighbor_lookup)
}

#' Check if two bins are spatial neighbors
#'
#' @description
#' Checks whether two bins are spatial neighbors based on the parsed
#' bin layout structure. Uses pre-computed neighbor lookup for fast checking.
#'
#' @param bin1 Numeric, first bin number
#' @param bin2 Numeric, second bin number
#' @param neighbor_lookup Named list from [parse_bin_layout()]
#' @return Logical, TRUE if bins are neighbors, FALSE otherwise
#' 
#' @keywords internal
#' @noRd
is_neighbour <- function(bin1, bin2, neighbor_lookup) {
  # Handle edge cases
  if (is.na(bin1) || is.na(bin2)) return(FALSE)
  if (bin1 == bin2) return(FALSE)
  
  # Check if bins exist in lookup
  bin1_char <- as.character(bin1)
  if (!bin1_char %in% names(neighbor_lookup)) return(FALSE)
  
  # Check if bin2 is in bin1's neighbor list
  neighbors <- neighbor_lookup[[bin1_char]]
  return(bin2 %in% neighbors)
}
