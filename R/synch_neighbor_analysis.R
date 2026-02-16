#' Analyze pair-wise spatial neighbor patterns
#'
#' @description
#' Analyzes how long each pair of animals spend at neighboring bins (spatial
#' proximity). Uses bin layout configuration to determine which bins are
#' spatial neighbors. For each pair of animals, calculates the number of bouts
#' they spent as neighbors, total time as neighbors, and average duration per bout.
#'
#' @param matrix_data Output from [matrix_process()], either a single day's matrices
#'   or a list of matrices for multiple days. Must contain `synch_master_bin2`
#'   component with Time column and animal ID columns (values are bin numbers).
#' @param bin_layout Character string representing spatial layout of bins,
#'   with rows separated by `\n` and bins within rows separated by "-".
#'   Example: `1-2-3\n4-5-6` represents first row has bin 1, 2, 3, second row has bin 4, 5, 6. 
#'   Default uses [bin_layout2()].
#' @param type Character, one of "feed", "drink", "feed_and_drink" to specify
#'   the type of activity being analyzed
#' @param resolution Character, either "sec" (default) or "min" for time resolution.
#'   Affects bout detection threshold and time calculations.
#' @inheritParams set_global_cols
#' @return List with three elements, each containing animal × animal matrices
#'   (one per day if multi-day input, or single matrix if single day input).
#'   Only upper triangle of matrices is filled:
#'   \itemize{
#'     \item \strong{bout}: Number of distinct bouts each pair spent as neighbors
#'     \item \strong{total_time}: Total time (seconds or minutes) each pair spent as neighbors
#'     \item \strong{avg_duration}: Average duration per bout for each pair
#'   }
#'   Matrix rows and columns are labeled with animal IDs. Pairs that were never
#'   neighbors have value 0.
#' 
#' @examples
#' # Create toy data with animals at different bins
#' toy_data <- data.frame(
#'   animal = c(1, 2, 3, 1, 2),
#'   start = lubridate::ymd_hms(c(
#'     "2023-01-01 10:00:00", "2023-01-01 10:00:01",
#'     "2023-01-01 10:00:05", "2023-01-01 10:00:10",
#'     "2023-01-01 10:00:11"
#'   )),
#'   end = lubridate::ymd_hms(c(
#'     "2023-01-01 10:00:03", "2023-01-01 10:00:04",
#'     "2023-01-01 10:00:08", "2023-01-01 10:00:13",
#'     "2023-01-01 10:00:14"
#'   )),
#'   bin = c(1, 2, 3, 1, 2),
#'   start_weight = c(10.5, 8.3, 9.1, 10.2, 8.0),
#'   end_weight = c(10.2, 8.1, 8.9, 9.9, 7.8)
#' )
#' 
#' # Process matrices
#' matrices <- matrix_process(toy_data, type = "feed",
#'                           id_col = "animal", start_col = "start",
#'                           end_col = "end", bin_col = "bin",
#'                           start_weight_col = "start_weight",
#'                           end_weight_col = "end_weight",
#'                           bins_feed = 1:3)
#' 
#' # Analyze neighbor patterns with linear layout
#' neighbor_results <- synch_neighbor_analysis(
#'   matrices, 
#'   bin_layout = "1-2-3",
#'   type = "feed",
#'   id_col = "animal"
#' )
#' 
#' 
#' @export
synch_neighbor_analysis <- function(matrix_data,
                                   bin_layout = bin_layout2(),
                                   type = c("feed", "drink", "feed_and_drink"),
                                   resolution = c("sec", "min"),
                                   id_col = id_col2()) {
  
  # Validate inputs
  type <- match.arg(trimws(tolower(type)), c("feed", "drink", "feed_and_drink"))
  resolution <- match.arg(trimws(tolower(resolution)), c("sec", "min"))
  
  if (is.null(matrix_data) || length(matrix_data) == 0) {
    stop("`matrix_data` cannot be NULL or empty", call. = FALSE)
  }
  
  # Check if matrix_data has the required component
  if (!"synch_master_bin2" %in% names(matrix_data)) {
    stop("`matrix_data` must contain 'synch_master_bin2' component from matrix_process()",
         call. = FALSE)
  }
  
  # Parse bin layout once (reuse for all days)
  neighbor_lookup <- parse_bin_layout(bin_layout)
  
  # Determine if single day or multi-day input
  bin_matrices <- matrix_data$synch_master_bin2
  
  # Handle single day (bin_matrices is a data frame, not list)
  single_day_input <- is.data.frame(bin_matrices)
  if (single_day_input) {
    bin_matrices <- list(day1 = bin_matrices)
  }
  
  # Validate that we have list of data frames
  if (!is.list(bin_matrices)) {
    stop("`synch_master_bin2` must be a data frame or list of data frames", call. = FALSE)
  }
  
  # Process each day
  bout_list <- list()
  total_time_list <- list()
  avg_duration_list <- list()
  
  for (i in seq_along(bin_matrices)) {
    day_name <- names(bin_matrices)[i]
    
    # Process this day's data
    day_results <- process_all_neighbors_one_day(
      bin_matrix = bin_matrices[[i]],
      neighbor_lookup = neighbor_lookup,
      resolution = resolution,
      id_col = id_col
    )
    
    bout_list[[i]] <- day_results$bout
    total_time_list[[i]] <- day_results$total_time
    avg_duration_list[[i]] <- day_results$avg_duration
    
    # Preserve names
    names(bout_list)[i] <- day_name
    names(total_time_list)[i] <- day_name
    names(avg_duration_list)[i] <- day_name
  }
  
  # If single day input, return single matrices instead of lists
  if (single_day_input) {
    return(list(
      bout = bout_list[[1]],
      total_time = total_time_list[[1]],
      avg_duration = avg_duration_list[[1]]
    ))
  } else {
    return(list(
      bout = bout_list,
      total_time = total_time_list,
      avg_duration = avg_duration_list
    ))
  }
}

#' Process all animal pairs for one day (neighbor analysis)
#'
#' @description
#' Internal function to process all unique animal pairs for a single day,
#' calculating bout, total time, and average duration for each pair's
#' time spent at neighboring bins.
#'
#' @param bin_matrix Data frame with Time column and animal ID columns
#'   (values are bin numbers the animals are using)
#' @param neighbor_lookup Named list from [parse_bin_layout()]
#' @param resolution Character, time resolution
#' @param id_col Character, column name for animal ID (used for validation)
#' @return List with three animal × animal matrices: bout, total_time, avg_duration
#' 
#' @keywords internal
#' @noRd
process_all_neighbors_one_day <- function(bin_matrix, neighbor_lookup, resolution, id_col) {
  
  # Validate input
  if (!is.data.frame(bin_matrix)) {
    stop("bin_matrix must be a data frame", call. = FALSE)
  }
  
  if (!"Time" %in% names(bin_matrix)) {
    stop("bin_matrix must have 'Time' column", call. = FALSE)
  }
  
  # Extract animal IDs from column names (exclude Time and date)
  exclude_cols <- c("Time", "date")
  animal_cols <- setdiff(names(bin_matrix), exclude_cols)
  
  if (length(animal_cols) == 0) {
    stop("No animal columns found in bin_matrix", call. = FALSE)
  }
  
  # Create empty matrices
  bout_matrix <- create_empty_pair_matrix(animal_cols)
  total_time_matrix <- bout_matrix
  avg_duration_matrix <- bout_matrix
  
  # Generate all unique pairs (upper triangle only)
  if (length(animal_cols) < 2) {
    # Only one animal, return empty matrices
    return(list(
      bout = bout_matrix,
      total_time = total_time_matrix,
      avg_duration = avg_duration_matrix
    ))
  }
  
  pairs <- utils::combn(animal_cols, 2, simplify = FALSE)
  
  # Process each pair
  for (pair in pairs) {
    animal1 <- pair[1]
    animal2 <- pair[2]
    
    # Extract time points when both animals are at neighboring bins
    time_vector <- extract_neighbor_activity(bin_matrix, animal1, animal2, neighbor_lookup)
    
    # Calculate bout statistics
    stats <- calculate_bout_duration(time_vector, resolution)
    
    # Fill upper triangle only
    bout_matrix[animal1, animal2] <- stats$bout
    total_time_matrix[animal1, animal2] <- stats$total_time
    avg_duration_matrix[animal1, animal2] <- stats$avg_duration
  }
  
  return(list(
    bout = bout_matrix,
    total_time = total_time_matrix,
    avg_duration = avg_duration_matrix
  ))
}

#' Extract time points when both animals are at neighboring bins
#'
#' @description
#' Internal function to extract timestamps when both specified animals
#' are active at spatially neighboring bins based on bin layout.
#'
#' @param bin_matrix Data frame with Time column and animal ID columns
#'   (values are bin numbers)
#' @param animal1 Character or numeric, first animal ID (column name)
#' @param animal2 Character or numeric, second animal ID (column name)
#' @param neighbor_lookup Named list from [parse_bin_layout()]
#' @return POSIXct vector of timestamps when both animals are at neighboring bins
#' 
#' @keywords internal
#' @noRd
extract_neighbor_activity <- function(bin_matrix, animal1, animal2, neighbor_lookup) {
  
  # Convert to character for column name matching
  animal1 <- as.character(animal1)
  animal2 <- as.character(animal2)
  
  # Validate columns exist
  if (!animal1 %in% names(bin_matrix)) {
    stop("Animal ", animal1, " not found in matrix", call. = FALSE)
  }
  if (!animal2 %in% names(bin_matrix)) {
    stop("Animal ", animal2, " not found in matrix", call. = FALSE)
  }
  
  # Get bin values for each animal
  bin1_values <- bin_matrix[[animal1]]
  bin2_values <- bin_matrix[[animal2]]
  
  # Filter rows where both animals are active (not 0)
  both_active <- (bin1_values != 0) & (bin2_values != 0)
  
  # Among active rows, check which ones have neighboring bins
  active_indices <- which(both_active)
  
  if (length(active_indices) == 0) {
    return(bin_matrix$Time[0])  # Return empty POSIXct vector
  }
  
  # Check neighbor relationship for each active time point
  neighbor_indices <- sapply(active_indices, function(idx) {
    is_neighbour(bin1_values[idx], bin2_values[idx], neighbor_lookup)
  })
  
  # Get time points where they are neighbors
  neighbor_times <- bin_matrix$Time[active_indices[neighbor_indices]]
  
  return(neighbor_times)
}
