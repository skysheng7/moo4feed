#' Analyze pair-wise co-occurrence patterns
#'
#' @description
#' Analyzes how long each pair of animals spend feeding or drinking together
#' simultaneously, regardless of their spatial location. For each pair of animals,
#' calculates the number of bouts they spent together, total time together, and
#' average duration per bout.
#'
#' @param matrix_data Output from [matrix_process()], either a single day's matrices
#'   or a list of matrices for multiple days. Must contain `synch_master_animal2`
#'   component with Time column and animal ID columns.
#' @param type Character, one of "feed", "drink", "feed_and_drink" to specify
#'   the type of activity being analyzed
#' @param resolution Character, either "sec" (default) or "min" for time resolution.
#'   Affects bout detection threshold and time calculations.
#' @inheritParams set_global_cols
#' @return List with three elements, each containing animal × animal matrices
#'   (one per day if multi-day input, or single matrix if single day input).
#'   Only upper triangle of matrices is filled:
#'   \itemize{
#'     \item \strong{bout}: Number of distinct bouts each pair spent together
#'     \item \strong{total_time}: Total time (seconds or minutes) each pair spent together
#'     \item \strong{avg_duration}: Average duration per bout for each pair
#'   }
#'   Matrix rows and columns are labeled with animal IDs. Pairs that never
#'   interacted have value 0.
#' 
#' @examples
#' # Create toy data with multiple animals
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
#' # Analyze pair-wise co-occurrence
#' pair_results <- synch_pair_analysis(matrices, type = "feed",
#'                                    id_col = "animal")
#' 
#' 
#' @importFrom cli cli_progress_bar cli_progress_update cli_progress_done
#' @export
synch_pair_analysis <- function(matrix_data,
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
  if (!"synch_master_animal2" %in% names(matrix_data)) {
    stop("`matrix_data` must contain 'synch_master_animal2' component from matrix_process()",
         call. = FALSE)
  }
  
  # Determine if single day or multi-day input
  animal_matrices <- matrix_data$synch_master_animal2
  
  # Handle single day (animal_matrices is a data frame, not list)
  single_day_input <- is.data.frame(animal_matrices)
  if (single_day_input) {
    animal_matrices <- list(day1 = animal_matrices)
  }
  
  # Validate that we have list of data frames
  if (!is.list(animal_matrices)) {
    stop("`synch_master_animal2` must be a data frame or list of data frames", call. = FALSE)
  }
  
  # Initialize progress bar
  n_days <- length(animal_matrices)
  cli::cli_progress_bar(
    "Analyzing pair-wise co-occurrence",
    total = n_days,
    format = "{cli::pb_spin} {cli::pb_bar} {cli::pb_current}/{cli::pb_total} [{cli::pb_percent}] | ETA: {cli::pb_eta}"
  )
  
  # Process each day
  bout_list <- list()
  total_time_list <- list()
  avg_duration_list <- list()
  
  for (i in seq_along(animal_matrices)) {
    day_name <- names(animal_matrices)[i]
    
    # Process this day's data
    day_results <- process_all_pairs_one_day(
      animal_matrix = animal_matrices[[i]],
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
    
    # Update progress
    cli::cli_progress_update()
  }
  
  # Complete progress bar
  cli::cli_progress_done()
  
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

#' Process all animal pairs for one day (pair analysis)
#'
#' @description
#' Internal function to process all unique animal pairs for a single day,
#' calculating bout, total time, and average duration for each pair's
#' co-occurrence.
#'
#' @param animal_matrix Data frame with Time column and animal ID columns
#' @param resolution Character, time resolution
#' @param id_col Character, column name for animal ID (used for validation)
#' @return List with three animal × animal matrices: bout, total_time, avg_duration
#' 
#' @keywords internal
#' @noRd
process_all_pairs_one_day <- function(animal_matrix, resolution, id_col) {
  
  # Validate input
  if (!is.data.frame(animal_matrix)) {
    stop("animal_matrix must be a data frame", call. = FALSE)
  }
  
  if (!"Time" %in% names(animal_matrix)) {
    stop("animal_matrix must have 'Time' column", call. = FALSE)
  }
  
  # Extract animal IDs from column names (exclude Time and derived columns)
  exclude_cols <- c("Time", "total_animal_num", "unoccupied_bin_num", "date")
  animal_cols <- setdiff(names(animal_matrix), exclude_cols)
  
  if (length(animal_cols) == 0) {
    stop("No animal columns found in animal_matrix", call. = FALSE)
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
    
    # Extract time points when both animals are active
    time_vector <- extract_pair_activity(animal_matrix, animal1, animal2)
    
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

#' Extract time points when both animals are active
#'
#' @description
#' Internal function to extract timestamps when both specified animals
#' are simultaneously active (both have value = 1 in their columns).
#'
#' @param animal_matrix Data frame with Time column and animal ID columns
#' @param animal1 Character or numeric, first animal ID (column name)
#' @param animal2 Character or numeric, second animal ID (column name)
#' @return POSIXct vector of timestamps when both animals are active
#'
#' @keywords internal
#' @noRd
extract_pair_activity <- function(animal_matrix, animal1, animal2) {
  
  # Convert to character for column name matching
  animal1 <- as.character(animal1)
  animal2 <- as.character(animal2)
  
  # Validate columns exist
  if (!animal1 %in% names(animal_matrix)) {
    stop("Animal ", animal1, " not found in matrix", call. = FALSE)
  }
  if (!animal2 %in% names(animal_matrix)) {
    stop("Animal ", animal2, " not found in matrix", call. = FALSE)
  }
  
  # Filter rows where both animals are active (value = 1)
  both_active <- (animal_matrix[[animal1]] == 1) & (animal_matrix[[animal2]] == 1)
  
  # Extract time vector
  time_vector <- animal_matrix$Time[both_active]
  
  return(time_vector)
} 