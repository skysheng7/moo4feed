#' Convert synchronicity matrices to pair-level data frame
#'
#' @description
#' Converts synchronicity matrices (from [synch_pair_analysis()] or
#' [synch_neighbor_analysis()]) to a tidy data frame format with one row per
#' animal pair. Extracts values from the upper triangle of the matrices and
#' organizes them into a data frame suitable for further analysis or visualization.
#'
#' @param synch_results List output from [synch_pair_analysis()] or
#'   [synch_neighbor_analysis()], containing `bout`, `total_time`, and
#'   `avg_duration` elements (matrices or lists of matrices)
#' @param min_time Numeric, minimum total_time threshold to include a pair
#'   (default 0, which includes all pairs with any activity). Use this to
#'   filter out pairs with negligible interaction.
#' @param sort_by Character, column name to sort by. Common options:
#'   "total_time" (default), "bouts", "avg_duration", or NULL for no sorting.
#' @param decreasing Logical, if TRUE (default) sorts in descending order,
#'   if FALSE sorts ascending
#'
#' @return Data frame with columns:
#'   \itemize{
#'     \item \code{animal1}: Character, first animal ID in pair
#'     \item \code{animal2}: Character, second animal ID in pair
#'     \item \code{bouts}: Integer, number of distinct bouts together
#'     \item \code{total_time}: Numeric, total time together (seconds or minutes)
#'     \item \code{avg_duration}: Numeric, average duration per bout
#'     \item \code{day}: Character, day identifier (only for multi-day input)
#'   }
#'   Only includes pairs where total_time > min_time. Sorted by sort_by column
#'   if specified.
#'
#' @examples
#' # Create toy data
#' toy_data <- data.frame(
#'   animal = c(1, 2, 3, 1, 2, 3),
#'   start = lubridate::ymd_hms(c(
#'     "2023-01-01 10:00:00", "2023-01-01 10:00:01",
#'     "2023-01-01 10:00:05", "2023-01-01 10:00:10",
#'     "2023-01-01 10:00:11", "2023-01-01 10:00:12"
#'   )),
#'   end = lubridate::ymd_hms(c(
#'     "2023-01-01 10:00:03", "2023-01-01 10:00:04",
#'     "2023-01-01 10:00:08", "2023-01-01 10:00:13",
#'     "2023-01-01 10:00:14", "2023-01-01 10:00:15"
#'   )),
#'   bin = c(1, 2, 3, 1, 2, 3),
#'   start_weight = c(10.5, 8.3, 9.1, 10.2, 8.0, 7.5),
#'   end_weight = c(10.2, 8.1, 8.9, 9.9, 7.8, 7.3)
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
#' # Analyze pairs
#' pair_results <- synch_pair_analysis(matrices, type = "feed",
#'                                    id_col = "animal")
#'
#' # Convert to data frame
#' pair_df <- synch_pairs_to_df(pair_results)
#' print(pair_df)
#'
#' @export
synch_pairs_to_df <- function(synch_results,
                              min_time = 0,
                              sort_by = "total_time",
                              decreasing = TRUE) {
  
  # Validate inputs
  if (!is.list(synch_results)) {
    stop("`synch_results` must be a list from synch_pair_analysis() or synch_neighbor_analysis()",
         call. = FALSE)
  }
  
  required_elements <- c("bout", "total_time", "avg_duration")
  missing_elements <- setdiff(required_elements, names(synch_results))
  if (length(missing_elements) > 0) {
    stop("`synch_results` must contain elements: ",
         paste(required_elements, collapse = ", "),
         call. = FALSE)
  }
  
  if (!is.numeric(min_time) || length(min_time) != 1 || min_time < 0) {
    stop("`min_time` must be a single non-negative number", call. = FALSE)
  }
  
  if (!is.logical(decreasing) || length(decreasing) != 1) {
    stop("`decreasing` must be TRUE or FALSE", call. = FALSE)
  }
  
  # Determine if single day or multi-day input
  single_day <- is.matrix(synch_results$bout)
  
  if (single_day) {
    # Single day: matrices directly
    result_df <- extract_upper_triangle(
      bout_matrix = synch_results$bout,
      time_matrix = synch_results$total_time,
      avg_matrix = synch_results$avg_duration,
      min_time = min_time
    )
  } else {
    # Multi-day: lists of matrices
    if (!is.list(synch_results$bout)) {
      stop("`bout` element must be a matrix or list of matrices", call. = FALSE)
    }
    
    # Process each day and combine
    all_days <- list()
    day_names <- names(synch_results$bout)
    
    for (i in seq_along(synch_results$bout)) {
      day_name <- if (!is.null(day_names)) day_names[i] else paste0("day", i)
      
      day_df <- extract_upper_triangle(
        bout_matrix = synch_results$bout[[i]],
        time_matrix = synch_results$total_time[[i]],
        avg_matrix = synch_results$avg_duration[[i]],
        min_time = min_time
      )
      
      # Add day column (only if data frame is not empty)
      if (nrow(day_df) > 0) {
        day_df$day <- day_name
      } else {
        # For empty data frame, add day column with correct type
        day_df$day <- character(0)
      }
      all_days[[i]] <- day_df
    }
    
    # Combine all days
    result_df <- do.call(rbind, all_days)
    rownames(result_df) <- NULL
    
    # Reorder columns to put day first (after animal1 and animal2)
    col_order <- c("animal1", "animal2", "day", "bouts", "total_time", "avg_duration")
    result_df <- result_df[, col_order]
  }
  
  # Sort if requested
  if (!is.null(sort_by) && nzchar(sort_by)) {
    if (!sort_by %in% names(result_df)) {
      stop("`sort_by` must be one of: ", paste(names(result_df), collapse = ", "),
           call. = FALSE)
    }
    
    result_df <- result_df[order(result_df[[sort_by]], decreasing = decreasing), ]
    rownames(result_df) <- NULL
  }
  
  return(result_df)
}


#' Extract upper triangle values from synchronicity matrices
#'
#' @description
#' Internal helper function to extract upper triangle values from three related
#' matrices (bout, total_time, avg_duration) and combine them into a data frame.
#'
#' @param bout_matrix Matrix of bout counts
#' @param time_matrix Matrix of total times
#' @param avg_matrix Matrix of average durations
#' @param min_time Numeric, minimum time threshold
#'
#' @return Data frame with animal1, animal2, bouts, total_time, avg_duration
#'
#' @keywords internal
#' @noRd
extract_upper_triangle <- function(bout_matrix, time_matrix, avg_matrix, min_time) {
  
  # Validate all matrices have same dimensions and row/col names
  if (!is.matrix(bout_matrix) || !is.matrix(time_matrix) || !is.matrix(avg_matrix)) {
    stop("All inputs must be matrices", call. = FALSE)
  }
  
  if (!identical(dim(bout_matrix), dim(time_matrix)) ||
      !identical(dim(bout_matrix), dim(avg_matrix))) {
    stop("All matrices must have the same dimensions", call. = FALSE)
  }
  
  if (!identical(rownames(bout_matrix), colnames(bout_matrix)) ||
      !identical(rownames(bout_matrix), rownames(time_matrix)) ||
      !identical(rownames(bout_matrix), rownames(avg_matrix))) {
    stop("All matrices must have identical row and column names", call. = FALSE)
  }
  
  # Get animal IDs
  animal_ids <- rownames(bout_matrix)
  
  if (length(animal_ids) < 2) {
    # Only one animal or empty, return empty data frame
    return(data.frame(
      animal1 = character(0),
      animal2 = character(0),
      bouts = numeric(0),
      total_time = numeric(0),
      avg_duration = numeric(0)
    ))
  }
  
  # Initialize result list
  result_list <- list()
  counter <- 1
  
  # Extract upper triangle (i < j)
  for (i in 1:(length(animal_ids) - 1)) {
    for (j in (i + 1):length(animal_ids)) {
      time_value <- time_matrix[i, j]
      
      # Only include if above threshold
      if (time_value > min_time) {
        result_list[[counter]] <- data.frame(
          animal1 = animal_ids[i],
          animal2 = animal_ids[j],
          bouts = bout_matrix[i, j],
          total_time = time_value,
          avg_duration = avg_matrix[i, j],
          stringsAsFactors = FALSE
        )
        counter <- counter + 1
      }
    }
  }
  
  # Combine into data frame
  if (length(result_list) == 0) {
    # No pairs above threshold
    return(data.frame(
      animal1 = character(0),
      animal2 = character(0),
      bouts = numeric(0),
      total_time = numeric(0),
      avg_duration = numeric(0)
    ))
  }
  
  result_df <- do.call(rbind, result_list)
  rownames(result_df) <- NULL
  
  return(result_df)
}


#' Compare neighbor time to total co-occurrence time
#'
#' @description
#' Compares the time pairs spend at neighboring bins to their total co-occurrence
#' time (regardless of location). Calculates the proportion of co-occurrence time
#' that is spent as spatial neighbors, which indicates pairs that prefer to be
#' close to each other when feeding/drinking together.
#'
#' @param pair_results List output from [synch_pair_analysis()], containing
#'   total co-occurrence matrices
#' @param neighbor_results List output from [synch_neighbor_analysis()],
#'   containing neighbor-specific matrices
#' @param min_cooccurrence Numeric, minimum co-occurrence time threshold to
#'   include a pair (default 0). Use this to filter out pairs with negligible
#'   total interaction.
#' @param sort_by Character, column name to sort by. Common options:
#'   "neighbor_ratio" (default), "neighbor_time", "cooccurrence_time", or
#'   NULL for no sorting.
#' @param decreasing Logical, if TRUE (default) sorts in descending order,
#'   if FALSE sorts ascending
#'
#' @return Data frame with columns:
#'   \itemize{
#'     \item \code{animal1}: Character, first animal ID in pair
#'     \item \code{animal2}: Character, second animal ID in pair
#'     \item \code{cooccurrence_time}: Numeric, total time together anywhere
#'     \item \code{neighbor_time}: Numeric, time together at neighboring bins
#'     \item \code{neighbor_ratio}: Numeric, neighbor_time / cooccurrence_time
#'     \item \code{day}: Character, day identifier (only for multi-day input)
#'   }
#'   The neighbor_ratio ranges from 0 (never neighbors) to 1 (always at
#'   neighboring bins when together). Only includes pairs where
#'   cooccurrence_time > min_cooccurrence.
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
#' # Analyze both total and neighbor patterns
#' pair_results <- synch_pair_analysis(matrices, type = "feed",
#'                                    id_col = "animal")
#' neighbor_results <- synch_neighbor_analysis(matrices,
#'                                            bin_layout = "1-2-3",
#'                                            type = "feed",
#'                                            id_col = "animal")
#'
#' # Compare neighbor preference
#' comparison <- synch_neighbor_compare(pair_results, neighbor_results)
#' print(comparison)
#'
#' @export
synch_neighbor_compare <- function(pair_results,
                                   neighbor_results,
                                   min_cooccurrence = 0,
                                   sort_by = "neighbor_ratio",
                                   decreasing = TRUE) {
  
  # Validate inputs
  if (!is.list(pair_results) || !is.list(neighbor_results)) {
    stop("`pair_results` and `neighbor_results` must be lists", call. = FALSE)
  }
  
  required_elements <- c("bout", "total_time", "avg_duration")
  
  missing_pair <- setdiff(required_elements, names(pair_results))
  if (length(missing_pair) > 0) {
    stop("`pair_results` must contain: ", paste(required_elements, collapse = ", "),
         call. = FALSE)
  }
  
  missing_neighbor <- setdiff(required_elements, names(neighbor_results))
  if (length(missing_neighbor) > 0) {
    stop("`neighbor_results` must contain: ", paste(required_elements, collapse = ", "),
         call. = FALSE)
  }
  
  if (!is.numeric(min_cooccurrence) || length(min_cooccurrence) != 1 || min_cooccurrence < 0) {
    stop("`min_cooccurrence` must be a single non-negative number", call. = FALSE)
  }
  
  if (!is.logical(decreasing) || length(decreasing) != 1) {
    stop("`decreasing` must be TRUE or FALSE", call. = FALSE)
  }
  
  # Determine if single day or multi-day
  single_day_pair <- is.matrix(pair_results$total_time)
  single_day_neighbor <- is.matrix(neighbor_results$total_time)
  
  if (single_day_pair != single_day_neighbor) {
    stop("`pair_results` and `neighbor_results` must both be single-day or both be multi-day",
         call. = FALSE)
  }
  
  if (single_day_pair) {
    # Single day
    result_df <- compare_matrices(
      cooccurrence_matrix = pair_results$total_time,
      neighbor_matrix = neighbor_results$total_time,
      min_cooccurrence = min_cooccurrence
    )
  } else {
    # Multi-day
    if (length(pair_results$total_time) != length(neighbor_results$total_time)) {
      stop("`pair_results` and `neighbor_results` must have the same number of days",
           call. = FALSE)
    }
    
    all_days <- list()
    day_names <- names(pair_results$total_time)
    
    for (i in seq_along(pair_results$total_time)) {
      day_name <- if (!is.null(day_names)) day_names[i] else paste0("day", i)
      
      day_df <- compare_matrices(
        cooccurrence_matrix = pair_results$total_time[[i]],
        neighbor_matrix = neighbor_results$total_time[[i]],
        min_cooccurrence = min_cooccurrence
      )
      
      # Add day column (only if data frame is not empty)
      if (nrow(day_df) > 0) {
        day_df$day <- day_name
      } else {
        # For empty data frame, add day column with correct type
        day_df$day <- character(0)
      }
      all_days[[i]] <- day_df
    }
    
    # Combine all days
    result_df <- do.call(rbind, all_days)
    rownames(result_df) <- NULL
    
    # Reorder columns
    col_order <- c("animal1", "animal2", "day", "cooccurrence_time",
                   "neighbor_time", "neighbor_ratio")
    result_df <- result_df[, col_order]
  }
  
  # Sort if requested
  if (!is.null(sort_by) && nzchar(sort_by)) {
    if (!sort_by %in% names(result_df)) {
      stop("`sort_by` must be one of: ", paste(names(result_df), collapse = ", "),
           call. = FALSE)
    }
    
    result_df <- result_df[order(result_df[[sort_by]], decreasing = decreasing), ]
    rownames(result_df) <- NULL
  }
  
  return(result_df)
}


#' Compare co-occurrence and neighbor matrices
#'
#' @description
#' Internal helper function to compare co-occurrence and neighbor time matrices,
#' calculating the neighbor ratio for each pair.
#'
#' @param cooccurrence_matrix Matrix of total co-occurrence times
#' @param neighbor_matrix Matrix of neighbor times
#' @param min_cooccurrence Numeric, minimum co-occurrence threshold
#'
#' @return Data frame with animal1, animal2, cooccurrence_time, neighbor_time,
#'   neighbor_ratio
#'
#' @keywords internal
#' @noRd
compare_matrices <- function(cooccurrence_matrix, neighbor_matrix, min_cooccurrence) {
  
  # Validate matrices
  if (!is.matrix(cooccurrence_matrix) || !is.matrix(neighbor_matrix)) {
    stop("Both inputs must be matrices", call. = FALSE)
  }
  
  if (!identical(dim(cooccurrence_matrix), dim(neighbor_matrix))) {
    stop("Matrices must have the same dimensions", call. = FALSE)
  }
  
  if (!identical(rownames(cooccurrence_matrix), rownames(neighbor_matrix))) {
    stop("Matrices must have the same animal IDs", call. = FALSE)
  }
  
  # Get animal IDs
  animal_ids <- rownames(cooccurrence_matrix)
  
  if (length(animal_ids) < 2) {
    # Only one animal or empty
    return(data.frame(
      animal1 = character(0),
      animal2 = character(0),
      cooccurrence_time = numeric(0),
      neighbor_time = numeric(0),
      neighbor_ratio = numeric(0)
    ))
  }
  
  # Initialize result list
  result_list <- list()
  counter <- 1
  
  # Extract upper triangle (i < j)
  for (i in 1:(length(animal_ids) - 1)) {
    for (j in (i + 1):length(animal_ids)) {
      co_time <- cooccurrence_matrix[i, j]
      
      # Only include if above threshold
      if (co_time > min_cooccurrence) {
        nb_time <- neighbor_matrix[i, j]
        
        # Calculate ratio (avoid division by zero)
        ratio <- if (co_time > 0) nb_time / co_time else 0
        
        result_list[[counter]] <- data.frame(
          animal1 = animal_ids[i],
          animal2 = animal_ids[j],
          cooccurrence_time = co_time,
          neighbor_time = nb_time,
          neighbor_ratio = ratio,
          stringsAsFactors = FALSE
        )
        counter <- counter + 1
      }
    }
  }
  
  # Combine into data frame
  if (length(result_list) == 0) {
    return(data.frame(
      animal1 = character(0),
      animal2 = character(0),
      cooccurrence_time = numeric(0),
      neighbor_time = numeric(0),
      neighbor_ratio = numeric(0)
    ))
  }
  
  result_df <- do.call(rbind, result_list)
  rownames(result_df) <- NULL
  
  return(result_df)
}
