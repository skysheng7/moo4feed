# -----------------------------------------------------------------------------#
# -------------------- External user-facing functions -------------------------#
# -----------------------------------------------------------------------------#

#' Process matrices and add derived columns
#'
#' @description
#' Main function to process synchronization matrices from feeding and drinking data.
#' Creates time-based matrices tracking animal activity, bin usage, and optionally
#' feed amounts. Adds derived columns like total number of animals active,
#' total bins occupied, and date information. Can handle both single
#' data frame input or list of data frames.
#'
#' @param data_list List of data frames to process or single data frame
#' @param type Character, one of 'feed', 'drink', 'feed_and_drink'
#' @param resolution Character, either "sec" (default) or "min" for time resolution
#' @param reorder_by_layout Logical, whether to reorder matrix columns by physical bin layout (default FALSE)
#' @inheritParams set_global_cols
#' @return List containing processed matrices (structure depends on input type and format):
#'   
#'   \strong{For feed type ("feed"):}
#'   \itemize{
#'     \item \strong{synch_master_animal2}: Time × Animal activity matrix
#'       \itemize{
#'         \item Rows: Time points (filtered to active periods only)
#'         \item Columns: Time + Animal IDs + derived columns
#'         \item Values in each cell: 1 = animal present, 0 = animal not present
#'         \item Derived columns: \code{total_animal_num}, \code{unoccupied_bin_num}, \code{date}
#'       }
#'     \item \strong{synch_master_bin2}: Time × Animal bin occupancy matrix
#'       \itemize{
#'         \item Rows: Time points (filtered to active periods only)
#'         \item Columns: Time + Animal IDs + date column only
#'         \item Values in each cell: Bin number animal is using (0 = not active)
#'         \item Derived columns: \code{date} only
#'       }
#'     \item \strong{synch_master_feed2}: Time × Bin feed amount matrix
#'       \itemize{
#'         \item Rows: Time points (filtered to active periods only)
#'         \item Columns: Time + Bin numbers + derived columns
#'         \item Values in each cell: Feed weight at each bin
#'         \item Derived columns: \code{totalFeed}, \code{date}
#'       }
#'   }
#'   
#'   \strong{For drink/feed_and_drink types ("drink", "feed_and_drink"):}
#'   \itemize{
#'     \item \strong{synch_master_animal2}: Same as above but for drinking activity
#'     \item \strong{synch_master_bin2}: Same as above but for water bin occupancy
#'   }
#'   
#'   \strong{Note:} If input is a single data frame, returns individual matrices. 
#'   If input is a list, returns lists of matrices (one per date).
#' 
#' @examples
#' # Create toy data with more realistic structure
#' toy_data <- list(
#'   day1 = data.frame(
#'     animal = c(1, 2),
#'     start = lubridate::ymd_hms(c("2023-01-01 10:00:00", "2023-01-01 10:00:01")),
#'     end = lubridate::ymd_hms(c("2023-01-01 10:00:02", "2023-01-01 10:00:03")),
#'     bin = c(1, 2),
#'     start_weight = c(10.5, 8.3),
#'     end_weight = c(10.2, 8.1)
#'   )
#' )
#' 
#' 
#' # Process with bin
#' result_reordered <- matrix_process(toy_data, type = "feed", 
#'                                   id_col = "animal", start_col = "start", 
#'                                   end_col = "end", bin_col = "bin",
#'                                   start_weight_col = "start_weight",
#'                                   end_weight_col = "end_weight",
#'                                   bins_feed = 1:2, 
#'                                   reorder_by_layout = FALSE)
#' names(result_reordered)
#' 
#' @export
matrix_process <- function(data_list, 
                          type = c("feed", "drink", "feed_and_drink"),
                          resolution = c("sec", "min"),
                          reorder_by_layout = FALSE,
                          id_col = id_col2(),
                          start_col = start_col2(),
                          end_col = end_col2(),
                          bin_col = bin_col2(),
                          start_weight_col = start_weight_col2(),
                          end_weight_col = end_weight_col2(),
                          bins_feed = bins_feed2(),
                          bins_wat = bins_wat2(),
                          bin_layout = bin_layout2()) {
  
  ## Validate inputs
  type <- match.arg(trimws(tolower(type)), c("feed", "drink", "feed_and_drink"))
  resolution <- match.arg(trimws(tolower(resolution)), c("sec", "min"))
  
  if (is.null(data_list) || length(data_list) == 0) {
    stop("`data_list` cannot be NULL or empty", call. = FALSE)
  }
  
  # Handle single data frame input by converting to list
  single_df_input <- is.data.frame(data_list)
  if (single_df_input) {
    data_list <- list(single_day = data_list)
  }
  
  ## Initialize and populate matrices
  matrices <- matrix_initialize_internal(data_list, type, resolution, id_col, start_col, end_col, 
                                        bin_col, start_weight_col, end_weight_col, bins_feed, bins_wat)
  
  synch_master_animal <- matrices$synch_master_animal
  synch_master_bin <- matrices$synch_master_bin
  if (type == "feed") {
    synch_master_feed <- matrices$synch_master_feed
  }
  
  ## Process all matrices and add derived columns in one integrated function
  ## Return results based on input type and format
  if (type == "feed") {
    processed_matrices <- process_matrices(synch_master_animal, synch_master_bin, synch_master_feed, type, bins_feed, bins_wat)
    synch_master_animal2 <- processed_matrices$animal
    synch_master_bin2 <- processed_matrices$bin
    synch_master_feed2 <- processed_matrices$feed
    
    # Prepare result structure
    if (single_df_input) {
      result <- list(
        synch_master_animal2 = synch_master_animal2[[1]],
        synch_master_bin2 = synch_master_bin2[[1]],
        synch_master_feed2 = synch_master_feed2[[1]]
      )
    } else {
      result <- list(
        synch_master_animal2 = synch_master_animal2,
        synch_master_bin2 = synch_master_bin2,
        synch_master_feed2 = synch_master_feed2
      )
    }
  } else {
    processed_matrices <- process_matrices(synch_master_animal, synch_master_bin, NULL, type, bins_feed, bins_wat)
    synch_master_animal2 <- processed_matrices$animal
    synch_master_bin2 <- processed_matrices$bin
    
    # Prepare result structure
    if (single_df_input) {
      result <- list(
        synch_master_animal2 = synch_master_animal2[[1]],
        synch_master_bin2 = synch_master_bin2[[1]]
      )
    } else {
      result <- list(
        synch_master_animal2 = synch_master_animal2,
        synch_master_bin2 = synch_master_bin2
      )
    }
  }
  
  ## Apply bin reordering if requested
  if (type == "feed_and_drink" && reorder_by_layout) {
    result <- bin_reorder(result, bin_layout = bin_layout)
  }
  
  return(result)
}


# -----------------------------------------------------------------------------#
# ------------------------ Internal helper functions --------------------------#
# -----------------------------------------------------------------------------#

#' Generate empty synchronization matrices for feed/water data (internal)
#' 
#' @description
#' Creates empty synchronization matrices (time-animal, time-bin, and optionally time-feed)
#' for analyzing feeding and drinking synchronicity patterns. Internal function that
#' only handles list inputs for efficiency.
#'
#' @param data_list List of data frames grouped by date
#' @param type Character, one of 'feed', 'drink', 'feed_and_drink'
#' @param resolution Character, either "sec" (default) or "min" for time resolution
#' @inheritParams set_global_cols
#' @return List containing lists of matrices (one per date)
#' 
#' @keywords internal
#' @noRd
empty_synch_matrix_internal <- function(data_list, 
                                       type,
                                       resolution,
                                       id_col,
                                       start_col,
                                       end_col,
                                       bin_col,
                                       bins_feed,
                                       bins_wat) {
  
  synch_master_animal <- list()
  synch_master_bin <- list()
  synch_master_feed <- list()
  
  for (y in seq_along(data_list)) {
    cur_data <- data_list[[y]]
    
    # Validate required columns
    required_cols <- c(id_col, start_col, end_col)
    missing_cols <- setdiff(required_cols, names(cur_data))
    if (length(missing_cols) > 0) {
      stop("Missing required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
    }
    
    # Sort data by time for optimal processing
    cur_data <- cur_data[order(cur_data[[start_col]], cur_data[[end_col]]), ]
    dateTime_seq <- create_time_sequence(cur_data, start_col, end_col, resolution)
    
    # Create matrices
    animal_time_matrix <- prepare_time_animal_matrix(cur_data, dateTime_seq, id_col)
    synch_master_animal[[y]] <- animal_time_matrix
    synch_master_bin[[y]] <- animal_time_matrix  # Start as copy, will be populated differently
    
    if (type == "feed") {
      time_feed_matrix <- prepare_time_feed_matrix(dateTime_seq, bins_feed)
      synch_master_feed[[y]] <- time_feed_matrix
    }
    
    # Preserve list element names
    names(synch_master_animal)[y] <- names(data_list)[y]
    names(synch_master_bin)[y] <- names(data_list)[y]
    if (type == "feed") {
      names(synch_master_feed)[y] <- names(data_list)[y]
    }
  }
  
  if (type == "feed") {
    return(list(
      synch_master_animal = synch_master_animal,
      synch_master_bin = synch_master_bin,
      synch_master_feed = synch_master_feed
    ))
  } else {
    return(list(
      synch_master_animal = synch_master_animal,
      synch_master_bin = synch_master_bin
    ))
  }
}

#' Initialize and populate synchronization matrices (internal)
#'
#' @description
#' Initializes synchronization matrices and processes feed/water data to populate 
#' the matrices with actual feeding/drinking events. Internal function optimized
#' for list processing.
#'
#' @param data_list List of data frames
#' @param type Character, type of data processing
#' @param resolution Character, time resolution
#' @inheritParams set_global_cols
#' @return List of populated matrices
#' 
#' @keywords internal
#' @noRd
matrix_initialize_internal <- function(data_list, 
                                      type,
                                      resolution,
                                      id_col,
                                      start_col,
                                      end_col,
                                      bin_col,
                                      start_weight_col,
                                      end_weight_col,
                                      bins_feed,
                                      bins_wat) {
  
  # Get empty matrices
  matrices <- empty_synch_matrix_internal(data_list, type, resolution, id_col, start_col, 
                                         end_col, bin_col, bins_feed, bins_wat)
  
  synch_master_animal <- matrices$synch_master_animal
  synch_master_bin <- matrices$synch_master_bin
  if (type == "feed") {
    synch_master_feed <- matrices$synch_master_feed
  }
  
  # Populate matrices with data (memory-efficient approach)
  for (y in seq_along(data_list)) {
    cur_data <- data_list[[y]]
    
    # Validate additional required columns based on type
    required_cols <- c(id_col, start_col, end_col, bin_col)
    if (type == "feed") {
      required_cols <- c(required_cols, start_weight_col, end_weight_col)
    }
    missing_cols <- setdiff(required_cols, names(cur_data))
    if (length(missing_cols) > 0) {
      stop("Missing required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
    }
    
    # Extract individual matrices for this day (avoids passing entire lists)
    animal_matrix <- synch_master_animal[[y]]
    bin_matrix <- synch_master_bin[[y]]
    feed_matrix <- if (type == "feed") synch_master_feed[[y]] else NULL
    
    # Process data for this day using individual matrices
    updated_matrices <- populate_matrices_for_day(
      cur_data = cur_data,
      animal_matrix = animal_matrix,
      bin_matrix = bin_matrix,
      feed_matrix = feed_matrix,
      type = type,
      resolution = resolution,
      id_col = id_col,
      start_col = start_col,
      end_col = end_col,
      bin_col = bin_col,
      start_weight_col = start_weight_col,
      end_weight_col = end_weight_col,
      bins_feed = bins_feed
    )
    
    # Update individual list elements directly (more memory efficient)
    synch_master_animal[[y]] <- updated_matrices$animal_matrix
    synch_master_bin[[y]] <- updated_matrices$bin_matrix
    if (type == "feed") {
      synch_master_feed[[y]] <- updated_matrices$feed_matrix
    }
  }
  
  if (type == "feed") {
    return(list(
      synch_master_animal = synch_master_animal,
      synch_master_bin = synch_master_bin,
      synch_master_feed = synch_master_feed
    ))
  } else {
    return(list(
      synch_master_animal = synch_master_animal,
      synch_master_bin = synch_master_bin
    ))
  }
}

#' Populate matrices for a single day with optimized time matching (memory efficient)
#'
#' @description
#' Internal function to populate individual matrices for one day of data with optimized
#' time index lookups. More memory-efficient approach that works on individual matrices
#' rather than entire lists.
#'
#' @param cur_data Data frame for the current day
#' @param animal_matrix Individual animal presence matrix for this day
#' @param bin_matrix Individual bin occupancy matrix for this day  
#' @param feed_matrix Individual feed weight matrix for this day (NULL for waterv or feed_and_drink)
#' @param type Character, type of data processing
#' @param resolution Character, time resolution
#' @param id_col Character, column name for animal ID
#' @param start_col Character, column name for start time
#' @param end_col Character, column name for end time
#' @param bin_col Character, column name for bin ID
#' @param start_weight_col Character, column name for start weight (feed only)
#' @param end_weight_col Character, column name for end weight (feed only)
#' @param bins_feed Numeric vector of feed bin IDs
#' 
#' @return List with updated individual matrices
#' @keywords internal
#' @noRd
populate_matrices_for_day <- function(cur_data, animal_matrix, bin_matrix, feed_matrix = NULL,
                                     type, resolution, id_col, start_col, end_col, bin_col, 
                                     start_weight_col = NULL, end_weight_col = NULL, bins_feed = NULL) {
  
  # Sort data and get unique animals
  cur_data <- cur_data[order(cur_data[[start_col]], cur_data[[end_col]]), ]
  animal_list <- sort(unique(cur_data[[id_col]]))
  
  # Pre-compute time sequence for fast lookups
  time_seq <- animal_matrix$Time
  tolerance <- if (resolution == "sec") 1 else 60
  
  # Setup feed bin info if needed
  if (type == "feed" && !is.null(bins_feed)) {
    min_feed_bin <- min(bins_feed)
    max_feed_bin <- max(bins_feed)
    bin_list <- seq(min_feed_bin, max_feed_bin, by = 1)
  }
  
  # Process each data row
  for (o in seq_len(nrow(cur_data))) {
    cur_animal <- cur_data[[id_col]][o]
    index_animal <- match(cur_animal, animal_list) + 1
    cur_start <- cur_data[[start_col]][o]
    cur_end <- cur_data[[end_col]][o]
    cur_bin <- cur_data[[bin_col]][o]
    
    # Fast time index lookup using findInterval (optimized)
    start_row_number <- find_closest_time_index(time_seq, cur_start, tolerance)
    end_row_number <- find_closest_time_index(time_seq, cur_end, tolerance)
    
    # Update animal matrix (presence)
    animal_matrix[start_row_number:end_row_number, index_animal] <- 1
    
    # Update bin matrix (bin occupancy)
    bin_matrix[start_row_number:end_row_number, index_animal] <- cur_bin
    
    # Update feed matrix if applicable
    if (type == "feed" && !is.null(feed_matrix) && !is.null(start_weight_col) && !is.null(end_weight_col)) {
      start_weight <- cur_data[[start_weight_col]][o]
      end_weight <- cur_data[[end_weight_col]][o]
      weight_list <- round(seq(start_weight, end_weight, 
                              length.out = (end_row_number - start_row_number + 1)), digits = 1)
      
      # Validate bin and update if valid
      if (cur_bin %in% bin_list) {
        index_bin <- match(cur_bin, bin_list) + 1
        feed_matrix[start_row_number:end_row_number, index_bin] <- weight_list
      } else {
        warning("Bin ", cur_bin, " is outside the expected range (", 
                min_feed_bin, "-", max_feed_bin, "), skipping", call. = FALSE)
      }
    }
  }
  
  # Return updated individual matrices
  return(list(
    animal_matrix = animal_matrix,
    bin_matrix = bin_matrix,
    feed_matrix = feed_matrix
  ))
}

#' Fast time index lookup using optimized search
#'
#' @description
#' Internal function to find the closest time index efficiently using binary search
#' principles for better time complexity than linear search.
#'
#' @keywords internal
#' @noRd
find_closest_time_index <- function(time_seq, target_time, tolerance) {
  time_nums <- as.numeric(time_seq)
  target_num <- as.numeric(target_time)
  
  # Use findInterval for fast binary search-like lookup
  idx <- findInterval(target_num, time_nums)
  
  # Check closest matches (current or next position)
  candidates <- c(idx, idx + 1)
  candidates <- candidates[candidates >= 1 & candidates <= length(time_nums)]
  
  if (length(candidates) == 0) {
    stop("No valid time indices found for time: ", target_time, call. = FALSE)
  }
  
  # Find the closest match
  diffs <- abs(time_nums[candidates] - target_num)
  best_idx <- candidates[which.min(diffs)]
  
  # Check tolerance
  if (min(diffs) > tolerance) {
    stop("No matching time found within tolerance for time: ", target_time, call. = FALSE)
  }
  
  return(best_idx)
}

#' Filter matrix and add date column (helper function)
#'
#' @description
#' Internal helper function to filter a matrix based on active records
#' and add a date column. Handles edge cases when no active records exist.
#'
#' @keywords internal
#' @noRd
filter_matrix_and_add_date <- function(matrix_data, active_records) {
  # Handle edge case when no active records
  if (length(active_records) == 0) {
    # Create empty matrix with same structure but no rows
    filtered_matrix <- matrix_data[0, , drop = FALSE]
    filtered_matrix$date <- as.Date(character(0))
  } else {
    filtered_matrix <- matrix_data[active_records, , drop = FALSE]
    filtered_matrix$date <- lubridate::date(filtered_matrix$Time)
  }
  return(filtered_matrix)
}

#' Process feed matrix data with NA handling and total computation
#'
#' @description
#' Internal helper function to process feed matrix data after filtering,
#' including NA value replacement and total feed computation.
#'
#' @keywords internal
#' @noRd
process_feed_matrix_data <- function(feed_matrix, bins_feed) {
  # Only process if we have data remaining after filtering
  if ((nrow(feed_matrix) > 0) && (ncol(feed_matrix) > 1)) {
    # Fill NA values and compute total feed
    feed_matrix <- process_cur_synch(feed_matrix, bins_feed)
    feed_matrix$date <- lubridate::date(feed_matrix$Time)
  } else {
    # Handle empty matrix case - create minimal structure with required columns
    feed_matrix$Time <- as.POSIXct(character(0))
    feed_matrix$date <- as.Date(character(0))
    feed_matrix$totalFeed <- numeric(0)
  }
  return(feed_matrix)
}

#' Process all matrices and add derived columns
#'
#' @description
#' Internal function to add total animal counts and empty bin statistics
#' to animal matrices, and filter all matrices (animal, bin, and optionally feed)
#' to remove inactive time periods in a single loop for efficiency.
#'
#' @keywords internal
#' @noRd
process_matrices <- function(synch_master_animal, synch_master_bin, synch_master_feed = NULL, type, bins_feed, bins_wat) {
  
  for (i in seq_along(synch_master_animal)) {
    # Compute total animals active and bins occupied
    if (ncol(synch_master_animal[[i]]) > 1) {
      # Get animal columns (exclude Time column)
      animal_cols <- 2:ncol(synch_master_animal[[i]])
      synch_master_animal[[i]]$total_animal_num <- rowSums(
        synch_master_animal[[i]][, animal_cols, drop = FALSE], na.rm = TRUE)
      
      # Calculate unoccupied bins based on type
      if (type == "feed") {
        synch_master_animal[[i]]$unoccupied_bin_num <- length(bins_feed) - synch_master_animal[[i]]$total_animal_num
      } else if (type == "drink") {
        synch_master_animal[[i]]$unoccupied_bin_num <- length(bins_wat) - synch_master_animal[[i]]$total_animal_num
      } else {
        # feed_and_drink: total feed bins + water bins - occupied bins
        synch_master_animal[[i]]$unoccupied_bin_num <- (length(bins_feed) + length(bins_wat)) - synch_master_animal[[i]]$total_animal_num
      }
      
      # Filter out inactive time periods and add date for all matrices using helper
      active_records <- which(synch_master_animal[[i]]$total_animal_num > 0)
    } else {
      # Handle edge case with no animal columns - no active records
      synch_master_animal[[i]]$total_animal_num <- 0
      synch_master_animal[[i]]$unoccupied_bin_num <- if (type == "feed") {
        length(bins_feed)
      } else if (type == "drink") {
        length(bins_wat)
      } else {
        length(bins_feed) + length(bins_wat)
      }
      active_records <- integer(0)  # No active records
    }
    
    # Apply filtering to all matrices (handles empty active_records gracefully)
    synch_master_animal[[i]] <- filter_matrix_and_add_date(synch_master_animal[[i]], active_records)
    synch_master_bin[[i]] <- filter_matrix_and_add_date(synch_master_bin[[i]], active_records)
    
    # Process feed matrix if provided
    if (!is.null(synch_master_feed) && i <= length(synch_master_feed)) {
      synch_master_feed[[i]] <- filter_matrix_and_add_date(synch_master_feed[[i]], active_records)
      synch_master_feed[[i]] <- process_feed_matrix_data(synch_master_feed[[i]], bins_feed)
    }
  }
  
  # Return results based on whether feed matrix was provided
  if (!is.null(synch_master_feed)) {
    return(list(
      animal = synch_master_animal,
      bin = synch_master_bin,
      feed = synch_master_feed
    ))
  } else {
    return(list(
      animal = synch_master_animal,
      bin = synch_master_bin
    ))
  }
}



#' Process the current synchronization data to replace NA values and compute total feed
#'
#' @description
#' Processes synchronization data by replacing NA values with appropriate values
#' and computing total feed across all bins. Internal helper function.
#'
#' @param cur_synch Data frame representing the current synchronization data
#' @param bins_feed Vector of feed bin IDs used in the analysis
#' @return Data frame with NA values replaced and a new column 'totalFeed'
#' 
#' @keywords internal
#' @noRd
process_cur_synch <- function(cur_synch, bins_feed = bins_feed2()) {
  if (is.null(cur_synch)) {
    stop("`cur_synch` cannot be NULL", call. = FALSE)
  }
  if (!"Time" %in% colnames(cur_synch)) {
    stop("Input matrix must contain a 'Time' column", call. = FALSE)
  }
  if (!is.numeric(bins_feed) || length(bins_feed) == 0) {
    stop("bins_feed must be a non-empty numeric vector", call. = FALSE)
  }
  
  # Calculate bin columns based on actual bins_feed
  bin_column_names <- as.character(bins_feed)
  available_bin_cols <- intersect(bin_column_names, colnames(cur_synch))
  
  if (length(available_bin_cols) == 0) {
    stop("No matching bin columns found in input matrix", call. = FALSE)
  }
  
  bin_cols_indices <- match(available_bin_cols, colnames(cur_synch))
  
  # Replace initial NA values with first non-NA value (vectorized approach)
  bin_data <- cur_synch[, bin_cols_indices, drop = FALSE]
  
  if (nrow(bin_data) == 1) {
    # replace NA with 0
    cur_synch[is.na(cur_synch)] <- 0

    # Add totalFeed column using vectorized rowSums
    cur_synch$totalFeed <- rowSums(cur_synch[, bin_cols_indices, drop = FALSE], na.rm = TRUE)
    return(cur_synch)  # Return original matrix if no bin columns
  } else {
    first_non_na <- vapply(bin_data, function(x) {
      non_na_vals <- x[!is.na(x)]
      if (length(non_na_vals) > 0) {
        return(non_na_vals[1])
      } else {
        return(0)  # Default to 0 if all values are NA
      }
    }, FUN.VALUE = numeric(1))
  }
  
  # Update first row where needed
  first_row_na <- is.na(bin_data[1, ])
  cur_synch[1, bin_cols_indices][first_row_na] <- first_non_na[first_row_na]
  
  # Forward fill NA values using zoo::na.locf for better performance
  if (nrow(bin_data) > 1) {
    for (col_idx in bin_cols_indices) {
      col_data <- cur_synch[, col_idx]
      # Only apply na.locf if there are some non-NA values and not all leading NAs
      if (any(!is.na(col_data)) && !is.na(col_data[1])) {
        cur_synch[, col_idx] <- zoo::na.locf(col_data)
      }
    }
  }
  
  # Add totalFeed column using vectorized rowSums
  cur_synch$totalFeed <- rowSums(cur_synch[, bin_cols_indices, drop = FALSE], na.rm = TRUE)
  
  return(cur_synch)
}

#' Reorder matrix columns by physical bin layout
#'
#' @description
#' Reorders the columns in synchronicity matrices to match the physical spatial
#' arrangement of bins. This is essential for neighbor analysis and spatial
#' synchronicity studies where physical proximity matters.
#'
#' @param synch_matrices List containing processed synchronicity matrices from matrix_process()
#' @param bin_layout Integer vector specifying physical order of bins (default from bin_layout2())
#' @return List with the same structure as input but with reordered columns
#' 
#' @details
#' This function reorders columns in both animal and bin matrices to reflect the
#' physical layout of bins. The Time column always remains first, followed by
#' animal/bin columns in the order specified by bin_layout, then any additional
#' derived columns (like total_animal_num, date, etc.) at the end.
#' 
#' @noRd
bin_reorder <- function(synch_matrices, 
                       bin_layout = bin_layout2()) {
  
  # Input validation
  if (is.null(synch_matrices) || length(synch_matrices) == 0) {
    stop("`synch_matrices` cannot be NULL or empty", call. = FALSE)
  }
  
  if (!is.list(synch_matrices)) {
    stop("`synch_matrices` must be a list", call. = FALSE)
  }
  
  if (!is.numeric(bin_layout) || length(bin_layout) == 0) {
    stop("`bin_layout` must be a non-empty numeric vector", call. = FALSE)
  }
  
  # Check if input has the expected structure from matrix_process
  expected_names <- c("synch_master_animal2", "synch_master_bin2")
  if (!all(expected_names %in% names(synch_matrices))) {
    stop("Input must be the result from matrix_process() with expected components: ", 
         paste(expected_names, collapse = ", "), call. = FALSE)
  }
  
  # Create a copy to avoid modifying original data
  result <- synch_matrices
  
  # Process animal matrices
  result$synch_master_animal2 <- reorder_matrix_columns(
    synch_matrices$synch_master_animal2, bin_layout, "animal"
  )
  
  # Process bin matrices  
  result$synch_master_bin2 <- reorder_matrix_columns(
    synch_matrices$synch_master_bin2, bin_layout, "bin"
  )
  
  # Process feed matrices if they exist
  if ("synch_master_feed2" %in% names(synch_matrices)) {
    result$synch_master_feed2 <- reorder_matrix_columns(
      synch_matrices$synch_master_feed2, bin_layout, "feed"
    )
  }
  
  return(result)
}

#' Reorder columns in a single matrix or list of matrices (internal helper)
#'
#' @description
#' Internal helper function to reorder columns in synchronicity matrices
#' based on physical bin layout. Handles both single matrices and lists of matrices.
#'
#' @param matrix_data Single data frame or list of data frames
#' @param bin_layout Integer vector specifying physical order of bins
#' @param matrix_type Character indicating matrix type ("animal", "bin", or "feed")
#' @return Reordered matrix or list of matrices
#' 
#' @keywords internal
#' @noRd
reorder_matrix_columns <- function(matrix_data, bin_layout, matrix_type) {
  
  # Handle single data frame input
  if (is.data.frame(matrix_data)) {
    return(reorder_single_matrix(matrix_data, bin_layout, matrix_type))
  }
  
  # Handle list of data frames
  if (is.list(matrix_data)) {
    result_list <- list()
    
    for (i in seq_along(matrix_data)) {
      if (is.data.frame(matrix_data[[i]])) {
        result_list[[i]] <- reorder_single_matrix(matrix_data[[i]], bin_layout, matrix_type)
        
        # Preserve names
        if (!is.null(names(matrix_data)[i])) {
          names(result_list)[i] <- names(matrix_data)[i]
        }
      } else {
        warning("Element ", i, " is not a data frame, skipping", call. = FALSE)
        result_list[[i]] <- matrix_data[[i]]
      }
    }
    
    return(result_list)
  }
  
  stop("matrix_data must be a data frame or list of data frames", call. = FALSE)
}

#' Reorder columns in a single matrix (internal helper)
#'
#' @description
#' Internal helper to reorder columns in a single synchronicity matrix.
#' Maintains Time column first, reorders animal/bin columns by physical layout,
#' and keeps derived columns at the end.
#'
#' @keywords internal
#' @noRd
reorder_single_matrix <- function(single_matrix, bin_layout, matrix_type) {
  
  if (!is.data.frame(single_matrix)) {
    stop("Input must be a data frame", call. = FALSE)
  }
  
  if (!"Time" %in% names(single_matrix)) {
    stop("Matrix must have a 'Time' column", call. = FALSE)
  }
  
  if (ncol(single_matrix) < 2) {
    # If only Time column exists, return as is
    return(single_matrix)
  }
  
  # Identify column types
  time_col <- "Time"
  derived_cols <- c("total_animal_num", "unoccupied_bin_num", "date", "totalFeed")
  derived_present <- intersect(derived_cols, names(single_matrix))
  
  # Get animal/bin columns (exclude Time and derived columns)
  data_cols <- setdiff(names(single_matrix), c(time_col, derived_present))
  
  if (length(data_cols) == 0) {
    # No data columns to reorder
    return(single_matrix)
  }
  
  # For animal matrices: data_cols are animal IDs (numeric)
  # For bin matrices: data_cols are animal IDs but values are bin numbers
  # For feed matrices: data_cols are bin numbers
  
  if (matrix_type == "feed") {
    # For feed matrices, columns represent bins, so reorder by bin_layout directly
    available_bins <- intersect(as.character(bin_layout), data_cols)
    unavailable_bins <- setdiff(data_cols, as.character(bin_layout))
    
    # Reorder: bins in layout order + any additional bins not in layout
    reordered_data_cols <- c(available_bins, unavailable_bins)
  } else {
    # For animal and bin matrices, columns represent animals
    # We can't reorder animals by bin layout, so keep original order
    # This function is mainly useful for feed matrices
    reordered_data_cols <- data_cols
  }
  
  # Construct final column order: Time + reordered data columns + derived columns
  final_col_order <- c(time_col, reordered_data_cols, derived_present)
  
  # Ensure all columns exist in the matrix
  final_col_order <- intersect(final_col_order, names(single_matrix))
  
  # Return reordered matrix
  return(single_matrix[, final_col_order, drop = FALSE])
} 