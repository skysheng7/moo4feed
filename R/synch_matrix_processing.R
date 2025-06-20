#' Synchronicity Matrix Processing Functions
#'
#' Functions for initializing and processing synchronicity matrices for animal feeding/drinking analysis.
#'
#' @section Functions:
#' - empty_synch_matrix
#' - matrix_initialize
#' - process_cur_synch
#' - matrix_process
#'
#' @name synch_matrix_processing
#' @keywords internal
NULL

#' Generate empty synchronization matrices for feed/water data
#'
#' @description
#' Creates empty synchronization matrices (time-animal, time-bin, and optionally time-feed)
#' for analyzing feeding and drinking synchronicity patterns.
#'
#' @param data_list List of data frames grouped by date
#' @param type Character, one of 'feed', 'drink', 'feed_and_drink'
#' @inheritParams set_global_cols
#' @return List of matrices depending on type:
#'   \itemize{
#'     \item If type = "feed": synch_master_animal, synch_master_bin, synch_master_feed
#'     \item If type = "drink" or "feed_and_drink": synch_master_animal, synch_master_bin
#'   }
#' 
#' @examples
#' # Create toy data
#' toy_data <- list(
#'   day1 = data.frame(
#'     animal = 1,
#'     start = lubridate::ymd_hms("2023-01-01 10:00:00"),
#'     end = lubridate::ymd_hms("2023-01-01 10:00:02"),
#'     bin = 1
#'   )
#' )
#' 
#' # Generate empty matrices with explicit parameters
#' matrices <- empty_synch_matrix(toy_data, type = "feed", 
#'                               id_col = "animal", start_col = "start", 
#'                               end_col = "end", bin_col = "bin", bins_feed = 1:3)
#' names(matrices)
#' 
#' @export
empty_synch_matrix <- function(data_list, 
                              type = "feed",
                              id_col = id_col2(),
                              start_col = start_col2(),
                              end_col = end_col2(),
                              bin_col = bin_col2(),
                              bins_feed = bins_feed2(),
                              bins_wat = bins_wat2()) {
  
  if (is.null(data_list) || length(data_list) == 0) stop("`data_list` cannot be NULL or empty")
  if (!type %in% c("feed", "drink", "feed_and_drink")) stop("`type` must be one of: 'feed', 'drink', 'feed_and_drink'")
  
  synch_master_animal <- list()
  synch_master_bin <- list()
  synch_master_feed <- list()
  
  for (y in seq_along(data_list)) {
    cur_data <- data_list[[y]]
    
    # Validate required columns
    required_cols <- c(id_col, start_col, end_col)
    missing_cols <- setdiff(required_cols, names(cur_data))
    if (length(missing_cols) > 0) {
      stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
    }
    
    cur_data <- cur_data[order(cur_data[[start_col]], cur_data[[end_col]]), ]
    dateTime_seq <- create_time_sequence(cur_data, start_col, end_col)
    
    animal_time_matrix <- prepare_time_animal_matrix(cur_data, dateTime_seq, id_col)
    time_bin_matrix <- animal_time_matrix
    
    synch_master_animal[[y]] <- animal_time_matrix
    synch_master_bin[[y]] <- time_bin_matrix
    
    # Rename the list elements
    names(synch_master_animal)[y] <- names(data_list)[y]
    names(synch_master_bin)[y] <- names(data_list)[y]
    
    if (type == "feed") {
      time_feed_matrix <- prepare_time_feed_matrix(dateTime_seq, bins_feed)
      synch_master_feed[[y]] <- time_feed_matrix
      names(synch_master_feed)[y] <- names(data_list)[y]
    }
  }
  
  if (type == "feed") {
    return(list(synch_master_animal = synch_master_animal,
                synch_master_bin = synch_master_bin,
                synch_master_feed = synch_master_feed))
  } else {
    return(list(synch_master_animal = synch_master_animal,
                synch_master_bin = synch_master_bin))
  }
}

#' Initialize and process synchronization matrices
#'
#' @description
#' Initializes synchronization matrices and processes feed/water data to populate 
#' the matrices with actual feeding/drinking events.
#'
#' @param data_list List of data frames
#' @param type Character, one of 'feed', 'drink', 'feed_and_drink'
#' @inheritParams set_global_cols
#' @return List of processed matrices
#' 
#' @examples
#' # Create toy data with more complete structure
#' toy_data <- list(
#'   day1 = data.frame(
#'     animal = 1,
#'     start = lubridate::ymd_hms("2023-01-01 10:00:00"),
#'     end = lubridate::ymd_hms("2023-01-01 10:00:02"),
#'     bin = 1,
#'     start_weight = 10.5,
#'     end_weight = 10.2
#'   )
#' )
#' 
#' # Initialize matrices with explicit parameters
#' processed <- matrix_initialize(toy_data, type = "feed",
#'                               id_col = "animal", start_col = "start", 
#'                               end_col = "end", bin_col = "bin",
#'                               start_weight_col = "start_weight", 
#'                               end_weight_col = "end_weight", bins_feed = 1:3)
#' names(processed)
#' 
#' @export
matrix_initialize <- function(data_list, 
                             type = "feed",
                             id_col = id_col2(),
                             start_col = start_col2(),
                             end_col = end_col2(),
                             bin_col = bin_col2(),
                             start_weight_col = start_weight_col2(),
                             end_weight_col = end_weight_col2(),
                             bins_feed = bins_feed2(),
                             bins_wat = bins_wat2()) {
  
  if (is.null(data_list) || length(data_list) == 0) stop("`data_list` cannot be NULL or empty")
  if (!type %in% c("feed", "drink", "feed_and_drink")) stop("`type` must be one of: 'feed', 'drink', 'feed_and_drink'")
  
  results <- empty_synch_matrix(data_list, type, id_col, start_col, end_col, bin_col, bins_feed, bins_wat)
  synch_master_animal <- results$synch_master_animal
  synch_master_bin <- results$synch_master_bin
  
  if (type == "feed") {
    synch_master_feed <- results$synch_master_feed
  }
  
  for (y in seq_along(data_list)) {
    cur_data <- data_list[[y]]
    
    # Validate required columns
    required_cols <- c(id_col, start_col, end_col, bin_col)
    if (type == "feed") {
      required_cols <- c(required_cols, start_weight_col, end_weight_col)
    }
    missing_cols <- setdiff(required_cols, names(cur_data))
    if (length(missing_cols) > 0) {
      stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
    }
    
    cur_data <- cur_data[order(cur_data[[start_col]], cur_data[[end_col]]), ]
    animal_list <- sort(unique(cur_data[[id_col]]))
    
    if (type == "feed") {
      min_feed_bin <- min(bins_feed)
      max_feed_bin <- max(bins_feed)
      bin_list <- seq(min_feed_bin, max_feed_bin, by = 1)
    }
    
    for (o in seq_len(nrow(cur_data))) {
      cur_animal <- cur_data[[id_col]][o]
      index_animal <- match(cur_animal, animal_list) + 1
      cur_start <- cur_data[[start_col]][o]
      cur_end <- cur_data[[end_col]][o]
      cur_bin <- cur_data[[bin_col]][o]
      
      # Robust time matching with tolerance (1 second)
      start_row_number <- which.min(abs(as.numeric(synch_master_animal[[y]]$Time) - as.numeric(cur_start)))
      if (abs(as.numeric(synch_master_animal[[y]]$Time[start_row_number]) - as.numeric(cur_start)) > 1) {
        stop("No matching start time found for animal ", cur_animal, " at time ", cur_start)
      }
      
      end_row_number <- which.min(abs(as.numeric(synch_master_animal[[y]]$Time) - as.numeric(cur_end)))
      if (abs(as.numeric(synch_master_animal[[y]]$Time[end_row_number]) - as.numeric(cur_end)) > 1) {
        stop("No matching end time found for animal ", cur_animal, " at time ", cur_end)
      }
      
      if (type == "feed") {
        start_weight <- cur_data[[start_weight_col]][o]
        end_weight <- cur_data[[end_weight_col]][o]
        weight_list <- round(seq(start_weight, end_weight, length.out = (end_row_number - start_row_number + 1)), digits = 1)
        
        # Fix BugBot issue: validate cur_bin is within expected range
        if (cur_bin %in% bin_list) {
          index_bin <- match(cur_bin, bin_list) + 1
          synch_master_feed[[y]][(start_row_number:end_row_number), index_bin] <- weight_list
        } else {
          warning("Bin ", cur_bin, " is outside the expected range (", min_feed_bin, "-", max_feed_bin, "), skipping")
        }
      }
      
      # Process matrix 1: time X animal on animal
      synch_master_animal[[y]][(start_row_number:end_row_number), index_animal] <- 1
      
      # Process matrix 2: time X animal on bin number
      synch_master_bin[[y]][(start_row_number:end_row_number), index_animal] <- cur_bin
    }
  }
  
  if (type == "feed") {
    return(list(synch_master_animal = synch_master_animal,
                synch_master_bin = synch_master_bin,
                synch_master_feed = synch_master_feed))
  } else {
    return(list(synch_master_animal = synch_master_animal,
                synch_master_bin = synch_master_bin))
  }
}

#' Process the current synchronization data to replace NA values and compute total feed
#'
#' @description
#' Processes synchronization data by replacing NA values with appropriate values
#' and computing total feed across all bins.
#'
#' @param cur_synch Data frame representing the current synchronization data
#' @param bins_feed Vector of feed bin IDs used in the analysis
#' @return Data frame with NA values replaced and a new column 'totalFeed'
#' 
#' @examples
#' \dontrun{
#' # Create toy synchronization data
#' time_seq <- seq(lubridate::ymd_hms("2023-01-01 10:00:00"), 
#'                 lubridate::ymd_hms("2023-01-01 10:00:02"), by = "sec")
#' sync_data <- data.frame(
#'   Time = time_seq,
#'   bin1 = c(NA, 2, 3),
#'   bin2 = c(1, NA, 3),
#'   check.names = FALSE
#' )
#' 
#' # Process the data with explicit bin parameters
#' processed <- process_cur_synch(sync_data, bins_feed = 1:2)
#' head(processed)
#' }
#' 
#' @keywords internal
process_cur_synch <- function(cur_synch, bins_feed = bins_feed2()) {
  if (is.null(cur_synch) || nrow(cur_synch) == 0) stop("`cur_synch` cannot be NULL or empty")
  if (!"Time" %in% colnames(cur_synch)) stop("Input matrix must contain a 'Time' column")
  if (ncol(cur_synch) <= 1) stop("Input matrix must have at least one bin column")
  if (!is.numeric(bins_feed) || length(bins_feed) == 0) stop("bins_feed must be a non-empty numeric vector")
  
  # Fix BugBot issue: Calculate bin columns based on actual bins_feed, not total_feed_bin
  bin_column_names <- as.character(bins_feed)
  available_bin_cols <- intersect(bin_column_names, colnames(cur_synch))
  
  if (length(available_bin_cols) == 0) {
    stop("No matching bin columns found in input matrix")
  }
  
  bin_cols_indices <- match(available_bin_cols, colnames(cur_synch))
  
  # Replace initial NA values with first non-NA value
  bin_data <- cur_synch[, bin_cols_indices, drop = FALSE]
  
  if (nrow(bin_data) == 1) {
    first_non_na <- bin_data[1, ]
  } else {
    first_non_na <- apply(bin_data, 2, function(x) {
      non_na_vals <- x[!is.na(x)]
      if (length(non_na_vals) > 0) {
        return(non_na_vals[1])
      } else {
        return(0)  # Fix BugBot issue: default to 0 if all values are NA
      }
    })
  }
  
  cur_synch[1, bin_cols_indices] <- ifelse(is.na(cur_synch[1, bin_cols_indices]), 
                                          first_non_na, 
                                          cur_synch[1, bin_cols_indices])
  
  # Replace NA values with last observed non-NA value
  if (nrow(bin_data) > 1) {
    for (col_idx in bin_cols_indices) {
      col_data <- cur_synch[, col_idx]
      # Only apply na.locf if there are some non-NA values and not all leading NAs
      if (any(!is.na(col_data)) && !is.na(col_data[1])) {
        cur_synch[, col_idx] <- zoo::na.locf(col_data)
      }
    }
  }
  
  # Add totalFeed column
  cur_synch$totalFeed <- rowSums(cur_synch[, bin_cols_indices, drop = FALSE], na.rm = TRUE)
  
  cur_synch
}

#' Process matrices and add derived columns
#'
#' @description
#' Processes synchronization matrices and adds derived columns like total number of animals,
#' total bins occupied, and date information.
#'
#' @param data_list List of data frames to process
#' @param type Character, one of 'feed', 'drink', 'feed_and_drink'
#' @inheritParams set_global_cols
#' @return List containing processed lists of data frames
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
#' # Process matrices with explicit parameters
#' result <- matrix_process(toy_data, type = "feed",
#'                         id_col = "animal", start_col = "start", 
#'                         end_col = "end", bin_col = "bin",
#'                         start_weight_col = "start_weight", 
#'                         end_weight_col = "end_weight", bins_feed = 1:3)
#' names(result)
#' 
#' @export
matrix_process <- function(data_list, 
                          type = "feed",
                          id_col = id_col2(),
                          start_col = start_col2(),
                          end_col = end_col2(),
                          bin_col = bin_col2(),
                          start_weight_col = start_weight_col2(),
                          end_weight_col = end_weight_col2(),
                          bins_feed = bins_feed2(),
                          bins_wat = bins_wat2()) {
  
  if (is.null(data_list) || length(data_list) == 0) stop("`data_list` cannot be NULL or empty")
  if (!type %in% c("feed", "drink", "feed_and_drink")) stop("Type must be one of: 'feed', 'drink', 'feed_and_drink'")
  
  results <- matrix_initialize(data_list, type, id_col, start_col, end_col, bin_col, 
                              start_weight_col, end_weight_col, bins_feed, bins_wat)
  synch_master_animal <- results$synch_master_animal
  synch_master_bin <- results$synch_master_bin
  
  if (type == "feed") {
    synch_master_feed <- results$synch_master_feed
  }
  
  # Create duplicates for processed versions
  synch_master_animal2 <- synch_master_animal
  synch_master_bin2 <- synch_master_bin
  if (type == "feed") {
    synch_master_feed2 <- synch_master_feed
  }
  
  for (i in seq_along(synch_master_animal)) {
    # Only compute rowSums if there are at least two columns (Time + at least one animal)
    if (ncol(synch_master_animal[[i]]) > 1) {
      synch_master_animal[[i]]$total_animal_num <- rowSums(synch_master_animal[[i]][, 2:ncol(synch_master_animal[[i]]), drop = FALSE], na.rm = TRUE)
      synch_master_animal[[i]]$total_bin_occupied <- synch_master_animal[[i]]$total_animal_num
      
      if (type == "feed") {
        total_feed_bins <- length(bins_feed)
        synch_master_animal[[i]]$empty_bin_num <- total_feed_bins - synch_master_animal[[i]]$total_bin_occupied
      } else if (type == "drink") {
        total_water_bins <- length(bins_wat)
        synch_master_animal[[i]]$empty_bin_num <- total_water_bins - synch_master_animal[[i]]$total_bin_occupied
      } else {
        synch_master_animal[[i]]$empty_bin_num <- NA
      }
    } else {
      synch_master_animal[[i]]$total_animal_num <- 0
      synch_master_animal[[i]]$total_bin_occupied <- 0
      synch_master_animal[[i]]$empty_bin_num <- if (type == "feed") length(bins_feed) else if (type == "drink") length(bins_wat) else NA
    }
    
    # Delete the time when no animal is feeding/drinking
    records_to_keep <- which(synch_master_animal[[i]]$total_animal_num > 0)
    synch_master_animal2[[i]] <- synch_master_animal[[i]][records_to_keep, , drop = FALSE]
    synch_master_bin2[[i]] <- synch_master_bin[[i]][records_to_keep, , drop = FALSE]
    
    if (type == "feed") {
      synch_master_feed2[[i]] <- synch_master_feed[[i]][records_to_keep, , drop = FALSE]
      
      # Add date
      synch_master_feed2[[i]]$date <- lubridate::date(synch_master_feed2[[i]]$Time)
      
      # Fill in feed amount at each second at each bin
      synch_master_feed2[[i]] <- process_cur_synch(synch_master_feed2[[i]], bins_feed)
    }
    
    # Add date to other matrices
    synch_master_animal2[[i]]$date <- lubridate::date(synch_master_animal2[[i]]$Time)
    synch_master_bin2[[i]]$date <- lubridate::date(synch_master_bin2[[i]]$Time)
  }
  
  if (type == "feed") {
    return(list(synch_master_animal2 = synch_master_animal2,
                synch_master_bin2 = synch_master_bin2,
                synch_master_feed2 = synch_master_feed2))
  } else {
    return(list(synch_master_animal2 = synch_master_animal2,
                synch_master_bin2 = synch_master_bin2))
  }
} 