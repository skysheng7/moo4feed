#' Feed & Drink Combined Synchronicity Matrix Functions
#'
#' Functions for processing combined feeding and drinking synchronicity matrices.
#'
#' @section Functions:
#' - total_animals_present
#' - delete_inactive_time
#' - add_date
#' - bin_update
#' - feed_drink_matrix_process
#'
#' @name synch_feed_drink_combined
#' @keywords internal
NULL

#' Calculate total number of animals present at each time point
#'
#' @description
#' Iterates through synchronicity matrices and calculates the total number of animals
#' present (feeding/drinking) at each second, along with bin occupancy statistics.
#'
#' @param feed_drink_synch_master_animal List of data frames with animals on columns and time on rows
#' @inheritParams set_global_cols
#' @return List of data frames with added columns:
#'   \itemize{
#'     \item total_animal_num: Total number of animals present at each time
#'     \item total_bin_occupied: Same as total_animal_num (each animal occupies one bin)
#'     \item empty_bin_num: Number of empty bins at each time
#'   }
#' 
#' @keywords internal
total_animals_present <- function(feed_drink_synch_master_animal,
                                 bins_feed = bins_feed2(),
                                 bins_wat = bins_wat2()) {
  
  # Input validation
  if (is.null(feed_drink_synch_master_animal) || length(feed_drink_synch_master_animal) == 0) {
    stop("`feed_drink_synch_master_animal` cannot be NULL or empty")
  }
  
  if (!is.numeric(bins_feed) || !is.numeric(bins_wat)) {
    stop("`bins_feed` and `bins_wat` must be numeric")
  }
  
  if (length(bins_feed) == 0 || length(bins_wat) == 0) {
    stop("`bins_feed` and `bins_wat` cannot be empty")
  }
  
  # Calculate total bins available (feed + water)
  total_fed_wat_bin <- length(bins_feed) + length(bins_wat)
  
  new_list <- list()
  
  for (y in seq_along(feed_drink_synch_master_animal)) {
    cur_data <- feed_drink_synch_master_animal[[y]]
    
    # Validate current data frame
    if (!is.data.frame(cur_data)) {
      stop("Each element in `feed_drink_synch_master_animal` must be a data frame")
    }
    
    if (!"Time" %in% names(cur_data)) {
      stop("Each data frame must have a 'Time' column")
    }
    
    if (ncol(cur_data) < 2) {
      stop("Each data frame must have at least one animal column")
    }
    
    # Calculate total animals present at each time point
    # Only sum animal columns (exclude Time column)
    animal_cols <- 2:ncol(cur_data)
    cur_data$total_animal_num <- rowSums(cur_data[, animal_cols, drop = FALSE], na.rm = TRUE)
    
    # Each animal occupies one bin, so total_bin_occupied equals total_animal_num
    cur_data$total_bin_occupied <- cur_data$total_animal_num
    
    # Calculate empty bins
    cur_data$empty_bin_num <- total_fed_wat_bin - cur_data$total_bin_occupied
    
    new_list[[y]] <- cur_data
    
    # Preserve original names if they exist
    if (!is.null(names(feed_drink_synch_master_animal)[y])) {
      names(new_list)[y] <- names(feed_drink_synch_master_animal)[y]
    }
  }
  
  new_list
}

#' Remove time periods when no animals are active
#'
#' @description
#' Filters synchronicity matrices to keep only time periods when at least one animal
#' is feeding or drinking, removing inactive periods to reduce data size.
#'
#' @param feed_drink_synch_master_animal List of data frames with animals on columns and time on rows
#' @param feed_drink_synch_master_bin List of data frames with animals on columns and bin numbers on rows
#' @return List containing two elements:
#'   \itemize{
#'     \item First element: Filtered animal matrices with only active time periods
#'     \item Second element: Filtered bin matrices with only active time periods
#'   }
#' 
#' @keywords internal
delete_inactive_time <- function(feed_drink_synch_master_animal, 
                                feed_drink_synch_master_bin) {
  
  # Input validation
  if (is.null(feed_drink_synch_master_animal) || length(feed_drink_synch_master_animal) == 0) {
    stop("`feed_drink_synch_master_animal` cannot be NULL or empty")
  }
  
  if (is.null(feed_drink_synch_master_bin) || length(feed_drink_synch_master_bin) == 0) {
    stop("`feed_drink_synch_master_bin` cannot be NULL or empty")
  }
  
  if (length(feed_drink_synch_master_animal) != length(feed_drink_synch_master_bin)) {
    stop("feed_drink_synch_master_animal and feed_drink_synch_master_bin must have the same length")
  }
  
  new_animal_list <- list()
  new_bin_list <- list()
  
  for (y in seq_along(feed_drink_synch_master_animal)) {
    cur_data_animal <- feed_drink_synch_master_animal[[y]]
    cur_data_bin <- feed_drink_synch_master_bin[[y]]
    
    # Validate current data frames
    if (!is.data.frame(cur_data_animal) || !is.data.frame(cur_data_bin)) {
      stop("Each element must be a data frame")
    }
    
    if (!"total_animal_num" %in% names(cur_data_animal)) {
      stop("Animal data frames must have a 'total_animal_num' column (run total_animals_present first)")
    }
    
    if (nrow(cur_data_animal) != nrow(cur_data_bin)) {
      stop("Corresponding animal and bin data frames must have the same number of rows")
    }
    
    # Find records where animals are active (total_animal_num > 0)
    records_to_keep <- which(cur_data_animal[["total_animal_num"]] > 0)
    
    if (length(records_to_keep) > 0) {
      cur_data_animal_filtered <- cur_data_animal[records_to_keep, , drop = FALSE]
      cur_data_bin_filtered <- cur_data_bin[records_to_keep, , drop = FALSE]
      
      new_animal_list[[y]] <- cur_data_animal_filtered
      new_bin_list[[y]] <- cur_data_bin_filtered
      
      # Preserve original names if they exist
      if (!is.null(names(feed_drink_synch_master_animal)[y])) {
        names(new_animal_list)[y] <- names(feed_drink_synch_master_animal)[y]
        names(new_bin_list)[y] <- names(feed_drink_synch_master_bin)[y]
      }
    } else {
      # If no active periods, create empty data frames with same structure
      empty_animal <- cur_data_animal[0, , drop = FALSE]
      empty_bin <- cur_data_bin[0, , drop = FALSE]
      
      new_animal_list[[y]] <- empty_animal
      new_bin_list[[y]] <- empty_bin
      
      if (!is.null(names(feed_drink_synch_master_animal)[y])) {
        names(new_animal_list)[y] <- names(feed_drink_synch_master_animal)[y]
        names(new_bin_list)[y] <- names(feed_drink_synch_master_bin)[y]
      }
    }
  }
  
  list(new_animal_list, new_bin_list)
}

#' Add date column to synchronicity matrices
#'
#' @description
#' Adds a date column to both animal and bin synchronicity matrices by extracting
#' the date from the Time column, and updates list names to match dates.
#'
#' @param feed_drink_synch_master_animal List of data frames with animals on columns and time on rows
#' @param feed_drink_synch_master_bin List of data frames with animals on columns and bin numbers on rows
#' @return List containing two elements:
#'   \itemize{
#'     \item First element: Animal matrices with added date column and updated names
#'     \item Second element: Bin matrices with added date column and updated names
#'   }
#' 
#' @keywords internal
add_date <- function(feed_drink_synch_master_animal, 
                    feed_drink_synch_master_bin) {
  
  # Input validation
  if (is.null(feed_drink_synch_master_animal) || length(feed_drink_synch_master_animal) == 0) {
    stop("`feed_drink_synch_master_animal` cannot be NULL or empty")
  }
  
  if (is.null(feed_drink_synch_master_bin) || length(feed_drink_synch_master_bin) == 0) {
    stop("`feed_drink_synch_master_bin` cannot be NULL or empty")
  }
  
  if (length(feed_drink_synch_master_animal) != length(feed_drink_synch_master_bin)) {
    stop("feed_drink_synch_master_animal and feed_drink_synch_master_bin must have the same length")
  }
  
  new_animal_list <- list()
  new_bin_list <- list()
  
  for (y in seq_along(feed_drink_synch_master_animal)) {
    cur_data_animal <- feed_drink_synch_master_animal[[y]]
    cur_data_bin <- feed_drink_synch_master_bin[[y]]
    
    # Validate current data frames
    if (!is.data.frame(cur_data_animal) || !is.data.frame(cur_data_bin)) {
      stop("Each element must be a data frame")
    }
    
    if (!"Time" %in% names(cur_data_animal) || !"Time" %in% names(cur_data_bin)) {
      stop("Both animal and bin data frames must have a 'Time' column")
    }
    
    if (nrow(cur_data_animal) != nrow(cur_data_bin)) {
      stop("Corresponding animal and bin data frames must have the same number of rows")
    }
    
    # Validate Time column is POSIXct
    if (!lubridate::is.POSIXct(cur_data_animal[["Time"]]) || !lubridate::is.POSIXct(cur_data_bin[["Time"]])) {
      stop("Time columns must be POSIXct")
    }
    
    # Add date column to both data frames
    if (nrow(cur_data_animal) > 0) {
      cur_data_animal$date <- lubridate::date(cur_data_animal[["Time"]])
      cur_data_bin$date <- lubridate::date(cur_data_bin[["Time"]])
      
      # Use the first date for naming
      date_name <- as.character(lubridate::date(cur_data_animal[["Time"]][1]))
    } else {
      # Handle empty data frames
      cur_data_animal$date <- as.Date(character(0))
      cur_data_bin$date <- as.Date(character(0))
      
      # Use a default name for empty data frames
      date_name <- paste0("empty_", y)
    }
    
    new_animal_list[[y]] <- cur_data_animal
    new_bin_list[[y]] <- cur_data_bin
    
    # Set names based on date
    names(new_animal_list)[y] <- date_name
    names(new_bin_list)[y] <- date_name
  }
  
  list(new_animal_list, new_bin_list)
}

#' Update bin numbers for combined feed and water analysis
#'
#' @description
#' Updates bin numbers in synchronicity matrices by remapping water bins and feed bins
#' to new numbering schemes for combined analysis. This standardizes bin numbering
#' across different bin types.
#'
#' @param feed_drink_synch_master_bin List of data frames with animals on columns and bin numbers on rows
#' @inheritParams set_global_cols
#' @return List of data frames with updated bin numbers and date-based names
#' 
#' @details
#' The function performs the following bin number updates:
#' \itemize{
#'   \item Water bins: Maps bins_wat to new sequential numbers starting from 200
#'   \item Feed bins: Maps bins_feed to new sequential numbers with offsets based on position
#' }
#' 
#' @keywords internal
bin_update <- function(feed_drink_synch_master_bin,
                      bins_feed = bins_feed2(),
                      bins_wat = bins_wat2()) {
  
  # Input validation
  if (is.null(feed_drink_synch_master_bin) || length(feed_drink_synch_master_bin) == 0) {
    stop("`feed_drink_synch_master_bin` cannot be NULL or empty")
  }
  
  if (!is.numeric(bins_feed) || !is.numeric(bins_wat)) {
    stop("`bins_feed` and `bins_wat` must be numeric")
  }
  
  if (length(bins_feed) == 0 || length(bins_wat) == 0) {
    stop("`bins_feed` and `bins_wat` cannot be empty")
  }
  
  new_list_bin <- list()
  
  for (y in seq_along(feed_drink_synch_master_bin)) {
    cur_data <- feed_drink_synch_master_bin[[y]]
    
    # Validate current data frame
    if (!is.data.frame(cur_data)) {
      stop("Each element must be a data frame")
    }
    
    if (!"Time" %in% names(cur_data)) {
      stop("Each data frame must have a 'Time' column")
    }
    
    # Create a copy to avoid modifying original data
    cur_data_updated <- cur_data
    
    # Update water bin numbers (hard-coded mapping from original)
    # Original mapping: 101->207, 102->208, 103->221, 104->222, 105->235
    water_mapping <- list(
      "101" = 207, "102" = 208, "103" = 221, "104" = 222, "105" = 235
    )
    
    for (i in seq_along(bins_wat)) {
      old_bin <- bins_wat[i]
      # Use hard-coded mapping if available, otherwise sequential from 207
      if (as.character(old_bin) %in% names(water_mapping)) {
        new_bin <- water_mapping[[as.character(old_bin)]]
      } else {
        new_bin <- 200 + i + 6  # Start from 207 for unmapped bins
      }
      
      # Replace all occurrences of old_bin with new_bin in all columns except Time and date
      non_time_cols <- setdiff(names(cur_data_updated), c("Time", "date"))
      for (col in non_time_cols) {
        cur_data_updated[[col]][cur_data_updated[[col]] == old_bin] <- new_bin
      }
    }
    
    # Update feed bin numbers with value-based offsets (not position-based)
    for (old_bin in bins_feed) {
      # Apply offset based on bin value (matching original logic)
      if (old_bin <= 6) {
        new_bin <- old_bin + 200
      } else if (old_bin <= 18) {
        new_bin <- old_bin + 202
      } else {
        new_bin <- old_bin + 204
      }
      
      # Replace all occurrences of old_bin with new_bin in all columns except Time and date
      non_time_cols <- setdiff(names(cur_data_updated), c("Time", "date"))
      for (col in non_time_cols) {
        cur_data_updated[[col]][cur_data_updated[[col]] == old_bin] <- new_bin
      }
    }
    
    new_list_bin[[y]] <- cur_data_updated
    
    # Set names based on date if available, otherwise preserve original names
    if ("date" %in% names(cur_data_updated) && nrow(cur_data_updated) > 0) {
      date_name <- as.character(cur_data_updated[["date"]][1])
      names(new_list_bin)[y] <- date_name
    } else if (!is.null(names(feed_drink_synch_master_bin)[y])) {
      names(new_list_bin)[y] <- names(feed_drink_synch_master_bin)[y]
    }
  }
  
  new_list_bin
}

#' Process feed and drink synchronicity matrices
#'
#' @description
#' Main orchestrator function that processes feeding and drinking data by creating
#' synchronicity matrices and applying all necessary transformations including
#' counting animals present, removing inactive time, adding dates, and updating bin numbers.
#'
#' @param all_comb List of data frames containing feeding and drinking information for each animal
#' @inheritParams set_global_cols
#' @return List containing two elements:
#'   \itemize{
#'     \item First element: Animal synchronicity matrices (processed and filtered)
#'     \item Second element: Bin synchronicity matrices (processed with updated bin numbers)
#'   }
#' 
#' @details
#' This function performs the complete workflow:
#' \enumerate{
#'   \item Initialize matrices using \code{matrix_initialize}
#'   \item Count total animals present using \code{total_animals_present}
#'   \item Remove inactive time periods using \code{delete_inactive_time}
#'   \item Add date columns using \code{add_date}
#'   \item Update bin numbers using \code{bin_update}
#' }
#' 
#' @examples
#' # Create toy feeding/drinking data (interval format)
#' toy_data <- list(
#'   day1 = data.frame(
#'     cow = c(1, 1, 2, 2),
#'     start = lubridate::ymd_hms(c("2023-01-01 10:00:00", "2023-01-01 10:00:03", 
#'                                  "2023-01-01 10:00:00", "2023-01-01 10:00:04")),
#'     end = lubridate::ymd_hms(c("2023-01-01 10:00:01", "2023-01-01 10:00:04", 
#'                                "2023-01-01 10:00:02", "2023-01-01 10:00:05")),
#'     bin = c(1, 101, 2, 102)
#'   )
#' )
#' 
#' # Process the data (uses default column names: cow, start, end, bin)
#' result <- feed_drink_matrix_process(toy_data, bins_feed = 1:30, bins_wat = 101:105)
#' # Returns processed animal and bin matrices
#' 
#' @export
feed_drink_matrix_process <- function(all_comb,
                                     id_col = id_col2(),
                                     start_col = start_col2(),
                                     end_col = end_col2(),
                                     bin_col = bin_col2(),
                                     bins_feed = bins_feed2(),
                                     bins_wat = bins_wat2()) {
  
  # Input validation
  if (is.null(all_comb) || length(all_comb) == 0) {
    stop("`all_comb` cannot be NULL or empty")
  }
  
  if (!is.list(all_comb)) {
    stop("`all_comb` must be a list")
  }
  
  # Validate that each element is a data frame with required columns
  for (i in seq_along(all_comb)) {
    if (!is.data.frame(all_comb[[i]])) {
      stop("Each element in `all_comb` must be a data frame")
    }
    
    required_cols <- c(id_col, start_col, end_col, bin_col)
    if (!all(required_cols %in% names(all_comb[[i]]))) {
      stop(paste("Each data frame must have columns:", paste(required_cols, collapse = ", ")))
    }
  }
  
  # Step 1: Initialize matrices
  initialized_matrix <- matrix_initialize(all_comb, 
                                         type = "feed_and_drink",
                                         id_col = id_col,
                                         start_col = start_col,
                                         end_col = end_col,
                                         bin_col = bin_col,
                                         bins_feed = bins_feed,
                                         bins_wat = bins_wat)
  
  feed_drink_synch_master_animal <- initialized_matrix[[1]]
  feed_drink_synch_master_bin <- initialized_matrix[[2]]
  
  # Step 2: Count total animals present
  feed_drink_synch_master_animal <- total_animals_present(
    feed_drink_synch_master_animal, 
    bins_feed = bins_feed, 
    bins_wat = bins_wat
  )
  
  # Step 3: Remove inactive time periods
  results_del <- delete_inactive_time(
    feed_drink_synch_master_animal, 
    feed_drink_synch_master_bin
  )
  
  feed_drink_synch_master_animal2 <- results_del[[1]]
  feed_drink_synch_master_bin2 <- results_del[[2]]
  
  # Step 4: Add date columns
  results_add_date <- add_date(
    feed_drink_synch_master_animal2,
    feed_drink_synch_master_bin2
  )
  
  feed_drink_synch_master_animal2 <- results_add_date[[1]]
  feed_drink_synch_master_bin2 <- results_add_date[[2]]
  
  # Step 5: Update bin numbers
  feed_drink_synch_master_bin2 <- bin_update(
    feed_drink_synch_master_bin2,
    bins_feed = bins_feed,
    bins_wat = bins_wat
  )
  
  list(feed_drink_synch_master_animal2, feed_drink_synch_master_bin2)
} 