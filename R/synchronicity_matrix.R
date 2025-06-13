#' @title Synchronicity Matrix Functions
#' @description Functions for processing combined feeding and drinking synchronicity matrices.

#' Calculate Total Cows Present at Each Time Point
#'
#' This function calculates the number of cows present at each second in the feeding/drinking data.
#' It adds derived columns for total cows, occupied bins, and empty bins.
#'
#' @param feed_drink_synch_master_cow A list of data frames containing feeding/drinking data,
#'                                   where each data frame has time on rows and cows on columns.
#' @param total_fed_wat_bin Integer. Total number of feed and water bins in the system.
#'
#' @return A list of data frames with additional columns:
#'         \itemize{
#'           \item total_cow_num: Number of cows present at each time point
#'           \item total_bin_occupied: Number of bins occupied at each time point
#'           \item empty_bin_num: Number of empty bins at each time point
#'         }
#' @examples
#' # Create sample data
#' sample_data <- list(
#'   data.frame(
#'     Time = seq(as.POSIXct("2024-01-01"), by = "sec", length.out = 3),
#'     Cow1 = c(1, 0, 1),
#'     Cow2 = c(0, 1, 1)
#'   )
#' )
#' # Calculate total cows present
#' result <- total_cows_present(sample_data, total_fed_wat_bin = 10)
#' @export
total_cows_present <- function(feed_drink_synch_master_cow, total_fed_wat_bin) {
  # Input validation
  if (length(feed_drink_synch_master_cow) == 0) {
    stop("Input list is empty.")
  }
  if (total_fed_wat_bin <= 0) {
    stop("total_fed_wat_bin must be positive.")
  }
  if (!is.numeric(total_fed_wat_bin)) {
    stop("total_fed_wat_bin must be numeric.")
  }
  
  message("Calculating total cows present...")
  new_list <- list()
  for (y in 1:length(feed_drink_synch_master_cow)) {
    cur_data <- feed_drink_synch_master_cow[[y]]
    if (!all(c("Time") %in% names(cur_data))) {
      stop("Each data frame must contain a 'Time' column.")
    }
    cur_data$total_cow_num <- rowSums(cur_data[, 2:ncol(cur_data), drop = FALSE], na.rm = TRUE)
    cur_data$total_bin_occupied <- cur_data$total_cow_num
    cur_data$empty_bin_num <- total_fed_wat_bin - cur_data$total_bin_occupied
    new_list[[y]] <- cur_data
  }
  message("Calculation complete.")
  return(new_list)
}

#' Remove Inactive Time Periods
#'
#' This function removes time periods when no cows are feeding or drinking from the data.
#'
#' @param feed_drink_synch_master_cow A list of data frames containing feeding/drinking data,
#'                                   where each data frame has time on rows and cows on columns.
#' @param feed_drink_synch_master_bin A list of data frames containing bin assignments,
#'                                   where each data frame has time on rows and cows on columns.
#'
#' @return A list containing two lists of data frames:
#'         \itemize{
#'           \item new_cow_list: Filtered cow presence data
#'           \item new_bin_list: Filtered bin assignment data
#'         }
#' @examples
#' # Create sample data
#' sample_cow_data <- list(
#'   data.frame(
#'     Time = seq(as.POSIXct("2024-01-01"), by = "sec", length.out = 3),
#'     Cow1 = c(1, 0, 1),
#'     Cow2 = c(0, 1, 1),
#'     total_cow_num = c(1, 1, 2)
#'   )
#' )
#' sample_bin_data <- list(
#'   data.frame(
#'     Time = seq(as.POSIXct("2024-01-01"), by = "sec", length.out = 3),
#'     Cow1 = c(1, 0, 1),
#'     Cow2 = c(0, 1, 1)
#'   )
#' )
#' # Remove inactive time periods
#' result <- delete_inactive_time(sample_cow_data, sample_bin_data)
#' @export
delete_inactive_time <- function(feed_drink_synch_master_cow, feed_drink_synch_master_bin) {
  # Input validation
  if (length(feed_drink_synch_master_cow) == 0 || length(feed_drink_synch_master_bin) == 0) {
    stop("Input lists cannot be empty.")
  }
  if (length(feed_drink_synch_master_cow) != length(feed_drink_synch_master_bin)) {
    stop("Cow and bin data lists must have the same length.")
  }
  
  message("Removing inactive time periods...")
  new_cow_list <- list()
  new_bin_list <- list()
  
  for (y in 1:length(feed_drink_synch_master_bin)) {
    cur_data_cow <- feed_drink_synch_master_cow[[y]]
    cur_data_bin <- feed_drink_synch_master_bin[[y]]
    
    if (!all(c("Time", "total_cow_num") %in% names(cur_data_cow))) {
      stop("Cow data must contain 'Time' and 'total_cow_num' columns.")
    }

    records_to_keep <- which(cur_data_cow$total_cow_num > 0)
    if (length(records_to_keep) > 0) {
      cur_data_cow2 <- cur_data_cow[records_to_keep, ]
      cur_data_bin2 <- cur_data_bin[records_to_keep, ]
      new_cow_list[[y]] <- cur_data_cow2
      new_bin_list[[y]] <- cur_data_bin2
    }
  }
  
  message("Inactive time periods removed.")
  return(list(new_cow_list, new_bin_list))
}

#' Add Date Information to Matrices
#'
#' This function adds a date column to the cow and bin matrices based on the Time column.
#'
#' @param feed_drink_synch_master_cow2 A list of data frames containing filtered feeding/drinking data.
#' @param feed_drink_synch_master_bin2 A list of data frames containing filtered bin assignments.
#'
#' @return A list containing two lists of data frames with added date columns:
#'         \itemize{
#'           \item new_cow_list: Cow presence data with dates
#'           \item new_bin_list: Bin assignment data with dates
#'         }
#' @examples
#' # Create sample data
#' sample_cow_data <- list(
#'   data.frame(
#'     Time = seq(as.POSIXct("2024-01-01"), by = "sec", length.out = 3),
#'     Cow1 = c(1, 0, 1),
#'     Cow2 = c(0, 1, 1)
#'   )
#' )
#' sample_bin_data <- list(
#'   data.frame(
#'     Time = seq(as.POSIXct("2024-01-01"), by = "sec", length.out = 3),
#'     Cow1 = c(1, 0, 1),
#'     Cow2 = c(0, 1, 1)
#'   )
#' )
#' # Add date information
#' result <- add_date(sample_cow_data, sample_bin_data)
#' @export
add_date <- function(feed_drink_synch_master_cow2, feed_drink_synch_master_bin2) {
  # Input validation
  if (length(feed_drink_synch_master_cow2) == 0 || length(feed_drink_synch_master_bin2) == 0) {
    stop("Input lists cannot be empty.")
  }
  if (length(feed_drink_synch_master_cow2) != length(feed_drink_synch_master_bin2)) {
    stop("Cow and bin data lists must have the same length.")
  }
  
  message("Adding date information...")
  new_cow_list <- list()
  new_bin_list <- list()
  
  for (y in 1:length(feed_drink_synch_master_cow2)) {
    cur_data_cow <- feed_drink_synch_master_cow2[[y]]
    cur_data_bin <- feed_drink_synch_master_bin2[[y]]
    
    if (!all(c("Time") %in% names(cur_data_cow)) || !all(c("Time") %in% names(cur_data_bin))) {
      stop("Both cow and bin data must contain a 'Time' column.")
    }
    
    cur_data_cow$date <- lubridate::date(cur_data_cow$Time)
    cur_data_bin$date <- lubridate::date(cur_data_bin$Time)

    new_cow_list[[y]] <- cur_data_cow
    new_bin_list[[y]] <- cur_data_bin

    names(new_cow_list)[y] <- as.character(lubridate::date(cur_data_cow$Time[1]))
    names(new_bin_list)[y] <- as.character(lubridate::date(cur_data_bin$Time[1]))
  }
  
  message("Date information added.")
  return(list(new_cow_list, new_bin_list))
}

#' Update Bin Numbers for Combined Feed/Water Analysis
#'
#' This function updates bin numbers to ensure unique identification across feed and water bins.
#' Water bins (101-105) are mapped to new numbers (207, 208, 221, 222, 235).
#' Feed bins (1-30) are mapped to new numbers based on their range.
#'
#' @param feed_drink_synch_master_bin2 A list of data frames containing bin assignments.
#'
#' @return A list of data frames with updated bin numbers.
#' @examples
#' # Create sample data
#' sample_data <- list(
#'   data.frame(
#'     Time = seq(as.POSIXct("2024-01-01"), by = "sec", length.out = 3),
#'     Cow1 = c(101, 1, 2),
#'     Cow2 = c(102, 3, 4)
#'   )
#' )
#' # Update bin numbers
#' result <- bin_update(sample_data)
#' @export
bin_update <- function(feed_drink_synch_master_bin2) {
  # Input validation
  if (length(feed_drink_synch_master_bin2) == 0) {
    stop("Input list is empty.")
  }
  
  message("Updating bin numbers...")
  new_list_bin <- list()
  
  for (y in 1:length(feed_drink_synch_master_bin2)) {
    cur_data <- feed_drink_synch_master_bin2[[y]]
    
    if (!all(c("Time") %in% names(cur_data))) {
      stop("Data must contain a 'Time' column.")
    }

    # Update water bin numbers
    cur_data[cur_data == 101] <- 207
    cur_data[cur_data == 102] <- 208
    cur_data[cur_data == 103] <- 221
    cur_data[cur_data == 104] <- 222
    cur_data[cur_data == 105] <- 235

    # Update feed bin numbers
    for (u in 1:30) {
      if (u <= 6) {
        cur_data[cur_data == u] <- (u + 200)
      } else if (u <= 18) {
        cur_data[cur_data == u] <- (u + 202)
      } else {
        cur_data[cur_data == u] <- (u + 204)
      }
    }
    
    new_list_bin[[y]] <- cur_data
    names(new_list_bin)[y] <- as.character(lubridate::date(cur_data$Time[1]))
  }
  
  message("Bin numbers updated.")
  return(new_list_bin)
}

#' Process Combined Feed/Water Synchronicity Matrices
#'
#' This function processes the feeding and drinking data by:
#' \itemize{
#'   \item Initializing matrices
#'   \item Calculating total cows present
#'   \item Removing inactive time periods
#'   \item Adding date information
#'   \item Updating bin numbers
#' }
#'
#' @param all.comb2 A list of data frames containing combined feeding and drinking data.
#' @param total_fed_wat_bin Integer. Total number of feed and water bins in the system.
#'
#' @return A list containing two processed matrices:
#'         \itemize{
#'           \item feed_drink_synch_master_cow2: Processed cow presence matrix
#'           \item feed_drink_synch_master_bin2: Processed bin assignment matrix
#'         }
#' @examples
#' # Create sample data
#' sample_data <- list(
#'   data.frame(
#'     Start = seq(as.POSIXct("2024-01-01"), by = "sec", length.out = 3),
#'     End = seq(as.POSIXct("2024-01-01"), by = "sec", length.out = 3) + 60,
#'     Cow = c("Cow1", "Cow2", "Cow1"),
#'     Bin = c(101, 1, 2)
#'   )
#' )
#' # Process matrices
#' result <- feed_drink_matrix_process(sample_data, total_fed_wat_bin = 10)
#' @export
feed_drink_matrix_process <- function(all.comb2, total_fed_wat_bin) {
  # Input validation
  if (length(all.comb2) == 0) {
    stop("Input list is empty.")
  }
  if (total_fed_wat_bin <= 0) {
    stop("total_fed_wat_bin must be positive.")
  }
  if (!is.numeric(total_fed_wat_bin)) {
    stop("total_fed_wat_bin must be numeric.")
  }
  
  # Validate required columns in input data
  required_cols <- c("Start", "End", "Cow", "Bin")
  for (i in seq_along(all.comb2)) {
    if (!all(required_cols %in% names(all.comb2[[i]]))) {
      stop(sprintf("Data frame %d is missing required columns: %s", 
                  i, 
                  paste(setdiff(required_cols, names(all.comb2[[i]])), collapse = ", ")))
    }
  }
  
  message("Processing feed/drink matrices...")
  
  # Initialize matrices
  message("Initializing matrices...")
  initialized_matrix <- matrix_initialize(all.comb2, type = "feed_and_drink")
  feed_drink_synch_master_cow <- initialized_matrix[[1]]
  feed_drink_synch_master_bin <- initialized_matrix[[2]]

  # Calculate total cows present
  message("Calculating total cows present...")
  feed_drink_synch_master_cow <- total_cows_present(feed_drink_synch_master_cow, total_fed_wat_bin)

  # Remove inactive time periods
  message("Removing inactive time periods...")
  results_del <- delete_inactive_time(feed_drink_synch_master_cow, feed_drink_synch_master_bin)
  feed_drink_synch_master_cow2 <- results_del[[1]]
  feed_drink_synch_master_bin2 <- results_del[[2]]

  # Add date information
  message("Adding date information...")
  results_add_date <- add_date(feed_drink_synch_master_cow2, feed_drink_synch_master_bin2)
  feed_drink_synch_master_cow2 <- results_add_date[[1]]
  feed_drink_synch_master_bin2 <- results_add_date[[2]]

  # Update bin numbers
  message("Updating bin numbers...")
  feed_drink_synch_master_bin2 <- bin_update(feed_drink_synch_master_bin2)

  message("Matrix processing complete.")
  return(list(feed_drink_synch_master_cow2, feed_drink_synch_master_bin2))
} 