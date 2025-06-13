#' @title Matrix Processing Functions
#' @description Functions for processing and initializing synchronization matrices for feed and water data analysis.
#' @importFrom lubridate date
#' @importFrom zoo na.locf
#' @importFrom zoo na.locf.default

#' Generate empty Synchronization Matrices for Feed/water Data
#'
#' This function creates empty synchronization matrices for time-cow, time-bin, and time-feed relationships
#' based on the input data. The matrices are used to track feeding/drinking behavior over time.
#'
#' @param data_list A list of data frames containing feed/water data, grouped by date.
#'                 Each data frame should have columns: Start, End, Cow, Bin.
#' @param min_feed_bin Integer. Minimum feeder bin value to keep.
#' @param max_feed_bin Integer. Maximum feeder bin value to keep.
#' @param type Character. Specifies the type of synchronicity analysis:
#'        \itemize{
#'          \item "feed" - For feeding data only
#'          \item "drink" - For drinking data only
#'          \item "feed_and_drink" - For combined feeding and drinking data
#'        }
#'
#' @return A list containing:
#'         \itemize{
#'           \item synch_master_cow: Matrix of time vs cow presence (1 = present, 0 = absent)
#'           \item synch_master_bin: Matrix of time vs bin number for each cow
#'           \item synch_master_feed: Matrix of time vs feed amount at each bin (only for type="feed")
#'         }
#' @export
empty_synch_matrix <- function(data_list, min_feed_bin, max_feed_bin, type) {
  synch_master_cow <- list()
  synch_master_bin <- list()
  synch_master_feed <- list()
  
  for (y in 1:length(data_list)) {
    cur_data <- data_list[[y]]
    cur_data <- cur_data[order(cur_data$Start, cur_data$End), ]
    dateTime_seq <- create_time_sequence(cur_data)

    cow_time_matrix <- prepare_time_cow_matrix(cur_data, dateTime_seq)
    time_bin_matrix <- prepare_time_bin_matrix(cow_time_matrix)

    synch_master_cow[[y]] <- cow_time_matrix
    synch_master_bin[[y]] <- time_bin_matrix

    # rename the list name
    names(synch_master_cow)[y] <- names(data_list)[y]
    names(synch_master_bin)[y] <- names(data_list)[y]

    if (type == "feed") {
      time_feed_matrix <- prepare_time_feed_matrix(dateTime_seq, min_feed_bin, max_feed_bin)
      synch_master_feed[[y]] <- time_feed_matrix
      names(synch_master_feed)[y] <- names(data_list)[y]
    }
  }

  if (type == "feed") {
    return(list(synch_master_cow = synch_master_cow,
                synch_master_bin = synch_master_bin,
                synch_master_feed = synch_master_feed))
  } else {
    return(list(synch_master_cow = synch_master_cow,
                synch_master_bin = synch_master_bin))
  }
}

#' Initialize and Process Synchronization Matrices
#'
#' This function initializes and populates synchronization matrices with feed/water data.
#' It processes three types of matrices:
#' \itemize{
#'   \item Time vs CowID matrix (which cow is eating/drinking)
#'   \item Time vs CowID matrix (which bin the cow is at)
#'   \item Time vs Bin matrix (feed amount at each bin)
#' }
#'
#' @param data_list A list of data frames containing feed/water data, grouped by date.
#'                 Each data frame should have columns: Start, End, Cow, Bin, Startweight, Endweight.
#' @param min_feed_bin Integer. Minimum feeder bin value to keep.
#' @param max_feed_bin Integer. Maximum feeder bin value to keep.
#' @param type Character. Specifies the type of synchronicity analysis:
#'        \itemize{
#'          \item "feed" - For feeding data only
#'          \item "drink" - For drinking data only
#'          \item "feed_and_drink" - For combined feeding and drinking data
#'        }
#'
#' @return A list containing populated matrices:
#'         \itemize{
#'           \item synch_master_cow: Matrix of time vs cow presence (1 = present, 0 = absent)
#'           \item synch_master_bin: Matrix of time vs bin number for each cow
#'           \item synch_master_feed: Matrix of time vs feed amount at each bin (only for type="feed")
#'         }
#' @export
matrix_initialize <- function(data_list, min_feed_bin, max_feed_bin, type) {
  results <- empty_synch_matrix(data_list, min_feed_bin, max_feed_bin, type)
  synch_master_cow <- results$synch_master_cow
  synch_master_bin <- results$synch_master_bin

  if (type == "feed") {
    synch_master_feed <- results$synch_master_feed
  }

  # go through every single day
  for (y in 1:length(data_list)) {
    cur_data <- data_list[[y]]
    cur_data <- cur_data[order(cur_data$Start, cur_data$End), ]
    cow_list <- sort(unique(cur_data$Cow))

    if (type == "feed") {
      bin_list <- seq(min_feed_bin, max_feed_bin, by = 1)
    }

    # Process matrices
    for (o in 1:nrow(cur_data)) {
      cur_cow <- cur_data$Cow[o]
      index_cow <- match(cur_cow, cow_list) + 1
      cur_start <- cur_data$Start[o]
      cur_end <- cur_data$End[o]
      cur_bin <- cur_data$Bin[o]
      start_weight <- cur_data$Startweight[o]
      end_weight <- cur_data$Endweight[o]
      start_row_number <- which(synch_master_cow[[y]]$Time == cur_start)
      end_row_number <- which(synch_master_cow[[y]]$Time == cur_end)

      if (type == "feed") {
        weight_list <- round(seq(start_weight, end_weight, length.out = (end_row_number - start_row_number + 1)), digits = 1)
        index_bin <- match(cur_bin, bin_list) + 1

        # process matrix 3, time X Bin
        synch_master_feed[[y]][(start_row_number:end_row_number), index_bin] <- weight_list
      }

      # process matrix 1, time X CowID on cow
      synch_master_cow[[y]][(start_row_number:end_row_number), index_cow] <- 1

      # process matrix 2, time X CowID on bin number
      synch_master_bin[[y]][(start_row_number:end_row_number), index_cow] <- cur_bin
    }
  }
  
  if (type == "feed") {
    return(list(synch_master_cow = synch_master_cow,
                synch_master_bin = synch_master_bin,
                synch_master_feed = synch_master_feed))
  } else {
    return(list(synch_master_cow = synch_master_cow,
                synch_master_bin = synch_master_bin))
  }
}

#' Process Current Synchronization Matrix
#'
#' This function processes a single synchronization matrix by:
#' \itemize{
#'   \item Filling NA values using last observation carried forward (LOCF)
#'   \item Calculating total feed across all bins
#'   \item Ensuring consistent data structure
#' }
#'
#' @param cur_synch A data frame containing the current synchronization data.
#'                 The first column should be 'Time', followed by bin columns.
#' @param total_feed_bin Integer. Total number of feed bins in the system.
#'
#' @return A processed data frame with:
#'         \itemize{
#'           \item All NA values filled using LOCF
#'           \item A new 'totalFeed' column containing the sum of feed across all bins
#'           \item Consistent data structure maintained
#'         }
#' @export
process_cur_synch <- function(cur_synch, total_feed_bin) {
  # Check if there is data to process
  if (nrow(cur_synch) == 0 || ncol(cur_synch) == 0) {
    return(cur_synch)
  }

  # Get the first non-NA values for each column
  first_non_na <- apply(cur_synch[, 2:(total_feed_bin + 1), drop = FALSE], 2, function(x) {
    x[!is.na(x)][1]
  })

  # Replace NAs in first row with first non-NA values
  cur_synch[1, 2:(total_feed_bin + 1)] <- first_non_na

  # Apply na.locf forward and backward to handle NAs at both ends
  for (col in 2:(total_feed_bin + 1)) {
    cur_synch[, col] <- zoo::na.locf(zoo::na.locf(cur_synch[, col], na.rm = FALSE), fromLast = TRUE, na.rm = FALSE)
  }

  # Calculate total feed
  cur_synch$totalFeed <- rowSums(cur_synch[, 2:(total_feed_bin + 1), drop = FALSE], na.rm = TRUE)

  return(cur_synch)
}

#' Process Matrices and Add Derived Columns
#'
#' This function processes the initialized matrices by:
#' \itemize{
#'   \item Calculating total cows present at each time point
#'   \item Computing bin occupancy statistics
#'   \item Adding date information
#'   \item Processing feed amounts
#'   \item Removing inactive time periods
#' }
#'
#' @param data_list A list of data frames containing feed/water data, grouped by date.
#' @param min_feed_bin Integer. Minimum feeder bin value to keep.
#' @param max_feed_bin Integer. Maximum feeder bin value to keep.
#' @param total_feed_bin Integer. Total number of feed bins in the system.
#'
#' @return A list containing three processed matrices:
#'         \itemize{
#'           \item synch_master_cow2: Processed cow presence matrix with derived columns
#'           \item synch_master_bin2: Processed bin assignment matrix with derived columns
#'           \item synch_master_feed2: Processed feed amount matrix with derived columns
#'         }
#'         Each matrix includes additional columns:
#'         \itemize{
#'           \item total_cow_num: Number of cows present at each time point
#'           \item total_bin_occupied: Number of bins occupied at each time point
#'           \item empty_bin_num: Number of empty bins at each time point
#'           \item date: Date of the observation
#'         }
#' @export
matrix_process <- function(data_list, min_feed_bin, max_feed_bin, total_feed_bin) {
  results <- matrix_initialize(data_list, min_feed_bin, max_feed_bin, type = "feed")
  synch_master_cow <- results$synch_master_cow
  synch_master_bin <- results$synch_master_bin
  synch_master_feed <- results$synch_master_feed

  # create duplicates
  synch_master_cow2 <- synch_master_cow
  synch_master_bin2 <- synch_master_bin
  synch_master_feed2 <- synch_master_feed

  for (i in 1:length(synch_master_cow)) {
    # calculate how many cows are present eating at each second
    synch_master_cow[[i]]$total_cow_num <- rowSums(synch_master_cow[[i]][, 2:ncol(synch_master_cow[[i]]), drop = FALSE], na.rm = TRUE)
    synch_master_cow[[i]]$total_bin_occupied <- synch_master_cow[[i]]$total_cow_num
    synch_master_cow[[i]]$empty_bin_num <- total_feed_bin - synch_master_cow[[i]]$total_bin_occupied

    # delete the time when no cow is eating
    records_to_keep <- which(synch_master_cow[[i]]$total_cow_num > 0)
    synch_master_cow2[[i]] <- synch_master_cow[[i]][records_to_keep, ]
    synch_master_bin2[[i]] <- synch_master_bin[[i]][records_to_keep, ]
    synch_master_feed2[[i]] <- synch_master_feed[[i]][records_to_keep, ]

    # add date
    synch_master_cow2[[i]]$date <- lubridate::date(synch_master_cow2[[i]]$Time)
    synch_master_bin2[[i]]$date <- lubridate::date(synch_master_bin2[[i]]$Time)
    synch_master_feed2[[i]]$date <- lubridate::date(synch_master_feed2[[i]]$Time)

    # fill in feed amount at each second at each bin
    synch_master_feed2[[i]] <- process_cur_synch(synch_master_feed2[[i]], total_feed_bin)
  }

  return(list(synch_master_cow2 = synch_master_cow2,
              synch_master_bin2 = synch_master_bin2,
              synch_master_feed2 = synch_master_feed2))
} 