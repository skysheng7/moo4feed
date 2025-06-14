#' Synchronicity Matrix Processing Functions
#'
#' Functions for initializing and processing synchronicity matrices for cow feeding/drinking analysis.
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
#' @param data_list List of data frames grouped by date.
#' @param min_feed_bin Integer, minimum bin number (required for type 'feed').
#' @param max_feed_bin Integer, maximum bin number (required for type 'feed').
#' @param type Character, one of 'feed', 'drink', 'feed_and_drink'.
#' @return List of matrices (see details in docs).
#' @export
empty_synch_matrix <- function(data_list, min_feed_bin = NULL, max_feed_bin = NULL, type = "feed") {
  if (is.null(data_list) || length(data_list) == 0) stop("Input matrix cannot be NULL or empty")
  if (!type %in% c("feed", "drink", "feed_and_drink")) stop("Type must be one of: 'feed', 'drink', 'feed_and_drink'")
  if (type == "feed") {
    if (is.null(min_feed_bin) || is.null(max_feed_bin)) stop("For type 'feed', both min_feed_bin and max_feed_bin must be provided")
    if (!is.numeric(min_feed_bin) || !is.numeric(max_feed_bin)) stop("Bin numbers must be numeric")
    if (min_feed_bin < 0 || max_feed_bin < 0) stop("Bin numbers must be positive")
    if (min_feed_bin > max_feed_bin) stop("min_feed_bin must be <= max_feed_bin")
  }
  synch_master_cow <- list()
  synch_master_bin <- list()
  synch_master_feed <- list()
  for (y in seq_along(data_list)) {
    cur_data <- data_list[[y]]
    cur_data <- cur_data[order(cur_data$Start, cur_data$End), ]
    dateTime_seq <- create_time_sequence(cur_data)
    cow_time_matrix <- prepare_time_cow_matrix(cur_data, dateTime_seq)
    time_bin_matrix <- cow_time_matrix
    synch_master_cow[[y]] <- cow_time_matrix
    synch_master_bin[[y]] <- time_bin_matrix
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

#' Initialize and process synchronization matrices
#'
#' @param data_list List of data frames.
#' @param min_feed_bin Integer, minimum bin number (required for type 'feed').
#' @param max_feed_bin Integer, maximum bin number (required for type 'feed').
#' @param type Character, one of 'feed', 'drink', 'feed_and_drink'.
#' @return List of processed matrices.
#' @export
matrix_initialize <- function(data_list, min_feed_bin = NULL, max_feed_bin = NULL, type = "feed") {
  if (is.null(data_list) || length(data_list) == 0) stop("Input matrix cannot be NULL or empty")
  if (!type %in% c("feed", "drink", "feed_and_drink")) stop("Type must be one of: 'feed', 'drink', 'feed_and_drink'")
  if (type == "feed") {
    if (is.null(min_feed_bin) || is.null(max_feed_bin)) stop("For type 'feed', both min_feed_bin and max_feed_bin must be provided")
    if (!is.numeric(min_feed_bin) || !is.numeric(max_feed_bin)) stop("Bin numbers must be numeric")
    if (min_feed_bin < 0 || max_feed_bin < 0) stop("Bin numbers must be positive")
    if (min_feed_bin > max_feed_bin) stop("min_feed_bin must be <= max_feed_bin")
  }
  results <- empty_synch_matrix(data_list, min_feed_bin, max_feed_bin, type)
  synch_master_cow <- results$synch_master_cow
  synch_master_bin <- results$synch_master_bin
  if (type == "feed") {
    synch_master_feed <- results$synch_master_feed
  }
  for (y in seq_along(data_list)) {
    cur_data <- data_list[[y]]
    cur_data <- cur_data[order(cur_data$Start, cur_data$End), ]
    cow_list <- sort(unique(cur_data$Cow))
    if (type == "feed") {
      bin_list <- seq(min_feed_bin, max_feed_bin, by = 1)
    }
    for (o in seq_len(nrow(cur_data))) {
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
        synch_master_feed[[y]][(start_row_number:end_row_number), index_bin] <- weight_list
      }
      synch_master_cow[[y]][(start_row_number:end_row_number), index_cow] <- 1
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

#' Process the current synchronization data to replace NA values and compute total feed
#'
#' @param cur_synch Data frame representing the current synchronization data.
#' @param total_feed_bin Integer, total number of bins.
#' @return Data frame with NA values replaced and a new column 'totalFeed'.
#' @keywords internal
process_cur_synch <- function(cur_synch, total_feed_bin) {
  if (is.null(cur_synch) || nrow(cur_synch) == 0) stop("Input matrix cannot be NULL or empty")
  if (!"Time" %in% colnames(cur_synch)) stop("Input matrix must contain a 'Time' column")
  if (ncol(cur_synch) <= 2) stop("Input matrix must have at least one bin column")
  if (!is.numeric(total_feed_bin) || total_feed_bin <= 0) stop("total_feed_bin must be a positive number")
  bin_cols <- 2:(ncol(cur_synch) - 1)
  if (length(bin_cols) == 0) stop("No bin columns found in input matrix")
  # Replace initial NA values with first non-NA value
  bin_data <- cur_synch[, bin_cols, drop = FALSE]
  if (nrow(bin_data) == 1) {
    first_non_na <- bin_data[1, ]
  } else {
    first_non_na <- apply(bin_data, 2, function(x) x[which(!is.na(x))[1]])
  }
  cur_synch[1, bin_cols] <- ifelse(is.na(cur_synch[1, bin_cols]), first_non_na, cur_synch[1, bin_cols])
  # Replace NA values with last observed non-NA value
  if (nrow(bin_data) == 1) {
    cur_synch[, bin_cols] <- bin_data
  } else {
    cur_synch[, bin_cols] <- apply(cur_synch[, bin_cols, drop = FALSE], 2, zoo::na.locf)
  }
  # Add totalFeed column
  cur_synch$totalFeed <- rowSums(cur_synch[, 2:(total_feed_bin + 1), drop = FALSE], na.rm = TRUE)
  cur_synch
}

#' Process matrices and add derived columns
#'
#' @param data_list List of data frames to process.
#' @param min_feed_bin Integer, minimum bin number.
#' @param max_feed_bin Integer, maximum bin number.
#' @param total_feed_bin Integer, total number of feed bins.
#' @param type Character, one of 'feed', 'drink', 'feed_and_drink'.
#' @return List of processed lists of data frames.
#' @export
matrix_process <- function(data_list, min_feed_bin = NULL, max_feed_bin = NULL, total_feed_bin = NULL, type = "feed") {
  if (is.null(data_list) || length(data_list) == 0) stop("Input matrix cannot be NULL or empty")
  if (!type %in% c("feed", "drink", "feed_and_drink")) stop("Type must be one of: 'feed', 'drink', 'feed_and_drink'")
  if (type == "feed") {
    if (is.null(min_feed_bin) || is.null(max_feed_bin) || is.null(total_feed_bin)) stop("For type 'feed', min_feed_bin, max_feed_bin, and total_feed_bin must be provided")
    if (!is.numeric(min_feed_bin) || !is.numeric(max_feed_bin) || !is.numeric(total_feed_bin)) stop("Bin numbers must be numeric")
    if (min_feed_bin < 0 || max_feed_bin < 0 || total_feed_bin < 0) stop("Bin numbers must be positive")
    if (min_feed_bin > max_feed_bin) stop("min_feed_bin must be <= max_feed_bin")
    if (total_feed_bin <= 0) stop("total_feed_bin must be a positive number")
  }
  results <- matrix_initialize(data_list, min_feed_bin, max_feed_bin, type)
  synch_master_cow <- results$synch_master_cow
  synch_master_bin <- results$synch_master_bin
  synch_master_feed <- results$synch_master_feed
  synch_master_cow2 <- synch_master_cow
  synch_master_bin2 <- synch_master_bin
  synch_master_feed2 <- synch_master_feed
  for (i in seq_along(synch_master_cow)) {
    # Only compute rowSums if there are at least two columns (Time + at least one cow)
    if (ncol(synch_master_cow[[i]]) > 1) {
      synch_master_cow[[i]]$total_cow_num <- rowSums(synch_master_cow[[i]][, 2:ncol(synch_master_cow[[i]]), drop = FALSE], na.rm = TRUE)
      synch_master_cow[[i]]$total_bin_occupied <- synch_master_cow[[i]]$total_cow_num
      synch_master_cow[[i]]$empty_bin_num <- total_feed_bin - synch_master_cow[[i]]$total_bin_occupied
    } else {
      synch_master_cow[[i]]$total_cow_num <- 0
      synch_master_cow[[i]]$total_bin_occupied <- 0
      synch_master_cow[[i]]$empty_bin_num <- total_feed_bin
    }
    records_to_keep <- which(synch_master_cow[[i]]$total_cow_num > 0)
    synch_master_cow2[[i]] <- synch_master_cow[[i]][records_to_keep, , drop = FALSE]
    synch_master_bin2[[i]] <- synch_master_bin[[i]][records_to_keep, , drop = FALSE]
    synch_master_feed2[[i]] <- synch_master_feed[[i]][records_to_keep, , drop = FALSE]
    synch_master_cow2[[i]]$date <- lubridate::date(synch_master_cow2[[i]]$Time)
    synch_master_bin2[[i]]$date <- lubridate::date(synch_master_bin2[[i]]$Time)
    synch_master_feed2[[i]]$date <- lubridate::date(synch_master_feed2[[i]]$Time)
    synch_master_feed2[[i]] <- process_cur_synch(synch_master_feed2[[i]], total_feed_bin)
  }
  return(list(synch_master_cow2 = synch_master_cow2,
              synch_master_bin2 = synch_master_bin2,
              synch_master_feed2 = synch_master_feed2))
} 