#' @title Matrix Creation Functions
#' @description Functions for creating and preparing various matrices used in the synchronicity analysis.
#' @importFrom lubridate seconds
#' @importFrom stats seq

#' Create a time sequence from start to end, by seconds
#'
#' @param cur_data A data frame containing feeding data, or drinking data or both.
#' @return A vector of time sequence.
#' @export
create_time_sequence <- function(cur_data) {
  total_start <- min(cur_data$Start)
  total_end <- max(cur_data$End)
  # Ensure timezone is set
  attr(total_start, "tzone") <- "UTC"
  attr(total_end, "tzone") <- "UTC"
  dateTime_seq <- seq(total_start, total_end, by = "sec") # get a list of time by seconds
  return(dateTime_seq)
}

#' Create MATRIX1: empty matrix preparation: CowID X Time for which cow is eating/drinking
#' Create a matrix where x axis contains cow ID, and y axis contains time (every seconds)
#'
#' @param cur_data A data frame containing feeding data, or drinking data or both.
#' @param dateTime_seq A vector of time sequence.
#' @return A matrix of Time and cowID.
#' @export
prepare_time_cow_matrix <- function(cur_data, dateTime_seq) {
  cow_list <- sort(unique(cur_data$Cow)) #original
  col_num <- length(cow_list) + 1
  synch_master_cow <- data.frame(matrix(0, length(dateTime_seq), col_num))
  colnames(synch_master_cow) <- c("Time", cow_list)
  synch_master_cow['Time'] <- dateTime_seq
  return(synch_master_cow)
}

#' Create MARTRIX2: empty matrix preparation: Time X CowID for which bin the cow is at
#'
#' @param cow_time_matrix A matrix of CowID and Time.
#' @return A matrix of Time and CowID.
#' @export
prepare_time_bin_matrix <- function(cow_time_matrix) {
  return(cow_time_matrix)
}

#' Create MATRIX3: Time X Bin for how much feed is at each bin at each second
#'
#' @param dateTime_seq A vector of time sequence.
#' @param min_feed_bin Minimum feeder bin value to keep.
#' @param max_feed_bin Maximum feeder bin value to keep.
#' @return A matrix of Time and Feed amount in each bin.
#' @export
prepare_time_feed_matrix <- function(dateTime_seq, min_feed_bin, max_feed_bin) {
  bin_list <- seq(min_feed_bin, max_feed_bin, by = 1)
  col_num <- length(bin_list) + 1
  synch_master_feed <- data.frame(matrix(NA, length(dateTime_seq), col_num))
  colnames(synch_master_feed) <- c("Time", bin_list)
  synch_master_feed['Time'] <- dateTime_seq
  return(synch_master_feed)
} 