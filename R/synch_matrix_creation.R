#' Synchronicity Matrix Creation Functions
#'
#' Functions for creating time sequences and empty matrices for cow feeding/drinking analysis.
#'
#' @section Functions:
#' - create_time_sequence
#' - prepare_time_cow_matrix
#' - prepare_time_bin_matrix
#' - prepare_time_feed_matrix
#'
#' @name synch_matrix_creation
#' @docType package

#' Create a time sequence from start to end, by seconds
#'
#' @param cur_data A data frame containing feeding or drinking data. Must have 'Start' and 'End' columns (POSIXct).
#' @return A POSIXct vector of time sequence.
#' @export
create_time_sequence <- function(cur_data) {
  if (!is.data.frame(cur_data)) stop("cur_data must be a data frame")
  if (!all(c("Start", "End") %in% names(cur_data))) stop("cur_data must have 'Start' and 'End' columns")
  if (!lubridate::is.POSIXct(cur_data$Start) || !lubridate::is.POSIXct(cur_data$End)) stop("'Start' and 'End' must be POSIXct")
  if (nrow(cur_data) == 0) stop("cur_data is empty")
  total_start <- min(cur_data$Start, na.rm = TRUE)
  total_end <- max(cur_data$End, na.rm = TRUE)
  if (is.na(total_start) || is.na(total_end)) stop("Start/End times cannot be NA")
  if (total_end < total_start) stop("End time cannot be earlier than start time")
  seq(total_start, total_end, by = "sec")
}

#' Create an empty time-cow matrix for synchronicity analysis
#'
#' @param cur_data Data frame with 'Cow' column and time columns.
#' @param dateTime_seq POSIXct vector of time sequence.
#' @return Data frame: first column 'Time', others are cow IDs, initialized to 0.
#' @export
prepare_time_cow_matrix <- function(cur_data, dateTime_seq) {
  if (!is.data.frame(cur_data)) stop("cur_data must be a data frame")
  if (!("Cow" %in% names(cur_data))) stop("cur_data must have a 'Cow' column")
  if (!lubridate::is.POSIXct(dateTime_seq)) stop("dateTime_seq must be POSIXct")
  if (length(dateTime_seq) == 0) stop("dateTime_seq cannot be empty")
  cow_list <- sort(unique(cur_data$Cow))
  if (length(cow_list) == 0) stop("No cows found in cur_data")
  col_num <- length(cow_list) + 1
  synch_master_cow <- data.frame(matrix(0, length(dateTime_seq), col_num))
  colnames(synch_master_cow) <- c("Time", cow_list)
  synch_master_cow[["Time"]] <- dateTime_seq
  synch_master_cow
}

#' Create an empty time-bin matrix for tracking bin occupancy
#'
#' @param cow_time_matrix Data frame as from prepare_time_cow_matrix.
#' @return Data frame with same structure as cow_time_matrix.
#' @export
prepare_time_bin_matrix <- function(cow_time_matrix) {
  if (!is.data.frame(cow_time_matrix)) stop("Input must be a data frame")
  if (!"Time" %in% names(cow_time_matrix)) stop("Input must have a 'Time' column")
  if (ncol(cow_time_matrix) < 2) stop("Input must have at least one cow column")
  cow_time_matrix
}

#' Create an empty time-feed matrix for feed bin tracking
#'
#' @param dateTime_seq POSIXct vector of time sequence.
#' @param min_feed_bin Integer, minimum bin number.
#' @param max_feed_bin Integer, maximum bin number.
#' @return Data frame: first column 'Time', others are bin numbers, initialized to NA.
#' @export
prepare_time_feed_matrix <- function(dateTime_seq, min_feed_bin, max_feed_bin) {
  if (!lubridate::is.POSIXct(dateTime_seq)) stop("dateTime_seq must be POSIXct")
  if (length(dateTime_seq) == 0) stop("dateTime_seq cannot be empty")
  if (!is.numeric(min_feed_bin) || !is.numeric(max_feed_bin)) stop("min_feed_bin and max_feed_bin must be numeric")
  if (length(min_feed_bin) != 1 || length(max_feed_bin) != 1) stop("min_feed_bin and max_feed_bin must be single values")
  if (min_feed_bin > max_feed_bin) stop("min_feed_bin cannot be greater than max_feed_bin")
  bin_list <- seq(min_feed_bin, max_feed_bin, by = 1)
  col_num <- length(bin_list) + 1
  synch_master_feed <- data.frame(matrix(NA, length(dateTime_seq), col_num))
  colnames(synch_master_feed) <- c("Time", bin_list)
  synch_master_feed[["Time"]] <- dateTime_seq
  synch_master_feed
} 