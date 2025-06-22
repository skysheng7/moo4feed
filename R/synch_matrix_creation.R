#' Create a time sequence from start to end, by seconds
#'
#' @description
#' Creates a sequence of timestamps by seconds from the earliest start time 
#' to the latest end time in the provided data.
#'
#' @param cur_data A data frame containing feeding or drinking data
#' @inheritParams set_global_cols
#' @return A POSIXct vector of time sequence by seconds
#' 
#' @examples
#' # Create toy data
#' toy_data <- data.frame(
#'   start = lubridate::ymd_hms("2023-01-01 10:00:00"),
#'   end = lubridate::ymd_hms("2023-01-01 10:00:05")
#' )
#' 
#' # Create time sequence
#' time_seq <- create_time_sequence(toy_data, start_col = "start", end_col = "end")
#' length(time_seq) # Should be 6 seconds
#' 
#' @noRd
create_time_sequence <- function(cur_data, 
                                start_col = start_col2(),
                                end_col = end_col2()) {
  if (!is.data.frame(cur_data)) stop("`cur_data` must be a data frame")
  
  required_cols <- c(start_col, end_col)
  missing_cols <- setdiff(required_cols, names(cur_data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  if (!lubridate::is.POSIXct(cur_data[[start_col]]) || !lubridate::is.POSIXct(cur_data[[end_col]])) {
    stop("Start and end columns must be POSIXct")
  }
  if (nrow(cur_data) == 0) stop("`cur_data` is empty")
  
  total_start <- min(cur_data[[start_col]], na.rm = TRUE)
  total_end <- max(cur_data[[end_col]], na.rm = TRUE)
  if (is.na(total_start) || is.na(total_end)) stop("Start/End times cannot be NA")
  if (total_end < total_start) stop("End time cannot be earlier than start time")
  
  seq(total_start, total_end, by = "sec")
}

#' Create an empty time-animal matrix for synchronicity analysis
#'
#' @description
#' Creates an empty matrix where rows represent time (by seconds) and columns 
#' represent individual animals, initialized to 0.
#'
#' @param cur_data Data frame containing animal data
#' @param dateTime_seq POSIXct vector of time sequence from create_time_sequence
#' @inheritParams set_global_cols
#' @return Data frame: first column 'Time', others are animal IDs, initialized to 0
#' 
#' @examples
#' # Use package data with explicit column parameters  
#' sample_data <- all_fed[[1]][1:10,]
#' 
#' # Create time sequence and animal matrix
#' time_seq <- create_time_sequence(sample_data, start_col = "start", end_col = "end")
#' animal_matrix <- prepare_time_animal_matrix(sample_data, time_seq, id_col = "cow")
#' head(animal_matrix)
#' 
#' @noRd
prepare_time_animal_matrix <- function(cur_data, 
                                      dateTime_seq,
                                      id_col = id_col2()) {
  if (!is.data.frame(cur_data)) stop("`cur_data` must be a data frame")
  
  if (!(id_col %in% names(cur_data))) {
    stop("Missing required column: ", id_col)
  }
  
  if (!lubridate::is.POSIXct(dateTime_seq)) stop("`dateTime_seq` must be POSIXct")
  if (length(dateTime_seq) == 0) stop("`dateTime_seq` cannot be empty")
  
  animal_list <- sort(unique(cur_data[[id_col]]))
  if (length(animal_list) == 0) stop("No animals found in `cur_data`")
  
  col_num <- length(animal_list) + 1
  synch_master_animal <- data.frame(matrix(0, length(dateTime_seq), col_num))
  colnames(synch_master_animal) <- c("Time", animal_list)
  synch_master_animal[["Time"]] <- dateTime_seq
  
  synch_master_animal
}

#' Create an empty time-bin matrix for tracking bin occupancy
#'
#' @description
#' Creates a matrix with the same structure as the time-animal matrix but 
#' for tracking which bin each animal is at.
#'
#' @param animal_time_matrix Data frame as returned from prepare_time_animal_matrix
#' @return Data frame with same structure as animal_time_matrix
#' 
#' @examples
#' # Use package data with explicit column parameters
#' sample_data <- all_fed[[1]][1:10,]
#' 
#' # Create matrices using explicit column parameters
#' time_seq <- create_time_sequence(sample_data, start_col = "start", end_col = "end")
#' animal_matrix <- prepare_time_animal_matrix(sample_data, time_seq, id_col = "cow")
#' bin_matrix <- prepare_time_bin_matrix(animal_matrix)
#' identical(dim(animal_matrix), dim(bin_matrix)) # Should be TRUE
#' 
#' @noRd
prepare_time_bin_matrix <- function(animal_time_matrix) {
  if (!is.data.frame(animal_time_matrix)) stop("`animal_time_matrix` must be a data frame")
  if (!"Time" %in% names(animal_time_matrix)) stop("`animal_time_matrix` must have a 'Time' column")
  if (ncol(animal_time_matrix) < 2) stop("`animal_time_matrix` must have at least one animal column")
  
  animal_time_matrix
}

#' Create an empty time-feed matrix for feed bin tracking
#'
#' @description
#' Creates a matrix where rows represent time (by seconds) and columns represent 
#' feed bins, initialized to NA.
#'
#' @param dateTime_seq POSIXct vector of time sequence from create_time_sequence
#' @inheritParams set_global_cols
#' @return Data frame: first column 'Time', others are bin numbers, initialized to NA
#' 
#' @examples
#' # Create feed matrix with explicit bin parameters
#' time_seq <- seq(lubridate::ymd_hms("2023-01-01 10:00:00"), 
#'                 lubridate::ymd_hms("2023-01-01 10:00:02"), by = "sec")
#' feed_matrix <- prepare_time_feed_matrix(time_seq, bins_feed = 1:3)
#' head(feed_matrix)
#' 
#' @noRd
prepare_time_feed_matrix <- function(dateTime_seq, 
                                    bins_feed = bins_feed2()) {
  if (!lubridate::is.POSIXct(dateTime_seq)) stop("`dateTime_seq` must be POSIXct")
  if (length(dateTime_seq) == 0) stop("`dateTime_seq` cannot be empty")
  if (!is.numeric(bins_feed)) stop("`bins_feed` must be numeric")
  if (length(bins_feed) == 0) stop("`bins_feed` cannot be empty")
  
  min_feed_bin <- min(bins_feed)
  max_feed_bin <- max(bins_feed)
  
  if (min_feed_bin > max_feed_bin) stop("Invalid bin range")
  
  bin_list <- seq(min_feed_bin, max_feed_bin, by = 1)
  col_num <- length(bin_list) + 1
  synch_master_feed <- data.frame(matrix(NA, length(dateTime_seq), col_num))
  colnames(synch_master_feed) <- c("Time", bin_list)
  synch_master_feed[["Time"]] <- dateTime_seq
  
  synch_master_feed
} 