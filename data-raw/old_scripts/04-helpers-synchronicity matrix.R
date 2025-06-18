#' DEPRECATED SYNCHRONICITY MATRIX FUNCTIONS - COMPATIBILITY LAYER
#'
#' This file contains deprecated versions of synchronicity matrix functions.
#' These functions are maintained for backward compatibility but now call
#' the modern refactored implementations from the main package.
#'
#' For new code, please use the functions in:
#' - R/synch_matrix_creation.R
#' - R/synch_matrix_processing.R
#'
#' @deprecated These functions will be removed in a future version.
#'             Please migrate to the new API with global variable support.

#' Create a time sequence from start to end, by seconds
#'
#' @param cur_data A data frame containing feeding data, or drinking data or both.
#' @return A vector of time sequence.
#' @deprecated Use the new functions with global variable support instead
create_time_sequence <- function(cur_data) {
  .Deprecated("create_time_sequence with global parameters", 
              package = "moo4feed",
              msg = "This function is deprecated. Use the new create_time_sequence() with start_col and end_col parameters.")
  
  # Call the new function with default hard-coded column names for compatibility
  moo4feed:::create_time_sequence(cur_data, start_col = "Start", end_col = "End")
}

#' create MATRIX1: empty matrix preparation: CowID X Time for which cow is eating/drinking
#' create a matrix where x axis contains cow ID, and y axis contains time (every seconds)
#'
#' @param cur_data A data frame containing feeding data, or drinking data or both.
#' @param dateTime_seq A vector of time sequence.
#' @return A matrix of Time and cowID.
#' @deprecated Use prepare_time_animal_matrix() instead
prepare_time_cow_matrix <- function(cur_data, dateTime_seq) {
  .Deprecated("prepare_time_animal_matrix", 
              package = "moo4feed",
              msg = "This function is deprecated. Use prepare_time_animal_matrix() with id_col parameter.")
  
  # Call the new function with hard-coded column name for compatibility
  moo4feed:::prepare_time_animal_matrix(cur_data, dateTime_seq, id_col = "Cow")
}

#' create MARTRIX2: empty matrix preparation: Time X CowID for which bin the cow is at
#'
#' @param cow_time_matrix A matrix of CowID and Time.
#' @return A matrix of Time and CowID.
#' @deprecated Use prepare_time_bin_matrix() instead
prepare_time_bin_matrix <- function(cow_time_matrix) {
  .Deprecated("prepare_time_bin_matrix", 
              package = "moo4feed",
              msg = "This function is deprecated. Use the new prepare_time_bin_matrix() function.")
  
  # Call the new function (this one didn't change much)
  moo4feed:::prepare_time_bin_matrix(cow_time_matrix)
}

#' create MATRIX3: Time X Bin for how much feed is at each bin at each second
#'
#' @param dateTime_seq A vector of time sequence.
#' @param min_feed_bin Minimum feeder bin value to keep.
#' @param max_feed_bin Maximum feeder bin value to keep.
#' @return A matrix of Time and Feed amount in each bin.
#' @deprecated Use prepare_time_feed_matrix() instead
prepare_time_feed_matrix <- function(dateTime_seq, min_feed_bin, max_feed_bin) {
  .Deprecated("prepare_time_feed_matrix with bins_feed parameter", 
              package = "moo4feed",
              msg = "This function is deprecated. Use prepare_time_feed_matrix() with bins_feed parameter.")
  
  # Create bins_feed vector from min/max for compatibility
  bins_feed <- seq(min_feed_bin, max_feed_bin, by = 1)
  moo4feed:::prepare_time_feed_matrix(dateTime_seq, bins_feed = bins_feed)
}

#' Generate empty Synchronization Matrices for Feed/water Data
#'
#' This function takes a list of feed data and creates synchronization matrices for time-cow,
#' time-bin, and time-feed based on each list element.The synchronization will
#' differ based on the variable "type". 3 types of synchronization:
#' 'feeding_synchronization' , 'drinking_synchronization' , 'feeding_and_drinking_synchronization'
#'
#' @param data_list A list of feed / water data frames grouped by date.
#' @param min_feed_bin Minimum feeder bin value to keep.
#' @param max_feed_bin Maximum feeder bin value to keep.
#' @param type Specifies the type of synchronicity. Type can have 3 possible values:
#'        "feed", "drink", "Feed_and_drink"
#' @return A list containing lists of matrices:
#'         if type = "feed" : synch_master_cow, synch_master_bin, synch_master_feed
#'         if type = "feed_and_drink" : synch_master_cow, synch_master_bin
#'         if type = "drink" :synch_master_cow, synch_master_bin
#' @deprecated Use empty_synch_matrix() with global parameters instead
empty_synch_matrix <- function(data_list, min_feed_bin = min_feed_bin, max_feed_bin = max_feed_bin, type) {
  .Deprecated("empty_synch_matrix with global parameters", 
              package = "moo4feed",
              msg = "This function is deprecated. Use empty_synch_matrix() with global variable parameters.")
  
  # Create bins_feed vector from min/max for compatibility
  bins_feed <- seq(min_feed_bin, max_feed_bin, by = 1)
  
  # Call the new function with hard-coded column names for compatibility
  result <- moo4feed::empty_synch_matrix(
    data_list = data_list,
    type = type,
    id_col = "Cow",
    start_col = "Start", 
    end_col = "End",
    bin_col = "Bin",
    bins_feed = bins_feed,
    bins_wat = bins_feed  # Using same for compatibility
  )
  
  # Convert animal back to cow for backward compatibility
  if ("synch_master_animal" %in% names(result)) {
    names(result)[names(result) == "synch_master_animal"] <- "synch_master_cow"
  }
  
  return(result)
}

#' Initialize and Process Synchronization Matrices
#'
#' This function initializes synchronization matrices and processes feed/water data
#' to populate the matrices based on each list element.
#' It process MATRIX1 (synch_master_cow): Time X CowID for which cow is eating/drinking
#' AND MARTRIX2 (synch_master_bin): Time X CowID for which bin the cow is at
#' AND MATRIX3 (synch_master_feed): Time X Bin for how much feed/water is at each bin at each second
#'
#' @param data_list A list of data frames.
#' @param min_feed_bin Minimum value of the feed bin.
#' @param max_feed_bin Maximum value of the feed bin.
#' @param type Type of synchronicity analysis
#'
#' @return A list containing three matrices:
#'         synch_master_cow, synch_master_bin, synch_master_feed.
#' @deprecated Use matrix_initialize() with global parameters instead
matrix_initialize <- function(data_list, min_feed_bin = min_feed_bin, max_feed_bin = max_feed_bin, type) {
  .Deprecated("matrix_initialize with global parameters", 
              package = "moo4feed",
              msg = "This function is deprecated. Use matrix_initialize() with global variable parameters.")
  
  # Create bins_feed vector from min/max for compatibility
  bins_feed <- seq(min_feed_bin, max_feed_bin, by = 1)
  
  # Call the new function with hard-coded column names for compatibility
  result <- moo4feed:::matrix_initialize(
    data_list = data_list,
    type = type,
    id_col = "Cow",
    start_col = "Start",
    end_col = "End", 
    bin_col = "Bin",
    start_weight_col = "Startweight",
    end_weight_col = "Endweight",
    bins_feed = bins_feed,
    bins_wat = bins_feed  # Using same for compatibility
  )
  
  # Convert animal back to cow for backward compatibility
  if ("synch_master_animal" %in% names(result)) {
    names(result)[names(result) == "synch_master_animal"] <- "synch_master_cow"
  }
  
  return(result)
}

#' Process the current synchronization data to replace NA values and compute total feed
#'
#' This function processes the provided `cur_synch` data matrix.
#' It replaces the initial NA values of each column with the first non-NA value in that column.
#' Then, it replaces any subsequent NA values in each column with the last observed non-NA value in that column.
#' Finally, it calculates the total feed in all bins and adds it as a new column.
#'
#' @param cur_synch A matrix/dataframe representing the current synchronization data.
#'                  The first column is expected to be 'Time', and the other columns represent bins.
#'                  The matrix should have NA values where feed data is not available.
#' @param total_feed_bin An integer indicating the total number of bins.
#'
#' @return A matrix/dataframe where the NA values in the bin columns are replaced,
#'         and a new column `totalFeed` is added which represents the sum of feeds in all bins.
#' @deprecated Use process_cur_synch() with bins_feed parameter instead
process_cur_synch <- function(cur_synch, total_feed_bin = total_feed_bin) {
  .Deprecated("process_cur_synch with bins_feed parameter", 
              package = "moo4feed",
              msg = "This function is deprecated. Use process_cur_synch() with bins_feed parameter.")
  
  # Create bins_feed vector from total_feed_bin for compatibility
  # Assuming bins are 1:total_feed_bin based on legacy usage
  bins_feed <- 1:total_feed_bin
  
  # Call the new function
  moo4feed:::process_cur_synch(cur_synch, bins_feed = bins_feed)
}

#' Process matrices and add derived columns.
#'
#' This function processes a list of matrices, adds several derived columns like total number of cows,
#' total bin occupied, and date, and then returns processed versions of the matrices.
#'
#' @param data_list A list of data frames to process.
#' @param total_feed_bin The total number of feed bins
#'
#' @return A list containing three processed lists of data frames: synch_master_cow2, synch_master_bin2, and synch_master_feed2.
#' @deprecated Use matrix_process() with global parameters instead
matrix_process <- function(data_list, total_feed_bin) {
  .Deprecated("matrix_process with global parameters", 
              package = "moo4feed",
              msg = "This function is deprecated. Use matrix_process() with global variable parameters.")
  
  # Create bins_feed vector from total_feed_bin for compatibility
  bins_feed <- 1:total_feed_bin
  
  # Call the new function with hard-coded column names for compatibility
  result <- moo4feed::matrix_process(
    data_list = data_list,
    type = "feed",  # Assuming feed type based on legacy usage
    id_col = "Cow",
    start_col = "Start",
    end_col = "End",
    bin_col = "Bin", 
    start_weight_col = "Startweight",
    end_weight_col = "Endweight",
    bins_feed = bins_feed,
    bins_wat = bins_feed
  )
  
  # Convert animal back to cow for backward compatibility
  if ("synch_master_animal2" %in% names(result)) {
    names(result)[names(result) == "synch_master_animal2"] <- "synch_master_cow2"
  }
  
  return(result)
}

###################################################################################################
############### Feeding & Drinking Combined Synchrony Matrix Preparation ##########################
###################################################################################################

#' Generates number of cows present at each second
#'
#' This function iterates through the list_feed_drink_synch_master_cow datasheet and for each data frame
#' calculates the number of cows present at every second
#'
#' @param feed_drink_synch_master_cow An empty matrix with cow on the x axis and time on the y axis
#'
#' @return A list of data frames containing information about the number of cows
#' present at every second.

total_cows_present <- function(feed_drink_synch_master_cow, total_fed_wat_bin) {
  .Deprecated("Use modern matrix processing functions", 
              package = "moo4feed",
              msg = "This function is deprecated. Use the new matrix processing functions with global variable support.")
  
  new_list <- list()
  for (y in 1:length(feed_drink_synch_master_cow)) {
    cur_data <- feed_drink_synch_master_cow[[y]]
    cur_data$total_cow_num <- rowSums(cur_data[, 2:ncol(cur_data)], na.rm = TRUE)
    cur_data$total_bin_occupied <- cur_data$total_cow_num
    cur_data$empty_bin_num <- total_fed_wat_bin - cur_data$total_bin_occupied
    new_list[[y]] <- cur_data
  }

  return (new_list)
}



#' Deletes times when no cow is feeding
#'
#' This function iterates through the lists feed_drink_synch_master_cow and feed_drink_synch_master_bin
#' and for each data frame, deletes the row (times) when no cow is feeding or drinking.
#'
#' @param feed_drink_synch_master_cow A list of data frames with cow on the x axis and time on the y axis
#' @param feed_drink_synch_master_bin A list of data frames with cow on the x axis and bin number on the y axis.
#'
#' @return 2 lists of data frames containing information only for the times the cow are feeding / drinking
delete_inactive_time <- function(feed_drink_synch_master_cow, feed_drink_synch_master_bin) {
  .Deprecated("Use modern matrix processing functions", 
              package = "moo4feed",
              msg = "This function is deprecated. Use the new matrix processing functions with global variable support.")
  
  new_cow_list <- list()
  new_bin_list <- list()
  for (y in 1:length(feed_drink_synch_master_bin)) {
    cur_data_cow <- feed_drink_synch_master_cow[[y]]
    cur_data_bin <- feed_drink_synch_master_bin[[y]]

    records_to_keep <- which(cur_data_cow['total_cow_num'] > 0)
    if (length(records_to_keep) > 0){
      cur_data_cow2 <- cur_data_cow[records_to_keep, ]
      cur_data_bin2 <- cur_data_bin[records_to_keep, ]
      new_cow_list[[y]] <- cur_data_cow2
      new_bin_list[[y]] <- cur_data_bin2
    }


  }

  return(list(new_cow_list,
              new_bin_list))

}

#' Adds Date
#'
#' This function add a date column to the 2 input matrices
#'
#' @param feed_drink_synch_master_cow2 An empty matrix with cow on the x axis and time on the y axis
#' @param feed_drink_synch_master_bin2 An empty matrix with cow on the x axis and bin number on the y axis.
#'
#' @return feed_drink_synch_master_cow and feed_drink_master_bin matrices with a date column
add_date <-  function(feed_drink_synch_master_cow2, feed_drink_synch_master_bin2){
  new_cow_list <- list()
  new_bin_list <- list()
  for (y in 1:length(feed_drink_synch_master_cow2)) {
    cur_data_cow <- feed_drink_synch_master_cow2[[y]]
    cur_data_bin <- feed_drink_synch_master_bin2[[y]]
    cur_data_cow$date <- date(cur_data_cow$Time)
    cur_data_bin$date <- date(cur_data_bin$Time)

    new_cow_list[[y]] = cur_data_cow
    new_bin_list[[y]] = cur_data_bin

    names(new_cow_list)[y] <- as.character(date(cur_data_cow$Time[1]))
    names(new_bin_list)[y] <- as.character(date(cur_data_bin$Time[1]))
  }

  return (list(new_cow_list,
               new_bin_list))

}

#' Updates Bin Number
#'
#' This function iterates through the list of data frames, and updates the feed and
#' water bins for each
#'
#' @param feed_drink_synch_master_bin2 A list of data frames with cow on the x axis and bin number on the y axis.
#'
#' @return new_list_bin a list of data frames with the updated bin numbers.
bin_update <- function(feed_drink_synch_master_bin2) {
  new_list_bin <- list()
  for (y in 1:length(feed_drink_synch_master_bin2)) {
    cur_data <- feed_drink_synch_master_bin2[[y]]

    #updating the water bin number
    cur_data[cur_data ==101] <- 207
    cur_data[cur_data ==102] <- 208
    cur_data[cur_data ==103] <- 221
    cur_data[cur_data ==104] <- 222
    cur_data[cur_data ==105] <- 235

    #update feed bin number
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

    names(new_list_bin)[y] <- as.character(date(cur_data$Time[1]))

  }
  return (new_list_bin)

}


#' Creates and processes synchronicity matrix
#'
#' This function processes the feeding and drinking master data by creating matrices
#'
#' @param all.comb2 A list of data frames containing feeding and drinking information
#' for each cow.
#'
#'
#' @return 2 lists of data frames:
#'                      - feeding_synch_master_cow2: a list of data frame indicating when the cow is feeding or drinking, separated by date
#'                      - feeding_synch_master_bin2: a list of data frames indicating which bin a cow is at, separated by date

feed_drink_matrix_process <- function(all.comb2,total_fed_wat_bin){
  .Deprecated("Use matrix_process() with global parameters", 
              package = "moo4feed",
              msg = "This function is deprecated. Use matrix_process() with global variable parameters.")
  
  initialized_matrix <- matrix_initialize(all.comb2,type = "feed_and_drink")

  feed_drink_synch_master_cow <- initialized_matrix[[1]]
  feed_drink_synch_master_bin <- initialized_matrix[[2]]


  feed_drink_synch_master_cow <- total_cows_present(feed_drink_synch_master_cow, total_fed_wat_bin = total_fed_wat_bin)

  results_del <- delete_inactive_time(feed_drink_synch_master_cow, feed_drink_synch_master_bin)

  feed_drink_synch_master_cow2 <- results_del[[1]]
  feed_drink_synch_master_bin2 <- results_del[[2]]

  results_add_date <- add_date(feed_drink_synch_master_cow2,feed_drink_synch_master_bin2)

  feed_drink_synch_master_cow2 <- results_add_date[[1]]
  feed_drink_synch_master_bin2 <- results_add_date[[2]]

  feed_drink_synch_master_bin2 <- bin_update(feed_drink_synch_master_bin2)

  return(list(feed_drink_synch_master_cow2,
              feed_drink_synch_master_bin2))


}


###################################################################################################
######################## Feeding & Drinking Synchrony analysis ####################################
###################################################################################################

#' Creating an empty Cow X Cow matrix
#' @param master_feeding_drinking2 is a matrix containing the feeding and drinking
#' information for all the cows
#'
#' @return an empty Cow x Cow matrix and the number of cows
empty_cow_matrix <-  function(master_feeding_drinking2) {
  cow_list <- sort(unique(master_feeding_drinking2$Cow))
  cow_num <- length(cow_list)
  empty_matrix <- matrix(0, cow_num, cow_num)
  colnames(empty_matrix) <- c(cow_list)
  rownames(empty_matrix) <- c(cow_list)
  return (list(empty_matrix,
               cow_num)
  )

}


#' Calculating bout and duration
#' @param cur_worksheet is a data frame containing information about 2 cows on a particular date.
#'
#' @return cur_worksheet with the duration and bout of the 2 cows
Insentec_bout_dur <- function(cur_worksheet) {
  cur_worksheet <- cur_worksheet[order(cur_worksheet$Time),] # sort based on time
  # clear any bout/duration
  cur_worksheet$bout <- 0
  cur_worksheet$duration <- 0
  total_row <- nrow(cur_worksheet)

  for (w in 1:total_row) {

    # if this is the first row
    if (w == 1) {
      cur_worksheet$bout[w] = 1 # set bout to be 1
      cur_worksheet$duration[w] = 1 # set duration to be 1
    }
    else { # if this is not the first row
      time_interval <- cur_worksheet$Time[w-1] %--% cur_worksheet$Time[w]
      time_dur <- as.duration(time_interval)
      time_dur_str <- tolower(as.character(time_dur))

      # if the time gap between current row and the row above is not 1s
      if (time_dur_str != "1s") {
        cur_worksheet$bout[w] <- cur_worksheet$bout[w-1] + 1 # bout number + 1
        cur_worksheet$duration[w] <- 1 # duration reset to 1
      } else { # if the time gap is 1s
        cur_worksheet$bout[w] <- cur_worksheet$bout[w-1] # bout number does not change
        cur_worksheet$duration[w] <- cur_worksheet$duration[w-1] + 1  # duration + 1
      }

    }
  }

  cur_worksheet <- cur_worksheet # make sure the datasheet get returned
  return(cur_worksheet)
}

#' Calculating bout and duration for paired cows
#' @param feed_drink_synch_master_cow2
#' @param feed_drink_synch_master_bin2
#' @param cow_num
#'
#' @return
paired_iterator <- function(feed_drink_synch_master_cow2, feed_drink_synch_master_bin2,cow_num) {

  # list the result sheets we want to get
  paired_feeding_drinking_bout <- list() # number of times 2 cows feeding together
  paired_feeding_drinking_total_time <- list() # total amount of time 2 cows are feeding together
  paired_feeding_drinking_average_dur <- list() # average duration of 2 cows feeding together

  for (i in 1:length(sort(unique(feed_drink_synch_master_cow2$date)))) {
    # read in the current datasheet corresponding to the date
    cur_date <- as.character(sort(unique(feed_drink_synch_master_cow2$date))[i])
    cur_master_sheet <- feed_drink_synch_master_cow2[[cur_date]]
    cur_master_bin_sheet <- feed_drink_synch_master_bin2[[cur_date]]
    used_time <- which(cur_master_sheet$total_cow_num > 1)
    cur_master_sheet <- cur_master_sheet[used_time, ]
    cur_master_bin_sheet <- cur_master_bin_sheet[used_time, ]
    # only when two cows are present, we can calculate paired eating


    # create matrix sheet to store result
    paired_feeding_drinking_bout[[i]] <- empty_matrix
    paired_feeding_drinking_total_time[[i]] <- empty_matrix
    paired_feeding_drinking_average_dur[[i]] <- empty_matrix

    # change names
    names(paired_feeding_drinking_bout)[i] <- as.character(cur_date)
    names(paired_feeding_drinking_total_time)[i] <- as.character(cur_date)
    names(paired_feeding_drinking_average_dur)[i] <- as.character(cur_date)


    # iterate through all cows
    for(k in 1:(cow_num-1)) {
      start_index <- k+1  # the index of the current cow on the cur_master_sheet
      matrix_row_index <- k # the index of cow on the row of result matrix

      # pair each cow up with the cow below her
      for (h in (k+1):cow_num) {
        end_index <- h+1 # the index of the paired other cow
        matrix_col_index <- h # the index of cow on the column of result matrix


        # Simply feeding together
        # get a datasheet with only this two cows' information on current date
        cur_pair <- cur_master_sheet[, c(1, start_index, end_index)]
        cur_pair$total <- rowSums(cur_pair[, 2:3], na.rm = TRUE)
        cur_pair2 <- cur_pair[which(cur_pair$total > 1), ]
        # if the paired cow ever eat together
        if (nrow(cur_pair2) > 0) {
          # calculate bout and duration of feeding together
          cur_pair2 <- Insentec_bout_dur(cur_pair2)
          total_feeding_time <- nrow(cur_pair2)
          total_bout <- max(cur_pair2$bout)
          average_feeding_dur <- total_feeding_time/total_bout
          # record result to the matrix
          paired_feeding_drinking_bout[[i]][matrix_row_index, matrix_col_index] <- total_bout
          paired_feeding_drinking_total_time[[i]][matrix_row_index, matrix_col_index] <- total_feeding_time
          paired_feeding_drinking_average_dur[[i]][matrix_row_index, matrix_col_index] <- average_feeding_dur

        }
        else { # if the paired cow never eat together
          # do nothing, because the matrix default value is 0
        }
      }
    }

  }
  return (list(paired_feeding_drinking_bout,
               paired_feeding_drinking_total_time,
               paired_feeding_drinking_average_dur))
}


#' Calculating bout and duration for neighboring cows
#' @param feed_drink_synch_master_cow2
#' @param feed_drink_synch_master_bin2
#' @param cow_num
#'
#' @return
neighbour_iterator <-  function(feed_drink_synch_master_cow2, feed_drink_synch_master_bin2,cow_num) {

  # list the result sheets we want to get
  neighbor_feeding_drinking_bout <- list() # number of times 2 cows are feeding neighbors
  neighbor_feeding_drinking_total_time <- list() # total amount of time 2 cows are feeding neighbors
  neighbor_feeding_drinking_average_dur <- list() # average duration of 2 cows are feeding neighbors

  for (i in 1:length(sort(unique(feed_drink_synch_master_cow2$date)))) {
    # read in the current datasheet corresponding to the date
    cur_date <- as.character(sort(unique(feed_drink_synch_master_cow2$date))[i])
    cur_master_sheet <- feed_drink_synch_master_cow2[[cur_date]]
    cur_master_bin_sheet <- feed_drink_synch_master_bin2[[cur_date]]
    used_time <- which(cur_master_sheet$total_cow_num > 1)
    cur_master_sheet <- cur_master_sheet[used_time, ]
    cur_master_bin_sheet <- cur_master_bin_sheet[used_time, ]

    # create matrix sheet to store result
    neighbor_feeding_drinking_bout[[i]] <- empty_matrix
    neighbor_feeding_drinking_total_time[[i]] <- empty_matrix
    neighbor_feeding_drinking_average_dur[[i]] <- empty_matrix
    # change names
    names(neighbor_feeding_drinking_bout)[i] <- as.character(cur_date)
    names(neighbor_feeding_drinking_total_time)[i] <- as.character(cur_date)
    names(neighbor_feeding_drinking_average_dur)[i] <- as.character(cur_date)

    # iterate through all cows
      for(k in 1:(cow_num-1)) {
        start_index <- k+1  # the index of the current cow on the cur_master_sheet
        matrix_row_index <- k # the index of cow on the row of result matrix

        # pair each cow up with the cow below her
        for (h in (k+1):cow_num) {
          end_index <- h+1 # the index of the paired other cow
          matrix_col_index <- h # the index of cow on the column of result matrix


          # Feeding Neighbors
          # get a datasheet with only this two cows' information
          cur_neighbor <- cur_master_bin_sheet[, c(1, start_index, end_index)]
          cur_neighbor <- cur_neighbor[which((cur_neighbor[3] != 0) & (cur_neighbor[2] != 0)),]
          cur_neighbor$bin_dif <- abs(cur_neighbor[3] - cur_neighbor[2]) # get the difference between bin number
          cur_neighbor2 <- cur_neighbor[which(cur_neighbor$bin_dif == 1), ]
          # if the paired neighbor ever eat together
          if (nrow(cur_neighbor2) > 0) {
            # calculate bout and duration of feeding neighbors
            cur_neighbor2 <- Insentec_bout_dur(cur_neighbor2)
            total_neighbor_time <- nrow(cur_neighbor2)
            total_neighbor_bout <- max(cur_neighbor2$bout)
            average_neighbor_dur <- total_neighbor_time/total_neighbor_bout
            # record result to the matrix
            neighbor_feeding_drinking_bout[[i]][matrix_row_index, matrix_col_index] <- total_neighbor_bout
            neighbor_feeding_drinking_total_time[[i]][matrix_row_index, matrix_col_index] <- total_neighbor_time
            neighbor_feeding_drinking_average_dur[[i]][matrix_row_index, matrix_col_index] <- average_neighbor_dur
          } else {
            # do nothing if the paired neighbor never eat together
          }
      }


    }
  }
  return (list(neighbor_feeding_drinking_bout,
               neighbor_feeding_drinking_total_time,
               neighbor_feeding_drinking_average_dur))
}

#' Feeding and drinking synchrony analysis
#' @param all.comb2 a list of data frames containing feeding and drinking
#' information for all the cows
#'
#' @return
feeding_drinking_analysis <- function(all.comb2){
  .Deprecated("Use modern synchrony analysis functions", 
              package = "moo4feed",
              msg = "This function is deprecated. Consider using modern animal synchrony analysis functions.")
  
  result_1 <- empty_cow_matrix(all.comb2)
  empty_matrix <- result_1[[1]]
  cow_num <- result_1[[2]]

  paired_results <- paired_iterator(feed_drink_synch_master_cow2, feed_drink_synch_master_bin2, cow_num)
  paired_feeding_drinking_bout <- paired_results[[1]]
  paired_feeding_drinking_total_time <- paired_results[[2]]
  paired_feeding_drinking_average_dur <- paired_results[[3]]

  neighbour_results <- neighbour_iterator(feed_drink_synch_master_cow2, feed_drink_synch_master_bin2, cow_num)
  neighbor_feeding_drinking_bout <- neighbour_results[[1]]
  neighbor_feeding_drinking_total_time <- neighbour_results[[2]]
  neighbor_feeding_drinking_average_dur <- neighbour_results[[3]]

  return(list(paired_feeding_drinking_bout,
              paired_feeding_drinking_total_time,
              paired_feeding_drinking_average_dur,
              neighbor_feeding_drinking_bout,
              neighbor_feeding_drinking_total_time,
              neighbor_feeding_drinking_average_dur))
}






