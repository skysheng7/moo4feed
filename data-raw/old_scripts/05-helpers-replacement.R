#' identify and recprd replacements for a single day. replacements are identified if the time interval
#' between the first cow leaving and the next cow entering is < 26s
#' 
#' @param cur_data The data frame containing feeding/drinking data
#' @param replacement_threshold threshold for replacement behaviours in seconds. 
#' the interval between the first cow leaving and the next cow entering
#' @return A data frame filtered by bins
record_replacement_1day <- function(cur_data, replacement_threshold = 26) {
  sorted_data <- cur_data[order(cur_data$Bin, cur_data$Start, cur_data$End),]
  sorted_data <- sorted_data[, c("Cow", "Bin", "Start", "End")]
  sorted_data$date <- date(sorted_data$Start)
  bin_list <- sort(unique(sorted_data$Bin))
  
  master_df <- data.frame()
  
  for (cur_bin in bin_list) {
    cur_data_bin <- sorted_data[which(sorted_data$Bin == cur_bin),]
    
    next_start_list <- cur_data_bin$Start[2:nrow(cur_data_bin)]
    next_cow_list <- cur_data_bin$Cow[2:nrow(cur_data_bin)]
    cur_data_bin <- cur_data_bin[-nrow(cur_data_bin), ]
    
    cur_data_bin$next_start <- next_start_list
    cur_data_bin$next_cow <- next_cow_list
    
    time_interval <- cur_data_bin$End %--% cur_data_bin$next_start
    cur_data_bin$time_dif <- as.duration(time_interval)
    
    replace_cutoff <- as.duration(paste0(replacement_threshold, "s"))
    cur_data_bin <- cur_data_bin[
      which((cur_data_bin$time_dif <= replace_cutoff) & 
              (cur_data_bin$Cow != cur_data_bin$next_cow)),]
    
    cur_data_bin <- cur_data_bin[, c("Cow", "Bin", "End", "date", "next_cow", "time_dif")]
    # the time of replacement is the end time of the reactor cow's feeding event
    colnames(cur_data_bin) <- c("Reactor_cow", "Bin", "Time", "date", "Actor_cow", "Bout_interval")
    
    master_df <- rbind(master_df, cur_data_bin)
  }
  
  return(master_df)
}

#' identify and record replacements for all the dates in a list. replacements are identified if the time interval
#' between the first cow leaving and the next cow entering is < 26s
#' 
#' @param data_list The data frame containing feeding/drinking data
#' @param replacement_threshold time interval threshold between the first cow leaving and the next cow entering to be considered as a replacement behaviour
#' the interval between the first cow leaving and the next cow entering
#' 
#' @return a list of dataframes containing replacements for each day
record_replacement_allDay <- function(data_list, replacement_threshold) {
  replacement_list_by_date <- list()
  for (i in 1:length(data_list)) {
    cur_data <- data_list[[i]]
    replacement_list_by_date[[i]] <- record_replacement_1day(cur_data, replacement_threshold)
    names(replacement_list_by_date)[i] <- names(data_list)[i]
  }

  return(replacement_list_by_date)
}

#' Check if Actor Cow Has an Alibi on a Specific Day
#'
#' This function determines whether, at the end time of the reactor cow's feeding event, 
#' the actor is eating/drinking from another bin. If that's the case, 
#' the replacement is potentially invalid as the actor has an alibi.
#'
#' @param cur_replacement A data frame of replacement data for a given day.
#' @param cur_feed_wat A data frame containing feeding and drinking events for a given day.
#'
#' @return A data frame of filtered replacement events after checking alibi
check_alibi_daily <- function(cur_replacement, cur_feed_wat) {
  
  cur_replacement$actor_at_another_bin <- 0
  for (k in 1:nrow(cur_replacement)) {
    cur_time <-cur_replacement$Time[k]
    cur_actor <- cur_replacement$Actor_cow[k]
    cur_actor_feed_wat <- cur_feed_wat[which((cur_feed_wat$Cow == cur_actor) & (cur_feed_wat$Start <= cur_time) & (cur_feed_wat$End >= cur_time)),]
    if(nrow(cur_actor_feed_wat)>0) {
      cur_replacement$actor_at_another_bin[k] <- 1
    }
  }
  
  #delete the helper column
  cur_replacement2 <- cur_replacement[which(cur_replacement$actor_at_another_bin == 0),]
  cur_replacement2$actor_at_another_bin <- NULL
  
  return(cur_replacement2)
}

#' Check if Actor Cow Has an Alibi Across Multiple Days
#'
#' This function applies the check_alibi_daily function across multiple days 
#' to determine valid replacement events.
#'
#' @param replacement_list_by_date A list of data frames, each containing replacement data for a specific day.
#' @param all_comb2 A list of data frames, each containing feeding and drinking data for a specific day.
#'
#' @return A list of data frames with filtered replacement events.
check_alibi_all <- function(replacement_list_by_date, all_comb2) {
  for (i in 1:length(all_comb2)) {
    cur_replacement <- replacement_list_by_date[[i]]
    cur_feed_wat <- all_comb2[[i]]
    modified_cur_replacement <- check_alibi_daily(cur_replacement, cur_feed_wat)
    replacement_list_by_date[[i]] <- modified_cur_replacement
  }
  
  return(replacement_list_by_date)
}
