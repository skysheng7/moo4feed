#' Calculate non-nutritive visits
#'
#' This function calculates the number of non-nutritive visits for each date.
#' In this case, a cow goes to a bin which has more than 0.5kg (calibration error) of feed left
#' and still chooses to not eat anything
#'
#' @param cur_date Date for which to calculate non-nutritive visits.
#' @param cur_list List of data for all dates.
#' @param calibration_error is adjustment for the errors in the readings of the feeder bins
#' @return A data frame with columns "Cow" and "number_of_non_nutritive_visits".
calculate_non_nutritive_visits <- function(cur_date, cur_list, calibration_error) {
  sep_sheet <- cur_list[which((cur_list$Intake <= calibration_error) & (cur_list$Startweight > calibration_error)),]
  non_nutritive_sheet <- count(sep_sheet, vars=c("Cow"))
  colnames(non_nutritive_sheet) <- c("Cow", "number_of_non_nutritive_visits")
  return(non_nutritive_sheet)
}
#' This function handles the data processing of the non-nutritive visits
#' @param non_nutritive_sheet is a data frame with columns "Cow" and "number_of_non_nutritive_visits".
#' @return a list of dataframes containing the count of non-nutritive visits for each cow
#' separated by days. 
non_nutritive_data_process <- function(non_nutritive_sheet) {
  cur_index <- length(non_nutritive_visits) + 1
  non_nutritive_visits[[cur_index]] <- non_nutritive_sheet
  names(non_nutritive_visits)[cur_index] <- as.character(cur_date)
  return (non_nutritive_visits)
}

#' Calculate visited but no feed frequency
#'
#' This function calculates the number of visits when there is no feed for each date.
#' In this case, a cow goes to a bin which has less than 0.5kg of feed left,
#' and does not eat anything
#'
#' @param cur_date Date for which to calculate visited but no feed frequency.
#' @param cur_list List of data for all dates.
#' @param calibration_error is adjustment for the errors in the readings of the feeder bins
#' @return A data frame with columns "Cow" and "number_of_visits_when_no_feed".
calculate_visited_but_no_feed_freq <- function(cur_date, cur_list, calibration_error) {
  no_feed <- cur_list[which((cur_list$Intake <= calibration_error) & (cur_list$Startweight <= calibration_error)),]
  non_feed_sheet <- count(no_feed, vars=c("Cow"))
  colnames(non_feed_sheet) <- c("Cow", "number_of_visits_when_no_feed")
  return(non_feed_sheet)
}
#' This function handles the data processing for visits but no feed
#' @param non_feed_sheet is a data frame with columns "Cow" and "number_of_visits_when_no_feed".
#' @return A list of dataframes containing the count of visits with no feed for each cow
#' separated by days.
no_feed_freq <- function(non_feed_sheet) {
  cur_index <- length(visited_but_no_feed_freq) + 1
  visited_but_no_feed_freq[[cur_index]] <- non_feed_sheet
  names(visited_but_no_feed_freq)[cur_index] <- as.character(cur_date)
  return (visited_but_no_feed_freq)
}

#' Process data for 'non-nutritive visits' and 'visited but no feed frequency'
#'
#' This function processes the data for 'non-nutritive visits' and 'visited but no feed frequency'
# for a given list of dates.
#'
#' @param all_fed2 A list of data for all dates.
#' @return A list containing two elements: 'non-nutritive visits' and 'visited but no feed frequency'.
nutrition_data_process <- function(all_fed2) {
  visited_but_no_feed_freq <- list()
  non_nutritive_visits <- list()
  
  for (i in 1:length(all.fed2)) {
    cur_date <- names(all.fed2)[i]
    cur_list <- all.fed2[[i]]
    
    # Calculate non-nutritive visits - feed left in the bin but cow ate nothing
    non_nutritive_sheet <- calculate_non_nutritive_visits
    non_nutritive_visits <- non_nutritive_data_process(non_nutritive_sheet)
    
    
    # Calculate visited but no feed frequency - cow ate nothing as no feed left in the bin
    non_feed_sheet <- calculate_visited_but_no_feed_freq(cur_date, cur_list, calibration_error)
    visited_but_no_feed_freq <- no_feed_freq(non_feed_sheet)
    
  }
  
  result <- list(non_nutritive_visits, 
                 visited_but_no_feed_freq)
  
  return(result)
}