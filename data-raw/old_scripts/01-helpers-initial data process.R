#' process daylight saving date records
#'
#' This function processes the dataframe recording 2 daylight saving dates in 
#' each year: concatenate year-month-date, convert data types, calculate the day
#' after daylight saving day
#' 
#'
#' @param daylight_saving_table A dataframe with 3 columns recording daylight 
#' saving dates once in the spring, once in the fall:
#' Year, Spring (date-month), Fall(date-month)
#' @return daylight_saving_table dataframe after processing.
#' @examples
#' daylight_saving_process(your_dataframe)
daylight_saving_process <- function(daylight_saving_table)
{
  colnames(daylight_saving_table) <- c("Year", "Spring", "Fall")
  daylight_saving_table$Year <- as.character(daylight_saving_table$Year)
  daylight_saving_table$Spring <- ydm(paste(daylight_saving_table$Year, daylight_saving_table$Spring, sep = "-"), tz=time_zone)
  daylight_saving_table$Fall <- ydm(paste(daylight_saving_table$Year, daylight_saving_table$Fall, sep = "-"), tz=time_zone)
  daylight_saving_table$Year <- as.integer(daylight_saving_table$Year)
  daylight_saving_table <- daylight_saving_table[order(daylight_saving_table$Year),]
  daylight_saving_table$Spring_nextDay <- daylight_saving_table$Spring + days(1)
  daylight_saving_table$Fall_nextDay <- daylight_saving_table$Fall + days(1)
  return(daylight_saving_table)
}

#' Determine the daylight saving change date and time depending on the season
#'
#' @param cur_year Current year.
#' @param cur_month Current month.
#' @param daylight_saving_table The table containing daylight saving information.
#'
#' @return A list containing the daylight change date, next day date, and times.
determine_daylight_saving_times <- function(cur_year, cur_month, daylight_saving_table) {
  cur_year_line <- daylight_saving_table[which(daylight_saving_table$Year == cur_year), ]
  
  if (cur_month > 9) {
    daylight_change_date <- cur_year_line$Fall[1]
    daylight_change_next_date <- cur_year_line$Fall_nextDay[1]
  } else {
    daylight_change_date <- cur_year_line$Spring[1]
    daylight_change_next_date <- cur_year_line$Spring_nextDay[1]
  }
  
  daylight_change_time <- hms("2:00:00")
  daylight_change_time2 <- hms("3:00:00")
  
  return(list(daylight_change_date = daylight_change_date,
              daylight_change_next_date = daylight_change_next_date,
              daylight_change_time = daylight_change_time,
              daylight_change_time2 = daylight_change_time2))
}

#' Adjust time entries in data for daylight saving in the fall. 
#' Daylight saving change date in the fall: time went from 2am back to 1am 
#'
#' This function modifies start and end times in a dataframe based on daylight saving changes.
#' It is designed to adjust dataframes with columns `Start` and `End` representing times of events.
#' Events happening between 2-3am are deleted, and times after 3am are shifted backwards by an hour.
#'
#' @param data_frame Feed or water visit dataframe containing columns `Start` and `End` with time entries.
#' @param daylight_change_time A \code{hms} object representing the start hour of daylight saving change.
#' @param daylight_change_time2 A \code{hms} object representing the end hour of the dayligh saving change.
#' @return A dataframe with adjusted time entries based on daylight saving in the fall.
#' @export
adjust_data_for_daylight_saving_fall <- function(data_frame, daylight_change_time, daylight_change_time2) {
  temp <- data_frame
  temp$Start2 <- hms(temp$Start)
  temp$End2 <- hms(temp$End)
  
  # delete all events with Start time or end time happened between 2-3am
  if (nrow(temp[which((temp$Start2 > daylight_change_time) & (temp$Start2 <= daylight_change_time2)), ]) > 0) {
    temp <- temp[-which((temp$Start2 > daylight_change_time) & (temp$Start2 <= daylight_change_time2)), ]
  } 
  if (nrow(temp[which((temp$End2 > daylight_change_time) & (temp$End2 <= daylight_change_time2)), ]) > 0) {
    temp <- temp[-which((temp$End2 > daylight_change_time) & (temp$End2 <= daylight_change_time2)), ]
  }
  
  # handle start time change
  before_change <- temp[which(temp$Start2 <= daylight_change_time), ]
  after_change <- temp[which(temp$Start2 > daylight_change_time2), ]
  after_change$Start2 <- after_change$Start2 - hours(1)
  after_change$Start <- paste(as.character(hour(after_change$Start2)), as.character(minute(after_change$Start2)), as.character(second(after_change$Start2)), sep = ":")
  temp <- rbind(before_change, after_change)
  
  # handle end time change
  before_change <- temp[which(temp$End2 <= daylight_change_time), ]
  after_change <- temp[which(temp$End2 > daylight_change_time2), ]
  after_change$End2 <- after_change$End2 - hours(1)
  after_change$End <- paste(as.character(hour(after_change$End2)), as.character(minute(after_change$End2)), as.character(second(after_change$End2)), sep = ":")
  temp <- rbind(before_change, after_change)
  
  # delete helping columns
  temp$Start2 <- NULL
  temp$End2 <- NULL
  
  return(temp)
}

#' Adjust time entries in data for daylight saving in the spring 
#' Daylight saving change date in the spring: time went from 2am to 3am directly
#'
#' This function adjusts start and end times in a dataframe for events that span across
#' a daylight saving time change. The function is designed to process dataframes with columns 
#' `Start` and `End` that represent times of events. Events starting before 2 and 
#' ending between 2am-3am are deleted. The other events' start and end times 
#' are shifted forward for 1 hour if happened after 2am 
#'
#' @param data_frame Feed or water visit dataframe containing columns `Start` and `End` with time entries.
#' @param daylight_change_time A \code{hms} object representing the start hour of daylight saving change.
#' @return A dataframe with adjusted time entries based on the daylight saving time change in spring.
adjust_data_for_daylight_saving_spring <- function(data_frame, daylight_change_time) {
  temp <- data_frame
  temp$Start2 <- hms(temp$Start)
  temp$End2 <- hms(temp$End)
  
  # handle special occasions when start time < daylight_change_time, but end time > daylight_change_time. delete all of those events
  if (nrow(temp[which((temp$Start2 <= daylight_change_time) & (temp$End2 > daylight_change_time)), ]) > 0) {
    temp <- temp[-which((temp$Start2 <= daylight_change_time) & (temp$End2 > daylight_change_time)), ]
  }
  
  # handle start time change
  before_change <- temp[which(temp$Start2 <= daylight_change_time), ]
  after_change <- temp[which(temp$Start2 > daylight_change_time), ]
  after_change$Start2 <- after_change$Start2 + hours(1)
  after_change$Start <- paste(as.character(hour(after_change$Start2)), as.character(minute(after_change$Start2)), as.character(second(after_change$Start2)), sep = ":")
  temp <- rbind(before_change, after_change)
  
  # handle end time change
  before_change <- temp[which(temp$End2 <= daylight_change_time), ]
  after_change <- temp[which(temp$End2 > daylight_change_time), ]
  after_change$End2 <- after_change$End2 + hours(1)
  after_change$End <- paste(as.character(hour(after_change$End2)), as.character(minute(after_change$End2)), as.character(second(after_change$End2)), sep = ":")
  temp <- rbind(before_change, after_change)
  
  # delete helping columns
  temp$Start2 <- NULL
  temp$End2 <- NULL
  
  return(temp)
}


#' Handle Events that Span Across Days After Daylight Time Change
#'
#' This function processes a dataframe to retain only the events that occur after
#' the feed bin system's time got adjusted to the time after daylight saving change.
#' Due to daylight saving change in the spring, dataframe recording visit data for 
#' the 2nd day after daylight saving change started with visits happened in the last 
#' few hours before midnight on the daylight saving day, which needs to be deleted
#' Specifically, it deletes rows until it finds a row where the start time is after 20:00 
#' and the subsequent row's start time is before 05:00.
#'
#' @param data_frame A dataframe containing a `Start` column with time entries.
#' @return A modified dataframe with selected rows based on the time threshold.
#' @export
handle_next_day_after_daylight <- function(data_frame) {
  temp <- data_frame
  temp$Start2 <- as.integer(hour(hms(temp$Start)))
  temp$toDelete <- 0
  
  for (p in 1:(nrow(temp)-1)) {
    temp$toDelete[p] <- 1
    if ((temp$Start2[p] > 20) & (temp$Start2[p+1] < 5)) {
      break
    }
  }
  
  temp2 <- temp[which(temp$toDelete == 0), ]
  temp2$toDelete <- NULL
  temp2$Start2 <- NULL
  
  return(temp2)
}


#' Adjust Data for Daylight Saving Changes
#'
#' This function adjusts the provided data based on daylight saving changes for a given year and month. 
#' Depending on the current month (spring or fall), and the exact date (whether it's the daylight saving change date 
#' or the subsequent day), it applies the necessary modifications to the data frame.
#'
#' @param df Dataframe with the events (feed or water visit data) containing columns `Start` and `End` with time entries.
#' @param cur_date the current date of the dataframe
#' @param cur_year Integer representing the current year.
#' @param cur_month Integer representing the current month.
#' @param daylight_saving_table Dataframe containing information about daylight saving changes, with columns for the year, spring and fall change dates, and the subsequent day dates.
#' 
#' @return A dataframe adjusted for daylight saving time changes.
daylight_saving_adjust <- function(df, cur_date, cur_year, cur_month, daylight_saving_table) {
  # 1. Determine the daylight saving times
  daylight_times <- determine_daylight_saving_times(cur_year, cur_month, daylight_saving_table)
  
  # 2. Adjust data for daylight saving based on conditions
  # determine if current period is in the spring or fall
  # this is fall: delete 2am-3am and move all time after 3am by 1 hour earlier
  if (cur_month > 9) {  
    # If current day is the day when daylight saving change happened
    if (cur_date == daylight_times$daylight_change_date) {
      df <- adjust_data_for_daylight_saving_fall(df, daylight_times$daylight_change_time, daylight_times$daylight_change_time2)
    } 
    
    # this is spring: move all time after 2am to be 1 hour later
  } else { 
    # If current day is the day when daylight saving change happened
    if (cur_date == daylight_times$daylight_change_date) {
      df <- adjust_data_for_daylight_saving_spring(df, daylight_times$daylight_change_time)
    
      # If current day is the next day after daylight saving change happened
    } else if (cur_date == daylight_times$daylight_change_next_date) {
      df <- handle_next_day_after_daylight(df)
    }
  }
  
  return(df)
}


#' Process file names
#' 
#' This function processes a vector of file names: trims white spaces, replaces 
#' slashes and spaces with underscores, and extracts date information. It then 
#' returns a data frame with original file names and extracted dates.
#' 
#' @param fileNames A character vector of file names.
#' @param col_name A string specifying the column name for the file names in the 
#' output data frame.
#' 
#' @return A data frame with original file names and extracted dates.
#' 
#' @examples
#' file_name_processing(c("feed/VR200715.DAT", "feed/VR200716.DAT"), "Feed_dir")
file_name_processing <- function(fileNames, col_name){
  file_name <- data.frame(fileNames)
  colnames(file_name) <- c(col_name)
  file_name_mod <- chartr("/ .", "___", trimws(file_name[,1]))
  date <- sapply(strsplit(file_name_mod, "_"), function(x) substring(x[length(x)-1], 3))
  cbind(file_name, date = date)
}

#' Compare feed and water file names
#' 
#' This function compares feed and water file names, and keeps only the file 
#' names that have both feed and water data available at the same time.
#' 
#' @param fileNames.f A character vector of feed file names.
#' @param fileNames.w A character vector of water file names.
#' 
#' @return A list with updated feed and water file names (dates with both feed 
#' and water data available at the same time).
#' 
#' @examples
#' compare_files(c("feed/VR200715.DAT", "feed/VR200716.DAT"), c("water/VW200715.DAT"))
compare_files <- function(fileNames.f, fileNames.w){
  feed_name <- file_name_processing(fileNames.f, "Feed_dir")
  wat_name <- file_name_processing(fileNames.w, "Drink_dir")
  compare_sheet <- merge(feed_name, wat_name, all = TRUE)
  compare_sheet <- compare_sheet[order(compare_sheet$date),]
  compare_sheet2 <- na.omit(compare_sheet)
  compare_sheet2$date <- ymd(compare_sheet2$date, tz=time_zone)
  compare_sheet2
}

#' Get date range for this dataset
#' 
#' This function takes in a dataframe generated by compare_files() 
#' and calculates the date range based on the date column.
#' 
#' @param df A dataframe generated by compare_files().
#' 
#' @return A list with start date, end date, and date range string.
#' 
#' @examples
#' df <- compare_files(c("feed/VR200715.DAT", "feed/VR200716.DAT"), c("water/VW200715.DAT"))
#' get_date_range(df)
get_date_range <- function(df){
  # Check if the dataframe has a date column
  if(!"date" %in% names(df)){
    stop("The input dataframe does not have a date column.")
  }
  
  # Check if the dataframe has any rows
  if(nrow(df) == 0){
    print("Warning: The input dataframe does not have any rows.")
    return(list())
  }
  
  # Calculate start and end date
  start_date <- min(df$date)
  end_date <- max(df$date)
  
  # Generate date range string
  date_range <- ifelse((start_date != end_date), paste(start_date, end_date, sep = "_"), as.character(start_date))
  
  list(start_date = start_date, end_date = end_date, date_range = date_range)
}


#' Remove specific cows from the dataframe
#'
#' This function filters out rows from the provided dataframe `df` 
#' that contain cows specified in the `cow_delete_list`.
#' 
#' @param df A dataframe containing a column named "Cow".
#' @param cow_delete_list A vector of cows to be deleted from `df`.
#'
#' @return A dataframe without the rows containing cows from `cow_delete_list`.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(Cow = c("A", "B", "C", "D"), Value = c(1, 2, 3, 4))
#' cow_delete_list <- c("A", "C")
#' cow_delete(df, cow_delete_list)
#' }
cow_delete <- function(df, cow_delete_list) {
  to_delete <- df[which(df$Cow %in% cow_delete_list),]
  if (nrow(to_delete) != 0) {
    df_processed <- df[-which(df$Cow %in% cow_delete_list),]
    return(df_processed)
  } else {
    return(df)
  }
}


#' Delete rows with specific transponder ID
#'
#' @param df Data frame from which to delete rows.
#' @param transponder_delete_list List of transponder ID to delete.
#' @return Data frame with rows that contain certain trasponder ID deleted.
transponder_delete <- function(df, transponder_delete_list) {
  to_delete <- df[which(df$Transponder %in% transponder_delete_list), ]
  if (nrow(to_delete) != 0) {
    df[-which(df$Transponder %in% transponder_delete_list), ]
  } else {
    df
  }
}

#' Delete rows outside specific bin range
#'
#' @param df Data frame from which to delete rows.
#' @param min_bin Minimum bin ID to keep.
#' @param max_bin Maximum bin ID to keep.
#' @return Data frame with rows deleted.
bin_delete <- function(df, min_bin, max_bin) {
  df[df$Bin >= min_bin & df$Bin <= max_bin, ]
}

#' Safely Read Data from a File
#'
#' This function attempts to read a data file and returns an empty data frame if the file appears empty.
#' Any other error during reading will also result in an empty data frame being returned.
#'
#' @param file_name Character string specifying the path to the file to read.
#'
#' @return A data frame containing the file contents. If the file is empty or another error occurs, an empty data frame is returned.

read_data_safely <- function(file_name) {
  result <- tryCatch(
    {
      data <- read.table(file_name, header = F, sep = ",")
      return(data)
    },
    error = function(e) {
      message(paste0("The file seems to be empty or there's another issue. Returning an empty data frame: ", file_name))
      return(data.frame())
    }
  )
  return(result)
}


#' Process feeder data
#'
#' @param file_name File path for the feeder data.
#' @param coln Column names for the feeder data.
#' @param cow_delete_list List of cow values to delete.
#' @param feed_transponder_delete_list List of transponder values specific to feeder data to delete.
#' @param min_feed_bin Minimum feeder bin value to keep.
#' @param max_feed_bin Maximum feeder bin value to keep.
#' @param feed_coln_to_keep Columns to keep in the feeder data.
#' @return Processed data frame for feeder.
process_feeder_data <- function(file_name, coln, cow_delete_list, feed_transponder_delete_list, min_feed_bin, max_feed_bin, feed_coln_to_keep) {
  feeder = read_data_safely(file_name)
  if (nrow(feeder) == 0) {
    return(data.frame())
  } else {
    colnames(feeder) = coln
    feeder = cow_delete(feeder, cow_delete_list)
    feeder = transponder_delete(feeder, feed_transponder_delete_list)
    feeder = bin_delete(feeder, min_feed_bin, max_feed_bin)
    feeder <- feeder[, feed_coln_to_keep]
    
    return(feeder)
  }
  
}

#' Process water data
#'
#' @param file_name File path for the water data.
#' @param coln_wat Column names for the water data.
#' @param cow_delete_list List of cow values to delete.
#' @param wat_transponder_delete_list List of transponder values specific to water data to delete.
#' @param min_wat_bin Minimum water bin value to keep.
#' @param max_wat_bin Maximum water bin value to keep.
#' @param wat_coln_to_keep Columns to keep in the water data.
#' @param bin_id_add The number to add to bin IDs in order to distinguish with feed bin ID
#
#' @return Processed data frame for water.
process_water_data <- function(file_name, coln_wat, cow_delete_list, wat_transponder_delete_list, min_wat_bin, max_wat_bin, wat_coln_to_keep, bin_id_add) {
  water = read_data_safely(file_name)
  if (nrow(water) == 0) {
    return(data.frame())
  } else {
    colnames(water) = coln_wat
    water = cow_delete(water, cow_delete_list)
    water = transponder_delete(water, wat_transponder_delete_list)
    water = bin_delete(water, min_wat_bin, max_wat_bin)
    water <- rename_water_bins(water[, wat_coln_to_keep],bin_id_add, min_wat_bin, max_wat_bin)
    
    return(water)
  }
  
}

#' Rename water bins
#'
#' @param water_df Data frame of water data.
#' @param bin_id_add The number to add to bin IDs in order to distinguish with feed bin ID
#' @param min_wat_bin Minimum water bin value for renaming.
#' @param max_wat_bin Maximum water bin value for renaming.
#' @return Data frame with water bins renamed.
rename_water_bins <- function(water_df, bin_id_add, min_wat_bin, max_wat_bin) {
  for (i in min_wat_bin:max_wat_bin) {
    water_df$Bin[which(water_df$Bin == i)] = bin_id_add + i
  }
  water_df
}

#' Process Multiple Feeder Data Files
#'
#' This function processes multiple feeder data files, trims the start and end times, 
#' adjusts for daylight saving, and finally returns a list of the processed data.
#'
#' @param fileNames.f A character vector containing file paths of feeder data to be processed. feed data files must be named as "**yymmdd.DAT"
#' @param coln Column names for the data.
#' @param cow_delete_list A list of cows to be deleted from the analysis.
#' @param feed_transponder_delete_list A list of feed transponders to be deleted from the analysis.
#' @param min_feed_bin Numeric. Minimum ID for the feed bin.
#' @param max_feed_bin Numeric. Maximum ID for the feed bin.
#' @param feed_coln_to_keep A character vector of column names to retain in the final output.
#'
#' @return A list of processed feeder data, with each list item corresponding to a file.ID
process_all_feed <- function(fileNames.f, coln, cow_delete_list, feed_transponder_delete_list, min_feed_bin, max_feed_bin, feed_coln_to_keep) {
  len = length(fileNames.f)
  all.fed=list()

  for(i in 1:len)
  {
    fed.1 = process_feeder_data(as.character(fileNames.f[i]), coln, cow_delete_list, feed_transponder_delete_list, min_feed_bin, max_feed_bin, feed_coln_to_keep)
    all.fed[[i]]=na.omit(fed.1)
    date=substring(as.character(fileNames.f[i]),nchar(as.character(fileNames.f[i]))-9,nchar(as.character(fileNames.f[i]))-4) #this gets the date from the file name
    cur_date <- ymd(date, tz=time_zone)
    date = as.character(cur_date)
    
    if (nrow(fed.1) > 0) {
      # trim the start and end time format
      all.fed[[i]]$Start <- trimws(all.fed[[i]]$Start, which = "both")
      all.fed[[i]]$End <- trimws(all.fed[[i]]$End, which = "both")
      
      ############################ Daylight saving change ########################
      
      cur_year <- as.integer(year(cur_date))
      cur_month <- as.integer(month(cur_date))
      all.fed[[i]] = daylight_saving_adjust(all.fed[[i]], cur_date, cur_year, cur_month, daylight_saving_table)
      
      #Adjusting start and end times to make R recognize the date and time format
      all.fed[[i]]$Start=paste(rep(date,dim(all.fed[[i]])[1]),all.fed[[i]]$Start)
      all.fed[[i]]$Start=ymd_hms(all.fed[[i]]$Start, tz=time_zone)
      all.fed[[i]]$End=paste(rep(date,dim(all.fed[[i]])[1]),all.fed[[i]]$End)
      all.fed[[i]]$End=ymd_hms(all.fed[[i]]$End, tz=time_zone)
    }

    #Adding dates as name
    names(all.fed)[i]=date
  }
  
  return(all.fed)
}



#' Process Multiple Water Data Files
#'
#' This function processes multiple water data files, adjusts timings, 
#' and returns a list of the processed data.
#'
#' @param fileNames.w A character vector containing file paths of water data to be processed. Water data files must be named as "**yymmdd.DAT"
#' @param coln.wat Column names for the water data.
#' @param cow_delete_list A list of cows to be deleted from the analysis.
#' @param wat_transponder_delete_list A list of water transponders to be deleted from the analysis.
#' @param min_wat_bin Numeric. Minimum value for water data.
#' @param max_wat_bin Numeric. Maximum value for water data.
#' @param wat_coln_to_keep A character vector of column names to retain in the final output.
#' @param bin_id_add The number to add to bin IDs in order to distinguish with feed bin ID
#'
#' @return A list of processed water data, with each list item corresponding to a file.
#' @export
#'
#' @examples
#' # This is a placeholder for example usage, it's good practice to include a working example here.
#' process_all_water(fileNames.w = c("path_to_water_file1", "path_to_water_file2"), ...)
process_all_water <- function(fileNames.w, coln.wat, cow_delete_list, wat_transponder_delete_list, min_wat_bin, max_wat_bin, wat_coln_to_keep, bin_id_add) {
  
  len = length(fileNames.w)
  all.wat=list()
  
  for(i in 1:len)
  {

    wat.1 = process_water_data(as.character(fileNames.w[i]), coln.wat, cow_delete_list, wat_transponder_delete_list, min_wat_bin, max_wat_bin, wat_coln_to_keep, bin_id_add)
    all.wat[[i]]=na.omit(wat.1)
    
    date=substring(as.character(fileNames.w[i]),nchar(as.character(fileNames.w[i]))-9,nchar(as.character(fileNames.w[i]))-4) #this gets the date from the file name
    cur_date <- ymd(date, tz=time_zone)
    date = as.character(cur_date)
    
    if (nrow(wat.1) > 0 ) {
      all.wat[[i]]=all.wat[[i]][which(all.wat[[i]]$Bin>bin_id_add),]
      
      # trim the start and end time format
      all.wat[[i]]$Start <- trimws(all.wat[[i]]$Start, which = "both")
      all.wat[[i]]$End <- trimws(all.wat[[i]]$End, which = "both")
      
      ############################ Daylight saving change ########################
      cur_year <- as.integer(year(cur_date))
      cur_month <- as.integer(month(cur_date))
      all.wat[[i]] = daylight_saving_adjust(all.wat[[i]], cur_date, cur_year, cur_month, daylight_saving_table)
      
      #Adjusting start and end times to make R recognize the date and time format
      all.wat[[i]]$Start=paste(rep(date,dim(all.wat[[i]])[1]),all.wat[[i]]$Start)
      all.wat[[i]]$Start=ymd_hms(all.wat[[i]]$Start, tz=time_zone)
      all.wat[[i]]$End=paste(rep(date,dim(all.wat[[i]])[1]),all.wat[[i]]$End)
      all.wat[[i]]$End=ymd_hms(all.wat[[i]]$End, tz=time_zone)
    }
    
    #Adding dates as name
    names(all.wat)[i]=date
  }
  
  return(all.wat)
}


#' Combine feeder and water data
#'
#' This function takes in lists containing feeder and water data and returns 
#' a combined list where each element is the row-bound version of the 
#' respective elements of the input lists.
#'
#' @param all.fed A list containing data frames of feeder data.
#' @param all.wat A list containing data frames of water data.
#'
#' @return A list combined feed and water data, grouped by dates
combine_feeder_and_water_data <- function(all.fed, all.wat) {
  
  # Check if the lengths of all.fed and all.wat are the same
  if(length(all.fed) != length(all.wat)) {
    stop("The lengths of all.fed and all.wat are not the same!")
  }
  
  all.comb <- list()
  
  for(i in seq_along(all.fed)) {
    all.comb[[i]] = rbind(all.fed[[i]], all.wat[[i]])
    #Adding dates as name
    date = names(all.fed)[i]
    names(all.comb)[i]=date
  }
  
  return(all.comb)
}

#' Merge data into a master sheet, and add a date column
#'
#' This function consolidates all the data frames present in the input list
#' and returns a single data frame.
#'
#' @param data_list A list containing data frames.
#'
#' @return A consolidated data frame.
merge_data_add_date <- function(data_list) {
  master_data <- merge_data(data_list)
  
  master_data$date <- date(master_data$Start)
  
  return(master_data)
}

#' Just merge data into a master sheet
#'
#' This function consolidates all the data frames present in the input list
#' and returns a single data frame.
#'
#' @param data_list A list containing data frames.
#'
#' @return A consolidated data frame.
merge_data <- function(data_list) {
  
  if (length(data_list) == 0) {
    stop("The input list is empty!")
  }
  
  # Use do.call with rbind to efficiently concatenate all data frames in the list
  master_data <- do.call(rbind, data_list)
  
  return(master_data)
}

#' Capitalize the first letter of a string.
#'
#' This function capitalizes the first letter of a given string while making all other characters lowercase.
#' 
#' @param s A character string to be capitalized.
#' 
#' @return A string with the first letter capitalized.
capitalizeFirst <- function(s) {
  paste0(toupper(substr(s, 1, 1)), tolower(substr(s, 2, nchar(s))))
}

#' De-capitalize the first letter of a string.
#'
#' This function de-capitalizes the first letter of a given string while keeping the rest of the characters unchanged.
#' 
#' @param s A character string whose first letter will be de-capitalized.
#' 
#' @return A string with the first letter de-capitalized.
deCapitalizeFirst <- function(s) {
  paste0(tolower(substr(s, 1, 1)), substr(s, 2, nchar(s)))
}

