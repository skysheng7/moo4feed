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
