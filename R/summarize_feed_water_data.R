utils::globalVariables(c("Cow", "date"))

#' Summarize Feed or Water Data
#'
#' This function aggregates and summarizes the total daily intake, duration, and visits from a data frame
#' containing feed or water data for cows.
#'
#' @param data_frame A data frame containing at least the columns 'Intake', 'Duration', 'date', and 'Cow'.
#' @param type A character string, either 'Feeding' or 'Drinking', indicating the type of data to be processed.
#'
#' @return A list containing three data frames: 'intake', 'duration', and 'visits' each summarizing the respective metric for each cow and date.
#' @export
#' @importFrom stats aggregate
summarize_feed_water_data <- function(data_frame, type = "Feeding") {
  type <- cap_first(type)
  # Ensure type is either "Feeding" or "Drinking"
  if(!type %in% c("Feeding", "Drinking")) {
    stop("The type should be either 'Feeding' or 'Drinking'.")
  }

  # Intake
  intake <- aggregate(data_frame[, "Intake"], list(data_frame$date, data_frame$Cow), sum)
  colnames(intake) <- c("date", "Cow", paste0(type, "_Intake(kg)"))

  # Duration
  duration <- aggregate(data_frame[, "Duration"], list(data_frame$date, data_frame$Cow), sum)
  colnames(duration) <- c("date", "Cow", paste0(type, "_Duration(s)"))

  # Visits
  # visits <- count(data_frame, vars = c("date", "Cow"))
  visits <- data_frame |>
    dplyr::count(date, Cow)
  colnames(visits) <- c("date", "Cow", paste0(type, "_Visits"))

  # Return a list of the three summary data.frames
  return(list(intake = intake,
              duration = duration,
              visits = visits))
}

