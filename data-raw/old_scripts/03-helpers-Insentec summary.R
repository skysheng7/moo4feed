#' Summarize Feed or Water Data
#'
#' This function aggregates and summarizes the total daily intake, duration, and visits from a data frame
#' containing feed or water data for cows.
#'
#' @param data_frame A data frame containing at least the columns 'Intake', 'Duration', 'date', and 'Cow'.
#' @param type A character string, either 'Feeding' or 'Drinking', indicating the type of data to be processed.
#'
#' @return A list containing three data frames: 'intake', 'duration', and 'visits' each summarizing the respective metric for each cow and date.
summarize_feed_water_data <- function(data_frame, type = "Feeding") {
  type <- capitalizeFirst(type)
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
  visits <- count(data_frame, vars = c("date", "Cow"))
  colnames(visits) <- c("date", "Cow", paste0(type, "_Visits"))

  # Return a list of the three summary data.frames
  return(list(intake = intake,
              duration = duration,
              visits = visits))
}

#' Check Intake Levels for Cows
#'
#' This function checks if the intake (either feeding or drinking) for cows is above or below
#' the specified thresholds and updates the warning data accordingly.
#'
#' @param intake_data A data frame containing intake data for cows. Should have columns
#'   for date, cow, and the respective intake values.
#' @param warning_data A data frame containing warning data where the results are added.
#' @param type A character string specifying the type of intake, either "feeding" or "drinking".
#' @param limit A character string specifying if the function should check for "low" or "high" intake.
#' @param feed_intake_low_bar A numeric threshold for low feeding intake.
#' @param feed_intake_high_bar A numeric threshold for high feeding intake.
#' @param water_intake_low_bar A numeric threshold for low water intake.
#' @param water_intake_high_bar A numeric threshold for high water intake.
#'
#' @return A data frame (`warning_data`) updated with any abnormal intakes.
check_intake <- function(intake_data, warning_data, type = c("feeding", "drinking"),
                         limit = c("low", "high"), feed_intake_low_bar = feed_intake_low_bar,
                         feed_intake_high_bar = feed_intake_high_bar,
                         water_intake_low_bar = water_intake_low_bar,
                         water_intake_high_bar = water_intake_high_bar) {
  type <- deCapitalizeFirst(type)

  # Define thresholds based on type and limit
  thresholds <- list(
    feeding = list(low = feed_intake_low_bar, high = feed_intake_high_bar),
    drinking = list(low = water_intake_low_bar, high = water_intake_high_bar)
  )

  threshold <- thresholds[[type]][[limit]]
  colname <- ifelse(type == "feeding", "Feeding_Intake(kg)", "Drinking_Intake(kg)")

  # Subset data based on threshold
  if (limit == "low") {
    abnormal_intake <- intake_data[which(intake_data[[colname]]< threshold), ]
  } else if (limit == "high") {
    abnormal_intake <- intake_data[which(intake_data[[colname]]> threshold), ]
  }


  if (nrow(abnormal_intake) > 0) {

    abnormal_intake$comb_str <- paste("Cow ", abnormal_intake$Cow, ", ", abnormal_intake[[colname]], "kg")

    for (i in 1:nrow(warning_data)) {
      cur_date <- warning_data$date[i]
      cur_day_abnormal <- abnormal_intake[which(abnormal_intake$date == cur_date), ]
      cur_day_abnormal_cow <- sort(unique(cur_day_abnormal$comb_str))
      middle_name <- ifelse(type == "feeding", "feed", "water")
      colname <- paste0(limit, "_daily_", middle_name, "_intake_cows")
      warning_data[[colname]][i] <- paste(unlist(cur_day_abnormal_cow), collapse = "; ")
    }
  }

  return(warning_data)
}

#' Merge Feed and Water Summary Data
#'
#' This function first summarizes the feed and water data for each cow on a daily basis.
#' It then checks and updates the warnings for cows with abnormally high or low intakes.
#' Finally, it merges the summaries and warnings together to return a final data set.
#'
#' @param master_f A data frame containing feed data.
#' @param master_d A data frame containing drink data.
#' @param Insentec_warning A data frame containing the initial warning data.
#' @param feed_intake_low_bar A numeric threshold for low feeding intake.
#' @param feed_intake_high_bar A numeric threshold for high feeding intake.
#' @param water_intake_low_bar A numeric threshold for low water intake.
#' @param water_intake_high_bar A numeric threshold for high water intake.
#'
#' @return A list containing two data frames: 'Insentec_final_summary' which summarizes
#'   the feed and water data, and 'Insentec_warning' which contains the updated warnings.
merge_feed_water_summary <- function(master_f = NULL, master_d = NULL, Insentec_warning,
                                     feed_intake_low_bar, feed_intake_high_bar,
                                     water_intake_low_bar, water_intake_high_bar) {

  # Initializing lists
  list_to_join <- list()

  # get feed and drinking summary for each day for each cow

  if (!is.null(master_f)) {
    feed_summary <- summarize_feed_water_data(master_f, type = "Feeding")
    # feeding
    feeding_intake <- feed_summary$intake
    feeding_duration <- feed_summary$duration
    feeding_visits <- feed_summary$visits

    # check for low & high feeding intake
    Insentec_warning <- check_intake(feeding_intake, Insentec_warning, type = "feeding",
                                     limit = "low", feed_intake_low_bar, feed_intake_high_bar,
                                     water_intake_low_bar, water_intake_high_bar)
    Insentec_warning <- check_intake(feeding_intake, Insentec_warning, type = "feeding",
                                     limit = "high", feed_intake_low_bar, feed_intake_high_bar,
                                     water_intake_low_bar, water_intake_high_bar)

    list_to_join <- c(list_to_join, list(feeding_intake, feeding_duration, feeding_visits))
  }

  if (!is.null(master_d)) {
    drink_summary <- summarize_feed_water_data(master_d, type = "Drinking")
    # drinking
    drinking_intake <- drink_summary$intake
    drinking_duration <- drink_summary$duration
    drinking_visits <- drink_summary$visits

    # check for low & high drinking intake
    Insentec_warning <- check_intake(drinking_intake, Insentec_warning, type = "drinking",
                                     limit = "low", feed_intake_low_bar, feed_intake_high_bar,
                                     water_intake_low_bar, water_intake_high_bar)
    Insentec_warning <- check_intake(drinking_intake, Insentec_warning, type = "drinking",
                                     limit = "high", feed_intake_low_bar, feed_intake_high_bar,
                                     water_intake_low_bar, water_intake_high_bar)

    list_to_join <- c(list_to_join, list(drinking_intake, drinking_duration, drinking_visits))
  }

  if (length(list_to_join) > 0) {
    Insentec_final_summary <- join_all(list_to_join, by = c("date", "Cow"))
    Insentec_final_summary <- Insentec_final_summary[order(Insentec_final_summary$date, Insentec_final_summary$Cow),]
    Insentec_final_summary[is.na(Insentec_final_summary)] <- 0 # replace NA with 0

    save(Insentec_warning, file = (here::here(paste0("data/results/", "Insentec_warning.rdata"))))
    save(Insentec_final_summary, file = (here::here(paste0("data/results/", "Feeding and drinking analysis.rdata"))))
    cache("Insentec_final_summary")
  }

  return(list(Insentec_final_summary = ifelse(exists("Insentec_final_summary"), Insentec_final_summary, NULL),
              Insentec_warning = Insentec_warning))
}




