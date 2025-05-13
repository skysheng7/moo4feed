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
#' @export
check_intake <- function(intake_data, warning_data, type = c("feeding", "drinking"),
                         limit = c("low", "high"), feed_intake_low_bar = feed_intake_low_bar,
                         feed_intake_high_bar = feed_intake_high_bar,
                         water_intake_low_bar = water_intake_low_bar,
                         water_intake_high_bar = water_intake_high_bar) {
  type <- lower_first(type)

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


