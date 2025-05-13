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
#'
#' @export
#' @importFrom here here
#' @importFrom memoise cache
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
    Insentec_final_summary <- purrr::reduce(list_to_join, dplyr::full_join, by = c("date", "Cow"))
    Insentec_final_summary <- Insentec_final_summary[order(Insentec_final_summary$date, Insentec_final_summary$Cow),]
    Insentec_final_summary[is.na(Insentec_final_summary)] <- 0 # replace NA with 0
    # Ensure extdata directory exists
    dir.create(here::here("inst/extdata"), recursive = TRUE, showWarnings = FALSE)

    save(Insentec_warning, file = here::here("inst/extdata/Insentec_warning.rdata"))
    save(Insentec_final_summary, file = here::here("inst/extdata/feeding_and_drinking_analysis.rdata"))
    my_cache <- memoise::cache_filesystem(here::here("inst/extdata"))
    cached_result <- memoise::memoise(function(x) x, cache = my_cache)
    cached_result("Insentec_final_summary")
  }

  return(list(
    Insentec_final_summary = if (exists("Insentec_final_summary")) Insentec_final_summary else NULL,
    Insentec_warning = Insentec_warning
  ))
}








