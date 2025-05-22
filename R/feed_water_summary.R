#' Summarize and check for abnormal feed & water intake
#' 
#' @description
#' This function summarizes daily feed and water intake, visit duration, and counts,
#' and updates intake warnings (if input warning dataframe `warn` is not `NULL`). 
#' If you don't want to update warnings, set `warn = NULL`, and only the summary will be returned.
#'
#' @inheritParams qc
#' @param feed Either a data frame containing all feed visits across days, 
#'  or a list of data frames grouped by date.
#'  If providing a list, each data frame must have a 'date' column.
#' @param water Either a data frame containing all water visits across days, 
#'  or a list of data frames grouped by date.
#'  If providing a list, each data frame must have a 'date' column.
#' @param warn Warning data frame to update
#' 
#' @return A list of:
#' - `summary`: merged data frame of daily intake, duration, and visit counts
#' - `warn`: updated warnings
#'
#' @examples
#' # Example with single data frames
#' feed_df <- tibble::tibble(
#'   date = as.Date(c("2024-01-01", "2024-01-01")),
#'   cow = c("A", "B"),
#'   intake = c(20, 80),
#'   duration = c(200, 300)
#' )
#'
#' water_df <- tibble::tibble(
#'   date = as.Date(c("2024-01-01", "2024-01-01")),
#'   cow = c("A", "B"),
#'   intake = c(50, 200),
#'   duration = c(100, 150)
#' )
#'
#' # Example with lists of data frames
#' feed_list <- list(
#'   "2024-01-01" = tibble::tibble(
#'     date = as.Date("2024-01-01"),
#'     cow = c("A", "B"),
#'     intake = c(20, 80),
#'     duration = c(200, 300)
#'   ),
#'   "2024-01-02" = tibble::tibble(
#'     date = as.Date("2024-01-02"),
#'     cow = c("A", "C"),
#'     intake = c(25, 85),
#'     duration = c(210, 310)
#'   )
#' )
#'
#' water_list <- list(
#'   "2024-01-01" = tibble::tibble(
#'     date = as.Date("2024-01-01"),
#'     cow = c("A", "B"),
#'     intake = c(50, 200),
#'     duration = c(100, 150)
#'   ),
#'   "2024-01-02" = tibble::tibble(
#'     date = as.Date("2024-01-02"),
#'     cow = c("A", "C"),
#'     intake = c(55, 210),
#'     duration = c(110, 160)
#'   )
#' )
#'
#' warn <- tibble::tibble(
#'   date = as.Date(c("2024-01-01", "2024-01-02")),
#'   low_daily_feed_intake_cows = NA_character_,
#'   high_daily_feed_intake_cows = NA_character_,
#'   low_daily_water_intake_cows = NA_character_,
#'   high_daily_water_intake_cows = NA_character_
#' )
#'
#' cfg <- qc_config()
#' 
#' # Using single data frames
#' feed_water_summary(feed_df, 
#'                   water_df, 
#'                   warn, 
#'                   cfg, 
#'                   id_col = "cow", 
#'                   intake_col = "intake", 
#'                   dur_col = "duration")
#'                   
#' # Using lists of data frames
#' feed_water_summary(feed_list, 
#'                   water_list, 
#'                   warn, 
#'                   cfg, 
#'                   id_col = "cow", 
#'                   intake_col = "intake", 
#'                   dur_col = "duration")
#'
#' @export
feed_water_summary <- function(feed = NULL, 
                             water = NULL, 
                             warn = NULL, 
                             cfg = qc_config(),
                             id_col = id_col2(),
                             intake_col = intake_col2(),
                             dur_col = duration_col2()) {
  # Input validation
  if (!is.data.frame(warn)) stop("`warn` must be a data frame.")
  if (is.null(water) && is.null(feed)) stop("`water` and `feed` cannot both be NULL.")
  
  # Validate feed input
  if (!is.null(feed)) {
    if (!is.data.frame(feed) && !(is.list(feed) && !inherits(feed, "data.frame"))) {
      stop("`feed` must be a data frame or a list of data frames.")
    }
    if (is.list(feed) && !inherits(feed, "data.frame")) {
      if (!all(sapply(feed, is.data.frame))) {
        stop("All elements in `feed` list must be data frames.")
      }
      if (!all(sapply(feed, function(df) "date" %in% names(df)))) {
        stop("All data frames in `feed` list must have a 'date' column.")
      }
    }
  }
  
  # Validate water input
  if (!is.null(water)) {
    if (!is.data.frame(water) && !(is.list(water) && !inherits(water, "data.frame"))) {
      stop("`water` must be a data frame or a list of data frames.")
    }
    if (is.list(water) && !inherits(water, "data.frame")) {
      if (!all(sapply(water, is.data.frame))) {
        stop("All elements in `water` list must be data frames.")
      }
      if (!all(sapply(water, function(df) "date" %in% names(df)))) {
        stop("All data frames in `water` list must have a 'date' column.")
      }
    }
  }

  list_to_merge <- list()

  if (!is.null(feed)) {
    # If feed is a list of data frames, merge it into a single data frame
    feed_combined <- if (is.list(feed) && !inherits(feed, "data.frame")) merge_list_df(feed) else feed
    feed_sum <- sum_visits(feed_combined, type = "feed", 
                          id_col = id_col, 
                          intake_col = intake_col, 
                          dur_col = dur_col)
    
    if (!is.null(warn)) {
      warn <- qc_check_intake(feed_sum, warn, type = "feed", cfg = cfg, id_col = id_col)
    }
    list_to_merge <- append(list_to_merge, list(feed_sum))
  }

  if (!is.null(water)) {
    # If water is a list of data frames, merge it into a single data frame
    water_combined <- if (is.list(water) && !inherits(water, "data.frame")) merge_list_df(water) else water
    water_sum <- sum_visits(water_combined, type = "water", 
                           id_col = id_col, 
                           intake_col = intake_col, 
                           dur_col = dur_col)
    
    if (!is.null(warn)) {
      warn <- qc_check_intake(water_sum, warn, type = "water", cfg = cfg, id_col = id_col)
    }
    list_to_merge <- append(list_to_merge, list(water_sum))
  }

  if (length(list_to_merge) > 0) {
    summary_df <- Reduce(function(x, y) dplyr::full_join(x, y, by = c("date", id_col)), list_to_merge)
    summary_df <- summary_df[order(summary_df[["date"]], summary_df[[id_col]]), ]
    summary_df[is.na(summary_df)] <- 0
  } else {
    summary_df <- NULL
  }

  if (!is.null(warn)) {
    return(list(summary = summary_df, warn = warn))
  } else {
    return(summary_df)
  }
}