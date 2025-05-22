#' Summarize and check feed & water intake
#' 
#' @importFrom dplyr full_join
#'
#' @description
#' This function summarizes daily feed and water intake, visit duration, and counts,
#' and updates intake warnings.
#'
#' @param feed A data frame of feed visits.
#' @param water A data frame of drinking visits.
#' @param warn A warning skeleton (e.g., from `qc_warning_skeleton()`).
#' @param cfg A config list. See [qc_config()].
#'
#' @return A list of:
#' - `summary`: merged data frame of daily intake, duration, and visit counts
#' - `warn`: updated warnings
#'
#' @inheritParams qc
#' @inheritParams qc_total_cows
#' @examples
#' feed <- tibble::tibble(
#'   date = as.Date(c("2024-01-01", "2024-01-01")),
#'   cow = c("A", "B"),
#'   intake = c(20, 80),
#'   duration = c(200, 300)
#' )
#'
#' water <- tibble::tibble(
#'   date = as.Date(c("2024-01-01", "2024-01-01")),
#'   cow = c("A", "B"),
#'   intake = c(50, 200),
#'   duration = c(100, 150)
#' )
#'
#' warn <- tibble::tibble(
#'   date = as.Date("2024-01-01"),
#'   low_daily_feed_intake_cows = NA_character_,
#'   high_daily_feed_intake_cows = NA_character_,
#'   low_daily_water_intake_cows = NA_character_,
#'   high_daily_water_intake_cows = NA_character_
#' )
#'
#' cfg <- qc_config()
#' merge_feed_water_summary(feed, water, warn, cfg)
#'
#' @export
merge_feed_water_summary <- function(feed = NULL, water = NULL, warn, cfg = qc_config()) {
  if (!is.data.frame(warn)) stop("`warn` must be a data frame.")
  if (!is.null(feed) && !is.data.frame(feed)) stop("`feed` must be a data frame or NULL.")
  if (!is.null(water) && !is.data.frame(water)) stop("`water` must be a data frame or NULL.")

  list_to_merge <- list()

  if (!is.null(feed)) {
    feed_sum <- summarize_feed_water_data(feed, type = "feeding")
    warn <- check_intake(feed_sum, warn, type = "feeding", cfg = cfg)
    list_to_merge <- append(list_to_merge, list(feed_sum))
  }

  if (!is.null(water)) {
    water_sum <- summarize_feed_water_data(water, type = "drinking")
    warn <- check_intake(water_sum, warn, type = "drinking", cfg = cfg)
    list_to_merge <- append(list_to_merge, list(water_sum))
  }

  if (length(list_to_merge) > 0) {
    summary_df <- Reduce(function(x, y) dplyr::full_join(x, y, by = c("date", id_col2())), list_to_merge)
    summary_df <- summary_df[order(summary_df$date, summary_df[[id_col2()]]), ]
    summary_df[is.na(summary_df)] <- 0
  } else {
    summary_df <- NULL
  }

  return(list(summary = summary_df, warn = warn))
}