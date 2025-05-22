#' Summarize and check feed & water intake
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
#' @examples
#' # (example usage will go here later)
#' @export
merge_feed_water_summary <- function(feed = NULL, water = NULL, warn, cfg = qc_config()) {
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

  # Merge all summary data
  if (length(list_to_merge) > 0) {
    summary_df <- Reduce(function(x, y) dplyr::full_join(x, y, by = c("date", id_col2())), list_to_merge)
    summary_df <- summary_df[order(summary_df$date, summary_df[[id_col2()]]), ]
    summary_df[is.na(summary_df)] <- 0
  } else {
    summary_df <- NULL
  }

  return(list(summary = summary_df, warn = warn))
}