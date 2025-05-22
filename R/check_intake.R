#' Check low/high intake levels
#'
#' @description
#' Add warnings to `warn` when cows exceed low or high daily intake thresholds for feed or water.
#'
#' @param df A data frame from `summarize_feed_water_data()`.
#' @param warn A warnings skeleton (see `qc_warning_skeleton()`).
#' @param type Either `"feeding"` or `"drinking"`.
#' @param cfg A config list, default from [qc_config()].
#'
#' @return An updated warning data frame with intake alerts added.
#' @keywords internal
#' @noRd
#'
#' @importFrom rlang sym .data
check_intake <- function(df, warn, type = c("feeding", "drinking"), cfg = qc_config()) {
  type <- match.arg(type)
  id_col <- rlang::sym(id_col2())

  # Get threshold and column names
  low_thresh  <- cfg[[paste0("low_", ifelse(type == "feeding", "feed", "wat"), "_intake")]]
  high_thresh <- cfg[[paste0("high_", ifelse(type == "feeding", "feed", "wat"), "_intake")]]
  intake_col  <- rlang::sym(paste0(type, "_intake"))

  # Check both low and high
  limits <- list(
    low = df[df[[as.character(intake_col)]] < low_thresh, ],
    high = df[df[[as.character(intake_col)]] > high_thresh, ]
  )

  for (lim in names(limits)) {
    flagged <- limits[[lim]]
    if (nrow(flagged) > 0) {
      flagged$comb_str <- paste("cow", flagged[[as.character(id_col)]], flagged[[as.character(intake_col)]])
      for (i in seq_len(nrow(warn))) {
        cur_date <- warn$date[i]
        these <- flagged[flagged$date == cur_date, "comb_str", drop = TRUE]
        colname <- paste0(lim, "_daily_", ifelse(type == "feeding", "feed", "water"), "_intake_cows")
        warn[[colname]][i] <- paste(sort(these), collapse = "; ")
      }
    }
  }

  return(warn)
}