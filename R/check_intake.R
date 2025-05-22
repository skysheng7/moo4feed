#' Check low/high intake levels
#' 
#' @inheritParams qc
#' @inheritParams qc_total_cows
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
#' @examples
#' df <- tibble::tibble(
#'   date = as.Date(c("2024-01-01", "2024-01-01", "2024-01-02")),
#'   cow = c("A", "B", "A"),
#'   feeding_intake = c(5, 200, 10)
#' )
#' warn <- tibble::tibble(
#'   date = as.Date(c("2024-01-01", "2024-01-02")),
#'   low_daily_feed_intake_cows = NA_character_,
#'   high_daily_feed_intake_cows = NA_character_,
#'   low_daily_water_intake_cows = NA_character_,
#'   high_daily_water_intake_cows = NA_character_
#' )
#' set_id_col2("cow")
#' cfg <- qc_config()
#' check_intake(df, warn, type = "feeding", cfg = cfg)
#' 
#' @importFrom rlang sym .data
check_intake <- function(df, warn, type = c("feeding", "drinking"), cfg = qc_config()) {
  type <- match.arg(type)
  id_col <- rlang::sym(id_col2())
  intake_col <- rlang::sym(paste0(type, "_intake"))

  # Thresholds
  low_thresh  <- cfg[[paste0("low_", ifelse(type == "feeding", "feed", "wat"), "_intake")]]
  high_thresh <- cfg[[paste0("high_", ifelse(type == "feeding", "feed", "wat"), "_intake")]]

  limits <- list(
    low  = df[df[[as.character(intake_col)]] < low_thresh, ],
    high = df[df[[as.character(intake_col)]] > high_thresh, ]
  )

  for (lim in names(limits)) {
    flagged <- limits[[lim]]
    if (nrow(flagged) > 0) {
      flagged$comb_str <- paste0(flagged[[as.character(id_col)]], ", ", flagged[[as.character(intake_col)]])
      for (i in seq_len(nrow(warn))) {
        cur_date <- warn$date[i]
        these <- flagged[flagged$date == cur_date, "comb_str", drop = TRUE]
        colname <- paste0(lim, "_daily_", ifelse(type == "feeding", "feed", "water"), "_intake_cows")
        warn[[colname]][i] <- if (length(these) > 0) paste(sort(these), collapse = "; ") else NA_character_
      }
    }
  }

  return(warn)
}