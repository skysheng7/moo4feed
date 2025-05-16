# -----------------------------------------------------------------------------#
# ------------------------ Internal helper functions --------------------------#
# -----------------------------------------------------------------------------#

#' Check and update total cow counts
#'
#' Computes the total number of unique cows in each day's data and
#' flags potential issues if the count differs from expected.
#'
#' @inheritParams qc_warning_skeleton
#' @inheritParams qc
#' @inheritParams process_feeder
#' @param warn Warning data frame to update
#'
#' @return Updated warning data frame with total cow information
#' @keywords internal
#' @noRd
qc_total_cows <- function(comb, warn, cfg = qc_config(), id_col = id_col2()) {
  # Iterate through each day's data
  for (i in seq_along(comb)) {
    date <- names(comb)[i]
    day_idx <- which(warn$date == date)

    if (length(day_idx) == 0 || nrow(comb[[i]]) == 0) {
      next
    }

    # Count unique cows
    cow_count <- length(unique(comb[[i]][[id_col]]))
    warn$total_cows[day_idx] <- as.character(cow_count)

    # Check against expected count if provided
    if (!is.na(cfg$total_cows_expected) && (cow_count < cfg$total_cows_expected)) {
      warn$missing_cow[day_idx] <- "Yes"
    }
  }

  warn$total_cows <- as.integer(warn$total_cows)

  return(warn)
}
