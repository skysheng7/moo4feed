# -----------------------------------------------------------------------------#
# ------------------------ Internal helper functions --------------------------#
# -----------------------------------------------------------------------------#

#' Check for visits with long durations and update the warning data frame
#'
#' This function inspects visit durations (for feeding or drinking) and flags visits
#' exceeding a predefined duration threshold from the configuration. A warning
#' data frame is updated with bins where long visits occurred.
#'
#' @inheritParams qc_negatives
#' @inheritParams set_global_cols
#' @param comb A named list of daily feed or water data frames.
#' @param type A string, either `"feed"` or `"water"` to indicate the type of data.
#'
#' @return The updated warning data frame with bins flagged for long-duration visits.
#' @keywords internal
#' @noRd
qc_long_dur <- function(comb,
                        warn,
                        cfg = qc_config(),
                        verbose = TRUE,
                        dur_col = duration_col2(),
                        bin_col = bin_col2(),
                        type = "feed") {

  # Determine threshold from cfg
  threshold <- if (type == "feed") cfg$large_intake_visit_feed else cfg$large_intake_visit_wat

  # Loop over days and flag long durations
  for (i in seq_along(comb)) {
    date <- names(comb)[i]
    day_idx <- which(warn$date == date)
    if (length(day_idx) == 0 || nrow(comb[[i]]) == 0) next

    day_data <- comb[[i]]
    long_visits <- day_data |>
      dplyr::filter(!!rlang::sym(dur_col) > threshold)

    if (nrow(long_visits) > 0) {
      long_bins <- sort(unique(long_visits[[bin_col]]))
      warn_col <- if (type == "feed") "long_dur_feeder" else "long_dur_drinker"
      warn[[warn_col]][day_idx] <- paste(long_bins, collapse = "; ")

      if (verbose) {
        cat("\n==== LONG DURATION VISITS DETECTED ====\n")
        print(long_visits)
      }
    }
  }

  return(warn)
}


#' Run long duration QC checks for feed and water data
#'
#' This wrapper applies `qc_long_dur()` to both feed and water datasets (if provided),
#' updating only the corresponding columns in the warning data frame.
#'
#' @inheritParams qc
#' @inheritParams qc_long_dur
#'
#' @return An updated warning data frame with long duration bins flagged for feed and/or water.
#' @keywords internal
#' @noRd
qc_all_long_durations <- function(feed = NULL,
                                  water = NULL,
                                  warn,
                                  cfg = qc_config(),
                                  verbose = TRUE,
                                  dur_col = duration_col2(),
                                  bin_col = bin_col2()) {
  if (!is.null(feed)) {
    warn <- qc_long_dur(
      comb = feed,
      warn = warn,
      cfg = cfg,
      verbose = verbose,
      type = "feed",
      dur_col = dur_col,
      bin_col= bin_col
    )
  }

  if (!is.null(water)) {
    warn <- qc_long_dur(
      comb = water,
      warn = warn,
      cfg = cfg,
      verbose = verbose,
      type = "water",
      dur_col = dur_col,
      bin_col= bin_col
    )
  }

  return(warn)
}

