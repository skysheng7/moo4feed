# -----------------------------------------------------------------------------#
# ------------------------ Internal helper functions --------------------------#
# -----------------------------------------------------------------------------#

#' Check for large intake values in visits
#'
#' Identifies and flags visits with unusually large intake values or rapid intake rates,
#' updating the warning data frame with the affected bins.
#'
#' @inheritParams qc_negatives
#' @inheritParams set_global_cols
#' @param type A string, either `"feed"` or `"water"` to indicate the type of data.
#'
#' @return The updated warning data frame with bins flagged for large intakes.
#' @keywords internal
#' @noRd
qc_large_intake <- function(comb,
                           warn,
                           cfg = qc_config(),
                           verbose = TRUE,
                           bin_col = bin_col2(),
                           intake_col = intake_col2(),
                           dur_col = duration_col2(),
                           type = "feed") {

  # Determine thresholds from cfg
  intake_threshold <- if (type == "feed") cfg$large_intake_visit_feed else cfg$large_intake_visit_water
  rate_threshold <- if (type == "feed") cfg$large_intake_rate_feed else cfg$large_intake_rate_water
  warn_col <- if (type == "feed") "large_intake_feed_visit" else "large_intake_water_visit"

  # Process each day
  for (i in seq_along(comb)) {
    date <- names(comb)[i]
    day_idx <- which(warn$date == date)
    if (length(day_idx) == 0 || nrow(comb[[i]]) == 0) next

    day_data <- comb[[i]]
    
    # Check for large intake values
    large_intake <- day_data |>
      dplyr::filter(!!rlang::sym(intake_col) > intake_threshold)
    
    # Check for rapid intake rates
    rapid_intake <- day_data |>
      dplyr::mutate(rate = !!rlang::sym(intake_col) / !!rlang::sym(dur_col)) |>
      dplyr::filter(!!rlang::sym(intake_col) > intake_threshold & rate > rate_threshold)
    
    # Combine problematic bins
    problematic_bins <- sort(unique(c(
      large_intake[[bin_col]],
      rapid_intake[[bin_col]]
    )))

    if (length(problematic_bins) > 0) {
      warn[[warn_col]][day_idx] <- paste(problematic_bins, collapse = "; ")

      if (verbose) {
        if (nrow(large_intake) > 0) {
          cat("\n==== LARGE INTAKE VALUES DETECTED ====\n")
          print(large_intake)
        }
        if (nrow(rapid_intake) > 0) {
          cat("\n==== RAPID INTAKE RATES DETECTED ====\n")
          print(rapid_intake)
        }
      }
    }
  }

  return(warn)
}

#' Run large intake QC checks for feed and water data
#'
#' This wrapper applies large intake checks to both feed and water datasets (if provided),
#' updating only the corresponding columns in the warning data frame.
#'
#' @inheritParams qc
#' @inheritParams qc_large_intake
#'
#' @return An updated warning data frame with large intake bins flagged for feed and/or water.
#' @keywords internal
#' @noRd
qc_all_large_intakes <- function(feed = NULL,
                                water = NULL,
                                warn,
                                cfg = qc_config(),
                                verbose = TRUE,
                                bin_col = bin_col2(),
                                intake_col = intake_col2(),
                                dur_col = duration_col2()) {
  if (!is.null(feed)) {
    warn <- qc_large_intake(
      comb = feed,
      warn = warn,
      cfg = cfg,
      verbose = verbose,
      type = "feed",
      bin_col = bin_col,
      intake_col = intake_col,
      dur_col = dur_col
    )
  }

  if (!is.null(water)) {
    warn <- qc_large_intake(
      comb = water,
      warn = warn,
      cfg = cfg,
      verbose = verbose,
      type = "water",
      bin_col = bin_col,
      intake_col = intake_col,
      dur_col = dur_col
    )
  }

  return(warn)
} 