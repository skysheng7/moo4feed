# -----------------------------------------------------------------------------#
# ------------------------ Internal helper functions --------------------------#
# -----------------------------------------------------------------------------#

#' Check for negative durations and intakes
#'
#' Identifies and flags records with negative duration or intake values,
#' combining them into a single warning column
#'
#' @inheritParams qc_double_detection
#' @inheritParams qc
#' @inheritParams set_global_cols
#'
#' @return Updated warning data frame with negative value information
#' @keywords internal
#' @noRd
qc_negatives <- function(comb,
                         warn,
                         verbose = TRUE,
                         cfg,
                         bin_col = bin_col2(),
                         dur_col = duration_col2(),
                         intake_col = intake_col2()) {


  dur_sym   <- rlang::sym(dur_col)
  intake_sym   <- rlang::sym(intake_col)

  # Process each day
  for (i in seq_along(comb)) {
    date <- names(comb)[i]
    day_idx <- which(warn$date == date)
    if (length(day_idx) == 0 || nrow(comb[[i]]) == 0) {next}

    day_data <- comb[[i]]
    problematic_bins <- integer(0)

    # Check for negative durations
    neg_duration <- day_data |>
      dplyr::filter(!!dur_sym < 0)

    if (nrow(neg_duration) > 0) {
      dur_bins <- unique(neg_duration[[bin_col]])
      problematic_bins <- c(problematic_bins, dur_bins)

      if (verbose) {
        cat("\n==== NEGATIVE DURATION VALUES DETECTED ====\n")
        print(neg_duration)
      }
    }

    # Check for significant negative intakes (< negative value of calibration_error)
    neg_intake <- day_data |>
      dplyr::filter((!!intake_sym < 0) & (abs(!!intake_sym) > cfg$calibration_error))

    if (nrow(neg_intake) > 0) {
      intake_bins <- unique(neg_intake[[bin_col]])
      problematic_bins <- c(problematic_bins, intake_bins)

      if (verbose) {
        cat("\n==== SIGNIFICANT NEGATIVE INTAKE VALUES DETECTED ====\n")
        print(neg_intake)
      }
    }

    # Update warning data frame with all problematic bins
    if (length(problematic_bins) > 0) {
      warn$negative_visit_bins[day_idx] <- paste(sort(unique(problematic_bins)), collapse = "; ")
    }
  }

  return(warn)
}
