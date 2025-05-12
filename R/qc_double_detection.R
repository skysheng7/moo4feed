# -----------------------------------------------------------------------------#
# ------------------------ Internal helper functions --------------------------#
# -----------------------------------------------------------------------------#


#' Check for double detection issues efficiently
#'
#' Identifies and records all types of concurrent detection problems:
#' 1. Same animal at multiple bins simultaneously
#' 2. Multiple animals at the same bin simultaneously
#' Records all problematic bins in a single column for easier review.
#'
#' @inheritParams qc_total_cows
#' @inheritParams qc
#'
#' @return Updated warning data frame with consolidated double detection information
#' @keywords internal
#' @noRd
qc_double_detection <- function(comb, warn, verbose) {
  # Process each day
  for (i in seq_along(comb)) {
    date <- names(comb)[i]
    day_idx <- which(warn$date == date)

    if (length(day_idx) == 0 || nrow(comb[[i]]) == 0) {
      next
    }

    # Get all problematic bins in a single efficient call
    problematic_bins <- qc_detect_all_double_detections(comb[[i]], verbose=verbose)

    # Update warning with all problematic bins in a single column
    if (length(problematic_bins) > 0) {
      warn$double_detection_bins[day_idx] <- paste(sort(problematic_bins), collapse = "; ")
    }
  }

  return(warn)
}

#' Detect all types of double detections efficiently
#'
#' Uses data.table for faster processing to detect both types of double detections:
#' - Same animal at multiple bins simultaneously (only the first bin is flagged as problematic)
#' - Multiple animals at the same bin simultaneously (all such bins are flagged)
#'
#' @param day_data A single day's data frame
#' @inheritParams qc
#'
#' @return Vector of unique bin IDs with detection issues
#' @keywords internal
#' @noRd
qc_detect_all_double_detections <- function(day_data, verbose) {
  # Get column names from global settings
  id_col <- id_col2()
  start_col <- start_col2()
  end_col <- end_col2()
  bin_col <- bin_col2()

  # Convert to data.table for better performance
  dt <- data.table::as.data.table(day_data)
  problematic_bins <- integer(0)

  # 1. Check for same animal at different bins
  if (nrow(dt) > 0) {
    # Sort by animal ID and start time
    data.table::setorderv(dt, cols = c(id_col, start_col))

    # For each animal, find overlapping time intervals
    dt[, overlap_prev := c(FALSE, get(start_col)[-1] < get(end_col)[-.N]), by = id_col]

    # Print double detection details if requested
    if (any(dt$overlap_prev)) {
      overlap_indices <- which(dt$overlap_prev)
      prev_indices <- overlap_indices - 1

      # Extract the bin IDs for these previous rows
      first_problematic_bins <- unique(dt[prev_indices, get(bin_col)])

      #Get only the FIRST bin in each overlapping pair (it's the problematic one)
      problematic_bins <- c(problematic_bins, first_problematic_bins)

      # Combine indices and ensure they're sorted by animal and start time
      all_overlap_row_indices <- sort(c(overlap_indices, prev_indices))

      # Print the double detection rows
      if (verbose) {
        cat("\n==== SAME ANIMAL RECORDED AT DIFFERENT BINS SIMULTANEOUSLY ====\n")
        print(dt[all_overlap_row_indices])
      }

    }

    # 2. Check for different animals at same bin
    # Sort by bin ID and start time
    data.table::setorderv(dt, cols = c(bin_col, start_col))

    # For each bin, find overlapping time intervals with different animals
    dt[, overlap_prev_bin := c(FALSE, get(start_col)[-1] < get(end_col)[-.N]), by = bin_col]

    # Print bin-based overlaps if requested
    if (any(dt$overlap_prev_bin)) {
      bin_overlap_indices <- which(dt$overlap_prev_bin)
      bin_prev_indices <- bin_overlap_indices - 1

      # Combine indices and ensure they're sorted by bin and start time
      all_bin_overlap_indices <- sort(c(bin_overlap_indices, bin_prev_indices))

      # Get bins involved in bin-based overlaps (all such bins are problematic)
      bin_overlap_bins <- dt[all_bin_overlap_indices, unique(get(bin_col))]
      problematic_bins <- c(problematic_bins, bin_overlap_bins)

      if(verbose){
        cat("\n==== DIFFERENT ANIMALS DETECTED AT SAME BIN SIMULTANEOUSLY ====\n")
        print(dt[all_bin_overlap_indices])
      }

    }

  }

  # Return unique problematic bins
  return(unique(problematic_bins))
}
