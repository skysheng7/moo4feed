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
#' @inheritParams process_all_feed
#'
#' @return Updated warning data frame with consolidated double detection information
#' @keywords internal
#' @noRd
qc_double_detection <- function(comb,
                                warn,
                                verbose = TRUE,
                                id_col = id_col2(), # e.g. "cow_id"
                                start_col = start_col2(), # e.g. "start_time"
                                end_col   = end_col2(), # e.g. "end_time"
                                bin_col   = bin_col2() # e.g. "bin_id"
                                ) {
  # Process each day
  for (i in seq_along(comb)) {
    date <- names(comb)[i]
    day_idx <- which(warn$date == date)

    if (length(day_idx) == 0 || nrow(comb[[i]]) == 0) {
      next
    }

    # Get all problematic bins in a single efficient call
    problematic_bins <- qc_detect_all_double_detections(comb[[i]],
                                                        verbose=verbose,
                                                        id_col = id_col,
                                                        start_col = start_col,
                                                        end_col   = end_col,
                                                        bin_col   = bin_col)

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
#' @inheritParams process_all_feed
#'
#' @return Vector of unique bin IDs with detection issues
#' @keywords internal
#' @noRd
qc_detect_all_double_detections <- function(day_data,
                                            verbose = TRUE,
                                            id_col = id_col2(), # e.g. "cow_id"
                                            start_col = start_col2(), # e.g. "start_time"
                                            end_col   = end_col2(), # e.g. "end_time"
                                            bin_col   = bin_col2() # e.g. "bin_id"
                                            ) {

  if (nrow(day_data) == 0L) return(integer(0))

  # Turn the character names into symbols for tidy-eval
  id_sym    <- rlang::sym(id_col)
  start_sym <- rlang::sym(start_col)
  end_sym   <- rlang::sym(end_col)
  bin_sym   <- rlang::sym(bin_col)

  #####################################################################
  # 1. SAME ANIMAL RECORDED AT >1 BIN DURING AN OVERLAP
  #####################################################################
  same_animal_overlap <- day_data |>
    dplyr::arrange(!!id_sym, !!start_sym) |>
    dplyr::group_by(!!id_sym)|>
    dplyr::mutate(
      overlap = (!!end_sym) > dplyr::lead(!!start_sym),
      overlap = tidyr::replace_na(overlap, FALSE)
    ) |>
    dplyr::ungroup()

  same_animal_overlap_bins <- same_animal_overlap |>
    dplyr::filter(overlap) |>
    dplyr::pull(!!bin_sym) |>
    unique()

  if (verbose && nrow(same_animal_overlap) > 0) {
    cat("\n==== SAME ANIMAL RECORDED AT DIFFERENT BINS SIMULTANEOUSLY ====\n")
    print(
      same_animal_overlap |>
        dplyr::filter(overlap | dplyr::lag(overlap, default = FALSE))
    )
  }

  #####################################################################
  # 2. DIFFERENT ANIMALS RECORDED AT THE SAME BIN DURING AN OVERLAP
  #####################################################################
  diff_animals_1bin_df <- day_data |>
    dplyr::arrange(!!bin_sym, !!start_sym) |>
    dplyr::group_by(!!bin_sym) |>
    dplyr::mutate(
      overlap = (!!end_sym) > dplyr::lead(!!start_sym),
      overlap = tidyr::replace_na(overlap, FALSE)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(overlap | dplyr::lag(overlap, default = FALSE))

  diff_animals_1bin_df_bins <- diff_animals_1bin_df |>
    dplyr::pull(!!bin_sym) |>
    unique()

  if (verbose && nrow(diff_animals_1bin_df) > 0) {
    cat("\n==== DIFFERENT ANIMALS DETECTED AT SAME BIN SIMULTANEOUSLY ====\n")
    print(
      diff_animals_1bin_df
    )
  }

  unique(c(same_animal_overlap_bins, diff_animals_1bin_df_bins))
}
