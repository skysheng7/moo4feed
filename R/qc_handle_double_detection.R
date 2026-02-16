# -----------------------------------------------------------------------------#
# ------------------------ Internal helper functions --------------------------#
# -----------------------------------------------------------------------------#

#' Handle double detection for the same animal at different bins
#'
#' When the same animal appears at different bins with overlapping time periods,
#' this function adjusts the end time of the first bout to be 1 second before
#' the start time of the second bout, preserving the integrity of the second bout.
#'
#' @inheritParams qc
#' @inheritParams set_global_cols
#'
#' @return A list of data frames with corrected end times for overlapping bouts.
#' @keywords internal
#' @noRd
handle_double_detection_cow <- function(comb,
                                       id_col = id_col2(),
                                       start_col = start_col2(),
                                       end_col = end_col2()) {
  
  id_sym <- rlang::sym(id_col)
  start_sym <- rlang::sym(start_col)
  end_sym <- rlang::sym(end_col)
  
  for (i in seq_along(comb)) {
    if (nrow(comb[[i]]) <= 1) next
    
    # Use tidyverse style to find and correct overlaps
    comb[[i]] <- comb[[i]] |>
      dplyr::arrange(!!id_sym, !!start_sym) |>
      dplyr::group_by(!!id_sym) |>
      dplyr::mutate(
        overlap = (!!end_sym) > dplyr::lead(!!start_sym),
        overlap = tidyr::replace_na(overlap, FALSE),
        # Adjust end time only where there's an overlap
        !!end_col := dplyr::if_else(
          overlap,
          dplyr::lead(!!start_sym) - lubridate::seconds(1),
          !!end_sym
        )
      ) |>
      dplyr::ungroup() |>
      dplyr::select(-overlap) # Remove the temporary overlap column
  }
  
  return(comb)
}

#' Handle double detection for the same bin with different animals
#'
#' When different animals appear at the same bin with overlapping time periods,
#' this function adjusts the end time of the first bout to be 1 second before
#' the start time of the second bout, preserving the integrity of the second bout.
#'
#' @inheritParams handle_double_detection_cow
#' @param bin_col Column name for bin ID.
#'
#' @return A list of data frames with corrected end times for overlapping bouts.
#' @keywords internal
#' @noRd
handle_double_detection_bin <- function(comb,
                                      bin_col = bin_col2(),
                                      start_col = start_col2(),
                                      end_col = end_col2()) {
  
  bin_sym <- rlang::sym(bin_col)
  start_sym <- rlang::sym(start_col)
  end_sym <- rlang::sym(end_col)
  
  for (i in seq_along(comb)) {
    if (nrow(comb[[i]]) <= 1) next
    
    # Use tidyverse style to find and correct overlaps
    comb[[i]] <- comb[[i]] |>
      dplyr::arrange(!!bin_sym, !!start_sym) |>
      dplyr::group_by(!!bin_sym) |>
      dplyr::mutate(
        overlap = (!!end_sym) > dplyr::lead(!!start_sym),
        overlap = tidyr::replace_na(overlap, FALSE),
        # Adjust end time only where there's an overlap
        !!end_col := dplyr::if_else(
          overlap,
          dplyr::lead(!!start_sym) - lubridate::seconds(1),
          !!end_sym
        )
      ) |>
      dplyr::ungroup() |>
      dplyr::select(-overlap) # Remove the temporary overlap column
  }
  
  return(comb)
}

#' Update duration column after adjusting timestamps
#'
#' Recalculates the duration column based on the start and end timestamps.
#'
#' @inheritParams handle_double_detection_cow
#' @param dur_col Column name for duration.
#'
#' @return A list of data frames with updated duration values.
#' @keywords internal
#' @noRd
update_duration <- function(comb,
                           start_col = start_col2(),
                           end_col = end_col2(),
                           dur_col = duration_col2()) {
  
  start_sym <- rlang::sym(start_col)
  end_sym <- rlang::sym(end_col)
  dur_sym <- rlang::sym(dur_col)
  
  for (i in seq_along(comb)) {
    if (nrow(comb[[i]]) == 0) next
    
    # Calculate duration in seconds
    comb[[i]] <- comb[[i]] |>
      dplyr::mutate(
        !!dur_sym := as.numeric(difftime(!!end_sym, !!start_sym, units = "secs"))
      )
  }
  
  return(comb)
}

#' Split corrected combined data back into feed and water data
#'
#' After handling double detections in the combined dataset, this function
#' splits the data back into separate feed and water lists.
#'
#' @inheritParams qc
#' @param comb A list of data frames with corrected feed and water data.
#' @param feed Original feed data list to update (can be NULL).
#' @param water Original water data list to update (can be NULL).
#'
#' @return A list containing updated feed and water lists.
#' @keywords internal
#' @noRd
split_feed_water <- function(comb,
                            feed = NULL,
                            water = NULL,
                            bin_col = bin_col2(),
                            bin_offset = bin_offset2()) {
  
  bin_sym <- rlang::sym(bin_col)
  
  # Only proceed if we have both feed and water data
  if (is.null(feed) || is.null(water)) {
    return(list(feed = feed, water = water))
  }
  
  for (i in seq_along(comb)) {
    date <- names(comb)[i]
    if (nrow(comb[[i]]) == 0) next
    
    # Split the data based on bin IDs
    feed[[date]] <- comb[[i]] |>
      dplyr::filter(!!bin_sym < bin_offset)
    
    water[[date]] <- comb[[i]] |>
      dplyr::filter(!!bin_sym >= bin_offset)
  }
  
  return(list(feed = feed, water = water))
}

#' Handle all double detection issues and recalculate durations
#'
#' This is the main function that orchestrates the double detection handling process.
#' It performs the following steps:
#' 1. Handles double detection for the same animal at different bins
#' 2. Handles double detection for the same bin with different animals
#' 3. Updates durations based on corrected timestamps
#' 4. Removes records with negative or zero durations
#' 5. Splits the corrected data back into feed and water lists
#'
#' @inheritParams qc
#'
#' @return A list containing cleaned feed, water, and combined data.
#' @keywords internal
#' @noRd
handle_all_double_detections <- function(feed = NULL,
                                        water = NULL,
                                        id_col = id_col2(),
                                        bin_col = bin_col2(),
                                        start_col = start_col2(),
                                        end_col = end_col2(),
                                        dur_col = duration_col2(),
                                        bin_offset = bin_offset2()) {
  
  # Combine feed and water data if both are provided
  if (!is.null(feed) && !is.null(water)) {
    comb <- combine_feed_water(feed, water)
  } else if (!is.null(feed)) {
    comb <- feed
  } else if (!is.null(water)) {
    comb <- water
  } else {
    stop("Both feed and water cannot be NULL.")
  }
  
  # Handle double detections
  comb <- handle_double_detection_cow(comb, id_col, start_col, end_col)
  comb <- handle_double_detection_bin(comb, bin_col, start_col, end_col)
  
  # Update durations
  comb <- update_duration(comb, start_col, end_col, dur_col)
  
  # Split back into feed and water if necessary
  if (!is.null(feed) && !is.null(water)) {
    result <- split_feed_water(comb, feed, water, bin_col, bin_offset)
    result$combined <- comb
    return(result)
  } else {
    return(list(
      feed = if (!is.null(feed)) comb else NULL,
      water = if (!is.null(water)) comb else NULL,
      combined = comb
    ))
  }
} 