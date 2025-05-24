# -----------------------------------------------------------------------------#
# -------------------- Internal user-facing functions -------------------------#
# -----------------------------------------------------------------------------#

#' Check for cows that haven't been seen after noon
#'
#' This function identifies cows that haven't been seen after noon (12pm)
#' and updates the warning data frame accordingly. This is to warn users 
#' in cases when a cow lost its ear tag and not able to access the feeder 
#' and drinker.
#'
#' @inheritParams qc
#' @inheritParams qc_warning_skeleton
#' @param warn Warning data frame to update
#'
#' @return Updated warning data frame with no-show information
#' @keywords internal
#' @noRd
qc_no_show <- function(comb,
                       warn,
                       id_col = id_col2(),
                       end_col = end_col2(),
                       tz = tz2(),
                       verbose = TRUE) {
  
  # Input validation
  if (!is.list(comb) || length(comb) == 0) {
    stop("`comb` must be a non-empty list of data frames.")
  }
  if (is.list(comb) && !inherits(comb, "data.frame")) {
      if (!all(sapply(comb, is.data.frame))) {
        stop("All elements in `comb` list must be data frames.")
      }
  }
  if (!is.data.frame(warn)) {
    stop("`warn` must be a data frame.")
  }
  
  # Process each day
  for (i in seq_along(comb)) {
    date <- names(comb)[i]
    day_idx <- which(warn$date == date)
    
    if (length(day_idx) == 0 || nrow(comb[[i]]) == 0) {
      next
    }
    
    # Convert column names to symbols for tidy evaluation
    id_sym <- rlang::sym(id_col)
    end_sym <- rlang::sym(end_col)
    
    # Define cutoff time (noon)
    noon_cutoff <- lubridate::ymd_hms(paste(date, "11:59:59"), tz = tz)
    
    # Get last seen times for cows
    last_seen_cows <- qc_determine_last_seen(comb[[i]], !!id_sym, !!end_sym)
    
    # Check cows not seen after noon
    warn$cows_disappeared_after_noon[day_idx] <- qc_extract_warnings(
      last_seen_cows, 
      !!id_sym,
      !!end_sym,
      cutoff_time = noon_cutoff
    )
  }
  
  return(warn)
}

#' Determine last seen time for each entity
#'
#' @inheritParams qc_no_show
#' @param id_sym Column containing entity IDs (unquoted)
#' @param end_sym Column containing end times (unquoted)
#' @param df A data frame containing visit records
#' 
#' @return A data frame with last seen times for each entity
#' @keywords internal
#' @noRd
qc_determine_last_seen <- function(df, id_sym, end_sym) {
  df |>
    dplyr::arrange(id_sym, end_sym) |>
    dplyr::group_by(id_sym) |>
    dplyr::slice_tail(n = 1) |>
    dplyr::ungroup()
}

#' Extract warnings for entities not seen after a particular time
#'
#' @inheritParams qc_no_show
#' @inheritParams qc_determine_last_seen
#' @param cutoff_time POSIXct time to check against
#' @return A character string with warnings, or NA if no warnings
#' @keywords internal
#' @noRd
qc_extract_warnings <- function(df, id_sym, end_sym, cutoff_time) {
  not_seen <- df |>
    dplyr::filter(end_sym < cutoff_time) |>
    dplyr::mutate(
      warning_str = paste(
        id_sym,
        format(end_sym, "%H:%M:%S"),
        sep = ", "
      )
    ) |>
    dplyr::pull(warning_str)
  
  if (length(not_seen) > 0) {
    return(paste(sort(not_seen), collapse = "; "))
  } else {
    return(NA_character_)
  }
} 