#' Detect Feed Addition Times
#'
#' @description
#' Detects when feed bins are refilled by identifying significant weight increases
#' between consecutive visits at the same bin.
#'
#' The function performs aggregation in two stages:
#'
#' **Stage 1: Within-bin aggregation (ALWAYS performed)**:
#' When a farmer adds feed to the same bin multiple times in quick succession
#' (within `max_bin_time_gap` seconds), these additions are automatically grouped
#' into a single feed event per bin. The returned `weight_increase` is the total
#' amount added, and `bin_weight_after_fill` is the bin weight after the final
#' addition. This stage always runs regardless of the `aggregate_all_bin` setting.
#'
#' **Stage 2: Across-bin aggregation (only when `aggregate_all_bin = TRUE`)**:
#' After within-bin aggregation, if `aggregate_all_bin = TRUE`, the function
#' groups individual bin additions into multi-bin feed events when multiple bins
#' are refilled within a time window.
#'
#' @param data A named list of daily data frames or a single data frame containing
#'   visit records with bin weights.
#' @param min_weight_increase Numeric. Minimum weight increase (in kg) to consider
#'   as a feed addition. Default is 5 kg.
#' @param max_bin_time_gap Numeric. Maximum time gap (in seconds) between bin
#'   additions to group them into the same feed event. Default is 3600 seconds (1 hour).
#' @param min_bins_for_group Integer. Minimum number of bins that must show feed
#'   additions within the time window to classify as an "all-bin" feed event.
#'   Default is 3.
#' @param aggregate_all_bin Logical. If TRUE, groups individual bin additions
#'   into multi-bin feed events. If FALSE, returns individual bin additions.
#'   Default is TRUE.
#' @inheritParams set_global_cols
#'
#' @return If `aggregate_all_bin = FALSE`, returns a data frame (or named list of
#'   data frames) with columns:
#'   \itemize{
#'     \item `date` - Date of the feed addition
#'     \item `[bin_col]` - Bin identifier
#'     \item `time` - Timestamp of the FIRST detected addition in this event
#'     \item `weight_increase` - Total amount of feed added across all additions
#'       in this event (kg). If multiple rapid additions occurred, this is the sum.
#'     \item `bin_weight_after_fill` - Total bin weight after the FINAL addition
#'       in this event (kg). This includes any residual feed that was already in
#'       the bin before additions began.
#'   } 
#'   If `aggregate_all_bin = TRUE`, returns a data frame (or named list of data
#'   frames) with columns:
#'   \itemize{
#'     \item `date` - Date of the feed event
#'     \item `event_id` - Unique identifier for the feed event
#'     \item `event_start` - Earliest addition time in the event
#'     \item `event_end` - Latest addition time in the event
#'     \item `bins_filled` - Number of bins refilled in the event
#'     \item `avg_weight_increase` - Average feed added across bins (kg)
#'     \item `min_weight_increase` - Minimum feed added to any bin in the event (kg)
#'     \item `max_weight_increase` - Maximum feed added to any bin in the event (kg)
#'   }
#'
#' @note
#' **Multiple rapid additions to the same bin**: When a farmer adds feed to the
#' same bin multiple times in quick succession (e.g., bin 1 gets 10kg, then 10
#' seconds later 5kg, then 10 seconds later another 10kg), these are **always**
#' automatically grouped into one feed event per bin, regardless of the
#' `aggregate_all_bin` setting. The `bin_weight_after_fill` will be the weight
#' after the final addition (e.g., 35kg if the bin had 10kg residual before the
#' additions), not just the sum of what was added (25kg). This within-bin
#' aggregation happens BEFORE any across-bin aggregation.
#'
#' The function correctly handles cases where different animals visit the same
#' bin. Feed additions are detected based on weight changes between consecutive
#' visits at each bin, regardless of which animal made the visits.
#'
#' @examples
#' # Create sample visit data
#' visits <- data.frame(
#'   date = "2024-01-01",
#'   cow = c("A", "A", "B", "B", "C"),
#'   bin = c(1, 1, 2, 2, 3),
#'   start = as.POSIXct(c(
#'     "2024-01-01 08:00:00",
#'     "2024-01-01 09:00:00",
#'     "2024-01-01 08:05:00",
#'     "2024-01-01 09:05:00",
#'     "2024-01-01 08:10:00"
#'   ), tz = "UTC"),
#'   end = as.POSIXct(c(
#'     "2024-01-01 08:10:00",
#'     "2024-01-01 09:10:00",
#'     "2024-01-01 08:15:00",
#'     "2024-01-01 09:15:00",
#'     "2024-01-01 08:20:00"
#'   ), tz = "UTC"),
#'   start_weight = c(50, 45, 50, 43, 50),
#'   end_weight = c(45, 40, 43, 38, 45)
#' )
#'
#' # Detect per-bin additions (with within-bin aggregation)
#' additions <- detect_feed_additions(
#'   data = visits,
#'   min_weight_increase = 5,
#'   max_bin_time_gap = 600,
#'   aggregate_all_bin = FALSE
#' )
#'
#' # Detect all-bin feed events (aggregated across multiple bins)
#' feed_events <- detect_feed_additions(
#'   data = visits,
#'   min_weight_increase = 5,
#'   max_bin_time_gap = 3600,
#'   min_bins_for_group = 2,
#'   aggregate_all_bin = TRUE
#' )
#'
#' @export
detect_feed_additions <- function(data,
                                   min_weight_increase = 5,
                                   max_bin_time_gap = 3600,
                                   min_bins_for_group = 3,
                                   aggregate_all_bin = TRUE,
                                   bin_col = bin_col2(),
                                   start_col = start_col2(),
                                   end_col = end_col2(),
                                   start_weight_col = start_weight_col2(),
                                   end_weight_col = end_weight_col2()) {

  # Validate inputs
  .validate_feed_addition_data(data, min_weight_increase, max_bin_time_gap, min_bins_for_group)

  # Determine if input is a list or single dataframe
  is_list <- is.list(data) && !is.data.frame(data)

  if (!is_list) {
    # Get date from data if available, otherwise use default
    day_name <- if ("date" %in% names(data) && nrow(data) > 0 && !is.na(data$date[1])) {
      as.character(data$date[1])
    } else {
      "day1"
    }
    data <- list(data)
    names(data) <- day_name
  }

  # Process each day
  result <- lapply(names(data), function(day_name) {
    df <- data[[day_name]]

    # Extract date from data if available
    date_val <- if ("date" %in% names(df)) df$date[1] else day_name

    # Check required columns
    required_cols <- c(bin_col, start_col, end_col, start_weight_col, end_weight_col)
    missing_cols <- setdiff(required_cols, names(df))
    if (length(missing_cols) > 0) {
      stop("Missing required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
    }

    # Detect bin weight jumps
    bin_additions <- .detect_bin_weight_jump(
      df = df,
      date = date_val,
      min_weight_increase = min_weight_increase,
      bin_col = bin_col,
      start_col = start_col,
      start_weight_col = start_weight_col,
      end_weight_col = end_weight_col
    )

    # If no additions found, return appropriate empty structure
    if (nrow(bin_additions) == 0) {
      if (aggregate_all_bin) {
        return(data.frame(
          date = character(0),
          event_id = integer(0),
          event_start = numeric(0),
          event_end = numeric(0),
          bins_filled = integer(0),
          avg_weight_increase = numeric(0),
          min_weight_increase = numeric(0),
          max_weight_increase = numeric(0),
          stringsAsFactors = FALSE
        ))
      } else {
        result_df <- data.frame(
          date = character(0),
          time = numeric(0),
          weight_increase = numeric(0),
          bin_weight_after_fill = numeric(0),
          stringsAsFactors = FALSE
        )
        result_df[[bin_col]] <- character(0)
        return(result_df)
      }
    }

    # ALWAYS aggregate within bins first (consolidate rapid additions to same bin)
    bin_additions_aggregated <- .aggregate_same_bin_additions(
      bin_additions = bin_additions,
      max_bin_time_gap = max_bin_time_gap,
      bin_col = bin_col
    )

    # Then aggregate across bins if requested
    if (aggregate_all_bin) {
      return(.aggregate_bin_additions(
        bin_additions = bin_additions_aggregated,
        max_bin_time_gap = max_bin_time_gap,
        min_bins_for_group = min_bins_for_group
      ))
    } else {
      return(bin_additions_aggregated)
    }
  })

  # Name the result list
  names(result) <- names(data)

  # Return list or single dataframe based on input
  if (!is_list) {
    return(result[[1]])
  } else {
    return(result)
  }
}


#' Detect Individual Bin Weight Jumps
#'
#' @description
#' Internal function to detect weight increases at individual bins between
#' consecutive visits.
#'
#' @keywords internal
#' @noRd
.detect_bin_weight_jump <- function(df, date, min_weight_increase,
                                     bin_col, start_col, start_weight_col,
                                     end_weight_col) {

  # Sort by bin and start time
  df <- df |>
    dplyr::arrange(.data[[bin_col]], .data[[start_col]])

  # Calculate weight jumps between consecutive visits at same bin
  df_jumps <- df |>
    dplyr::group_by(.data[[bin_col]]) |>
    dplyr::mutate(
      next_start_weight = dplyr::lead(.data[[start_weight_col]]),
      next_end_weight = dplyr::lead(.data[[end_weight_col]]),
      next_start_time = dplyr::lead(.data[[start_col]]),
      weight_jump = .data$next_start_weight - .data[[end_weight_col]]
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(.data$weight_jump), .data$weight_jump >= min_weight_increase)

  # Return empty if no jumps detected
  if (nrow(df_jumps) == 0) {
    return(data.frame(
      date = character(0),
      time = numeric(0),
      weight_increase = numeric(0),
      bin_weight_after_fill = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  # Format output
  # bin_weight_after_fill is the maximum weight during the next visit
  # (handles case where feed is added during the visit, making end_weight > start_weight)
  result <- data.frame(
    date = rep(date, nrow(df_jumps)),
    time = df_jumps$next_start_time,  # Time when next visit started (after feed added)
    weight_increase = df_jumps$weight_jump,
    bin_weight_after_fill = pmax(df_jumps$next_start_weight, df_jumps$next_end_weight, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  result[[bin_col]] <- df_jumps[[bin_col]]

  return(result)
}


#' Aggregate Bin Additions into Feed Events
#'
#' @description
#' Internal function to group individual bin additions into multi-bin feed events
#' based on time proximity.
#'
#' @keywords internal
#' @noRd
.aggregate_bin_additions <- function(bin_additions, max_bin_time_gap,
                                      min_bins_for_group) {

  if (nrow(bin_additions) == 0) {
    return(data.frame(
      date = character(0),
      event_id = integer(0),
      event_start = numeric(0),
      event_end = numeric(0),
      bins_filled = integer(0),
      avg_weight_increase = numeric(0),
      min_weight_increase = numeric(0),
      max_weight_increase = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  # Sort by time
  bin_additions <- bin_additions |>
    dplyr::arrange(.data$time)

  # Assign event IDs based on time gaps
  # Get first time for default value
  first_time <- bin_additions$time[1]

  bin_additions <- bin_additions |>
    dplyr::mutate(
      time_gap = as.numeric(.data$time - dplyr::lag(.data$time, default = first_time)),
      new_event = .data$time_gap > max_bin_time_gap | dplyr::row_number() == 1,
      event_id = cumsum(.data$new_event)
    )

  # Group by event and summarize
  events <- bin_additions |>
    dplyr::group_by(.data$date, .data$event_id) |>
    dplyr::summarise(
      event_start = min(.data$time),
      event_end = max(.data$time),
      bins_filled = dplyr::n(),
      avg_weight_increase = mean(.data$weight_increase),
      min_weight_increase = min(.data$weight_increase),
      max_weight_increase = max(.data$weight_increase),
      .groups = "drop"
    ) |>
    dplyr::filter(.data$bins_filled >= min_bins_for_group)

  return(as.data.frame(events))
}


#' Aggregate Same-Bin Additions Within Time Window
#'
#' @description
#' Internal function to aggregate multiple feed additions to the same bin that
#' occur within max_bin_time_gap seconds of each other. Each bin gets at most
#' one feed event per time window.
#'
#' For each aggregated event:
#' - time = the time of the FIRST detected addition
#' - weight_increase = the SUM of all weight increases
#' - bin_weight_after_fill = the bin weight after the FINAL addition
#'
#' @param bin_additions Data frame with individual bin additions (from .detect_bin_weight_jump)
#' @param max_bin_time_gap Maximum time gap (in seconds) to group additions
#' @param bin_col Name of the bin column
#'
#' @return Data frame with aggregated bin additions
#'
#' @keywords internal
#' @noRd
.aggregate_same_bin_additions <- function(bin_additions, max_bin_time_gap, bin_col) {

  if (nrow(bin_additions) == 0) {
    return(bin_additions)
  }


  # Process each bin separately
  bins <- unique(bin_additions[[bin_col]])

  result_list <- lapply(bins, function(current_bin) {
    bin_data <- bin_additions[bin_additions[[bin_col]] == current_bin, ]

    # Sort by time within this bin
    bin_data <- bin_data[order(bin_data$time), ]

    if (nrow(bin_data) == 1) {
      return(bin_data)
    }

    # Calculate time gaps between consecutive additions for this bin
    time_diffs <- c(0, diff(as.numeric(bin_data$time)))

    # Assign event IDs: new event when time gap exceeds max_bin_time_gap
    event_ids <- cumsum(c(TRUE, time_diffs[-1] > max_bin_time_gap))

    bin_data$event_id <- event_ids

    # Aggregate by event_id
    aggregated <- do.call(rbind, lapply(unique(event_ids), function(eid) {
      event_data <- bin_data[bin_data$event_id == eid, ]

      data.frame(
        date = event_data$date[1],
        time = event_data$time[1],  # FIRST time
        weight_increase = sum(event_data$weight_increase),  # SUM of weight increases
        bin_weight_after_fill = event_data$bin_weight_after_fill[nrow(event_data)],  # FINAL bin weight
        stringsAsFactors = FALSE
      )
    }))

    aggregated[[bin_col]] <- current_bin
    return(aggregated)
  })

  # Combine all bins
  result <- do.call(rbind, result_list)

  # Sort by time across all bins
  result <- result[order(result$time), ]
  rownames(result) <- NULL

  return(result)
}


#' Validate Feed Addition Data
#'
#' @description
#' Internal function to validate inputs for feed addition detection.
#'
#' @keywords internal
#' @noRd
.validate_feed_addition_data <- function(data, min_weight_increase,
                                          max_bin_time_gap, min_bins_for_group) {

  # Check data is not NULL
  if (is.null(data)) {
    stop("data cannot be NULL", call. = FALSE)
  }

  # Check numeric parameters
  if (!is.numeric(min_weight_increase) || min_weight_increase <= 0) {
    stop("min_weight_increase must be a positive number", call. = FALSE)
  }

  if (!is.numeric(max_bin_time_gap) || max_bin_time_gap <= 0) {
    stop("max_bin_time_gap must be a positive number", call. = FALSE)
  }

  if (!is.numeric(min_bins_for_group) || min_bins_for_group < 1) {
    stop("min_bins_for_group must be at least 1", call. = FALSE)
  }
}
