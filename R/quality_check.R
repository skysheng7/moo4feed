# -----------------------------------------------------------------------------#
# -------------------- External user-facing functions -------------------------#
# -----------------------------------------------------------------------------#

#' Run full quality-control check on feeder and drinker data
#'
#' @param feed  A list of daily **feed** data frames named by date, or `NULL`
#'  if you don't have feeder data
#' @param water A list of daily **water** data frames named by date, or `NULL`
#'  if you don't have water data
#' @param cfg   A configuration list created by [qc_config()].
#' @param tz    Time-zone for all analysis related to timestamps.  Defaults to [tz2()].
#' @param verbose Logical. If TRUE, print details of data where errors were detected
#'
#' @return A list with four elements:
#' \describe{
#'   \item{`warnings`}{a tidy data frame with one row per day and one column
#'                    per warning code.}
#'   \item{`feed`}{cleaned feed list (or `NULL`).}
#'   \item{`water`}{cleaned water list (or `NULL`).}
#'   \item{`combined`}{merged feed + water list (after all fixes).}
#' }
#'
#' @examples
#' cfg <- qc_config(high_dur_feed = 2500, low_visit_threshold = 5)
#' out <- qc(feed = all_feed, water = all_water, cfg = cfg)
#' out$warnings
#' @export
qc <- function(feed      = NULL,
               water     = NULL,
               cfg       = qc_config(),
               tz        = tz2(),
               verbose = TRUE) {

  # --- 0. combine ----------------------------------------------------------
  if (!is.null(feed) && !is.null(water)) {
    comb <- combine_feed_water(feed, water)
  } else if (!is.null(feed)) {
    comb <- feed
  } else if (!is.null(water)) {
    comb <- water
  } else {
    stop("`feed` and `water` can't all be NULL. One of them needs to be a list of dataframe.")
  }

  # --- 1. prepare warning skeleton ----------------------------------------
  warn  <- qc_warning_skeleton(comb, tz = tz,
                               has_feed  = !is.null(feed),
                               has_water = !is.null(water))

  # --- 2. run QC modules ---------------------------------------------------
  warn <- qc_total_cows(comb, warn, cfg)
  warn <- qc_double_detection(comb, warn, verbose = verbose)
  warn <- qc_negatives(comb, warn)
  warn <- qc_long_duration(feed,  warn, cfg, "feed")
  warn <- qc_long_duration(water, warn, cfg, "water")
  warn <- qc_large_intake(feed,  warn, cfg, "feed")
  warn <- qc_large_intake(water, warn, cfg, "water")
  warn <- qc_visit_activity(comb, warn, cfg, tz)

  # --- 3. clean data -------------------------------------------------------
  comb <- qc_fix_overlaps(comb)
  comb <- qc_drop_negatives(comb)

  feed  <- if (!is.null(feed))  extract_feed(comb, cfg$bin_offset)  else NULL
  water <- if (!is.null(water)) extract_water(comb, cfg$bin_offset) else NULL

  # --- 4. return -----------------------------------------------------------
  list(warnings = warn,
       feed     = feed,
       water    = water,
       combined = comb)
}













#' Check for unusually long visit durations
#'
#' Identifies and flags records with visit durations exceeding thresholds
#'
#' @param data List of daily data frames (feed or water)
#' @param warn Warning data frame to update
#' @param cfg Configuration list with duration thresholds
#' @param type String specifying the data type ("feed" or "water")
#'
#' @return Updated warning data frame with long duration information
#' @keywords internal
#' @noRd
qc_long_duration <- function(data, warn, cfg, type = "feed") {
  # Skip if data is NULL
  if (is.null(data)) {
    return(warn)
  }

  # Set the appropriate threshold and column name based on type
  threshold <- if (type == "feed") cfg$high_dur_feed else cfg$high_dur_water
  warning_col <- if (type == "feed") "long_dur_feeder" else "long_dur_drinker"

  # Process each day
  for (i in seq_along(data)) {
    date <- names(data)[i]
    day_idx <- which(warn$date == date)

    if (length(day_idx) == 0 || nrow(data[[i]]) == 0) {
      next
    }

    # Identify records with long durations
    long_visits <- data[[i]][data[[i]]$Duration > threshold, ]

    if (nrow(long_visits) > 0) {
      long_bins <- sort(unique(long_visits$Bin))
      warn[day_idx, warning_col] <- paste(long_bins, collapse = "; ")
    }
  }

  return(warn)
}

#' Check for unusually large intakes
#'
#' Identifies and flags records with intake values exceeding thresholds
#'
#' @param data List of daily data frames (feed or water)
#' @param warn Warning data frame to update
#' @param cfg Configuration list with intake thresholds
#' @param type String specifying the data type ("feed" or "water")
#'
#' @return Updated warning data frame with large intake information
#' @keywords internal
#' @noRd
qc_large_intake <- function(data, warn, cfg, type = "feed") {
  # Skip if data is NULL
  if (is.null(data)) {
    return(warn)
  }

  # Set the appropriate threshold and column name based on type
  intake_threshold <- if (type == "feed") cfg$large_intake_feeder else cfg$large_intake_drinker
  rate_threshold <- if (type == "feed") cfg$large_intake_rate_feed else cfg$large_intake_rate_water

  intake_col <- if (type == "feed") "large_intake_feeder" else "large_intake_drinker"
  rate_col <- if (type == "feed") "large_intake_rate_feeder" else "large_intake_rate_drinker"

  # Process each day
  for (i in seq_along(data)) {
    date <- names(data)[i]
    day_idx <- which(warn$date == date)

    if (length(day_idx) == 0 || nrow(data[[i]]) == 0) {
      next
    }

    day_data <- data[[i]]

    # Calculate intake rate if not already present
    if (!("rate" %in% colnames(day_data))) {
      day_data$rate <- day_data$Intake / day_data$Duration
      day_data$rate[is.infinite(day_data$rate) | is.nan(day_data$rate)] <- 0
    }

    # Identify large intakes
    large_intakes <- day_data[day_data$Intake > intake_threshold, ]
    if (nrow(large_intakes) > 0) {
      large_bins <- sort(unique(large_intakes$Bin))
      warn[day_idx, intake_col] <- paste(large_bins, collapse = "; ")
    }

    # Identify rapid intake rates
    rapid_intakes <- day_data[day_data$Intake > 0 & day_data$rate > rate_threshold, ]
    if (nrow(rapid_intakes) > 0) {
      rapid_bins <- sort(unique(rapid_intakes$Bin))
      warn[day_idx, rate_col] <- paste(rapid_bins, collapse = "; ")
    }
  }

  return(warn)
}

#' Check for bin visit activity patterns
#'
#' Identifies issues like bins with low traffic, cows that disappeared after noon,
#' and bins that were never visited.
#'
#' @param comb List of combined daily data frames
#' @param warn Warning data frame to update
#' @param cfg Configuration list with thresholds
#' @param tz Time zone string
#'
#' @return Updated warning data frame with visit activity information
#' @keywords internal
#' @noRd
qc_visit_activity <- function(comb, warn, cfg, tz) {
  # Create a complete bin list from configuration
  all_bins <- c(cfg$bins_feed, cfg$bins_wat)

  # Process each day
  for (i in seq_along(comb)) {
    date <- names(comb)[i]
    day_idx <- which(warn$date == date)

    if (length(day_idx) == 0 || nrow(comb[[i]]) == 0) {
      next
    }

    day_data <- comb[[i]]

    # Check for bins that were never visited
    visited_bins <- unique(day_data$Bin)
    never_visited <- setdiff(all_bins, visited_bins)
    if (length(never_visited) > 0) {
      warn$bins_never_visited[day_idx] <- paste(sort(never_visited), collapse = "; ")
    }

    # Check for bins with low traffic
    bin_visits <- table(day_data$Bin)
    low_traffic_bins <- names(bin_visits)[bin_visits < cfg$low_visit_threshold]
    if (length(low_traffic_bins) > 0) {
      warn$bins_low_traffic[day_idx] <- paste(sort(as.numeric(low_traffic_bins)), collapse = "; ")
    }

    # Check for cows that disappeared after noon
    noon_time <- as.POSIXct(paste(date, "12:00:00"), tz = tz)
    last_seen <- qc_determine_last_seen_time(day_data, noon_time, tz)
    if (length(last_seen) > 0) {
      warn$cows_disappeared_after_noon[day_idx] <- paste(sort(last_seen), collapse = "; ")
    }
  }

  return(warn)
}

#' Determine which cows were last seen before a specific time
#'
#' @param day_data A single day's data frame
#' @param cutoff_time Time cutoff for determining disappearance
#' @param tz Time zone string
#'
#' @return Vector of cow IDs that were last seen before the cutoff time
#' @keywords internal
#' @noRd
qc_determine_last_seen_time <- function(day_data, cutoff_time, tz) {
  # Ensure times are in POSIXct format
  day_data$End <- as.POSIXct(day_data$End, tz = tz)

  # Get unique cows
  cows <- unique(day_data$Cow)
  disappeared_cows <- character()

  # Check each cow's last appearance
  for (cow in cows) {
    cow_data <- day_data[day_data$Cow == cow, ]
    last_time <- max(cow_data$End)

    if (last_time < cutoff_time) {
      disappeared_cows <- c(disappeared_cows, as.character(cow))
    }
  }

  return(disappeared_cows)
}

#' Fix overlapping visit records
#'
#' Resolves issues where the same cow has overlapping visit times at different bins,
#' or where the same bin has overlapping visit times for different cows.
#'
#' @param comb List of combined daily data frames
#'
#' @return List of data frames with overlaps fixed
#' @keywords internal
#' @noRd
qc_fix_overlaps <- function(comb) {
  # Process each day
  for (i in seq_along(comb)) {
    if (nrow(comb[[i]]) == 0) {
      next
    }

    # Fix overlaps where the same cow is at different bins
    comb[[i]] <- qc_fix_cow_overlaps(comb[[i]])

    # Fix overlaps where different cows are at the same bin
    comb[[i]] <- qc_fix_bin_overlaps(comb[[i]])

    # Recalculate durations after fixing overlaps
    comb[[i]] <- qc_recalculate_durations(comb[[i]])
  }

  return(comb)
}

#' Fix overlapping records for the same cow at different bins
#'
#' @param day_data A single day's data frame
#'
#' @return Data frame with cow overlaps fixed
#' @keywords internal
#' @noRd
qc_fix_cow_overlaps <- function(day_data) {
  # Sort by cow and start time
  day_data <- day_data[order(day_data$Cow, day_data$Start), ]

  # Fix overlaps by adjusting end times
  for (k in 2:nrow(day_data)) {
    if (day_data$Cow[k] == day_data$Cow[k-1] &&
        day_data$Start[k] < day_data$End[k-1]) {
      # Adjust the end time of the previous visit to be 1 second before the start of the current visit
      day_data$End[k-1] <- day_data$Start[k] - 1
    }
  }

  return(day_data)
}



