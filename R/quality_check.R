# -----------------------------------------------------------------------------#
# -------------------- External user-facing functions -------------------------#
# -----------------------------------------------------------------------------#


#' Build a configuration list for data quality-control
#'
#' This helper centralises **every threshold** used by [qc()].
#' It lets you define what should be flagged as *abnormal* in your
#' feed–water data: extra-long bouts, unrealistically large intakes, bins with
#' too few visits, etc.  Each argument has a sensible default, so you normally
#' override only the handful you care about—then pass the resulting list to
#' `qc(cfg = qc_config(...))`.
#'
#' @param high_dur_feed  Numeric. Seconds that define a *long* feeding visit.
#' @param high_dur_water Numeric. Seconds that define a *long* drinking visit.
#' @param large_intake_feeder  Numeric. Kilograms that flag a single, unusually
#'   large **feed** intake event.
#' @param large_intake_drinker Numeric. Litres that flag a single, unusually
#'   large **water** intake event.
#' @param large_intake_rate_feed  Numeric. Kilograms/s considered a
#'   rapid feed-intake rate.
#' @param large_intake_rate_water Numeric. Litres/s considered a
#'   rapid water-intake rate.
#' @param low_visit_threshold Integer. Bins visited fewer than this number of
#'   times per day are flagged as *low traffic*.
#' @param total_cows_expected Integer. Expected herd size; if `NA` (default) the
#'   pipeline counts unique IDs automatically.
#' @param bin_offset Integer. Offset added to water-bin IDs so they do not
#'   overlap with feed-bin IDs. Default derives from [bin_offset2()].
#' @param low_feed_intake  Numeric. kg/day.  Daily feed intake flagged as *low*
#'  for a animal (default **35**).
#' @param high_feed_intake Numeric. kg/day. Daily feed intake flagged as *high*
#'  for an animal (default **75**).
#' @param low_wat_intake   Numeric. L/day. Daily water intake flagged as *low*
#'  for an animal (default **60**).
#' @param high_wat_intake  Numeric. L/day. Daily water intake flagged as *high*
#'  for an animal (default **180**).
#' @param replacement_threshold Numeric. Seconds. Time gap to classify replacement
#'   behaviour (default **26 s**).
#' @param calibration_error Numeric. Allowed feeder calibration error (default **0.5** kg/L).
#' @param bins_feed Integer vector. Valid **feed** bin IDs. Default
#'   [bins_feed2()].
#' @param bins_wat  Integer vector. Valid **water** bin IDs. Default
#'   [bins_wat2()].
#' @param ... Reserved for future or project-specific tweaks. Named elements
#'   here are appended to the returned list.
#'
#' @return A named list consumed by [qc()] and its internal `qc_*()`
#'   helpers.
#'
#' @examples
#' # Use all defaults
#' cfg <- qc_config()
#'
#' # Tighten the "long feeding visit" threshold
#' cfg2 <- qc_config(high_dur_feed = 1800)
#'
#' @export
qc_config <- function(
    high_dur_feed           = 2000,
    high_dur_water          = 1800,
    large_intake_feeder       = 8,     # kg per bout
    large_intake_drinker      = 30,     # L per bout
    large_intake_rate_feed  = 0.008,  # kg/s
    large_intake_rate_water = 0.35,  # L/s
    low_visit_threshold     = 10,
    total_cows_expected     = NA,
    low_feed_intake         = 35,
    high_feed_intake        = 75,
    low_wat_intake          = 60,
    high_wat_intake         = 180,
    replacement_threshold   = 26,
    calibration_error       = 0.5,
    bin_offset              = bin_offset2(),
    bins_feed               = bins_feed2(),
    bins_wat                = bins_wat2(),
    ...
) {
  c(
    list(
      high_dur_feed           = high_dur_feed,
      high_dur_water          = high_dur_water,
      large_intake_feeder       = large_intake_feeder,
      large_intake_drinker      = large_intake_drinker,
      large_intake_rate_feed  = large_intake_rate_feed,
      large_intake_rate_water = large_intake_rate_water,
      low_visit_threshold     = low_visit_threshold,
      total_cows_expected     = total_cows_expected,
      low_feed_intake         = low_feed_intake,
      high_feed_intake        = high_feed_intake,
      low_wat_intake          = low_wat_intake,
      high_wat_intake         = high_wat_intake,
      replacement_threshold   = replacement_threshold,
      calibration_error       = calibration_error,
      bin_offset              = bin_offset,
      bins_feed               = bins_feed,
      bins_wat                = bins_wat
    ),
    list(...)
  )
}


#' Run full quality-control check on feeder and drinker data
#'
#' @param feed  A list of daily **feed** data frames named by date, or `NULL`
#'  if you don't have feeder data
#' @param water A list of daily **water** data frames named by date, or `NULL`
#'  if you don't have water data
#' @param cfg   A configuration list created by [qc_config()].
#' @param tz    Time-zone for all analysis related to timestamps.  Defaults to [tz2()].
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
                        tz        = tz2()) {

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
  warn <- qc_double_detection(comb, warn)
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


# -----------------------------------------------------------------------------#
# ------------------------ Internal helper functions --------------------------#
# -----------------------------------------------------------------------------#

#' Create an empty warnings tibble with one row per day
#'
#' @param comb      List of daily data frames (feed, water or combined).
#' @param tz        Character.  Time‑zone for parsing the date names.
#' @param has_feed  Logical.  Whether *feed*‑specific columns are needed.
#' @param has_water Logical.  Whether *water*‑specific columns are needed.
#'
#' @return A dataframe with pre‑defined character columns ready for QC modules to
#'   populate.
#' @keywords internal
#' @noRd
qc_warning_skeleton <- function(comb,
                                tz        = tz2(),
                                has_feed  = TRUE,
                                has_water = TRUE) {
  # Ensure the input list is not empty
  if (length(comb) == 0) {
    stop("The input list is empty!")
  }

  # Get date list
  date_list <- names(comb)

  # Create the initial data frame
  warn_df <- data.frame(date = ymd(date_list, tz = tz))

  # Adding additional columns with default values (blank)
  general_columns <- c(
    "total_cows", "missing_cow", "double_bin_detect",
    "double_cow_detect", "negative_duration", "negative_intake",
    "cows_disappeared_after_noon", "bins_never_visited", "bins_low_traffic"
  )

  feed_columns <- c(
    "long_dur_feeder", "large_intake_feeder",
    "large_intake_rate_feeder",
    "low_daily_feed_intake_cows",
    "high_daily_feed_intake_cows",
    "feed_add_time_no_found"
  )

  wat_columns <- c(
    "long_dur_drinker", "large_intake_drinker",
    "large_intake_rate_drinker",
    "low_daily_water_intake_cows",
    "high_daily_water_intake_cows"
  )

  if (has_feed && has_water) {
    warning_columns <- c(general_columns, feed_columns, wat_columns)
  } else if (has_feed) {
    warning_columns <- c(general_columns, feed_columns)
  } else if (has_water) {
    warning_columns <- c(general_columns, wat_columns)
  }


  # Add warning columns to the data frame
  for (col in warning_columns) {
    warn_df[[col]] <- ""
  }

  return(warn_df)

}



#' Check and update total cow counts
#'
#' Computes the total number of unique cows in each day's data and
#' flags potential issues if the count differs from expected.
#'
#' @param comb List of combined daily data frames
#' @param warn Warning data frame to update
#' @param cfg Configuration list with expected cow counts
#'
#' @return Updated warning data frame with total cow information
#' @keywords internal
#' @noRd
qc_total_cows <- function(comb, warn, cfg) {
  # Iterate through each day's data
  for (i in seq_along(comb)) {
    date <- names(comb)[i]
    day_idx <- which(warn$date == date)

    if (length(day_idx) == 0 || nrow(comb[[i]]) == 0) {
      next
    }

    # Count unique cows
    cow_count <- length(unique(comb[[i]]$Cow))
    warn$total_cows[day_idx] <- as.character(cow_count)

    # Check against expected count if provided
    if (!is.na(cfg$total_cows_expected) && cow_count < cfg$total_cows_expected) {
      warn$missing_cow[day_idx] <- "Yes"
    }
  }

  return(warn)
}

#' Check for double detection issues
#'
#' Identifies and records cases where the same cow is detected at multiple bins
#' simultaneously, or when multiple cows are detected at the same bin simultaneously.
#'
#' @param comb List of combined daily data frames
#' @param warn Warning data frame to update
#'
#' @return Updated warning data frame with double detection information
#' @keywords internal
#' @noRd
qc_double_detection <- function(comb, warn) {
  # Process each day
  for (i in seq_along(comb)) {
    date <- names(comb)[i]
    day_idx <- which(warn$date == date)

    if (length(day_idx) == 0 || nrow(comb[[i]]) == 0) {
      next
    }

    # Sort data by cow and start time for processing
    day_data <- comb[[i]]

    # Check for 1 cow at 2 bins (double bin detection)
    double_bin_detect <- qc_detect_double_bins(day_data)
    if (nrow(double_bin_detect) > 0) {
      faulty_bins <- sort(unique(double_bin_detect$Bin))
      warn$double_bin_detect[day_idx] <- paste(faulty_bins, collapse = "; ")
    }

    # Check for 1 bin with 2 cows (double cow detection)
    double_cow_detect <- qc_detect_double_cows(day_data)
    if (nrow(double_cow_detect) > 0) {
      faulty_bins <- sort(unique(double_cow_detect$Bin))
      warn$double_cow_detect[day_idx] <- paste(faulty_bins, collapse = "; ")
    }
  }

  return(warn)
}

#' Detect when the same cow is recorded at multiple bins simultaneously
#'
#' @param day_data A single day's data frame
#'
#' @return Data frame with rows where double bin detections occurred
#' @keywords internal
#' @noRd
qc_detect_double_bins <- function(day_data) {
  # Sort the data by cow ID and start time
  day_data <- day_data[order(day_data$Cow, day_data$Start), ]

  # Create a data frame to store double detections
  double_detect <- data.frame()

  # Get unique cows
  cows <- unique(day_data$Cow)

  # Check each cow's data for overlapping visits
  for (cow in cows) {
    cow_data <- day_data[day_data$Cow == cow, ]

    if (nrow(cow_data) > 1) {
      for (k in 2:nrow(cow_data)) {
        # If the start time of current visit is before the end time of previous visit
        if (cow_data$Start[k] < cow_data$End[k-1]) {
          # This is a double detection - add both rows
          double_detect <- rbind(double_detect, cow_data[(k-1):k, ])
        }
      }
    }
  }

  return(double_detect)
}

#' Detect when multiple cows are recorded at the same bin simultaneously
#'
#' @param day_data A single day's data frame
#'
#' @return Data frame with rows where double cow detections occurred
#' @keywords internal
#' @noRd
qc_detect_double_cows <- function(day_data) {
  # Sort the data by bin ID and start time
  day_data <- day_data[order(day_data$Bin, day_data$Start), ]

  # Create a data frame to store double detections
  double_detect <- data.frame()

  # Get unique bins
  bins <- unique(day_data$Bin)

  # Check each bin's data for overlapping visits
  for (bin in bins) {
    bin_data <- day_data[day_data$Bin == bin, ]

    if (nrow(bin_data) > 1) {
      for (k in 2:nrow(bin_data)) {
        # If the start time of current visit is before the end time of previous visit
        if (bin_data$Start[k] < bin_data$End[k-1]) {
          # This is a double detection - add both rows
          double_detect <- rbind(double_detect, bin_data[(k-1):k, ])
        }
      }
    }
  }

  return(double_detect)
}

#' Check for negative durations and intakes
#'
#' Identifies and flags records with negative duration or intake values
#'
#' @param comb List of combined daily data frames
#' @param warn Warning data frame to update
#'
#' @return Updated warning data frame with negative value information
#' @keywords internal
#' @noRd
qc_negatives <- function(comb, warn) {
  # Process each day
  for (i in seq_along(comb)) {
    date <- names(comb)[i]
    day_idx <- which(warn$date == date)

    if (length(day_idx) == 0 || nrow(comb[[i]]) == 0) {
      next
    }

    day_data <- comb[[i]]

    # Check for negative durations
    neg_duration <- day_data[day_data$Duration < 0, ]
    if (nrow(neg_duration) > 0) {
      neg_bins <- sort(unique(neg_duration$Bin))
      warn$negative_duration[day_idx] <- paste(neg_bins, collapse = "; ")
    }

    # Check for significant negative intakes (< -1)
    neg_intake <- day_data[day_data$Intake < 0 & abs(day_data$Intake) > 1, ]
    if (nrow(neg_intake) > 0) {
      neg_bins <- sort(unique(neg_intake$Bin))
      warn$negative_intake[day_idx] <- paste(neg_bins, collapse = "; ")
    }
  }

  return(warn)
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



