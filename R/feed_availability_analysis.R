#' Calculate Percentage of Feed Remaining at Each Visit
#'
#' @description
#' Calculates the percentage of feed remaining at each visit based on the most
#' recent feed addition for that specific bin. Requires per-bin feed addition data
#' from [detect_feed_additions()]. Returns both visit-level data with feed
#' percentages and daily summary statistics.
#'
#' @param visit_data A named list of daily data frames or a single data frame
#'   containing visit records.
#' @param feed_addition_data A named list of daily data frames or a single data
#'   frame containing feed addition events from [detect_feed_additions()].
#'   Must have `aggregate_all_bin = FALSE` to get per-bin feed additions.
#' @inheritParams set_global_cols
#'
#' @return A list with two elements:
#'   \itemize{
#'     \item `visits` - Visit-level data with added columns:
#'       \itemize{
#'         \item `feed_addition_time` - Time when feed was added to this bin
#'         \item `feed_added_weight` - Weight of feed added to this bin (kg)
#'         \item `pct_feed_remaining` - Percentage of feed remaining at visit start
#'       }
#'     \item `daily_summary` - Named list (or data frame) with columns:
#'       \itemize{
#'         \item `date` - Date
#'         \item `[id_col]` - Animal identifier
#'         \item `mean_pct_feed_remaining` - Average percentage across visits
#'         \item `median_pct_feed_remaining` - Median percentage across visits
#'         \item `sd_pct_feed_remaining` - Standard deviation of percentage
#'         \item `total_visits_analyzed` - Number of visits analyzed
#'       }
#'   }
#'
#' @details
#' The function matches each visit to the most recent feed addition for that
#' specific bin. Feed percentages are calculated as:
#' `(start_weight / bin_weight_after_fill) * 100`, capped at 100%.
#'
#' The `bin_weight_after_fill` represents the total bin weight immediately after
#' feed was added, which accounts for any residual feed that was already in the bin.
#' This is more accurate than using just the amount of feed added. For example,
#' if a bin had 10kg residual and the farmer added 25kg in total (possibly across
#' multiple rapid additions), `bin_weight_after_fill` would be 35kg, not 25kg.
#'
#' **Important**: When calling [detect_feed_additions()], multiple rapid additions
#' to the same bin (within `max_bin_time_gap`) are automatically aggregated into
#' a single event. The `bin_weight_after_fill` from that aggregation represents
#' the bin weight after the final addition, which is exactly what this function
#' needs for accurate percentage calculations.
#'
#' **Multi-day behavior**: When processing a list of daily data, visits early in a
#' day (before any feed addition on that day) are matched to feed additions from the
#' previous calendar day. The function calculates the previous day by subtracting one
#' day from the current date (extracted from the `date` column in visits or parsed
#' from the list name). This works correctly even if:
#'
#' * Days are provided out of chronological order in the list
#' * There are gaps in the data (missing days)
#' * Day names use different date formats
#'
#' If the previous calendar day is not found in the data, or if dates cannot be parsed,
#' visits before the first feed addition will have NA values. The first day in the list
#' only uses its own feed additions.
#'
#' Visits that occur before any feed addition to that bin (including previous day
#' for multi-day lists) will have NA values.
#'
#' @examples
#' # Create sample visit data
#' visits <- data.frame(
#'   date = "2024-01-01",
#'   cow = c("A", "A", "B", "B"),
#'   bin = c(1, 1, 2, 2),
#'   start = as.POSIXct(c(
#'     "2024-01-01 08:00:00",
#'     "2024-01-01 10:00:00",
#'     "2024-01-01 08:05:00",
#'     "2024-01-01 10:05:00"
#'   ), tz = "UTC"),
#'   end = as.POSIXct(c(
#'     "2024-01-01 08:10:00",
#'     "2024-01-01 10:10:00",
#'     "2024-01-01 08:15:00",
#'     "2024-01-01 10:15:00"
#'   ), tz = "UTC"),
#'   start_weight = c(50, 45, 50, 43),
#'   end_weight = c(45, 40, 43, 38)
#' )
#'
#' # Detect per-bin feed additions
#' feed_additions <- detect_feed_additions(
#'   data = visits,
#'   aggregate_all_bin = FALSE
#' )
#'
#' # Calculate feed availability at each visit
#' availability <- calculate_feed_availability(
#'   visit_data = visits,
#'   feed_addition_data = feed_additions
#' )
#'
#' # Access visit-level data
#' visit_pct <- availability$visits
#'
#' # Access daily summaries
#' daily_pct <- availability$daily_summary
#'
#' @export
calculate_feed_availability <- function(visit_data,
                                        feed_addition_data,
                                        id_col = id_col2(),
                                        bin_col = bin_col2(),
                                        start_col = start_col2(),
                                        start_weight_col = start_weight_col2()) {
  # Validate inputs
  .validate_feed_availability_data(visit_data, feed_addition_data)

  # Determine if input is a list or single dataframe
  is_list <- is.list(visit_data) && !is.data.frame(visit_data)

  if (!is_list) {
    day_name <- if ("date" %in% names(visit_data) && nrow(visit_data) > 0 && !is.na(visit_data$date[1])) {
      as.character(visit_data$date[1])
    } else {
      "day1"
    }
    visit_data <- list(visit_data)
    feed_addition_data <- list(feed_addition_data)
    names(visit_data) <- day_name
    names(feed_addition_data) <- day_name
  }

  # Ensure both have same names
  if (!all(names(visit_data) == names(feed_addition_data))) {
    stop("visit_data and feed_addition_data must have matching day names",
      call. = FALSE
    )
  }

  # Process each day for visit-level data
  visit_results <- lapply(names(visit_data), function(day_name) {
    visits <- visit_data[[day_name]]
    feed_events <- feed_addition_data[[day_name]]

    # Check required columns in visits
    required_visit_cols <- c(id_col, bin_col, start_col, start_weight_col)
    missing_cols <- setdiff(required_visit_cols, names(visits))
    if (length(missing_cols) > 0) {
      stop("Missing required columns in visit_data: ",
        paste(missing_cols, collapse = ", "),
        call. = FALSE
      )
    }

    # Check required columns in feed_events
    required_feed_cols <- c(bin_col, "time", "weight_increase", "bin_weight_after_fill")
    missing_feed_cols <- setdiff(required_feed_cols, names(feed_events))
    if (length(missing_feed_cols) > 0) {
      stop("Missing required columns in feed_addition_data: ",
        paste(missing_cols, collapse = ", "),
        ". Use detect_feed_additions() with aggregate_all_bin = FALSE.",
        call. = FALSE
      )
    }

    # Combine current day's feed events with previous day's (if exists)
    # Only keep required columns to ensure consistent structure across days
    required_feed_cols <- c(bin_col, "time", "weight_increase", "bin_weight_after_fill")

    combined_feed_events <- feed_events |>
      dplyr::select(dplyr::all_of(required_feed_cols))

    # Try to find previous day's feed additions using date
    prev_day_name <- .get_previous_day_name(day_name, visits, names(feed_addition_data))

    if (!is.null(prev_day_name)) {
      prev_feed_events <- feed_addition_data[[prev_day_name]] |>
        dplyr::select(dplyr::all_of(required_feed_cols))

      # Only combine if previous day has feed events
      if (nrow(prev_feed_events) > 0) {
        # Ensure column types match before combining
        prev_feed_events <- .harmonize_column_types(
          prev_feed_events, combined_feed_events, required_feed_cols
        )
        combined_feed_events <- dplyr::bind_rows(prev_feed_events, combined_feed_events)
      }
    }

    # If no feed events (current or previous), return visits with NA values
    if (nrow(combined_feed_events) == 0) {
      if (nrow(visits) == 0) {
        # Empty dataframe: add empty columns of correct type
        visits$feed_addition_time <- structure(numeric(0), class = c("POSIXct", "POSIXt"))
        visits$feed_added_weight <- numeric(0)
        visits$bin_weight_after_fill <- numeric(0)
        visits$pct_feed_remaining <- numeric(0)
      } else {
        # Non-empty dataframe: add NA values
        visits$feed_addition_time <- as.POSIXct(NA)
        visits$feed_added_weight <- NA_real_
        visits$bin_weight_after_fill <- NA_real_
        visits$pct_feed_remaining <- NA_real_
      }
      return(visits)
    }

    # If no visits, return empty structure with correct columns
    if (nrow(visits) == 0) {
      visits$feed_addition_time <- structure(numeric(0), class = c("POSIXct", "POSIXt"))
      visits$feed_added_weight <- numeric(0)
      visits$bin_weight_after_fill <- numeric(0)
      visits$pct_feed_remaining <- numeric(0)
      return(visits)
    }

    # Match each visit to most recent feed addition for that specific bin
    # Using vectorized join approach with combined feed events
    visits <- .match_visits_to_feed_additions_vectorized(
      visits = visits,
      feed_events = combined_feed_events,
      bin_col = bin_col,
      start_col = start_col,
      start_weight_col = start_weight_col
    )

    return(visits)
  })

  # Name the visit results
  names(visit_results) <- names(visit_data)

  # Calculate daily summaries
  daily_summaries <- lapply(names(visit_results), function(day_name) {
    visits <- visit_results[[day_name]]

    # Extract date
    date_val <- if ("date" %in% names(visits)) visits$date[1] else day_name

    # Filter to visits with valid feed percentage
    valid_visits <- visits |>
      dplyr::filter(!is.na(.data$pct_feed_remaining))

    # If no valid visits, return empty
    if (nrow(valid_visits) == 0) {
      return(.empty_feed_availability_summary(date_val, id_col))
    }

    # Summarize per animal per day
    daily <- valid_visits |>
      dplyr::group_by(.data[[id_col]]) |>
      dplyr::summarise(
        mean_pct_feed_remaining = mean(.data$pct_feed_remaining, na.rm = TRUE),
        median_pct_feed_remaining = stats::median(.data$pct_feed_remaining,
          na.rm = TRUE
        ),
        sd_pct_feed_remaining = stats::sd(.data$pct_feed_remaining, na.rm = TRUE),
        total_visits_analyzed = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::mutate(date = date_val) |>
      dplyr::select("date", dplyr::everything())

    return(as.data.frame(daily))
  })

  # Name the daily summaries
  names(daily_summaries) <- names(visit_data)

  # Return appropriate structure
  if (!is_list) {
    return(list(
      visits = visit_results[[1]],
      daily_summary = daily_summaries[[1]]
    ))
  } else {
    return(list(
      visits = visit_results,
      daily_summary = daily_summaries
    ))
  }
}


#' Match Visits to Feed Additions Using Vectorized Join
#'
#' @description
#' Internal function to match each visit to the most recent feed addition for
#' that specific bin using a vectorized approach with left_join and window functions.
#'
#' @keywords internal
#' @noRd
.match_visits_to_feed_additions_vectorized <- function(visits, feed_events,
                                                        bin_col, start_col,
                                                        start_weight_col) {
  # Store the original timezone
  original_tz <- attr(visits[[start_col]], "tzone")

  # Add a row identifier for later ordering
  visits <- visits |>
    dplyr::mutate(.visit_row_id = dplyr::row_number())

  # Rename feed_events columns to avoid conflicts
  feed_events_renamed <- feed_events |>
    dplyr::rename(
      feed_addition_time = "time",
      feed_added_weight = "weight_increase"
    )

  # Harmonize bin column type between visits and feed_events before joining
  feed_events_renamed <- .harmonize_column_types(
    feed_events_renamed, visits, c(bin_col)
  )

  # Cross join visits with feed additions for same bin, keeping only prior additions
  # Then select the most recent feed addition per visit
  matched <- visits |>
    dplyr::left_join(
      feed_events_renamed,
      by = bin_col,
      relationship = "many-to-many"
    ) |>
    dplyr::filter(is.na(.data$feed_addition_time) | .data$feed_addition_time <= .data[[start_col]]) |>
    dplyr::group_by(.data$.visit_row_id) |>
    dplyr::slice_max(order_by = .data$feed_addition_time, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()

  # Handle visits with no matching feed additions
  # These will have NA values from the left join
  if (nrow(matched) < nrow(visits)) {
    # Some visits had no feed additions for their bin - need to keep them with NAs
    missing_visits <- visits |>
      dplyr::filter(!.data$.visit_row_id %in% matched$.visit_row_id) |>
      dplyr::mutate(
        feed_addition_time = as.POSIXct(NA, tz = original_tz),
        feed_added_weight = NA_real_,
        bin_weight_after_fill = NA_real_
      )

    matched <- dplyr::bind_rows(matched, missing_visits) |>
      dplyr::arrange(.data$.visit_row_id)
  }

  # Calculate percentage of feed remaining using bin_weight_after_fill
  result <- matched |>
    dplyr::mutate(
      pct_feed_remaining = .calculate_feed_percentage(
        .data[[start_weight_col]],
        .data$bin_weight_after_fill
      )
    ) |>
    dplyr::select(-".visit_row_id")

  # Ensure feed_addition_time has correct timezone
  if (!is.null(original_tz)) {
    result$feed_addition_time <- structure(
      as.numeric(result$feed_addition_time),
      class = c("POSIXct", "POSIXt"),
      tzone = original_tz
    )
  }

  return(result)
}


#' Calculate Feed Percentage
#'
#' @description
#' Internal function to compute percentage of feed remaining.
#' Uses bin_weight_after_fill (total bin weight after addition) as the denominator.
#' Caps at 100% (cannot exceed what was available after fill).
#'
#' @param start_weight Numeric. The weight at the start of the visit.
#' @param bin_weight_after_fill Numeric. The bin weight right after feed was added.
#'
#' @keywords internal
#' @noRd
.calculate_feed_percentage <- function(start_weight, bin_weight_after_fill) {
  # If bin_weight_after_fill is 0 or NA, return NA
  pct <- ifelse(is.na(bin_weight_after_fill) | bin_weight_after_fill == 0,
    NA_real_,
    (start_weight / bin_weight_after_fill) * 100
  )

  # Cap at 100%
  pct <- ifelse(!is.na(pct) & pct > 100, 100, pct)

  return(pct)
}


#' Create Empty Feed Availability Summary
#'
#' @description
#' Internal function to create an empty result dataframe with proper structure.
#'
#' @keywords internal
#' @noRd
.empty_feed_availability_summary <- function(date_val, id_col) {
  result <- data.frame(
    date = character(0),
    mean_pct_feed_remaining = numeric(0),
    median_pct_feed_remaining = numeric(0),
    sd_pct_feed_remaining = numeric(0),
    total_visits_analyzed = integer(0),
    stringsAsFactors = FALSE
  )
  # Add id_col as second column
  result[[id_col]] <- character(0)
  result <- result |>
    dplyr::select("date", dplyr::all_of(id_col), dplyr::everything())
  return(result)
}


#' Get Previous Day Name for Cross-Day Feed Matching
#'
#' @description
#' Internal function to find the name of the previous calendar day in a named list.
#' Handles different day name formats and returns NULL if previous day not found.
#'
#' @param current_day_name Character. Name of current day in the list.
#' @param visits Data frame. Visit data for current day (used to extract date if available).
#' @param available_day_names Character vector. All available day names in feed_addition_data.
#'
#' @return Character or NULL. Name of previous day if found, NULL otherwise.
#'
#' @keywords internal
#' @noRd
.get_previous_day_name <- function(current_day_name, visits, available_day_names) {
  # Try to extract date from visits data
  current_date <- NULL

  if ("date" %in% names(visits) && nrow(visits) > 0) {
    date_val <- visits$date[1]
    # Try to parse as date
    current_date <- tryCatch(
      {
        suppressWarnings(lubridate::as_date(date_val))
      },
      error = function(e) NULL
    )
  }

  # If no date in visits, try to parse day_name as date
  if (is.null(current_date)) {
    current_date <- tryCatch(
      {
        suppressWarnings(lubridate::as_date(current_day_name))
      },
      error = function(e) NULL
    )
  }

  # If we successfully got a date, calculate previous day and look it up
  if (!is.null(current_date)) {
    prev_date <- current_date - lubridate::days(1)
    prev_date_str <- as.character(prev_date)

    # Check if previous day exists in available_day_names
    if (prev_date_str %in% available_day_names) {
      return(prev_date_str)
    }
  }

  # Could not find previous day
  return(NULL)
}


#' Harmonize Column Types Between Two Data Frames
#'
#' @description
#' Internal function to ensure specified columns have the same types in both data frames
#' before combining them with bind_rows(). Converts the first dataframe's columns
#' to match the types of the second dataframe's columns.
#'
#' @param df1 First data frame (will be modified)
#' @param df2 Second data frame (reference for types)
#' @param cols Character vector of column names to harmonize
#'
#' @return Modified df1 with column types matching df2
#'
#' @keywords internal
#' @noRd
.harmonize_column_types <- function(df1, df2, cols) {
  for (col in cols) {
    if (!(col %in% names(df1)) || !(col %in% names(df2))) {
      next
    }
    
    # Get the types of columns
    type1 <- class(df1[[col]])[1]
    type2 <- class(df2[[col]])[1]
    
    # If types match, skip
    if (type1 == type2) {
      next
    }
    
    # Handle POSIXct conversion specially
    if (type2 %in% c("POSIXct", "POSIXt")) {
      if (type1 == "numeric" || type1 == "double") {
        # Convert numeric to POSIXct using timezone from df2
        tz <- attr(df2[[col]], "tzone")
        if (is.null(tz)) tz <- "UTC"
        df1[[col]] <- structure(
          df1[[col]],
          class = c("POSIXct", "POSIXt"),
          tzone = tz
        )
      }
    } else if (type2 == "character") {
      df1[[col]] <- as.character(df1[[col]])
    } else if (type2 == "numeric" || type2 == "double") {
      df1[[col]] <- as.numeric(df1[[col]])
    } else if (type2 == "integer") {
      df1[[col]] <- as.integer(df1[[col]])
    }
  }
  
  return(df1)
}


#' Validate Feed Availability Data
#'
#' @description
#' Internal function to validate inputs for feed availability calculation.
#'
#' @keywords internal
#' @noRd
.validate_feed_availability_data <- function(visit_data, feed_addition_data) {
  if (is.null(visit_data)) {
    stop("visit_data cannot be NULL", call. = FALSE)
  }

  if (is.null(feed_addition_data)) {
    stop("feed_addition_data cannot be NULL", call. = FALSE)
  }

  # Check if feed_addition_data has required structure (per-bin additions)
  check_feed <- if (is.data.frame(feed_addition_data)) {
    feed_addition_data
  } else if (is.list(feed_addition_data) && length(feed_addition_data) > 0) {
    feed_addition_data[[1]]
  } else {
    stop("feed_addition_data must be a data frame or named list of data frames",
      call. = FALSE
    )
  }

  # Check that it's NOT aggregated data (do this first before checking required columns)
  if ("event_id" %in% names(check_feed)) {
    stop("feed_addition_data appears to be aggregated (contains event_id). ",
      "Use detect_feed_additions() with aggregate_all_bin = FALSE ",
      "to get per-bin feed additions.",
      call. = FALSE
    )
  }

  # Validate that it's per-bin feed addition data (has required columns)
  required_cols <- c("time", "weight_increase", "bin_weight_after_fill")
  missing_cols <- setdiff(required_cols, names(check_feed))

  if (length(missing_cols) > 0) {
    stop("feed_addition_data must be created with detect_feed_additions() ",
      "using aggregate_all_bin = FALSE. Missing columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
}
