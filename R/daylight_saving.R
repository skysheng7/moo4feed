# -----------------------------------------------------------------------------#
# -------------------- External user-facing functions -------------------------#
# -----------------------------------------------------------------------------#

#' Get DST Switch Dates and Exact Transition Times
#'
#' A wrapper around `dst_switch_day()` and `dst_switch_hm()` to generate a
#' data frame with both the dates and exact times (hour and minute) when
#' Daylight Saving Time (DST) transitions occur.
#'
#' @param years A numeric vector specifying the range of years to evaluate. Use the format `c(start_year, end_year)`, e.g., `years = c(2020, 2021)`. Must contain at least one numeric value.
#' @param tz A valid time zone name (default is the time zone of your current physical location), used to determine DST rules. Use `OlsonNames()` to see all valid options.
#' @param interval An integer (1–59) specifying the time resolution in minutes used to detect DST changes (default: 1).
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{year}{The year of DST transitions}
#'   \item{spring}{Date when DST begins ("spring forward")}
#'   \item{fall}{Date when DST ends ("fall back")}
#'   \item{spring_next_day}{The day after spring DST change}
#'   \item{fall_next_day}{The day after fall DST change}
#'   \item{spring_time}{Exact timestamp just before DST starts}
#'   \item{fall_time}{Exact timestamp just before DST ends}
#' }
#' Returns an empty data frame with all columns if no transitions are found.
#'
#' @details
#' Internally calls `dst_switch_day()` to get the switch dates, and
#' `dst_switch_hm()` to detect the transition times for each spring and fall date.
#' If transition times cannot be detected, `NA` values are returned in the corresponding columns.
#'
#' @examples
#' dst_full <- get_dst_switch_info(years = 2021:2022, tz = "America/Vancouver")
#'
#' @export
get_dst_switch_info <- function(years = c(2020, 2021), tz = Sys.timezone(), interval = 1) {
  # --- Error handling ---
  if (!is.numeric(interval) || interval %% 1 != 0 || interval < 1 || interval > 59) {
    stop("`interval` must be an integer between 1 and 59 (unit is minutes).")
  }

  # --- Call dst_switch_day ---
  dst_df <- dst_switch_day(years = years, tz = tz)

  if (nrow(dst_df) == 0) {
    dst_df$spring_time <- as.POSIXct(character(0))
    dst_df$fall_time <- as.POSIXct(character(0))
    return(dst_df)
  }

  # --- Internal helper for safe DST time detection ---
  get_dst_time_safe <- function(date) {
    tryCatch({
      result <- dst_switch_hm(date, tz = tz, interval = interval)
      if (is.null(result)) NA else as.character(result)
    }, error = function(e) {
      warning(paste0("Error detecting DST time for ", date, ": ", e$message))
      return(NA)
    })
  }

  # --- Apply to each row ---
  dst_df$spring_time <- sapply(dst_df$spring, get_dst_time_safe)
  dst_df$fall_time   <- sapply(dst_df$fall,   get_dst_time_safe)
  dst_df$spring_time <- lubridate::ymd_hms(dst_df$spring_time, tz=tz)
  dst_df$fall_time   <- lubridate::ymd_hms(dst_df$fall_time, tz=tz)

  return(dst_df)
}

#' Detect Daylight Saving Time (DST) Change Dates
#'
#' Returns a data frame with the start ("spring") and end ("fall") dates of Daylight Saving Time for each year in the input range.
#'
#' @inheritParams get_dst_switch_info

#' @return A data frame with columns: year, spring (DST start for global north), fall (DST end for global south), spring_next_day (the second day after daylight saving changes in spring), fall_next_day (the second day after daylight saving changes in fall).
#' If no DST transitions are found for the specified years and time zone, an empty data frame is returned
#' with the expected column names.
#'
#' @details
#' If `years` is not numeric, has zero length, <= 1907 (start year of daylight saving changes in history) or if `tz` is not a recognized time zone name,
#' the function will throw an error. For regions without DST (e.g., `Etc/UTC`), the function returns an empty data frame and issues a warning.
#'
#' @export
#'
#' @examples
#' dst_switch_day(years = c(2020, 2021), tz = "America/Vancouver")
#' # Returns a data frame like:
#' #   year    spring      fall   spring_next_day  fall_next_day
#' #   2020 2020-03-08 2020-11-01   2020-03-09      2020-11-02
#' #   2021 2021-03-14 2021-11-07   2021-03-15      2021-11-08
dst_switch_day <- function(years = c(2020, 2021), tz = Sys.timezone()){
  # --- Error handling ---
  if (!is.numeric(years)) {
    stop("`years` must be a numeric vector.")
  }
  if (length(years) < 1) {
    stop("`years` must contain at least one year.")
  }
  if (any(years < 1908 | years > 9999)) {
    stop("All values in `years` must be four-digit numbers greater than 1907.")
  }
  if (!tz %in% OlsonNames()) {
    stop("`tz` must be a valid time zone name. See OlsonNames() for options.")
  }

  # --- Core logic ---
  start_date <- paste0(as.character(min(years)), "-01-01")
  end_date <- paste0(as.character(max(years)), "-12-31")
  date_seq <- seq(lubridate::ymd(start_date),
                  lubridate::ymd(end_date),
                  by = "1 day")
  date_seq_tz <- lubridate::force_tz(date_seq, tz = tz)
  dst_status <- lubridate::dst(date_seq_tz)
  dst_change <- date_seq[which(diff(dst_status) != 0)]

  if (length(dst_change) == 0) {
    warning("No DST transitions found for the given years and time zone.")
    return(data.frame(
      year = integer(0),
      spring = as.Date(character(0)),
      fall = as.Date(character(0)),
      spring_next_day = as.Date(character(0)),
      fall_next_day = as.Date(character(0))
    ))
  }

  dst_df <- data.frame(date = dst_change) |>
    dplyr::mutate(year = lubridate::year(date),
                  season = dplyr::if_else((lubridate::month(date)<9), "spring", "fall")) |>
    tidyr::pivot_wider(names_from = season,
                       values_from = date) |>
    dplyr::mutate(spring_next_day = (spring + lubridate::days(1)),
                  fall_next_day = (fall + lubridate::days(1)))


  return(dst_df)
}

#' Detect the Exact Time (hour and minute; hm) of Daylight Saving Time (DST) Change
#'
#' Identifies the precise local time when a Daylight Saving Time (DST) transition occurs
#' on a given date in a specified time zone.
#'
#' @inheritParams get_dst_switch_info
#' @param date A string in "YYYY-MM-DD" format or a Date object.
#' @param interval An integer specifying the time resolution in minutes (default is 1). Must be between 1 and 59.
#'
#' @return A POSIXct object indicating the time just before the DST transition.
#' If no DST change is detected on the specified date, the function returns `NULL` with a warning.
#'
#' @details
#' The function detects DST changes using differences in DST status across time intervals during the given date.
#' It handles "skipped" or "duplicated" times (as occurs during spring and fall transitions) using both `dst()` and `NA` time gaps.
#' In the case of ambiguous or unresolvable transitions, `NULL` is returned with a warning.
#'
#' @examples
#' dst_switch_hm("2020-03-08", tz = "America/Vancouver")
#' dst_switch_hm("2020-11-01", tz = "America/Vancouver")
#' dst_switch_hm("2020-07-01", tz = "Etc/UTC")  # No DST change
#'
#' @export
dst_switch_hm <- function(date, tz = Sys.timezone(), interval = 1) {
  # --- Error handling ---
  if (!inherits(date, "Date") && !is.character(date)) {
    stop("`date` should be either a string object or a Date object.")
  }
  if (!inherits(date, "Date")) {
    date <- tryCatch(
      lubridate::ymd(date),
      warning = function(w) {
        if (grepl("All formats failed to parse. No formats found.", w$message)) {
          stop("`date` must be a valid Date object or string in 'YYYY-MM-DD' format.")
        } else {
          warning(w)  # pass through other warnings
        }
      },
      error = function(e) {
        stop("`date` must be a valid Date object or string in 'YYYY-MM-DD' format.")
      }
    )
  }
  if (!tz %in% OlsonNames()) {
    stop("`tz` must be a valid time zone name. See OlsonNames() for options.")
  }
  if (!is.numeric(interval) || interval %% 1 != 0 || interval < 1 || interval > 59) {
    stop("`interval` must be an integer between 1 and 59 (unit is minutes).")
  }

  # --- Convert date and generate time sequence ---
  interval = as.integer(interval)
  date <- lubridate::ymd(date)
  times <- seq(
    from = lubridate::ymd_hms(paste0(date, " 00:00:00")),
    to   = lubridate::ymd_hms(paste0(date, " 23:59:59")),
    by   = paste0(interval, " min")
  )

  times_tz <- lubridate::force_tz(times, tzone = tz)
  dst_status <- lubridate::dst(times_tz)

  transition_index <- which(diff(dst_status) != 0)
  total_na <- sum(is.na(times_tz))

  # if there is no transition, and no NA (NA are introduced when there is time jump, like daylight saving time jumped from 2am to 3 am directly)
  if ((length(transition_index) == 0) && total_na == 0) {
    warning("No DST transition detected on this date in the specified time zone.")
    return(NULL)

    # if there is transition
  } else if(length(transition_index) > 0) {
    # Return the last time before the DST change
    return(times_tz[transition_index])

    # if there is no transition but there is NA
  } else {
    na_index <- which(is.na(times_tz))[1]
    if (!is.na(na_index) && na_index > 1) {
      return(times_tz[na_index - 1])
    } else {
      warning("DST transition may occur at the first time point or is ambiguous.")
      return(NULL)
    }

  }


}

#' Adjust Time Stamp for Daylight Saving Time (DST) Transitions
#'
#' This function adjusts a dataframe of visit events (e.g., feed or water visits) based on
#' Daylight Saving Time (DST) changes. It detects whether the current date is a DST transition day
#' (in spring or fall), or the day after the spring DST change, and applies the appropriate time correction logic.
#'
#' The adjustment logic for each case is as follows:
#'
#' \strong{Fall DST Change Day (e.g., 1st Sunday of November):}
#' \itemize{
#'   \item North American clocks fall back from 2:00 AM to 1:00 AM, repeating the hour from 1:00 to 2:00 AM.
#'   \item Some sensors do not repeat the 1–2 AM hour but instead continue recording 2–3 AM as if time never changed.
#'   \item This function:
#'     \itemize{
#'       \item Removes visits that occur in the ambiguous hour (2:00 AM–3:00 AM, as recorded).
#'       \item Shifts all visits after 3:00 AM back by 1 hour to align with the post-fallback clock time.
#'     }
#' }
#'
#' \strong{Spring DST Change Day (e.g., 2nd Sunday of March):}
#' \itemize{
#'   \item North American clocks in the spring will jump forward from 2:00 AM to 3:00 AM, skipping the hour between 2–3 AM.
#'   \item Some sensors continue recording local time, resulting in inconsistent timestamps.
#'   \item This function:
#'     \itemize{
#'       \item Removes visits that start before the DST jump (2:00 AM) and end during the skipped hour (2:00–3:00 AM).
#'       \item Shifts all visits that occur after 2:00 AM forward by 1 hour to align with the new clock time.
#'     }
#' }
#'
#' \strong{Day After Spring DST Change:}
#' \itemize{
#'   \item On the day after spring DST change, sensors may still include late-night entries from the previous (DST) day, due to time "over-spill"
#'   \item This function:
#'     \itemize{
#'       \item Identifies the first time point where time resets (e.g., from 23:00 to 01:00), based on the start time column.
#'       \item Removes all rows before that backward time jump to exclude spillover data from the DST change day.
#'     }
#' }
#'
#' @inheritParams get_dst_switch_info
#' @inheritParams dst_switch_hm
#' @param data_frame A data frame with two columns representing the start and end time of each visit,
#'  formatted as "HH:MM:SS".
#' @param start_col Name of the start time column (quoted), e.g.: start_col = "Start"
#' @param end_col Name of the end time column (quoted). e.g.: end_col = "End"
#' @param dst_df A data frame created by \code{\link{get_dst_switch_info}}, containing DST change dates and times per year.
#' @param daylight_change_duration An integer for the fallback duration in minutes (default = 60).
#'
#' @return A data frame adjusted for daylight saving time, or the original data if no adjustment applies.
#'
#' @examples
#' dst_info <- get_dst_switch_info(years = 2021, tz = "America/Vancouver")
#' df <- data.frame(Start = c("01:30:00", "02:30:00", "04:00:00"),
#'                  End = c("02:00:00", "03:00:00", "04:30:00"))
#' daylight_saving_adjust(df,
#'                       date = "2021-11-07",
#'                       start_col = "Start",
#'                       end_col = "End",
#'                       dst_df = dst_info,
#'                       tz = "America/Vancouver")
#'
#' @export
daylight_saving_adjust <- function(data_frame, date, start_col = "Start", end_col = "End", dst_df, daylight_change_duration=60, tz = Sys.timezone()) {
  # --- Error handling ---
  if (!inherits(date, "Date")) {
    date <- tryCatch(
      lubridate::ymd(date, tz=tz),
      warning = function(w) {
        if (grepl("All formats failed to parse. No formats found.", w$message)) {
          stop("`date` must be a valid Date object or string in 'YYYY-MM-DD' format.")
        } else {
          warning(w)  # pass through other warnings
        }
      },
      error = function(e) {
        stop("`date` must be a valid Date object or string in 'YYYY-MM-DD' format.")
      }
    )
  }
  if ((!is.character(start_col)) || (!is.character(end_col))) {
    stop("`start_col` and `end_col` must be a string indicating column names")
  } else {
    start_col = rlang::sym(start_col)
    end_col = rlang::sym(end_col)
  }
  if (!as.character(start_col) %in% colnames(data_frame)) {
    stop(paste0("Column `", as.character(start_col), "` not found in `data_frame`."))
  }

  if (!as.character(end_col) %in% colnames(data_frame)) {
    stop(paste0("Column `", as.character(end_col), "` not found in `data_frame`."))
  }

  # Extract the relevant DST row for the current year
  dst_row <- dst_df[dst_df$year == lubridate::year(date), ]
  if (nrow(dst_row) == 0) {
    stop("The `dst_df` you provided does not contain daylight saving time information for the year your data was recorded. Please re-run `dst_info <- get_dst_switch_info(years = 2021, tz = XXX)` to generate the appropriate DST reference table.")
  }

  # Fall back: clocks repeat 1–2am (typically early November)
  if (date == lubridate::ymd(dst_row$fall, tz=tz)) {
    data_frame <- adjust_dst_fall(
      data_frame = data_frame,
      dst_detected_time = lubridate::hms(paste(as.character(lubridate::hour(dst_row$fall_time)),
                                as.character(lubridate::minute(dst_row$fall_time)),
                                as.character(lubridate::second(dst_row$fall_time)),
                                sep = ":")),
      start_col = start_col,
      end_col = end_col,
      daylight_change_duration = daylight_change_duration
    )

    # Spring forward: clocks jump 2am → 3am (typically early March)
  } else if (date == lubridate::ymd(dst_row$spring, tz=tz)) {
    data_frame <- adjust_dst_spring(
      data_frame = data_frame,
      dst_detected_time = lubridate::hms(paste(as.character(lubridate::hour(dst_row$spring_time)),
                                       as.character(lubridate::minute(dst_row$spring_time)),
                                       as.character(lubridate::second(dst_row$spring_time)),
                                       sep = ":")),
      start_col = start_col,
      end_col = end_col,
      daylight_change_duration = daylight_change_duration
    )

    # Handle the day *after* DST spring forward — usually messy data at the begining of that dataframe
  } else if (date == lubridate::ymd(dst_row$spring_next_day, tz=tz)) {
    data_frame <- next_day_after_spring_dst(data_frame, start_col = start_col)
  }

  return(data_frame)
}



# -----------------------------------------------------------------------------#
# ------------------------ Internal helper functions --------------------------#
# -----------------------------------------------------------------------------#

#' Adjust visit times for Daylight Saving Time (DST) fallback (fall transition)
#'
#' This function adjusts feed or water visit times that fall during the end of Daylight Saving Time (DST) in the fall.
#' In North America, clocks fall back from 2:00 AM to 1:00 AM, repeating the hour between 1–2 AM.
#' However, some sensors do not reset the time. Instead of repeating the hour, they continue recording
#' as if it were 2:00–3:00 AM, creating duplicate or misaligned timestamps.
#'
#' To correct for this, the function removes visits that occur in the ambiguous period after 2:00 AM,
#' and shifts all visits after 3:00 AM back by one hour, to reflect the actual (post-fallback) clock time.
#'
#' @param data_frame A data frame with two columns representing the start and end time of each visit,
#'  formatted as "HH:MM:SS".
#' @param start_col Name of the start time column (unquoted).
#' @param end_col Name of the end time column (unquoted).
#' @param dst_detected_time A `lubridate::hms` object indicating the time when DST fallback starts (e.g., 01:59:00).
#' @param daylight_change_duration An integer for the fallback duration in minutes (default = 60).
#' @return A data frame with updated start and end time columns after adjusting for DST fallback.
#'
#' @details We used North American DST shift pattern as an example, describing the fallback behavior of jumping from 2:00 AM back to 1:00 AM).
#' This function can also be used for other DST fallback patterns in other countries if appropriate values for `dst_detected_time` and `daylight_change_duration` are provided.
#' @importFrom rlang :=
adjust_dst_fall <- function(data_frame,
                             dst_detected_time,
                             start_col = Start,
                             end_col = End,
                             daylight_change_duration = 60) {

  # --- Error handling ---
  if (!is.data.frame(data_frame)) {
    stop("`data_frame` must be a data frame.")
  }

  if (!as.character(start_col) %in% colnames(data_frame)) {
    stop(paste0("Column `", as.character(start_col), "` not found in `data_frame`."))
  }

  if (!as.character(end_col) %in% colnames(data_frame)) {
    stop(paste0("Column `", as.character(end_col), "` not found in `data_frame`."))
  }

  # Try parsing dst_detected_time into hms if it isn't already
  if (!inherits(dst_detected_time, "hms")) {
    dst_detected_time <- tryCatch({
      lubridate::hms(dst_detected_time)
    }, error = function(e) {
      stop("`dst_detected_time` must be an hms object or a string like '01:59:00'")
    })
  }

  # Check daylight_change_duration is a valid integer between 1 and 60
  if (!is.numeric(daylight_change_duration) ||
      length(daylight_change_duration) != 1 ||
      daylight_change_duration %% 1 != 0 ||
      daylight_change_duration < 1 || daylight_change_duration > 60) {
    stop("`daylight_change_duration` must be a single integer between 1 and 60.")
  }

  # --- Define fallback hour window ---
  fallback_start <- dst_detected_time + lubridate::minutes(daylight_change_duration)
  fallback_end <- fallback_start + lubridate::minutes(daylight_change_duration)

  # --- Main logic: parse time, filter and shift ---
  data_frame <- data_frame |>
    dplyr::mutate(
      {{ start_col }} := lubridate::hms({{ start_col }}),
      {{ end_col }}   := lubridate::hms({{ end_col }})
    ) |>
    dplyr::filter(
      !({{ start_col }} > fallback_start & {{ start_col }} <= fallback_end),
      !({{ end_col }} > fallback_start & {{ end_col }} <= fallback_end)
    ) |>
    dplyr::mutate(
      {{ start_col }} := ifelse(
        {{ start_col }} > fallback_end,
        {{ start_col }} - lubridate::minutes(daylight_change_duration),
        {{ start_col }}
      ),
      {{ end_col }} := ifelse(
        {{ end_col }} > fallback_end,
        {{ end_col }} - lubridate::minutes(daylight_change_duration),
        {{ end_col }}
      )
    )

  return(data_frame)
}

#' Adjust visit times for Daylight Saving Time (DST) in the spring (spring forward)
#'
#' This function adjusts feed or water visit times that fall during the start of Daylight Saving Time (DST) in the spring.
#' In North America, clocks jump forward from 2:00 AM to 3:00 AM, skipping the hour between 2–3 AM.
#' Some sensors continue recording using local time, which may result in invalid or misaligned timestamps.
#'
#' The function removes visits that start before the DST jump and end during the skipped hour (2–3 AM),
#' and shifts all visits that occur after the DST jump forward by one hour to reflect the actual (post-DST) clock time.
#'
#' @inheritParams adjust_dst_fall
#' @return A data frame with updated start and end time columns after adjusting for spring DST jump.
#'
#' @details This function assumes North American spring DST behavior (2:00 AM → 3:00 AM), but can be adapted
#' for other timezones with a suitable `dst_detected_time` and `daylight_change_duration`.
#'
#' @importFrom rlang :=
adjust_dst_spring <- function(data_frame,
                               dst_detected_time,
                               start_col = Start,
                               end_col = End,
                               daylight_change_duration = 60) {
  # --- Error handling ---
  if (!is.data.frame(data_frame)) {
    stop("`data_frame` must be a data frame.")
  }

  if (!as.character(start_col) %in% colnames(data_frame)) {
    stop(paste0("Column `", as.character(start_col), "` not found in `data_frame`."))
  }

  if (!as.character(end_col) %in% colnames(data_frame)) {
    stop(paste0("Column `", as.character(end_col), "` not found in `data_frame`."))
  }

  # Try parsing dst_detected_time into hms if needed
  if (!inherits(dst_detected_time, "hms")) {
    dst_detected_time <- tryCatch({
      lubridate::hms(dst_detected_time)
    }, error = function(e) {
      stop("`dst_detected_time` must be an hms object or a string like '01:59:00'")
    })
  }

  # Check daylight_change_duration is a valid integer between 1 and 60
  if (!is.numeric(daylight_change_duration) ||
      length(daylight_change_duration) != 1 ||
      daylight_change_duration %% 1 != 0 ||
      daylight_change_duration < 1 || daylight_change_duration > 60) {
    stop("`daylight_change_duration` must be a single integer between 1 and 60.")
  }

  spring_gap_start <- dst_detected_time
  spring_gap_end <- dst_detected_time + lubridate::minutes(daylight_change_duration)

  data_frame <- data_frame |>
    dplyr::mutate(
      {{ start_col }} := lubridate::hms({{ start_col }}),
      {{ end_col }}   := lubridate::hms({{ end_col }})
    ) |>
    dplyr::filter(
      !({{ start_col }} <= spring_gap_start & {{ end_col }} > spring_gap_start)
    ) |>
    dplyr::mutate(
      {{ start_col }} := ifelse(
        {{ start_col }} > spring_gap_start,
        {{ start_col }} + lubridate::minutes(daylight_change_duration),
        {{ start_col }}
      ),
      {{ end_col }} := ifelse(
        {{ end_col }} > spring_gap_start,
        {{ end_col }} + lubridate::minutes(daylight_change_duration),
        {{ end_col }}
      )
    )

  return(data_frame)
}

#' Handle Error Reading on the Second Day after Daylight Saving Change in the Spring
#'
#' This internal function removes all rows prior to a time discontinuity caused by a daylight saving time adjustment.
#' Since spring daylight saving change will push the time forward by 1 hour, this causes "time overspill".
#' Ususally on the second day after daylight saving change in the spring, the datasheet starts with before midnight
#' recordings from the previous day, instead of recordings of the current day.
#'
#' This function detects the first instance where the start time goes backward (i.e., the next row's time is earlier than the previous row's time),
#' and removes all rows before that point.
#'
#' @param data_frame A data frame with a time column (formatted as "HH:MM:SS") representing visit start times.
#' @param start_col Name of the time column (unquoted), usually this is the start time of the event
#'
#' @return A filtered data frame with only rows after the DST-induced reset point.
next_day_after_spring_dst <- function(data_frame, start_col = Start) {
  if (!is.data.frame(data_frame)) {
    stop("`data_frame` must be a data frame.")
  }

  if (!as.character(start_col) %in% colnames(data_frame)) {
    stop(paste0("Column `", as.character(start_col), "` not found in `data_frame`."))
  }

  time_vals <- lubridate::hms(dplyr::pull(data_frame, {{ start_col }}))

  # diff will be negative where time goes backward
  backward_jump_idx <- which(diff(time_vals) < 0)[1]

  if (is.na(backward_jump_idx)) {
    return(data_frame)
  }

  return(data_frame[(backward_jump_idx + 1):nrow(data_frame), ])
}

