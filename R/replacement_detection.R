# -----------------------------------------------------------------------------#
# -------------------- External user-facing functions -------------------------#
# -----------------------------------------------------------------------------#

#' Identify and record replacement events across multiple days
#'
#' @description
#' This function detects potential replacement behaviors in feeding or drinking
#' bins across multiple days of data. A replacement is defined as instances where the time interval
#' between one cow leaving and the next entering a bin is < 26 seconds.
#'
#' @param data_list A named list of data frames, each representing one day's worth
#'   of feeding or drinking data.
#' @param replacement_threshold Time threshold (in seconds) between a cow leaving
#'   and the next cow entering to be considered a replacement.
#'
#' @return A named list of data frames, one per day, containing replacement events.
#'
#' @details
#' This function wraps `record_replacement_day()` and applies it across all
#' elements of `data_list`. It ensures consistent column formatting and filtering.
#'
#' @seealso [check_alibi_days()]
#'
#' @examples
#' # Use example data from the built-in all_fed dataset
#' replacements <- record_replacement_days(all_fed)
#'
#' @export
record_replacement_days <- function(data_list,
                                    replacement_threshold = 26) {
  # ------------------------ Error handling ------------------------ #
  if (!is.list(data_list)) {
    stop("`data_list` must be a named list of data frames.")
  }

  # ------------------------ Main logic ---------------------------- #
  replacement_list_by_date <- lapply(names(data_list), function(name) {
    record_replacement_day(data_list[[name]], replacement_threshold)
  })
  names(replacement_list_by_date) <- names(data_list)
  return(replacement_list_by_date)
}

#' Validate replacements by checking cow alibis across days
#'
#' @description
#' This function verifies validity of detected replacement events across multiple days by
#' checking if the actor cow was simultaneously active at another bin (i.e., have an alibi).
#'
#' @param replacement_list_by_date A list of data frames, one per day, containing replacement events.
#' @param all_comb2 A list of data frames, one per day, containing feeding and drinking data.
#'
#' @return A list of data frames with valid replacements.
#'
#' @details This function wraps `check_alibi_day()` and applies it across all elements of
#' `replacement_list_by_date`. A replacement is removed if the actor cow is found to be engaged
#' in another feeding or drinking event at the replacement timestamp.
#'
#' @seealso [record_replacement_days()]
#'
#' @examples
#' # Use example data from the built-in all_fed dataset
#' replacements <- record_replacement_days(all_fed)
#' valid_replacements <- check_alibi_days(replacements, all_fed)
#'
#' @export
check_alibi_days <- function(replacement_list_by_date, all_comb2) {
  # ------------------------ Error handling ------------------------ #
  if (length(replacement_list_by_date) == 0) return(list())
  if (length(replacement_list_by_date) != length(all_comb2)) {
    stop("replacement_list_by_date and all_comb2 must be the same length.")
  }

  # ------------------------ Main logic ---------------------------- #
  out <- lapply(seq_along(all_comb2), function(i) {
    check_alibi_day(replacement_list_by_date[[i]], all_comb2[[i]])
  })
  names(out) <- names(all_comb2)
  return(out)
}

# -----------------------------------------------------------------------------#
# ------------------------ Internal helper functions --------------------------#
# -----------------------------------------------------------------------------#

#' Identify and record replacement events for a single day
#'
#' @description
#' Internal function to identify replacement behavior within a single day of data.
#' Replacements are defined as instances where the time interval between one cow
#' leaving and the next entering a bin is < 26 seconds.
#'
#' @param cur_data A data frame containing feeding/drinking events for a single day.
#' Must include columns: `Cow`, `Bin`, `Start`, `End`.
#' @param replacement_threshold Numeric time threshold (in seconds) for defining replacement.
#'
#' @return A data frame of detected replacement events filtered by bins, with columns:
#' `Reactor_cow`, `Bin`, `Time`, `date`, `Actor_cow`, `Bout_interval`.
#'
#' @details
#' The function iterates over each bin and computes the time interval between
#' cows leaving and entering. If the interval is below the threshold and involves
#' different cows, a replacement is recorded.
#'
#' @seealso [record_replacement_days()]
#'
#' @noRd
record_replacement_day <- function(cur_data,
                                   replacement_threshold = 26) {
  id_col <- id_col2()
  bin_col <- bin_col2()
  start_col <- start_col2()
  end_col <- end_col2()

  # ------------------------ Error handling ------------------------ #
  required_cols <- c(id_col2(), bin_col2(), start_col2(), end_col2())
  if (!all(required_cols %in% names(cur_data))) {
    stop("`cur_data` must include id, bin, start, and end columns.")
  }

  # ------------------------ Main logic ---------------------------- #
  sorted_data <- cur_data[order(cur_data[[bin_col]], cur_data[[start_col]], cur_data[[end_col]]), ]
  sorted_data <- sorted_data[, c(id_col, bin_col, start_col, end_col)]
  sorted_data$date <- lubridate::date(sorted_data[[start_col]])

  bin_list <- sort(unique(sorted_data[[bin_col]]))
  master_df <- data.frame()

  for (cur_bin in bin_list) {
    cur_data_bin <- sorted_data[sorted_data[[bin_col]] == cur_bin, ]

    next_start_list <- cur_data_bin[[start_col]][-1]
    next_cow_list <- cur_data_bin[[id_col]][-1]
    cur_data_bin <- cur_data_bin[-nrow(cur_data_bin), ]

    cur_data_bin$next_start <- next_start_list
    cur_data_bin$next_cow <- next_cow_list

    time_interval <- lubridate::interval(cur_data_bin[[end_col]], cur_data_bin$next_start)
    cur_data_bin$time_dif <- lubridate::as.duration(time_interval)

    replace_cutoff <- lubridate::as.duration(paste0(replacement_threshold, "s"))
    cur_data_bin <- cur_data_bin[
      cur_data_bin$time_dif <= replace_cutoff &
        cur_data_bin[[id_col]] != cur_data_bin$next_cow,
    ]

    cur_data_bin <- cur_data_bin[, c(id_col, bin_col, end_col, "date", "next_cow", "time_dif")]
    colnames(cur_data_bin) <- c("Reactor_cow", "Bin", "Time", "date", "Actor_cow", "Bout_interval")

    master_df <- rbind(master_df, cur_data_bin)
  }

  return(master_df)
}

#' Filter invalid replacements based on cow alibi on a single day
#'
#' @description
#' This function determines whether actor cows were engaged in another bout at the time of the event.
#' In which case, the replacement is potentially invalid (e.g., the actor cow has an alibi).
#'
#' @param cur_replacement A data frame of replacements for a single day.
#' Must include columns `Actor_cow` and `Time`.
#' @param cur_feed_wat A data frame of feeding/drinking events for that day.
#' Must include `Cow`, `Start`, and `End` columns.
#'
#' @return A filtered data frame of valid replacements (i.e., actor cows had no alibi).
#'
#' @details
#' The actor cow is said to have an alibi if they were recorded in another
#' feeding/drinking session that overlaps with the replacement timestamp.
#'
#' @seealso [check_alibi_days()]
#'
#' @noRd
check_alibi_day <- function(cur_replacement, cur_feed_wat) {
  id_col <- id_col2()
  start_col <- start_col2()
  end_col <- end_col2()

  # ------------------------ Error handling ------------------------ #
  if (nrow(cur_replacement) == 0) {
    return(cur_replacement)
  }
  if (!all(c("Actor_cow", "Time") %in% names(cur_replacement))) {
    stop("`cur_replacement` must include Actor_cow and Time columns.")
  }

  # ------------------------ Main logic ---------------------------- #
  cur_replacement$actor_at_another_bin <- 0
  for (k in seq_len(nrow(cur_replacement))) {
    cur_time <- cur_replacement$Time[k]
    cur_actor <- cur_replacement$Actor_cow[k]
    overlapping <- cur_feed_wat[
      cur_feed_wat[[id_col]] == cur_actor &
        cur_feed_wat[[start_col]] <= cur_time &
        cur_feed_wat[[end_col]] >= cur_time,
    ]
    if (nrow(overlapping) > 0) {
      cur_replacement$actor_at_another_bin[k] <- 1
    }
  }

  result <- cur_replacement[cur_replacement$actor_at_another_bin == 0, ]
  result$actor_at_another_bin <- NULL
  return(result)
}
