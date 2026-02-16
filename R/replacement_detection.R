# -----------------------------------------------------------------------------#
# -------------------- External user-facing functions -------------------------#
# -----------------------------------------------------------------------------#

#' Identify and validate replacement events across multiple days
#'
#' @description
#' This function detects potential replacement behaviors in feeding or drinking
#' bins across multiple days of data, and validates them by checking if actor cows
#' were simultaneously engaged elsewhere (i.e., have an alibi). A replacement is
#' defined as instances where the time interval between one cow leaving and the next
#' entering a bin is < 26 seconds.
#'
#' @inheritParams qc
#' @param comb      List of daily data frames (feed, water or combined).
#'
#' @return A named list of data frames, one per day, containing validated replacement events.
#'
#' @details
#' This function first calls internal function `record_replacement_day()` and applies it across all
#' elements of `comb` to identify replacements. It then calls internal function `check_alibi_days()`
#' to validate those events by removing actor cows with an alibi.
#'
#' @examples
#' # Use example data from the built-in all_fed dataset
#' valid_replacements <- record_replacement_days(all_fed)
#' head(valid_replacements[[1]])
#'
#' @export
record_replacement_days <- function(comb,
                                    cfg = qc_config(),
                                    id_col = id_col2(),
                                    bin_col = bin_col2(),
                                    start_col = start_col2(),
                                    end_col = end_col2()) {
  # ------------------------ Error handling ------------------------ #
  if (!is.list(comb)) {
    stop("`comb` must be a named list of data frames.")
  }

  # ------------------------ Main logic ---------------------------- #
  replacement_list_by_date <- lapply(names(comb), function(name) {
    record_replacement_day(comb[[name]], cfg = cfg, id_col = id_col, bin_col = bin_col, start_col = start_col, end_col = end_col)
  })
  names(replacement_list_by_date) <- names(comb)

  validated <- check_alibi_days(replacement_list_by_date, comb, id_col = id_col, start_col = start_col, end_col = end_col)
  return(validated)
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
#' Must include columns specified by [id_col], [bin_col], [start_col], and [end_col].
#' @param cfg Configuration list (default: qc_config()) containing replacement_threshold.
#' @param id_col,bin_col,start_col,end_col Column names to use (default: from global getters).
#'
#' @return A data frame of detected replacement events filtered by bins, with columns:
#' `reactor_cow`, `bin`, `time`, `date`, `actor_cow`, `bout_interval`.
#'
#' @details
#' The function iterates over each bin and computes the time interval between
#' cows leaving and entering. If the interval is below the threshold and involves
#' different cows, a replacement is recorded.
#'
#' @seealso [record_replacement_days()]
#'
#' @keywords internal
#' @noRd
record_replacement_day <- function(cur_data,
                                   cfg = qc_config(),
                                   id_col = id_col2(),
                                   bin_col = bin_col2(),
                                   start_col = start_col2(),
                                   end_col = end_col2()) {
  replacement_threshold <- cfg$replacement_threshold

  # ------------------------ Error handling ------------------------ #
  required_cols <- c(id_col, bin_col, start_col, end_col)
  if (!all(required_cols %in% names(cur_data))) {
    stop(paste("`cur_data` must include columns:", paste(required_cols, collapse = ", ")))
  }

  # ------------------------ Main logic ---------------------------- #
  sorted_data <- cur_data |>
    dplyr::arrange(!!rlang::sym(bin_col), !!rlang::sym(start_col), !!rlang::sym(end_col)) |>
    dplyr::select(!!rlang::sym(id_col), !!rlang::sym(bin_col), 
                 !!rlang::sym(start_col), !!rlang::sym(end_col))
  
  sorted_data$date <- lubridate::date(sorted_data[[start_col]])

  bin_list <- sort(unique(sorted_data[[bin_col]]))
  master_df <- data.frame()

  for (cur_bin in bin_list) {
    cur_data_bin <- sorted_data |>
      dplyr::filter(!!rlang::sym(bin_col) == cur_bin)

    next_start_list <- cur_data_bin[[start_col]][-1]
    next_cow_list <- cur_data_bin[[id_col]][-1]
    cur_data_bin <- cur_data_bin[-nrow(cur_data_bin), ]

    cur_data_bin$next_start <- next_start_list
    cur_data_bin$next_cow <- next_cow_list

    time_interval <- lubridate::interval(cur_data_bin[[end_col]], cur_data_bin$next_start)
    cur_data_bin$time_dif <- lubridate::as.duration(time_interval)

    replace_cutoff <- lubridate::as.duration(paste0(replacement_threshold, "s"))
    
    cur_data_bin <- cur_data_bin |>
      dplyr::filter(time_dif <= replace_cutoff, 
                   !!rlang::sym(id_col) != next_cow) |>
      dplyr::select(!!rlang::sym(id_col), !!rlang::sym(bin_col), 
                   !!rlang::sym(end_col), date, next_cow, time_dif)
    
    colnames(cur_data_bin) <- c("reactor_cow", "bin", "time", "date", "actor_cow", "bout_interval")

    master_df <- rbind(master_df, cur_data_bin)
  }

  return(master_df)
}

#' Validate replacements by checking cow alibis across days
#'
#' @description
#' This function verifies validity of detected replacement events across multiple days by
#' checking if the actor cow was simultaneously active at another bin (i.e., have an alibi).
#'
#' @inheritParams qc_warning_skeleton
#' @param replacement_list_by_date A list of data frames, one per day, containing replacement events.
#' @param id_col,start_col,end_col Column names to use (default: from global getters).
#'
#' @return A list of data frames with valid replacements.
#'
#' @details This function wraps `check_alibi_day()` and applies it across all elements of
#' `replacement_list_by_date`. A replacement is removed if the actor cow is found to be engaged
#' in another feeding or drinking event at the replacement timestamp.
#'
#' @seealso [record_replacement_days()]
#'
#' @keywords internal
#' @noRd
check_alibi_days <- function(replacement_list_by_date, comb,
                             id_col = id_col2(),
                             start_col = start_col2(),
                             end_col = end_col2()) {
  # ------------------------ Error handling ------------------------ #
  if (length(replacement_list_by_date) == 0) {
    return(list())
  }
  if (length(replacement_list_by_date) != length(comb)) {
    stop("replacement_list_by_date and comb must be the same length.")
  }

  # ------------------------ Main logic ---------------------------- #
  out <- lapply(seq_along(comb), function(i) {
    check_alibi_day(replacement_list_by_date[[i]], comb[[i]], id_col = id_col, start_col = start_col, end_col = end_col)
  })
  names(out) <- names(comb)
  return(out)
}

#' Filter invalid replacements based on cow alibi on a single day
#'
#' @description
#' Internal helper for `check_alibi_days()`. For a single day's data, this function filters out replacement events
#' (from `record_replacement_day()`) where the actor cow was simultaneously engaged in another feeding or drinking
#' event (i.e., has an alibi).
#'
#' @param cur_replacement A data frame of replacement events for one day, as returned by `record_replacement_day()`.
#' @param cur_feed_wat A data frame of feeding/drinking events for the same day.
#' @param id_col,start_col,end_col Column names to use (default: from global getters).
#'
#' @return A filtered data frame of valid replacements (i.e., actor cows had no alibi).
#'
#' @details
#' The actor cow is said to have an alibi if they were recorded in another
#' feeding/drinking session that overlaps with the replacement timestamp.
#'
#' @seealso [check_alibi_days()]
#'
#' @keywords internal
#' @noRd
check_alibi_day <- function(cur_replacement, cur_feed_wat,
                            id_col = id_col2(),
                            start_col = start_col2(),
                            end_col = end_col2()) {

  # ------------------------ Error handling ------------------------ #
  if (nrow(cur_replacement) == 0) {
    return(cur_replacement)
  }
  if (!all(c("actor_cow", "time") %in% names(cur_replacement))) {
    stop("`cur_replacement` must include actor_cow and time columns.")
  }

  # ------------------------ Main logic ---------------------------- #
  cur_replacement$actor_at_another_bin <- 0
  for (k in seq_len(nrow(cur_replacement))) {
    cur_time <- cur_replacement$time[k]  
    cur_actor <- cur_replacement$actor_cow[k]  
    
    overlapping <- cur_feed_wat |>
      dplyr::filter(!!rlang::sym(id_col) == cur_actor,
                   !!rlang::sym(start_col) <= cur_time,
                   !!rlang::sym(end_col) >= cur_time)
    
    if (nrow(overlapping) > 0) {
      cur_replacement$actor_at_another_bin[k] <- 1
    }
  }

  result <- cur_replacement |>
    dplyr::filter(actor_at_another_bin == 0) |>
    dplyr::select(-actor_at_another_bin)
    
  return(result)
}
