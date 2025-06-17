#' Calculate non-nutritive visits per animal per day
#'
#' @description
#' Calculates the number of non-nutritive visits for each animal on each day.
#' A non-nutritive visit occurs when an animal visits a bin that has feed
#' available
#' (more than the calibration error) but does not eat anything.
#'
#' @param data A named list of daily data frames (one per day),
#' each containing visit-level data.
#' @param cfg A configuration list created by [qc_config()].
#' @inheritParams set_global_cols
#' @inheritParams qc_config
#'
#' @return A named list of data frames, one per day, each containing columns
#' for animal ID and the count of non-nutritive visits.
#'
#' @examples
#' toy_data <- list(
#'   "2023-01-01" = data.frame(
#'     cow = c("1", "2", "3", "1"),
#'     intake = c(0.2, 0.3, 0.4, 0.0),
#'     start_weight = c(10, 15, 20, 12)
#'   )
#' )
#' cfg <- qc_config(calibration_error = 0.5)
#' result <- calculate_non_nutritive_visits(toy_data, cfg = cfg)
#' result[[1]]
#'
#' @export
calculate_non_nutritive_visits <- function(
  data,
  cfg = qc_config(),
  id_col = id_col2(),
  intake_col = intake_col2(),
  start_weight_col = start_weight_col2()
) {
  calibration_error <- cfg$calibration_error
  # Input validation
  if (!is.list(data) || length(data) == 0 || !all(sapply(data, is.data.frame))) {
    stop("`data` must be a non-empty list of data frames.")
  }
  .validate_calibration_error(calibration_error)

  result <- lapply(data, function(df) {
    .validate_daily_data(df, id_col, intake_col, start_weight_col)
    dplyr::as_tibble(df) |>
      dplyr::filter(.data[[intake_col]] <= calibration_error,
                    .data[[start_weight_col]] > calibration_error) |>
      dplyr::count(.data[[id_col]], name = "number_of_non_nutritive_visits")
  })
  names(result) <- names(data)
  return(result)
}

#' Calculate visits with no feed available per animal per day
#'
#' @description
#' Calculates the number of visits for each animal on each day
#' when there was no feed available (start weight less than or equal to
#' calibration error and intake less than or equal to calibration error).
#'
#' @param data A named list of daily data frames (one per day), each containing visit-level data.
#' @param cfg A configuration list created by [qc_config()].
#' @inheritParams set_global_cols
#' @inheritParams qc_config
#'
#' @return A named list of data frames, one per day, each containing columns
#' for animal ID and the count of visits with no feed available.
#'
#' @examples
#' toy_data <- list(
#'   "2023-01-01" = data.frame(
#'     cow = c("1", "2", "3", "1"),
#'     intake = c(0.2, 0.3, 0.4, 0.0),
#'     start_weight = c(0.3, 0.2, 0.1, 0.4)
#'   )
#' )
#' cfg <- qc_config(calibration_error = 0.5)
#' result <- calculate_no_feed_visits(toy_data, cfg = cfg)
#' result[[1]]
#'
#' @export
calculate_no_feed_visits <- function(
  data,
  cfg = qc_config(),
  id_col = id_col2(),
  intake_col = intake_col2(),
  start_weight_col = start_weight_col2()
) {
  calibration_error <- cfg$calibration_error
  # Input validation
  if (!is.list(data) || length(data) == 0 || !all(sapply(data, is.data.frame))) {
    stop("`data` must be a non-empty list of data frames.")
  }
  .validate_calibration_error(calibration_error)

  result <- lapply(data, function(df) {
    .validate_daily_data(df, id_col, intake_col, start_weight_col)
    dplyr::as_tibble(df) |>
      dplyr::filter(.data[[intake_col]] <= calibration_error,
                    .data[[start_weight_col]] <= calibration_error) |>
      dplyr::count(.data[[id_col]], name = "number_of_visits_when_no_feed")
  })
  names(result) <- names(data)
  return(result)
}

# Internal helper functions ----------------------------------------------------

#' Validate calibration_error parameter
#'
#' @description
#' Checks that the calibration_error parameter is a single positive numeric value and not NA.
#'
#' @param calibration_error Numeric. The calibration error threshold to validate.
#'
#' @return None. Throws an error if validation fails.
#' @keywords internal
#' @noRd
.validate_calibration_error <- function(calibration_error) {
  if (!is.numeric(calibration_error) || length(calibration_error) != 1 || 
      calibration_error <= 0 || is.na(calibration_error)) {
    stop("`calibration_error` must be a positive numeric scalar", call. = FALSE)
  }
}

#' Validate required columns in daily data
#'
#' @description
#' Checks that the required columns are present in the input data frame for non-nutritive visit calculations.
#'
#' @param df A data frame to check for required columns.
#' @param id_col Character. Name of the animal ID column.
#' @param intake_col Character. Name of the intake column.
#' @param start_weight_col Character. Name of the start weight column.
#'
#' @return None. Throws an error if any required columns are missing.
#' @keywords internal
#' @noRd
.validate_daily_data <- function(df, id_col, intake_col, start_weight_col) {
  required_cols <- c(id_col, intake_col, start_weight_col)
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", 
         paste(missing_cols, collapse = ", "), 
         call. = FALSE)
  }
}