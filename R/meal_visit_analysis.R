#' Analyze Non-Nutritive and Empty Bin Visits Within Meals
#'
#' @description
#' Analyzes visits within meals to identify and summarize non-nutritive visits
#' (no feed intake but feed available) and empty bin visits (no feed available).
#' Input data must already have meal labels from [meal_label_visits()].
#'
#' @param data A named list of daily data frames or a single data frame containing
#'   visit records with meal labels. Must be output from [meal_label_visits()],
#'   which adds the required `meal_id` and `meal_start` columns.
#' @param cfg A configuration list created by [qc_config()]. Used to determine
#'   calibration error threshold for classifying visits.
#' @inheritParams set_global_cols
#'
#' @return A data frame (or named list of data frames) with columns:
#'   \itemize{
#'     \item `date` - Date
#'     \item `[id_col]` - Animal identifier
#'     \item `mean_non_nutritive_per_meal` - Average non-nutritive visits per meal
#'     \item `median_non_nutritive_per_meal` - Median non-nutritive visits per meal
#'     \item `sd_non_nutritive_per_meal` - Standard deviation of non-nutritive visits
#'     \item `mean_empty_bin_per_meal` - Average empty bin visits per meal
#'     \item `median_empty_bin_per_meal` - Median empty bin visits per meal
#'     \item `sd_empty_bin_per_meal` - Standard deviation of empty bin visits
#'     \item `total_non_nutritive_visits` - Total non-nutritive visits
#'     \item `total_empty_bin_visits` - Total empty bin visits
#'     \item `total_meals` - Total number of meals
#'   }
#'
#' @examples
#' # Create sample visit data with meal labels
#' visits <- data.frame(
#'   date = "2024-01-01",
#'   cow = c("A", "A", "A", "B", "B"),
#'   bin = c(1, 2, 3, 1, 2),
#'   start_weight = c(50, 0.01, 45, 48, 0.02),
#'   intake = c(2.5, 0.01, 0.02, 3.0, 0.01),
#'   meal_id = c(1, 1, 1, 1, 1),
#'   meal_start = as.POSIXct("2024-01-01 08:00:00", tz = "UTC")
#' )
#'
#' # Analyze non-nutritive and empty bin visits
#' meal_visits <- meal_non_nutritive_summary(
#'   data = visits,
#'   cfg = qc_config()
#' )
#'
#' @export
meal_non_nutritive_summary <- function(data,
                                       cfg = qc_config(),
                                       id_col = id_col2(),
                                       intake_col = intake_col2(),
                                       start_weight_col = start_weight_col2()) {

  # Validate inputs
  .validate_meal_labeled_data(data)

  # Determine if input is a list or single dataframe
  is_list <- is.list(data) && !is.data.frame(data)

  if (!is_list) {
    day_name <- if ("date" %in% names(data) && nrow(data) > 0 && !is.na(data$date[1])) {
      as.character(data$date[1])
    } else {
      "day1"
    }
    data <- list(data)
    names(data) <- day_name
  }

  # Get calibration error from config
  calibration_error <- cfg$calibration_error

  # Process each day
  result <- lapply(names(data), function(day_name) {
    df <- data[[day_name]]

    # Extract date from data if available
    date_val <- if ("date" %in% names(df)) df$date[1] else day_name

    # Check required columns
    required_cols <- c(id_col, intake_col, start_weight_col, "meal_id", "meal_start")
    missing_cols <- setdiff(required_cols, names(df))
    if (length(missing_cols) > 0) {
      stop("Missing required columns: ", paste(missing_cols, collapse = ", "),
           ". Data must be labeled with meal_label_visits() first.", call. = FALSE)
    }

    # Filter out outlier visits (meal_id == 0)
    df <- df |>
      dplyr::filter(.data$meal_id > 0)

    # If no valid meals, return empty structure
    if (nrow(df) == 0) {
      return(.empty_meal_visit_summary(date_val, id_col))
    }

    # Classify each visit
    df <- df |>
      dplyr::mutate(
        visit_type = .classify_visit_type(
          .data[[intake_col]],
          .data[[start_weight_col]],
          calibration_error
        )
      )

    # Summarize per meal
    meal_summary <- df |>
      dplyr::group_by(.data[[id_col]], .data$meal_id) |>
      dplyr::summarise(
        non_nutritive_count = sum(.data$visit_type == "non_nutritive"),
        empty_bin_count = sum(.data$visit_type == "empty_bin"),
        .groups = "drop"
      )

    # If no meals, return empty
    if (nrow(meal_summary) == 0) {
      return(.empty_meal_visit_summary(date_val, id_col))
    }

    # Summarize per animal per day
    daily_summary <- meal_summary |>
      dplyr::group_by(.data[[id_col]]) |>
      dplyr::summarise(
        mean_non_nutritive_per_meal = mean(.data$non_nutritive_count),
        median_non_nutritive_per_meal = stats::median(.data$non_nutritive_count),
        sd_non_nutritive_per_meal = stats::sd(.data$non_nutritive_count),
        mean_empty_bin_per_meal = mean(.data$empty_bin_count),
        median_empty_bin_per_meal = stats::median(.data$empty_bin_count),
        sd_empty_bin_per_meal = stats::sd(.data$empty_bin_count),
        total_non_nutritive_visits = sum(.data$non_nutritive_count),
        total_empty_bin_visits = sum(.data$empty_bin_count),
        total_meals = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::mutate(date = date_val) |>
      dplyr::select("date", dplyr::everything())

    return(as.data.frame(daily_summary))
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


#' Classify Visit Type
#'
#' @description
#' Internal function to classify each visit as non-nutritive, empty bin, or nutritive.
#'
#' @keywords internal
#' @noRd
.classify_visit_type <- function(intake, start_weight, calibration_error) {
  dplyr::case_when(
    intake <= calibration_error & start_weight > calibration_error ~ "non_nutritive",
    intake <= calibration_error & start_weight <= calibration_error ~ "empty_bin",
    TRUE ~ "nutritive"
  )
}


#' Create Empty Meal Visit Summary
#'
#' @description
#' Internal function to create an empty result dataframe with proper structure.
#'
#' @keywords internal
#' @noRd
.empty_meal_visit_summary <- function(date_val, id_col) {
  result <- data.frame(
    date = character(0),
    mean_non_nutritive_per_meal = numeric(0),
    median_non_nutritive_per_meal = numeric(0),
    sd_non_nutritive_per_meal = numeric(0),
    mean_empty_bin_per_meal = numeric(0),
    median_empty_bin_per_meal = numeric(0),
    sd_empty_bin_per_meal = numeric(0),
    total_non_nutritive_visits = integer(0),
    total_empty_bin_visits = integer(0),
    total_meals = integer(0),
    stringsAsFactors = FALSE
  )
  # Add id_col as second column
  result[[id_col]] <- character(0)
  result <- result |>
    dplyr::select("date", dplyr::all_of(id_col), dplyr::everything())
  return(result)
}


#' Validate Meal Labeled Data
#'
#' @description
#' Internal function to validate that input data has meal labels.
#'
#' @keywords internal
#' @noRd
.validate_meal_labeled_data <- function(data) {
  if (is.null(data)) {
    stop("data cannot be NULL", call. = FALSE)
  }

  # Check if data is list or single df
  check_df <- if (is.data.frame(data)) {
    data
  } else if (is.list(data) && length(data) > 0) {
    data[[1]]
  } else {
    stop("data must be a data frame or named list of data frames", call. = FALSE)
  }

  # Check for meal columns
  meal_cols <- c("meal_id", "meal_start")
  missing_meal_cols <- setdiff(meal_cols, names(check_df))

  if (length(missing_meal_cols) > 0) {
    stop("Data must have meal labels. Run meal_label_visits() first. ",
         "Missing columns: ", paste(missing_meal_cols, collapse = ", "),
         call. = FALSE)
  }
}
