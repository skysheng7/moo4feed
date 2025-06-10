#' Process meal clustering for a single dataframe
#'
#' @param df Single dataframe with feeding visit data
#' @inheritParams cluster_meals
#'
#' @return Dataframe with meal summaries
#' @keywords internal
#' @noRd
cluster_meals_single_df <- function(df, eps, min_pts, id_col, start_col, 
                                   end_col, bin_col, intake_col, dur_col) {
  
  # Validate required columns
  required_cols <- c(id_col, start_col, end_col, bin_col, intake_col, dur_col)
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Add date column if not present
  if (!"date" %in% names(df)) {
    df$date <- lubridate::date(df[[start_col]])
  }
  
  # Group by cow and date, then process each group
  results <- df |>
    dplyr::group_by(.data[[id_col]], date) |>
    dplyr::group_split() |>
    lapply(function(group_df) {
      cluster_meals_cow_day(group_df, eps, min_pts, id_col, start_col, 
                           end_col, bin_col, intake_col, dur_col)
    })
  
  # Combine results
  if (length(results) > 0) {
    result <- do.call(rbind, results)
    rownames(result) <- NULL
    return(result)
  } else {
    # Return empty dataframe with correct structure
    return(create_empty_meal_df(id_col))
  }
}