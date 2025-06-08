#' Un-merge a combined data frame into a list of data frames by date
#'
#' This function does the opposite of [merge_list_df] by splitting a combined data frame
#' into a list of data frames, with each data frame containing data for a single date.
#' The list elements will be named by the date.
#'
#' @param df A data frame containing a date column.
#' @param date_col Character. Name of the column containing date information (default: "date"). 
#'  Values under this column should be in YYYY-MM-DD format.
#'
#' @return A named list of data frames, with each data frame containing data for a single date.
#'   The names of the list elements are the dates.
#'
#' @examples
#' # create a combined data frame
#' combined_df <- data.frame(   
#'   date = c("2023-01-01", "2023-01-01", "2023-01-02", "2023-01-02"),
#'   cow = c(1, 2, 3, 4),
#'   intake = c(10, 20, 30, 40)
#' )
#' 
#' # Un-merge a combined data frame
#' df_list <- unmerge_data(combined_df)
#' 
#' # Access data for a specific date
#' names(df_list)
#'
#' @export
unmerge_data <- function(df, date_col = "date") {
  # 1) Validate input
  if (!inherits(df, "data.frame")) {
    stop("`df` must be a data frame.")
  }
  
  if (!date_col %in% names(df)) {
    stop(paste("Date column", date_col, "not found in the data frame."))
  }
  
  # Split data frame by date column directly
  # Group by the date column
  grouped_dfs <- df |>
    dplyr::group_split(.data[[date_col]])
  
  # Extract date values to use as names
  date_names <- sapply(grouped_dfs, function(x) as.character(x[[date_col]][1]))
  
  # Name the list elements with their corresponding dates
  names(grouped_dfs) <- date_names
  
  return(grouped_dfs)
}
