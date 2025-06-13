#' Merge meal clustering results with original visit data
#'
#' @description
#' Merges the meal-level results from [cluster_meals()] with the original 
#' feeding visit data to assign meal information to each individual visit.
#' Visits not assigned to any meal (outliers) are labeled with meal_id = 0.
#'
#' @param visit_data Original feeding visit data (dataframe or list of dataframes)
#' @param meal_results Results from [cluster_meals()] function
#' @inheritParams set_global_cols
#'
#' @return Same structure as input visit_data (dataframe or list of dataframes) 
#' with original visit data plus meal information columns:
#' \describe{
#'   \item{meal_id}{Sequential meal number within animal-day (0 for outliers)}
#'   \item{meal_start}{Start time of the meal this visit belongs to (NA for outliers)}
#'   \item{meal_end}{End time of the meal this visit belongs to (NA for outliers)}
#'   \item{meal_duration}{Total duration of the meal this visit belongs to (NA for outliers)}
#'   \item{total_intake}{Total intake of the meal this visit belongs to (NA for outliers)}
#'   \item{visit_count}{Number of visits in the meal this visit belongs to (NA for outliers)}
#' }
#'
#' @details
#' The function matches visits to meals based on animal ID, date, and whether the
#' visit time falls within the meal's start and end times. Visits that don't
#' match any meal are considered outliers and assigned meal_id = 0.
#'
#' @examples
#' # Cluster meals first (all_fed is included in the package)
#' meals <- cluster_meals(all_fed[[1]])
#' 
#' # Merge results with original data
#' merged_data <- merge_cluster_results(all_fed[[1]], meals)
#' 
#' # Check how many visits were assigned vs. outliers
#' table(merged_data$meal_id == 0)
#' 
#' @export
merge_cluster_results <- function(visit_data,
                                 meal_results,
                                 id_col = id_col2(),
                                 start_col = start_col2(),
                                 end_col = end_col2(), 
                                 tz = tz2()) {
  
  # Validate inputs
  if (is.null(visit_data) || is.null(meal_results)) {
    stop("Both visit_data and meal_results must be provided")
  }
  
  if (!is.data.frame(meal_results)) {
    stop("meal_results must be a dataframe")
  }
  
  # Handle different input structures
  if (is.data.frame(visit_data)) {
    # Single dataframe input - return single dataframe
    return(merge_cluster_results_single(visit_data, meal_results, id_col, start_col, end_col, tz))
    
  } else if (is.list(visit_data)) {
    # List of dataframes input - return list of dataframes
    if (length(visit_data) == 0) {
      return(list())
    }
    
    if (!all(sapply(visit_data, is.data.frame))) {
      stop("All items in visit_data list must be dataframes")
    }
    
    # Apply helper function to each dataframe in the list
    result_list <- lapply(visit_data, function(df) {
      merge_cluster_results_single(df, meal_results, id_col, start_col, end_col, tz)
    })
    
    return(result_list)
    
  } else {
    stop("visit_data must be a dataframe or list of dataframes")
  }
}

#' Merge clustering results for a single dataframe
#'
#' @description
#' Helper function that processes a single dataframe of visit data and merges
#' it with meal clustering results.
#'
#' @inheritParams merge_cluster_results
#'
#' @return Dataframe with visit data plus meal information columns
#'
#' @noRd
#' @keywords internal
merge_cluster_results_single <- function(visit_df,
                                        meal_results,
                                        id_col = id_col2(),
                                        start_col = start_col2(),
                                        end_col = end_col2(),
                                        tz = tz2()) {
  
  # Validate required columns in visit data
  required_visit_cols <- c(id_col, start_col, end_col)
  missing_visit_cols <- setdiff(required_visit_cols, names(visit_df))
  if (length(missing_visit_cols) > 0) {
    stop("Missing required columns in visit_data: ", paste(missing_visit_cols, collapse = ", "))
  }
  
  # Validate required columns in meal results
  required_meal_cols <- c(id_col, "date", "meal_id", "meal_start", "meal_end", 
                         "meal_duration", "total_intake", "visit_count")
  missing_meal_cols <- setdiff(required_meal_cols, names(meal_results))
  if (length(missing_meal_cols) > 0) {
    stop("Missing required columns in meal_results: ", paste(missing_meal_cols, collapse = ", "))
  }
  
  # Handle empty dataframe case
  if (nrow(visit_df) == 0) {
    # Add date column if not present
    if (!"date" %in% names(visit_df)) {
      visit_df$date <- as.Date(character(0))
    }
    
    # Initialize meal columns for empty dataframe
    visit_df$meal_id <- integer(0)
    visit_df$meal_start <- lubridate::as_datetime(character(0), tz = tz)
    visit_df$meal_end <- lubridate::as_datetime(character(0), tz = tz)
    visit_df$meal_duration <- numeric(0)
    visit_df$total_intake <- numeric(0)
    visit_df$visit_count <- integer(0)
    
    return(visit_df)
  }

  # Handle empty meal_results - all visits will be outliers
  if (nrow(meal_results) == 0) {
    # Add date column if not present
    if (!"date" %in% names(visit_df)) {
      visit_df$date <- lubridate::date(visit_df[[start_col]])
    }
    
    # Initialize meal columns with outlier values
    visit_df$meal_id <- 0L
    visit_df$meal_start <- lubridate::as_datetime(NA, tz = tz)
    visit_df$meal_end <- lubridate::as_datetime(NA, tz = tz)
    visit_df$meal_duration <- NA_real_
    visit_df$total_intake <- NA_real_
    visit_df$visit_count <- NA_integer_
    
    return(visit_df)
  }
  
  # Add date column to visit data if not present
  if (!"date" %in% names(visit_df)) {
    visit_df$date <- lubridate::date(visit_df[[start_col]])
  }
  
  # Initialize meal columns in visit data
  visit_df$meal_id <- 0L  # Default to outlier
  visit_df$meal_start <- lubridate::as_datetime(NA, tz = tz)
  visit_df$meal_end <- lubridate::as_datetime(NA, tz = tz)
  visit_df$meal_duration <- NA_real_
  visit_df$total_intake <- NA_real_
  visit_df$visit_count <- NA_integer_
  
  # Convert datetime columns to ensure compatibility
  visit_df[[start_col]] <- lubridate::as_datetime(visit_df[[start_col]], tz = tz)
  visit_df[[end_col]] <- lubridate::as_datetime(visit_df[[end_col]], tz = tz)
  meal_results$meal_start <- lubridate::as_datetime(meal_results$meal_start, tz = tz)
  meal_results$meal_end <- lubridate::as_datetime(meal_results$meal_end, tz = tz)
  
  # Vectorized approach: for each visit, find matching meal
  for (i in seq_len(nrow(visit_df))) {
    visit_animal <- visit_df[[id_col]][i]
    visit_date <- visit_df$date[i]
    visit_start <- visit_df[[start_col]][i]
    visit_end <- visit_df[[end_col]][i]
    
    # Find meals for this animal on this date
    animal_day_meals <- meal_results[meal_results[[id_col]] == visit_animal & 
                                    as.character(meal_results$date) == as.character(visit_date), ]
    
    if (nrow(animal_day_meals) == 0) {
      next  # No meals for this animal-day, visit remains as outlier (meal_id = 0)
    }
    
    # Vectorized overlap check: find all meals that overlap with this visit
    overlaps <- animal_day_meals[which((animal_day_meals$meal_start <= visit_start) & 
                (animal_day_meals$meal_end >= visit_end)),]
    
    if (nrow(overlaps) > 0) {
      # Assign this visit to the matched meal
      visit_df$meal_id[i] <- overlaps$meal_id[1]
      visit_df$meal_start[i] <- overlaps$meal_start[1]
      visit_df$meal_end[i] <- overlaps$meal_end[1]
      visit_df$meal_duration[i] <- overlaps$meal_duration[1]
      visit_df$total_intake[i] <- overlaps$total_intake[1]
      visit_df$visit_count[i] <- overlaps$visit_count[1]
    }
  }
  
  return(visit_df)
}
