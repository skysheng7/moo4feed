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
#' @importFrom stats setNames
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
    
    # Apply helper function to each dataframe in the list with progress bar
    n <- length(visit_data)
    pb_id <- cli::cli_progress_bar(
      format = "Merging cluster results {cli::pb_current}/{cli::pb_total} | ETA: {cli::pb_eta}",
      total = n,
      clear = FALSE
    )
    
    result_list <- lapply(seq_along(visit_data), function(i) {
      cli::cli_progress_update(id = pb_id)
      merge_cluster_results_single(visit_data[[i]], meal_results, id_col, start_col, end_col, tz)
    })
    
    cli::cli_progress_done(id = pb_id)
    
    # Preserve names from input list
    names(result_list) <- names(visit_data)
    
    return(result_list)
    
  } else {
    stop("visit_data must be a dataframe or list of dataframes")
  }
}

#' Validate required columns in dataframes
#'
#' @description
#' Helper function to check that required columns exist in a dataframe.
#'
#' @param df Dataframe to validate
#' @param required_cols Character vector of required column names
#' @param df_name Name of the dataframe for error messages
#'
#' @return NULL (stops execution if validation fails)
#'
#' @noRd
#' @keywords internal
validate_columns <- function(df, required_cols, df_name) {
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in ", df_name, ": ", 
         paste(missing_cols, collapse = ", "))
  }
}

#' Initialize meal columns for empty or unmatched visits
#'
#' @description
#' Helper function to add meal columns with default outlier values.
#'
#' @param visit_df Visit dataframe
#' @param tz Timezone for datetime columns
#' @param empty If TRUE, creates empty vectors; if FALSE, creates NA values
#'
#' @return Dataframe with meal columns added
#'
#' @noRd
#' @keywords internal
initialize_meal_columns <- function(visit_df, tz, empty = FALSE) {
  if (empty) {
    visit_df$meal_id <- integer(0)
    visit_df$meal_start <- lubridate::as_datetime(character(0), tz = tz)
    visit_df$meal_end <- lubridate::as_datetime(character(0), tz = tz)
    visit_df$meal_duration <- numeric(0)
    visit_df$total_intake <- numeric(0)
    visit_df$visit_count <- integer(0)
  } else {
    visit_df$meal_id <- 0L
    visit_df$meal_start <- lubridate::as_datetime(NA, tz = tz)
    visit_df$meal_end <- lubridate::as_datetime(NA, tz = tz)
    visit_df$meal_duration <- NA_real_
    visit_df$total_intake <- NA_real_
    visit_df$visit_count <- NA_integer_
  }
  return(visit_df)
}

#' Prepare visit and meal data for merging
#'
#' @description
#' Helper function to add date columns and convert datetime formats.
#'
#' @inheritParams merge_cluster_results
#' @param visit_df Visit dataframe
#' @param meal_results Meal results dataframe
#'
#' @return List with prepared visit_df and meal_results
#'
#' @noRd
#' @keywords internal
prepare_data_for_merge <- function(visit_df, meal_results, start_col, end_col, tz) {
  # Add date column to visit data if not present
  if (!"date" %in% names(visit_df)) {
    visit_df$date <- lubridate::date(visit_df[[start_col]])
  }
  
  # Convert datetime columns to ensure compatibility
  visit_df[[start_col]] <- lubridate::as_datetime(visit_df[[start_col]], tz = tz)
  visit_df[[end_col]] <- lubridate::as_datetime(visit_df[[end_col]], tz = tz)
  meal_results$meal_start <- lubridate::as_datetime(meal_results$meal_start, tz = tz)
  meal_results$meal_end <- lubridate::as_datetime(meal_results$meal_end, tz = tz)
  
  list(visit_df = visit_df, meal_results = meal_results)
}

#' Find visits that match meals based on time overlap
#'
#' @description
#' Helper function that performs vectorized join and filtering to find
#' which visits belong to which meals.
#'
#' @inheritParams merge_cluster_results
#' @param visit_df Visit dataframe (with .visit_idx column added)
#' @param meal_results Meal results dataframe
#'
#' @return Dataframe with matched visit-meal pairs
#'
#' @noRd
#' @keywords internal
find_visit_meal_matches <- function(visit_df, meal_results, id_col, start_col, end_col) {
  # Select only necessary columns from meal_results to reduce memory during join
  meal_subset <- meal_results[, c(id_col, "date", "meal_id", "meal_start", "meal_end", 
                                  "meal_duration", "total_intake", "visit_count")]
  
  # Join visits with meals on animal ID and date
  # This creates all possible visit-meal combinations for each animal-day
  joined <- dplyr::inner_join(
    visit_df,
    meal_subset,
    by = c(setNames(id_col, id_col), "date"),
    relationship = "many-to-many"
  )
  
  # Filter to only keep overlapping visits
  # A visit overlaps with a meal if: meal_start <= visit_start AND meal_end >= visit_end
  matched_visits <- joined[
    joined$meal_start <= joined[[start_col]] & 
    joined$meal_end >= joined[[end_col]],
  ]
  
  # For visits matching multiple meals (shouldn't happen often), keep the first match
  matched_visits <- matched_visits[!duplicated(matched_visits$.visit_idx), ]
  
  return(matched_visits)
}

#' Merge matched meal assignments back to visit data
#'
#' @description
#' Helper function to merge the meal assignments back to the original visit data
#' using a left join to preserve all visits including outliers.
#'
#' @param visit_df Visit dataframe
#' @param matched_visits Dataframe with matched visit-meal pairs
#'
#' @return Dataframe with meal assignments merged
#'
#' @noRd
#' @keywords internal
merge_meal_assignments <- function(visit_df, matched_visits) {
  if (nrow(matched_visits) == 0) {
    return(visit_df)
  }
  
  # Select only the meal columns we need
  meal_assignments <- matched_visits[, c(".visit_idx", "meal_id", "meal_start", 
                                         "meal_end", "meal_duration", "total_intake", 
                                         "visit_count")]
  
  # Merge back to original visit data using left join to keep all visits (including outliers)
  result_df <- dplyr::left_join(
    visit_df,
    meal_assignments,
    by = ".visit_idx"
  )
  
  return(result_df)
}

#' Fill NA values in meal columns with outlier defaults
#'
#' @description
#' Helper function to replace NA values in meal columns with default values
#' for visits that weren't matched to any meal (outliers).
#'
#' @param result_df Dataframe with meal columns
#' @param tz Timezone for datetime columns
#'
#' @return Dataframe with NA values replaced
#'
#' @noRd
#' @keywords internal
fill_outlier_values <- function(result_df, tz) {
  # Define meal columns and their default values
  meal_cols <- list(
    meal_id = 0L,
    meal_start = lubridate::as_datetime(NA, tz = tz),
    meal_end = lubridate::as_datetime(NA, tz = tz),
    meal_duration = NA_real_,
    total_intake = NA_real_,
    visit_count = NA_integer_
  )
  
  # Fill each column
  for (col_name in names(meal_cols)) {
    if (!col_name %in% names(result_df)) {
      result_df[[col_name]] <- meal_cols[[col_name]]
    } else {
      result_df[[col_name]][is.na(result_df[[col_name]])] <- meal_cols[[col_name]]
    }
  }
  
  return(result_df)
}

#' Merge clustering results for a single dataframe
#'
#' @description
#' Helper function that processes a single dataframe of visit data and merges
#' it with meal clustering results using a vectorized join-based approach.
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
  
  # Validate required columns
  validate_columns(visit_df, c(id_col, start_col, end_col), "visit_data")
  validate_columns(meal_results, 
                  c(id_col, "date", "meal_id", "meal_start", "meal_end", 
                    "meal_duration", "total_intake", "visit_count"),
                  "meal_results")
  
  # Handle empty dataframe case
  if (nrow(visit_df) == 0) {
    if (!"date" %in% names(visit_df)) {
      visit_df$date <- as.Date(character(0))
    }
    return(initialize_meal_columns(visit_df, tz, empty = TRUE))
  }

  # Handle empty meal_results - all visits will be outliers
  if (nrow(meal_results) == 0) {
    if (!"date" %in% names(visit_df)) {
      visit_df$date <- lubridate::date(visit_df[[start_col]])
    }
    return(initialize_meal_columns(visit_df, tz, empty = FALSE))
  }
  
  # Prepare data: add date column and convert datetime formats
  prepared <- prepare_data_for_merge(visit_df, meal_results, start_col, end_col, tz)
  visit_df <- prepared$visit_df
  meal_results <- prepared$meal_results
  
  # Add row index to preserve original order
  visit_df$.visit_idx <- seq_len(nrow(visit_df))
  
  # Find visits that match meals based on time overlap
  matched_visits <- find_visit_meal_matches(visit_df, meal_results, id_col, start_col, end_col)
  
  # Merge meal assignments back to visit data
  result_df <- merge_meal_assignments(visit_df, matched_visits)
  
  # Fill NA values for outliers
  result_df <- fill_outlier_values(result_df, tz)
  
  # Remove helper column
  result_df <- result_df[, !names(result_df) %in% c(".visit_idx")]
  
  return(result_df)
}
