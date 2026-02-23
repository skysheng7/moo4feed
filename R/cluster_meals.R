#' Cluster feeding visits into meals using DBSCAN
#'
#' @description
#' This function clusters individual feeding visits into meals using DBSCAN (Density-Based Spatial Clustering).
#' For each animal on each day, visits that occur close together in time are grouped into meals.
#' The function automatically determines optimal clustering parameters if not specified.
#'
#' @param data A single dataframe or list of dataframes containing feeding visit data
#' @param eps DBSCAN epsilon parameter (maximum time gap in minutes between visits in same meal).
#'   If NULL (default), the parameter is automatically determined using statistical methods.
#' @param min_pts DBSCAN minimum points parameter (minimum visits to form a dense cluster). Default is 2.
#'   This follows the DBSCAN recommendation of setting min_pts to D + 1 where D is the number of dimensions 
#'   (we have only 1 dimension: time, so min_pts = 1 + 1 = 2).
#' @param method Character string specifying the automatic eps determination method when eps=NULL.
#'   Options are "gmm" (default), "percentile", or "both". 
#' @param percentile Numeric value between 0 and 1 specifying which percentile to use 
#'   for automatic eps determination when method="percentile" or "both". Default is 0.93.
#' @param eps_scope Character string specifying the scope for automatic eps determination when eps=NULL.
#'   Options are:
#'   \itemize{
#'     \item "all_animals" (default): calculate an universal optimal interval (eps) for all animals across all days
#'     \item "one_animal_all_days": calculate optimal interval (eps) 
#'            differently for different animals, but within each animal, we use the same eps across all days
#'     \item "one_animal_single_day": calculate optimal interval (eps) 
#'            differently for different animals, and calculate different eps for each day within the same animal
#'   }
#' @param lower_bound Numeric value for lower bound of the optimal interval, if NULL, no lower bound is applied. Default is 5.
#' @param upper_bound Numeric value for upper bound of the optimal interval, if NULL, no upper bound is applied. Default is 60.
#' @param use_log_transform Logical indicating whether to use log transformation for GMM fitting. Default is TRUE. 
#'  Log transformation often provides better separation of within-meal and between-meal gaps.
#' @param log_multiplier Numeric value for multiplier of log transformation. Default is 20.
#' @param log_offset Numeric value for offset of log transformation. Default is 1.
#' @inheritParams set_global_cols
#'
#' @return A dataframe with meal-level summaries containing:
#' \describe{
#'   \item{[id_col2()]}{Animal ID}
#'   \item{date}{Date}
#'   \item{meal_id}{Sequential meal number within animal-day}
#'   \item{meal_start}{Start time of first visit in meal}
#'   \item{meal_end}{End time of last visit in meal}
#'   \item{meal_duration}{Total time from meal start to end (seconds)}
#'   \item{visit_count}{Number of visits in meal}
#'   \item{total_intake}{Sum of intake across all visits in meal}
#'   \item{feeding_percentage}{Percentage of meal time spent actively feeding}
#'   \item{unique_bins_count}{Number of unique bins visited in meal}
#' }
#'
#' @details
#' The function uses DBSCAN clustering on visit start times (converted to minutes from midnight).
#' Visits are clustered based on temporal proximity, with the eps parameter determining the maximum
#' time gap between visits in the same meal. Single visits or visits classified as "noise" by DBSCAN
#' are treated as noise points and excluded from meal summaries.
#'
#' When eps=NULL, the function automatically determines the optimal parameter using:
#' - 93rd percentile of inter-visit gaps
#' - Gaussian mixture modeling
#' - We will pick the minimum eps of the two methods, 
#' with a minimum of 5 minutes and a maximum of 60 minutes, to be conservative
#'
#' @examples
#' 
#' # Cluster meals with automatic parameter determination (all_fed is a list of 
#' # dataframes included in the package)
#' meals <- cluster_meals(all_fed[[1]], eps = 90, min_pts = 2, id_col="cow", 
#'                        start_col="start", end_col="end", bin_col="bin", 
#'                        intake_col="intake", dur_col="duration")
#' 
#' @importFrom cli cli_progress_bar cli_progress_update cli_progress_done cli_alert_info
#' @export
cluster_meals <- function(data,
                         eps = NULL,
                         min_pts = 2,
                         method = "gmm",
                         percentile = 0.93,
                         eps_scope = "all_animals",
                         lower_bound = 5,
                         upper_bound = 60,
                         use_log_transform = TRUE,
                         log_multiplier = 20,
                         log_offset = 1,
                         id_col = id_col2(),
                         start_col = start_col2(),
                         end_col = end_col2(),
                         bin_col = bin_col2(),
                         intake_col = intake_col2(),
                         dur_col = duration_col2(),
                         tz = tz2()) {
  
  # Input validation
  if (is.null(data)) {
    stop("data cannot be NULL")
  }

  
  # Validate min_pts parameter
  if (!is.numeric(min_pts) || length(min_pts) != 1 || min_pts < 1 || min_pts != round(min_pts)) {
    stop("min_pts must be a single positive integer")
  }
  
  # Validate method parameter
  valid_methods <- c("both", "percentile", "gmm")
  if (!method %in% valid_methods) {
    stop("method must be one of: ", paste(valid_methods, collapse = ", "))
  }
  
  # Validate percentile parameter
  if (!is.numeric(percentile) || length(percentile) != 1 || percentile <= 0 || percentile >= 1) {
    stop("percentile must be a single numeric value between 0 and 1")
  }
  
  # Validate bounds parameters
  if (!is.null(lower_bound) && (!is.numeric(lower_bound) || length(lower_bound) != 1 || lower_bound < 0)) {
    stop("lower_bound must be a single non-negative numeric value or NULL")
  }
  
  if (!is.null(upper_bound) && (!is.numeric(upper_bound) || length(upper_bound) != 1 || upper_bound < 0)) {
    stop("upper_bound must be a single non-negative numeric value or NULL")
  }
  
  if (!is.null(lower_bound) && !is.null(upper_bound) && lower_bound > upper_bound) {
    stop("lower_bound must be less than or equal to upper_bound")
  }
  
  # Convert to unified format (always work with single combined dataframe)
  if (is.data.frame(data)) {
    combined_data <- data
  } else if (is.list(data)) {
    # check if the list is empty
    if (length(data) == 0) {
      stop("data list is empty")
    }

    # check if all items in the list are dataframes
    if (!all(sapply(data, is.data.frame))) {
      stop("All items in the list must be dataframes")
    }
    # Combine all dataframes
    combined_data <- do.call(rbind, data)
    rownames(combined_data) <- NULL
  } else {
    stop("data must be a dataframe or list of dataframes")
  }
  
  # Check if we have any data
  if (nrow(combined_data) == 0) {
    warning("No data provided, returning empty meal dataframe")
    return(create_empty_meal_df(id_col, tz))
  }
  
  # Validate required columns
  required_cols <- c(id_col, start_col, end_col, bin_col, intake_col, dur_col)
  missing_cols <- setdiff(required_cols, names(combined_data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Add date column if not present
  if (!"date" %in% names(combined_data)) {
    combined_data$date <- lubridate::date(combined_data[[start_col]])
  }

  # check eps_scope is one of the allowed values
  allowed_eps_scopes <- c("one_animal_single_day", "one_animal_all_days", "all_animals")
  if (!eps_scope %in% allowed_eps_scopes) {
    stop("eps_scope must be one of: ", paste(allowed_eps_scopes, collapse = ", "))
  }
  
  # Handle clustering based on eps_scope
  if (eps_scope == "one_animal_single_day") {
    # Each animal-day gets its own eps
    result <- cluster_meals_by_animal_day(combined_data, eps, min_pts, method, percentile, eps_scope,
                                         lower_bound, upper_bound, use_log_transform, log_multiplier, log_offset,
                                         id_col, start_col, end_col, bin_col, intake_col, dur_col, tz)
  } else if (eps_scope == "one_animal_all_days") {
    # Each animal gets its own eps, applied to all their days
    result <- cluster_meals_by_animal(combined_data, eps, min_pts, method, percentile, eps_scope,
                                     lower_bound, upper_bound, use_log_transform, log_multiplier, log_offset,
                                     id_col, start_col, end_col, bin_col, intake_col, dur_col, tz)
  } else { # eps_scope == "all_animals"
    # Single universal eps for all animals
    result <- cluster_meals_universal_eps(combined_data, eps, min_pts, method, percentile, eps_scope,
                                         lower_bound, upper_bound, use_log_transform, log_multiplier, log_offset,
                                         id_col, start_col, end_col, bin_col, intake_col, dur_col, tz)
  }
  
  return(result)
}

#' Create empty meal summary dataframe with correct structure
#'
#' @param id_col Name of the ID column
#' @inheritParams set_global_cols
#'
#' @return Empty dataframe with meal summary structure
#' @keywords internal
#' @noRd
create_empty_meal_df <- function(id_col, tz = tz2()) {
  empty_df <- data.frame(
    date = as.Date(character(0)),
    meal_id = integer(0),
    meal_start = lubridate::as_datetime(character(0), tz = tz),
    meal_end = lubridate::as_datetime(character(0), tz = tz),
    meal_duration = numeric(0),
    visit_count = integer(0),
    total_intake = numeric(0),
    feeding_percentage = numeric(0),
    unique_bins_count = integer(0),
    stringsAsFactors = FALSE
  )
  # Add the id column with the correct name
  empty_df[[id_col]] <- character(0)
  # Reorder columns to put id_col first
  empty_df <- empty_df[c(id_col, setdiff(names(empty_df), id_col))]
  return(empty_df)
}

#' Cluster meals with different eps for each animal-day combination
#'
#' @inheritParams cluster_meals
#' @param combined_data Single combined dataframe
#' @param lower_bound Lower bound for eps determination
#' @param upper_bound Upper bound for eps determination
#' @param use_log_transform Logical indicating whether to use log transformation for GMM fitting
#'
#' @return Dataframe with meal summaries
#' @keywords internal
#' @noRd
cluster_meals_by_animal_day <- function(combined_data, eps, min_pts, method, percentile, eps_scope,
                                       lower_bound, upper_bound, use_log_transform, log_multiplier, log_offset,
                                       id_col, start_col, end_col, bin_col, intake_col, dur_col, tz) {
  
  # Group by animal and date, process each combination
  animal_day_groups <- combined_data |>
    dplyr::group_by(.data[[id_col]], .data[["date"]]) |>
    dplyr::group_split()
  
  n_groups <- length(animal_day_groups)
  
  # Initialize progress bar
  cli::cli_progress_bar(
    "Clustering meals (per animal-day)",
    total = n_groups,
    format = "{cli::pb_spin} {cli::pb_bar} {cli::pb_current}/{cli::pb_total} [{cli::pb_percent}] | ETA: {cli::pb_eta}"
  )
  
  results <- vector("list", n_groups)
  for (i in seq_len(n_groups)) {
    animal_day_df <- animal_day_groups[[i]]
    
    # Determine eps for this specific animal-day if not provided
    current_eps <- eps
    if (is.null(current_eps)) {
      # Calculate gaps for just this animal-day
      gaps <- calculate_gaps_by_animal(animal_day_df, id_col, start_col, end_col, tz)
      current_eps <- optimal_interval_from_gaps(gaps, method, percentile, lower_bound, upper_bound, use_log_transform, log_multiplier, log_offset)
    }
    
    # Cluster meals for this animal-day
    results[[i]] <- cluster_meals_cow_day(animal_day_df, current_eps, min_pts, 
                         id_col, start_col, end_col, bin_col, intake_col, dur_col, tz)
    
    # Update progress
    cli::cli_progress_update()
  }
  
  # Complete progress bar
  cli::cli_progress_done()
  
  # Combine results
  if (length(results) > 0) {
    result <- do.call(rbind, results)
    rownames(result) <- NULL
    return(result)
  } else {
    return(create_empty_meal_df(id_col, tz))
  }
}

#' Cluster meals with different eps for each animal (but same eps across all days for each animal)
#'
#' @inheritParams cluster_meals
#' @param combined_data Single combined dataframe
#' @param lower_bound Lower bound for eps determination
#' @param upper_bound Upper bound for eps determination
#' @param use_log_transform Logical indicating whether to use log transformation for GMM fitting
#'
#' @return Dataframe with meal summaries
#' @keywords internal
#' @noRd
cluster_meals_by_animal <- function(combined_data, eps, min_pts, method, percentile, eps_scope,
                                   lower_bound, upper_bound, use_log_transform, log_multiplier, log_offset,
                                   id_col, start_col, end_col, bin_col, intake_col, dur_col, tz) {
  
  # Get unique animals
  unique_animals <- unique(combined_data[[id_col]])
  n_animals <- length(unique_animals)
  
  # Initialize progress bar
  cli::cli_progress_bar(
    "Clustering meals (per animal)",
    total = n_animals,
    format = "{cli::pb_spin} {cli::pb_bar} {cli::pb_current}/{cli::pb_total} [{cli::pb_percent}] | ETA: {cli::pb_eta}"
  )
  
  # Process each animal
  animal_results <- vector("list", n_animals)
  for (i in seq_len(n_animals)) {
    animal_id <- unique_animals[i]
    
    # Get all data for this animal
    animal_data <- combined_data[combined_data[[id_col]] == animal_id, ]
    
    # Determine eps for this animal if not provided
    current_eps <- eps
    if (is.null(current_eps)) {
      # Calculate gaps for all days of this animal
      gaps <- calculate_gaps_by_animal(animal_data, id_col, start_col, end_col, tz)
      current_eps <- optimal_interval_from_gaps(gaps, method, percentile, lower_bound, upper_bound, use_log_transform, log_multiplier, log_offset)
    }
    
    # Cluster meals for each day of this animal using the same eps
    animal_day_results <- animal_data |>
      dplyr::group_by(date) |>
      dplyr::group_split() |>
      lapply(function(animal_day_df) {
        cluster_meals_cow_day(animal_day_df, current_eps, min_pts, 
                             id_col, start_col, end_col, bin_col, intake_col, dur_col, tz)
      })
    
    # Combine results for this animal
    if (length(animal_day_results) > 0) {
      animal_results[[i]] <- do.call(rbind, animal_day_results)
    } else {
      animal_results[[i]] <- create_empty_meal_df(id_col, tz)
    }
    
    # Update progress
    cli::cli_progress_update()
  }
  
  # Complete progress bar
  cli::cli_progress_done()
  
  # Combine results from all animals
  if (length(animal_results) > 0) {
    result <- do.call(rbind, animal_results)
    rownames(result) <- NULL
    return(result)
  } else {
    return(create_empty_meal_df(id_col, tz))
  }
}

#' Cluster meals with universal eps for all animals
#'
#' @inheritParams cluster_meals
#' @param combined_data Single combined dataframe
#' @param lower_bound Lower bound for eps determination
#' @param upper_bound Upper bound for eps determination
#' @param use_log_transform Logical indicating whether to use log transformation for GMM fitting
#'
#' @return Dataframe with meal summaries
#' @keywords internal
#' @noRd
cluster_meals_universal_eps <- function(combined_data, eps, min_pts, method, percentile, eps_scope,
                                       lower_bound, upper_bound, use_log_transform, log_multiplier, log_offset,
                                       id_col, start_col, end_col, bin_col, intake_col, dur_col, tz) {
  
  # Determine universal eps if not provided
  current_eps <- eps
  if (is.null(current_eps)) {
    cli::cli_alert_info("Calculating optimal meal interval for all animals...")
    # Calculate gaps from all animals (but properly grouped by animal)
    gaps <- calculate_gaps_by_animal(combined_data, id_col, start_col, end_col, tz)
    current_eps <- optimal_interval_from_gaps(gaps, method, percentile, lower_bound, upper_bound, use_log_transform, log_multiplier, log_offset)
  }
  
  # Cluster meals for each animal-day using the same universal eps
  animal_day_groups <- combined_data |>
    dplyr::group_by(.data[[id_col]], .data[["date"]]) |>
    dplyr::group_split()
  
  n_groups <- length(animal_day_groups)
  
  # Initialize progress bar
  cli::cli_progress_bar(
    "Clustering meals (universal interval)",
    total = n_groups,
    format = "{cli::pb_spin} {cli::pb_bar} {cli::pb_current}/{cli::pb_total} [{cli::pb_percent}] | ETA: {cli::pb_eta}"
  )
  
  results <- vector("list", n_groups)
  for (i in seq_len(n_groups)) {
    animal_day_df <- animal_day_groups[[i]]
    
    results[[i]] <- cluster_meals_cow_day(animal_day_df, current_eps, min_pts, 
                           id_col, start_col, end_col, bin_col, intake_col, dur_col, tz)
    
    # Update progress
    cli::cli_progress_update()
  }
  
  # Complete progress bar
  cli::cli_progress_done()
  
  # Combine results
  if (length(results) > 0) {
    result <- do.call(rbind, results)
    rownames(result) <- NULL
    return(result)
  } else {
    return(create_empty_meal_df(id_col, tz))
  }
}


