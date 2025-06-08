# -----------------------------------------------------------------------------#
# -------------------- External user-facing functions -------------------------#
# -----------------------------------------------------------------------------#

#' Detect outliers using k-nearest neighbors (KNN) method
#'
#' This function identifies outliers in feeding or drinking data using the K-nearest
#' neighbors (KNN) algorithm. It's based on the idea that outliers will have larger
#' average distances to their k-nearest neighbors.
#'
#' @param df A data frame containing feeding or drinking data.
#' @param k Integer. Number of nearest neighbors to consider (default: 50).
#'   Will be automatically adjusted if it exceeds the number of rows in the data.
#' @param threshold_percentile Numeric. Percentile threshold for outlier detection.
#'   Points with average distances above this percentile are considered outliers.
#'   Must be between 0 and 100. Default is 99.
#' @param custom_scaling A named list with scaling factors for input variables.
#'   Default is NULL, which means no scaling is applied (all factors = 1).
#' @param intake_col Character. Name of the column containing intake data (default: from global_var.R).
#' @param duration_col Character. Name of the column containing duration data (default: from global_var.R).
#' @param remove_outliers Logical. Whether to remove outliers from the data frame.
#'
#' @return A data frame with the same structure as the input, with an additional
#'   column 'outlier' indicating whether each row is an outlier ("Y") or not ("N").
#'   If remove_outliers=TRUE, returns the data frame with outliers removed and the
#'   outlier column dropped.
#'
#' @examples
#' # Detect outliers in feeding data
#' cleaned_feed <- knn_outlier_detection(all_fed[[1]], threshold_percentile = 99.936)
#' cleaned_feed[which(cleaned_feed$outlier == "Y"), ]
#'
#' @export
knn_outlier_detection <- function(df, 
                                  k = 50, 
                                  threshold_percentile = 99,
                                  custom_scaling = NULL,
                                  intake_col = intake_col2(),
                                  duration_col = duration_col2(),
                                  remove_outliers = FALSE) {

  # Validate inputs
  if (!is.data.frame(df)) {
    stop("df must be a data frame")
  }

  if (!is.numeric(threshold_percentile) || threshold_percentile < 0 || threshold_percentile > 100) {
    stop("threshold_percentile must be a number between 0 and 100")
  }
  
  if (!is.numeric(k) || k < 1) {
    stop("k must be a non-negative integer with a minimum value of 1")
  } else {
    k <- as.integer(k)
  }
  
  # Handle empty dataframe case
  if (nrow(df) == 0) {
    df$outlier <- character(0)
    return(df)
  }
                      
  # Ensure required columns exist
  required_cols <- c(duration_col, intake_col)
  if (!all(required_cols %in% names(df))) {
    stop("Required columns not found in data frame. Need duration and intake columns.")
  }
  
  # If dataset is too small, return without outlier detection
  if (nrow(df) < 3) {
    df$outlier <- "N"
    return(df)
  }
  
  # Add original row order for proper restoration later
  df$.original_row_order <- seq_len(nrow(df))
  
  # Create rate column if it doesn't exist
  if (!"rate" %in% names(df)) {
    df$rate <- df[[intake_col]] / df[[duration_col]]
  }
  
  # Separate data with NA or Inf rate values
  problematic_indices <- which(is.na(df$rate) | !is.finite(df$rate))
  
  if (length(problematic_indices) > 0) {
    warning("NAs or Inf values detected in rate calculation. These rows will be automatically marked as outliers.")
    
    # Split the data frame into clean and problematic parts
    df_problematic <- df[problematic_indices, , drop = FALSE]
    df_clean <- df[-problematic_indices, , drop = FALSE]
    
    # Add outlier column to problematic data, marking all as outliers
    df_problematic$outlier <- "Y"
    
    # If all rows have problematic rates, return with all marked as outliers
    if (nrow(df_clean) == 0) {
      result <- df_problematic[order(df_problematic$.original_row_order), ]
      result$.original_row_order <- NULL
      return(result)
    }
  } else {
    # No problematic values, process the entire data frame
    df_clean <- df
    df_problematic <- df[0, , drop = FALSE]  # Empty data frame with same structure
  }
  
  # Get scaling factors
  scaling <- get_scaling_factors(custom_scaling)
  
  # Adjust k if it's larger than the number of rows
  k_adjusted <- min(k, nrow(df_clean) - 1)
  if (k_adjusted < 1) {
    # If we can't do KNN, just mark all as non-outliers
    df_clean$outlier <- "N"
    
    # Combine and sort by original row order
    result <- rbind(df_clean, df_problematic)
    result <- result[order(result$.original_row_order), ]
    result$.original_row_order <- NULL
    
    return(result)
  }
  
  # Create scaled matrix for KNN
  X <- create_scaled_matrix(df_clean, scaling, intake_col, duration_col)
  
  # Perform KNN and calculate average distances
  knn_result <- FNN::knn.dist(X, k = k_adjusted)
  avg_distances <- apply(knn_result, 1, mean)
  
  # Determine threshold for outliers
  threshold <- stats::quantile(avg_distances, threshold_percentile/100, na.rm = TRUE)
  
  # Identify outliers
  outlier_indices <- which(avg_distances > threshold)
  
  # Add outlier column to clean data frame
  df_clean$outlier <- "N"
  if (length(outlier_indices) > 0) {
    df_clean$outlier[outlier_indices] <- "Y"
  }
  
  # Combine the clean data with the problematic data
  result <- rbind(df_clean, df_problematic)
  
  # Restore original row order
  result <- result[order(result$.original_row_order), ]
  result$.original_row_order <- NULL
  
  if (remove_outliers) {
    result <- result[result$outlier == "N", ] |>
      dplyr::select(-outlier)
  }
  
  return(result)
}

#' Process multiple days of feeding data and remove outliers using KNN
#'
#' @inheritParams knn_outlier_detection
#' @param feed_data A list of daily feeding data frames or a single data frame.
#' @param date_col Character. Name of the date column if feed_data is a list that needs to be unmerged (default: "date").
#'
#' @return If input is a list: a list of data frames with outliers detected.
#'   If input is a data frame: a data frame with outliers detected.
#'   If remove_outliers=TRUE, returns data with outliers removed.
#' @export
knn_clean_feed <- function(feed_data, 
                          k = 50, 
                          threshold_percentile = 99.936,
                          custom_scaling = list(rate = 10000, intake = 7, duration = 0.03),
                          intake_col = intake_col2(),
                          duration_col = duration_col2(),
                          remove_outliers = FALSE,
                          date_col = "date") {
                           
  # Determine if input is a list or a single data frame
  is_list_input <- (!is.data.frame(feed_data)) && (is.list(feed_data))
  
  if (is_list_input) {
    # Check that all elements are data frames
    if (!all(sapply(feed_data, inherits, "data.frame"))) {
      stop("All elements in feed_data must be data frames")
    }
    
    # Merge all data frames into one for better outlier detection
    merged_df <- merge_list_df(feed_data)
  } else if (inherits(feed_data, "data.frame")) {
    # If input is already a data frame, use it directly
    merged_df <- feed_data
  } else {
    stop("feed_data must be either a list of data frames or a single data frame")
  }
  
  # Apply outlier detection on the merged data
  cleaned_df <- knn_outlier_detection(
    merged_df, 
    k = k, 
    threshold_percentile = threshold_percentile,
    custom_scaling = custom_scaling,
    intake_col = intake_col,
    duration_col = duration_col,
    remove_outliers = remove_outliers
  )
  
  # If the input was a list, unmerge the cleaned data back into a list
  if (is_list_input) {
    if (!(date_col %in% names(cleaned_df))) {
      stop(paste("Date column", date_col, "not found in the merged data frame."))
    }
    result <- unmerge_data(cleaned_df, date_col = date_col)
    
    return(result)
  } else {
    # Otherwise, return the cleaned data frame
    return(cleaned_df)
  }
}

#' Process multiple days of water data and remove outliers using KNN
#'
#' @inheritParams knn_outlier_detection
#' @param water_data A list of daily water data frames or a single data frame.
#' @param date_col Character. Name of the date column if water_data is a list that needs to be unmerged (default: "date").
#'
#' @return If input is a list: a list of data frames with outliers detected.
#'   If input is a data frame: a data frame with outliers detected.
#'   If remove_outliers=TRUE, returns data with outliers removed.
#' @export
knn_clean_water <- function(water_data, 
                           k = 50, 
                           threshold_percentile = 99.9,
                           custom_scaling = list(rate = 20, intake = 1, duration = 0.01),
                           intake_col = intake_col2(),
                           duration_col = duration_col2(),
                           remove_outliers = FALSE,
                           date_col = "date") {
                           
  # Determine if input is a list or a single data frame
  is_list_input <- (!is.data.frame(water_data)) && (is.list(water_data))
  
  if (is_list_input) {
    # Check that all elements are data frames
    if (!all(sapply(water_data, inherits, "data.frame"))) {
      stop("All elements in water_data must be data frames")
    }
    
    # Merge all data frames into one for better outlier detection
    merged_df <- merge_list_df(water_data)
  } else if (inherits(water_data, "data.frame")) {
    # If input is already a data frame, use it directly
    merged_df <- water_data
  } else {
    stop("water_data must be either a list of data frames or a single data frame")
  }
  
  # Apply outlier detection on the merged data
  cleaned_df <- knn_outlier_detection(
    merged_df, 
    k = k, 
    threshold_percentile = threshold_percentile,
    custom_scaling = custom_scaling,
    intake_col = intake_col,
    duration_col = duration_col,
    remove_outliers = remove_outliers
  )
  
  # If the input was a list, unmerge the cleaned data back into a list
  if (is_list_input) {
    if (!(date_col %in% names(cleaned_df))) {
      stop(paste("Date column", date_col, "not found in the merged data frame."))
    }
    result <- unmerge_data(cleaned_df, date_col = date_col)
  
    return(result)
  } else {
    # Otherwise, return the cleaned data frame
    return(cleaned_df)
  }
}




# -----------------------------------------------------------------------------#
# -------------------- Internal helper functions -----------------------------#
# -----------------------------------------------------------------------------#

#' Create scaled data for KNN processing
#' 
#' @param df_clean Data frame with clean data
#' @param scaling List of scaling factors
#' @param intake_col Name of intake column
#' @param duration_col Name of duration column
#' 
#' @return Matrix of scaled data for KNN processing
#' @noRd
create_scaled_matrix <- function(df_clean, scaling, intake_col, duration_col) {
  df_scaled <- df_clean
  
  # Apply scaling
  df_scaled$rate <- df_scaled$rate * scaling$rate
  df_scaled$intake <- df_scaled[[intake_col]] * scaling$intake
  df_scaled$duration <- df_scaled[[duration_col]] * scaling$duration
  
  # Return matrix for KNN
  as.matrix(df_scaled[, c("duration", "intake", "rate")])
}

#' Get default or validated custom scaling factors
#' 
#' @param custom_scaling Custom scaling factors or NULL
#' 
#' @return List of scaling factors
#' @noRd
get_scaling_factors <- function(custom_scaling) {
  if (is.null(custom_scaling)) {
    return(list(rate = 1, intake = 1, duration = 1))
  } 
  
  # Ensure all required scaling factors exist
  scaling_defaults <- list(rate = 1, intake = 1, duration = 1)
  missing_fields <- setdiff(c("rate", "intake", "duration"), names(custom_scaling))
  
  if (length(missing_fields) > 0) {
    for (field in missing_fields) {
      custom_scaling[[field]] <- scaling_defaults[[field]]
    }
    warning("Missing scaling factors were set to 1: ", paste(missing_fields, collapse = ", "))
  }
  
  custom_scaling
}