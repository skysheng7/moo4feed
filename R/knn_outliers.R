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
#' @param custom_scaling A named list with scaling factors for input variables.
#'   Default is NULL, which uses predetermined scaling for feed or water data.
#' @param intake_col Character. Name of the column containing intake data (default: from global_var.R).
#' @param duration_col Character. Name of the column containing duration data (default: from global_var.R).
#'
#' @return A data frame with the same structure as the input, with an additional
#'   column 'outlier' indicating whether each row is an outlier ("Y") or not ("N").
#'
#' @examples
#' # Detect outliers in feeding data
#' cleaned_feed <- knn_outlier_detection(all_fed[[1]], threshold_percentile = 99.936)
#' cleaned_feed[which(cleaned_feed$outlier == "Y"), ]
#'
#' @export
knn_outlier_detection <- function(df, 
                                  k = 50, 
                                  threshold_percentile,
                                  custom_scaling = NULL,
                                  intake_col = intake_col2(),
                                  duration_col = duration_col2()) {

                      
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
  
  # Create rate column if it doesn't exist
  if (!"rate" %in% names(df)) {
    df$rate <- df[[intake_col]] / df[[duration_col]]
  }
  
  # Create a copy of the data frame to avoid modifying the original
  df_scaled <- df
  
  # Determine if it's feed or water data based on scaling values
  is_feed_data <- is.null(custom_scaling) && 
                 (threshold_percentile > 99.9 || threshold_percentile == 99.936)
  
  # Define scaling factors
  if (is.null(custom_scaling)) {
    if (is_feed_data) {
      # Option 4 from Python script - for feed data
      scaling <- list(
        rate = 10000,      # Feed rate scaling factor
        intake = 7,        # Feed intake scaling factor
        duration = 0.03    # Feed duration scaling factor
      )
    } else { # water or custom data
      scaling <- list(
        rate = 20,         # Water rate scaling factor
        intake = 1,        # Water intake scaling factor
        duration = 0.01    # Water duration scaling factor
      )
    }
  } else {
    scaling <- custom_scaling
  }
  
  # Apply scaling
  df_scaled$rate <- df_scaled$rate * scaling$rate
  df_scaled$intake <- df_scaled[[intake_col]] * scaling$intake
  df_scaled$duration <- df_scaled[[duration_col]] * scaling$duration
  
  # Create matrix for KNN
  X <- as.matrix(df_scaled[, c("duration", "intake", "rate")])
  
  # Adjust k if it's larger than the number of rows
  k_adjusted <- min(k, nrow(df) - 1)
  if (k_adjusted < 1) {
    # If we can't do KNN, just mark all as non-outliers
    df$outlier <- "N"
    return(df)
  }
  
  # Perform KNN and calculate average distances
  knn_result <- FNN::knn.dist(X, k = k_adjusted)
  avg_distances <- apply(knn_result, 1, mean)
  
  # Determine threshold for outliers
  threshold <- stats::quantile(avg_distances, threshold_percentile/100, na.rm = TRUE)
  
  # Identify outliers
  outlier_indices <- which(avg_distances > threshold)
  
  # Add outlier column to original data frame
  df$outlier <- "N"
  if (length(outlier_indices) > 0) {
    df$outlier[outlier_indices] <- "Y"
  }
  
  return(df)
}

#' Process multiple days of feeding data and remove outliers using KNN
#'
#' @param feed_list A list of daily feeding data frames.
#' @param k Integer. Number of nearest neighbors to consider.
#' @param threshold_percentile Numeric. Percentile threshold for outlier detection.
#' @param custom_scaling A named list with scaling factors for input variables.
#' @param intake_col Character. Name of the column containing intake data.
#' @param duration_col Character. Name of the column containing duration data.
#'
#' @return A list of data frames with outliers detected (outlier column added).
#' @export
knn_clean_feed <- function(feed_list, k = 50, threshold_percentile = 99.936,
                           custom_scaling = NULL,
                           intake_col = intake_col2(),
                           duration_col = duration_col2()) {
  lapply(feed_list, function(day_data) {
    knn_outlier_detection(day_data, 
                         k = k, 
                         threshold_percentile = threshold_percentile,
                         custom_scaling = custom_scaling,
                         intake_col = intake_col,
                         duration_col = duration_col)
  })
}

#' Process multiple days of water data and remove outliers using KNN
#'
#' @param water_list A list of daily water data frames.
#' @param k Integer. Number of nearest neighbors to consider.
#' @param threshold_percentile Numeric. Percentile threshold for outlier detection.
#' @param custom_scaling A named list with scaling factors for input variables.
#' @param intake_col Character. Name of the column containing intake data.
#' @param duration_col Character. Name of the column containing duration data.
#'
#' @return A list of data frames with outliers detected (outlier column added).
#' @export
knn_clean_water <- function(water_list, k = 50, threshold_percentile = 99.9,
                           custom_scaling = NULL,
                           intake_col = intake_col2(),
                           duration_col = duration_col2()) {
  lapply(water_list, function(day_data) {
    knn_outlier_detection(day_data, 
                         k = k, 
                         threshold_percentile = threshold_percentile,
                         custom_scaling = custom_scaling,
                         intake_col = intake_col,
                         duration_col = duration_col)
  })
}


# -----------------------------------------------------------------------------#
# ------------------------ Internal helper functions --------------------------#
# -----------------------------------------------------------------------------#

#' Quality check to identify and mark outliers using KNN
#'
#' @inheritParams qc_warning_skeleton
#' @inheritParams qc
#' @param warn Warning data frame to update
#'
#' @return Updated data and warning data frames
#' @keywords internal
#' @noRd
qc_knn_outliers <- function(comb, warn, cfg = qc_config(), 
                           data_type = c("feed", "water"),
                           intake_col = intake_col2(),
                           duration_col = duration_col2()) {
  
  data_type <- match.arg(data_type)
  
  # Determine threshold percentile based on data type
  threshold_percentile <- if (data_type == "feed") 99.936 else 99.9
  
  # Determine appropriate column names for warnings
  outlier_col <- if (data_type == "feed") "knn_outliers_feed" else "knn_outliers_water"
  
  # Ensure the warning column exists
  if (!outlier_col %in% names(warn)) {
    warn[[outlier_col]] <- NA_character_
  }
  
  # Process each day's data
  for (i in seq_along(comb)) {
    date <- names(comb)[i]
    day_idx <- which(warn$date == date)
    
    if (length(day_idx) == 0 || nrow(comb[[i]]) == 0) {
      next
    }
    
    # Apply KNN outlier detection
    result <- knn_outlier_detection(
      comb[[i]], 
      k = 50,
      threshold_percentile = threshold_percentile,
      intake_col = intake_col,
      duration_col = duration_col
    )
    
    # Update the data with outlier information
    comb[[i]] <- result
    
    # Count outliers
    outlier_count <- sum(result$outlier == "Y")
    
    # Update warning if outliers found
    if (outlier_count > 0) {
      warn[[outlier_col]][day_idx] <- paste0(outlier_count, " outliers")
    }
  }
  
  return(list(data = comb, warn = warn))
} 