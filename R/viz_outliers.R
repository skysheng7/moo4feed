# -----------------------------------------------------------------------------#
# -------------------- External user-facing functions -------------------------#
# -----------------------------------------------------------------------------#

#' Visualize outliers from KNN outlier detection
#'
#' This function creates a scatter plot visualizing the results of KNN outlier detection.
#' It allows users to plot any two variables against each other (e.g., intake vs. duration,
#' rate vs. intake, etc.), with outliers highlighted in a different color.
#'
#' @param data A data frame containing outlier detection results, or a list of such data frames.
#'   If a list is provided, the data will be merged before visualization.
#' @param x_var Character. Name of the column to display on the x-axis. Default is [duration_col2()].
#' @param y_var Character. Name of the column to display on the y-axis. Default is [intake_col2()].
#' @param x_lab Character. Label for the x-axis. Default is NULL, which uses the value of x_var.
#' @param y_lab Character. Label for the y-axis. Default is NULL, which uses the value of y_var.
#' @param jitter_amount Numeric. Amount of jitter to add to the points to prevent overplotting (default: 0.2).
#' @param alpha Numeric. Transparency level for the points, between 0 (completely transparent) and 1 (opaque).
#'   Default is 0.7.
#' @param title Character. Plot title (default: "Outlier Detection Results").
#' @param regular_color Character. Color for regular (non-outlier) points (default: "lightblue").
#' @param outlier_color Character. Color for outlier points (default: "orange").
#'
#' @return A ggplot object that can be further customized or printed.
#'
#' @examples
#' # Create a toy dataset with some normal feeding data and a few outliers
#' set.seed(123)
#' # Generate 100 normal feeding events
#' df_feed <- data.frame(
#'   cow = rep(1:10, each = 10),
#'   duration = runif(100, 100, 300),
#'   intake = runif(100, 5, 15),
#'   bin = sample(1:5, 100, replace = TRUE),
#'   outlier = rep("N", 100)
#' )
#' 
#' # Add 5 outlier events
#' df_outliers <- data.frame(
#'   cow = sample(1:10, 5),
#'   duration = c(500, 600, 150, 700, 100),
#'   intake = c(35, 40, 45, 5, 50),
#'   bin = sample(1:5, 5, replace = TRUE),
#'   outlier = rep("Y", 5)
#' )
#' 
#' # Combine the normal and outlier data
#' df_combined <- rbind(df_feed, df_outliers)
#' 
#' # Visualize intake vs. duration
#' p1 <- viz_outliers(df_combined, x_var = "duration", y_var = "intake")
#' 
#' # Visualize with custom labels and title
#' p2 <- viz_outliers(df_combined, 
#'              x_var = "duration",
#'              y_var = "intake",
#'              x_lab = "Feeding Duration (seconds)", 
#'              y_lab = "Feed Intake (kg)",
#'              title = "Feed Intake Outlier Analysis")
#'
#' @export
viz_outliers <- function(data, 
                         x_var = duration_col2(), 
                         y_var = intake_col2(), 
                         x_lab = NULL,
                         y_lab = NULL,
                         jitter_amount = 0.2,
                         alpha = 0.7,
                         title = "Outlier Detection Results",
                         regular_color = "lightblue",
                         outlier_color = "orange") {
  
  if (!is.character(x_var) || !is.character(y_var)) {
    stop("'x_var' and 'y_var' must be character strings")
  }

  # Prepare data for visualization
  if (!is.data.frame(data) && is.list(data)) {
    # If it's a list of data frames, merge them
    if (!all(sapply(data, inherits, "data.frame"))) {
      stop("All elements in 'data' must be data frames")
    }
    
    df <- merge_list_df(data)
  } else if (is.data.frame(data)) {
    # If it's already a data frame, use it directly
    df <- data
  } else {
    stop("'data' must be either a data frame or a list of data frames")
  }
  
  # Check for outlier column
  if (!("outlier" %in% names(df))) {
    stop("The data must contain an 'outlier' column with values 'Y' or 'N'. ",
         "Run knn_outlier_detection() first.")
  }
  
  # Check if required variables exist
  if (!(x_var %in% names(df))) {
    stop(paste("Column", x_var, "not found in the data"))
  }
  if (!(y_var %in% names(df))) {
    stop(paste("Column", y_var, "not found in the data"))
  }
  
  # Check for NA or infinite values in the chosen variables
  if (any(is.na(df[[x_var]])) || any(is.infinite(df[[x_var]])) || 
      any(is.na(df[[y_var]])) || any(is.infinite(df[[y_var]]))) {
    warning("There are NA or infinite values in the selected variables. These will be removed from the visualization.")
    df <- df |>
      dplyr::filter(!is.na(.data[[x_var]]) & !is.infinite(.data[[x_var]]) &
                   !is.na(.data[[y_var]]) & !is.infinite(.data[[y_var]]))
  }

  if (is.null(x_lab)) {
    x_lab <- x_var
  }
  if (is.null(y_lab)) {
    y_lab <- y_var
  }
  
  # Create the plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]])) +
    ggplot2::geom_point(ggplot2::aes(color = outlier), 
                         position = ggplot2::position_jitter(width = jitter_amount, height = jitter_amount),
                         alpha = alpha) +
    ggplot2::scale_color_manual(values = c("N" = regular_color, "Y" = outlier_color),
                                labels = c("N" = "Regular", "Y" = "Outlier")) +
    ggplot2::labs(title = title,
                  x = x_lab,
                  y = y_lab,
                  color = "Outlier") +
    ggplot2::theme_minimal()
  
  return(p)
} 