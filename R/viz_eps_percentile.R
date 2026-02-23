#' Visualize gap distribution and percentile-based optimal interval (eps) selection
#'
#' @description
#' Creates a histogram visualization of inter-visit gaps with the optimal eps value
#' (determined using percentile method) highlighted as a vertical line. This helps 
#' you understand how the percentile method selects the optimal interval for meal clustering.
#' \itemize{
#'   \item If you wish to calculate optimal interval for a single animal across multiple days, 
#'   make sure data belongs to a single animal across multiple days.
#'   \item If you wish to calculate optimal interval for all animals across all days, 
#'   make sure data recorded all animals across all days. 
#'   \item If you wish to calculate optimal interval for a single animal on a single day, 
#'   make sure data belongs to a single animal and only 1 day.
#' }
#'
#' @param data A single dataframe or list of dataframes containing feeding visit data
#' @param percentile Numeric value between 0 and 1 specifying which percentile to use 
#'   for eps determination. Default is 0.93.
#' @param lower_bound Numeric value for lower bound of the optimal interval, if NULL, no lower bound is applied.
#' @param upper_bound Numeric value for upper bound of the optimal interval, if NULL, no upper bound is applied.
#' @param bins Number of bins for the histogram. Default is 100.
#' @param colors Character vector of 2 colors to use. Default uses `grDevices::hcl.colors(2, "Set 3")`.
#' @param title_prefix Character string for plot title prefix. Default is "Distribution of time gap between visits & optimal meal interval (eps)".
#' @param xlim Numeric value for x-axis limit. Default is 20.
#' @inheritParams set_global_cols
#'  
#' @return A ggplot2 object showing the gap distribution histogram with optimal eps line
#'
#' @details
#' The function internally calls [meal_interval()] with method="percentile" to determine
#' the optimal eps value, then creates a histogram of all inter-visit gaps with the 
#' optimal eps marked as a vertical dashed line.
#'
#' @examples
#' # Create toy dataset
#' toy_data <- all_fed[[1]][which(all_fed[[1]]$cow == 5114),]
#' # Visualize with 93rd percentile
#' plot <- viz_eps_percentile(toy_data, id_col = "cow", 
#'                            start_col = "start", end_col = "end", 
#'                            tz = "America/Vancouver", 
#'                            percentile = 0.93)
#'
#' @export
viz_eps_percentile <- function(data, 
                               percentile = 0.93,
                               lower_bound = NULL,
                               upper_bound = NULL,
                               bins = 100,
                               colors = grDevices::hcl.colors(2, "Set 3"),
                               title_prefix = "Distribution of time gap between visits & \noptimal meal interval (eps)",
                               xlim = 20,
                               id_col = id_col2(),
                               start_col = start_col2(),
                               end_col = end_col2(),
                               tz = tz2()) {
  
  # Input validation
  if (is.null(data)) {
    stop("data cannot be NULL")
  }
  
  if (!is.numeric(percentile) || length(percentile) != 1 || percentile <= 0 || percentile >= 1) {
    stop("percentile must be a single numeric value between 0 and 1")
  }
  
  if (!is.null(lower_bound) && (!is.numeric(lower_bound) || length(lower_bound) != 1 || lower_bound < 0)) {
    stop("lower_bound must be a single non-negative numeric value or NULL")
  }
  
  if (!is.null(upper_bound) && (!is.numeric(upper_bound) || length(upper_bound) != 1 || upper_bound < 0)) {
    stop("upper_bound must be a single non-negative numeric value or NULL")
  }
  
  if (!is.null(lower_bound) && !is.null(upper_bound) && lower_bound > upper_bound) {
    stop("lower_bound must be less than or equal to upper_bound")
  }
  
  if (!is.numeric(bins) || length(bins) != 1 || bins <= 0 || bins != round(bins)) {
    stop("bins must be a single positive integer")
  }
  
  if (!is.character(colors) || length(colors) == 0) {
    stop("colors must be a non-empty character vector")
  }
  
  if (!is.character(title_prefix) || length(title_prefix) != 1) {
    stop("title_prefix must be a single character string")
  }
  
  # Calculate optimal eps using percentile method
  cli::cli_alert_info("Calculating optimal meal interval using percentile method...")
  optimal_eps <- meal_interval(data, 
                               method = "percentile", 
                               percentile = percentile,
                               lower_bound = lower_bound,
                               upper_bound = upper_bound,
                               id_col = id_col,
                               start_col = start_col,
                               end_col = end_col,
                               tz = tz)
  
  # Get gaps for visualization (need to handle input format)
  if (is.data.frame(data)) {
    combined_data <- data
  } else if (is.list(data)) {
    if (length(data) == 0) {
      stop("data list cannot be empty")
    }

    if (!all(sapply(data, is.data.frame))) {
      stop("All items in the list must be dataframes")
    }
    combined_data <- do.call(rbind, data)
    rownames(combined_data) <- NULL
  } else {
    stop("data must be a dataframe or list of dataframes")
  }
  
  # Validate required columns
  required_cols <- c(id_col, start_col, end_col)
  missing_cols <- setdiff(required_cols, names(combined_data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Check if we have any data
  if (nrow(combined_data) == 0) {
    warning("No data provided, cannot create visualization")
    return(ggplot2::ggplot() + ggplot2::theme_void() + 
           ggplot2::labs(title = "No data available for visualization"))
  }
  
  # Calculate gaps using existing helper function
  cli::cli_alert_info("Calculating time gaps between visits...")
  gaps <- calculate_gaps_by_animal(combined_data, id_col, start_col, end_col, tz)
  
  if (length(gaps) == 0) {
    warning("No gaps between visits found, cannot create meaningful visualization")
    return(ggplot2::ggplot() + ggplot2::theme_void() + 
           ggplot2::labs(title = "No gaps found between visits"))
  }
  
  # Create histogram data
  gaps_df <- data.frame(gap_minutes = gaps)
  
  # Determine reasonable x-axis limits
  x_limit <- xlim
  
  # Create the plot
  p <- ggplot2::ggplot(gaps_df, ggplot2::aes(x = gap_minutes)) +
    ggplot2::geom_histogram(bins = bins, fill = colors[1], color = colors[1], alpha = 0.7) +
    ggplot2::geom_vline(xintercept = optimal_eps, 
                        linetype = "dashed", 
                        color = colors[2], 
                        linewidth = 1.2) +
    ggplot2::labs(
      title = paste0(title_prefix, " (", round(percentile * 100), "th Percentile)"),
      subtitle = paste0("Optimal eps = ", round(optimal_eps, 2), " minutes"),
      x = "Visit Gap Duration (minutes)",
      y = "Frequency"
    ) +
    ggplot2::scale_x_continuous(limits = c(0, x_limit)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 11),
      axis.text = ggplot2::element_text(size = 10)
    )
  
  cli::cli_alert_success("Visualization complete!")
  
  return(p)
} 