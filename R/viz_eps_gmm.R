#' Visualize gap distribution with GMM fit and optimal interval (eps)
#'
#' @description
#' Creates a histogram visualization of inter-visit gaps with Gaussian Mixture Model 
#' (GMM) component distributions overlaid and the optimal interval (eps) value highlighted. 
#' This helps you understand how the GMM method identifies distinct gap patterns 
#' and selects the optimal interval for meal clustering.
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
#' @param lower_bound Numeric value for lower bound of the optimal interval, if NULL, no lower bound is applied.
#' @param upper_bound Numeric value for upper bound of the optimal interval, if NULL, no upper bound is applied.
#' @param bins Number of bins for the histogram.
#' @param colors Character vector of colors to use. Default uses `grDevices::hcl.colors(4, "Set 3")`.
#' @param title_prefix Character string for plot title prefix. Default is "Distribution of time gap between visits & 
#'  GMM-based meal interval (eps)".
#' @param show_components Logical indicating whether to show individual GMM components. Default is TRUE.
#' @param xlim Numeric value for x-axis limit. 
#' @param use_log_transform Logical indicating whether to use log transformation for GMM fitting. Default is TRUE. 
#'  Log transformation often provides better separation of within-meal and between-meal gaps.
#' @param log_multiplier Numeric value for multiplier of log transformation. Default is 20.
#' @param log_offset Numeric value for offset of log transformation. Default is 1.
#' @inheritParams set_global_cols
#'
#' @return A ggplot2 object showing the gap distribution histogram with GMM fit and optimal eps line
#'
#' @details
#' The function internally calls [meal_interval()] with method="gmm" to determine
#' the optimal eps value and fits a 2-component Gaussian mixture model to visualize
#' the separation between within-meal and between-meal gaps.
#' 
#' By default, the function uses log transformation for GMM fitting (`use_log_transform = TRUE`),
#' which often provides better separation between within-meal and between-meal gaps due
#' to the typically right-skewed nature of gap distributions. When log transformation is used,
#' the component distributions are displayed as log-normal distributions in the original scale.
#' 
#' If GMM fitting fails or there are insufficient data points (< 10), the function 
#' falls back to percentile method with a warning.
#'
#' @examples
#' toy_data <- all_fed[[1]][which(all_fed[[1]]$cow == 5114),]
#' 
#' # Visualize with GMM method (default uses log transformation)
#' plot <- viz_eps_gmm(toy_data, id_col = "cow", start_col = "start", 
#'                    end_col = "end", tz = "America/Vancouver",
#'                    use_log_transform = FALSE)
#'
#' @export
viz_eps_gmm <- function(data, 
                        lower_bound = NULL,
                        upper_bound = NULL,
                        bins = 100,
                        colors = grDevices::hcl.colors(4, "Set 3"),
                        title_prefix = "Distribution of time gap between visits \n& GMM-based meal interval (eps)",
                        show_components = TRUE,
                        xlim = 10,
                        use_log_transform = TRUE,
                        log_multiplier = 20,
                        log_offset = 1,
                        id_col = id_col2(),
                        start_col = start_col2(),
                        end_col = end_col2(),
                        tz = tz2()) {
  
  # Input validation
  if (is.null(data)) {
    stop("data cannot be NULL")
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
  
  if (!is.character(colors) || length(colors) < 3) {
    stop("colors must be a character vector with at least 3 colors")
  }
  
  if (!is.character(title_prefix) || length(title_prefix) != 1) {
    stop("title_prefix must be a single character string")
  }
  
  if (!is.logical(show_components) || length(show_components) != 1) {
    stop("show_components must be a single logical value")
  }
  
  if (!is.logical(use_log_transform) || length(use_log_transform) != 1) {
    stop("use_log_transform must be a single logical value")
  }
  
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
  gaps <- calculate_gaps_by_animal(combined_data, id_col, start_col, end_col, tz)
  
  if (length(gaps) == 0) {
    warning("No gaps between visits found, cannot create meaningful visualization")
    return(ggplot2::ggplot() + ggplot2::theme_void() + 
           ggplot2::labs(title = "No gaps found between visits"))
  }
  
  # Create histogram data
  gaps_df <- data.frame(gap_minutes = gaps)
  
  # Determine reasonable x-axis limits for better visualization
  x_limit <- xlim
  
  # Fit GMM and get optimal eps (always calculate, even if not showing components)
  gmm_result <- fit_gmm_model(gaps, use_log_transform, log_multiplier, log_offset)
  
  # Get optimal eps value
  if (gmm_result$fit_successful) {
    optimal_eps <- gmm_result$intersection_point
  } else {
    # Fallback to percentile method
    optimal_eps <- stats::quantile(gaps, 0.9, na.rm = TRUE)
    warning("GMM fitting failed, using 90th percentile as fallback")
  }
  
  # Transform gaps and eps if using log transformation
  if (gmm_result$use_log_transform) {
    gaps_df$gap_minutes <- log(log_multiplier*gaps_df$gap_minutes + log_offset)
    # optimal_eps is already in log space from GMM fitting
  } else {
    # optimal_eps is already in original space
  }
  
  # Try to fit GMM for component visualization
  gmm_fitted <- FALSE
  
  if (length(gaps) >= 10 && show_components && gmm_result$fit_successful) {
    gmm_fitted <- TRUE
    
    x_seq <- seq(0, x_limit, length.out = nrow(combined_data))
    
    # Calculate the actual component densities (properly weighted)
    component1_density <- gmm_result$lambda1 * stats::dnorm(x_seq, 
                                                            mean = gmm_result$mu1, 
                                                            sd = gmm_result$sigma1)
    component2_density <- gmm_result$lambda2 * stats::dnorm(x_seq, 
                                                            mean = gmm_result$mu2, 
                                                            sd = gmm_result$sigma2)
    
    # Create data frames for plotting
    component1_data <- data.frame(
      x = x_seq,
      density = component1_density,
      component = "Within-meal gaps"
    )
    
    component2_data <- data.frame(
      x = x_seq,
      density = component2_density,
      component = "Between-meal gaps"
    )
  }
  
  # Create the base plot using density histogram for proper scaling
  p <- ggplot2::ggplot(gaps_df, ggplot2::aes(x = gap_minutes)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), 
                           bins = bins, fill = colors[1], color = colors[1], alpha = 0.7)
  
  # Add GMM component density curves if successfully fitted
  if (gmm_fitted && show_components) {
    # Combine component data for proper legend handling
    component_data <- rbind(component1_data, component2_data)
    
    p <- p + 
      ggplot2::geom_line(data = component_data, 
                         ggplot2::aes(x = x, y = density, color = component), 
                         size = 1.2, alpha = 0.8) +
      ggplot2::geom_area(data = component_data, 
                         ggplot2::aes(x = x, y = density, fill = component), 
                         alpha = 0.3) +
      ggplot2::scale_color_manual(values = c("Within-meal gaps" = colors[2], 
                                            "Between-meal gaps" = colors[3]),
                                 name = "GMM Components") +
      ggplot2::scale_fill_manual(values = c("Within-meal gaps" = colors[2], 
                                           "Between-meal gaps" = colors[3]),
                                name = "GMM Components")
  }
  
  # Add optimal eps line
  eps_color <- if (length(colors) >= 4) colors[4] else "orange"
  p <- p + 
    ggplot2::geom_vline(xintercept = optimal_eps, 
                        linetype = "dashed", 
                        color = eps_color, 
                        size = 1.2)
  
  # Add labels and formatting
  subtitle_text <- paste0("Optimal interval = ", round(optimal_eps, 2))
  if (gmm_fitted && show_components) {
    subtitle_text <- subtitle_text
  } else if (length(gaps) < 10 || !gmm_result$fit_successful) {
    subtitle_text <- paste0(subtitle_text, " (fallback to percentile method)")
  }

  if (gmm_result$use_log_transform) {
    title_prefix <- paste0(title_prefix, " (log-transformed)")
    subtitle_text <- paste0(subtitle_text, " (log-transformed)")
    x_label <- paste0("log(Visit Gap Duration * ", log_multiplier, " + ", log_offset, ")")
  } else {
    subtitle_text <- paste0(subtitle_text, " minutes")
    x_label <- "Visit Gap Duration (minutes)"
  }
  
  p <- p +
    ggplot2::labs(
      title = title_prefix,
      subtitle = subtitle_text,
      x = x_label,
      y = "Density"
    ) +
    ggplot2::scale_x_continuous(limits = c(0, x_limit)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 11),
      axis.text = ggplot2::element_text(size = 10),
      legend.position = "bottom"
    )
  
  return(p)
} 