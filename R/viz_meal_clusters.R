# -----------------------------------------------------------------------------#
# -------------------- External user-facing functions -------------------------#
# -----------------------------------------------------------------------------#

#' Visualize meal clustering results as timeline plots
#'
#' @description
#' Creates timeline plots showing meal assignments over time. Points represent 
#' individual feeding visits, colored by meal_id. Outliers (meal_id = 0) are 
#' shown in a customizable color. The function can handle both single dataframes 
#' and lists of dataframes from [merge_cluster_results()].
#'
#' @param data Merged visit data with meal assignments from [merge_cluster_results()].
#'   Can be a single dataframe or list of dataframes.
#' @param point_size Numeric. Size of the points representing visits (default: 2).
#' @param point_alpha Numeric. Transparency of points, between 0 and 1 (default: 0.7).
#' @param ncol_facet Numeric. Number of columns for faceting when creating overview plots (default: 1).
#' @param date_format Character. Format for date labels (default: "%Y-%m-%d").
#' @param time_breaks Character. Time axis breaks, passed to [ggplot2::scale_x_datetime()] (default: "4 hours").
#' @param time_labels Character. Time axis label format (default: "%H").
#' @param color_palette Character. Color palette name for meal colors. Options include 
#'   any palette from [grDevices::hcl.colors()] such as "Set 3", "Dark 3", "Pastel 1", etc. 
#'   (default: "Set 3").
#' @param outlier_color Character. Color for outlier points (meal_id = 0) (default: "grey50").
#' @param title_prefix Character. Prefix for plot titles. Set to NULL or "" for no title 
#'   (default: "Animal"). Will be followed by the animal ID (e.g., "Animal 123").
#' @param text_size Numeric. Base text size for all plot text elements (default: 12).
#' @param title_size Numeric. Size of plot titles when creating overview plots. 
#'   If NULL, uses text_size + 2 (default: NULL).
#' @inheritParams set_global_cols
#'
#' @return 
#' If ≤5 animal-day combinations: Single ggplot object with faceted plots.
#' If >5 combinations: Nested list structure:
#' \describe{
#'   \item{plots}{List of plots organized by animal then date}
#' }
#'
#' @details
#' The function creates timeline plots with:
#' - X-axis: Time of day (hours)
#' - Y-axis: Date (vertical for single plots, horizontal for multiple stacked plots)
#' - Points: Individual feeding visits
#' - Colors: meal_id using categorical color palette (customizable outlier color for meal_id = 0)
#' - Title: Customizable prefix + animal ID (e.g., "Animal XX")
#'
#' @examples
#' # Create toy visit data with meal assignments
#' toy_data <- all_fed[[1]][which(all_fed[[1]]$cow == 5114),]
#' 
#' # Cluster and label meals
#' labeled <- meal_label_visits(toy_data, id_col = 'cow', start_col = 'start', 
#' end_col = 'end', bin_col = 'bin', intake_col = 'intake', dur_col = 'duration',
#' tz = 'America/Vancouver')
#' 
#' # Customize colors and text
#' p <- viz_meal_clusters(labeled, id_col = 'cow', start_col = 'start',
#'                        color_palette = "Dark 3", text_size = 14, 
#'                        title_prefix = "Cow")
#' 
#' @export
viz_meal_clusters <- function(data,
                             point_size = 2,
                             point_alpha = 0.7,
                             ncol_facet = 1,
                             date_format = "%Y-%m-%d",
                             time_breaks = "4 hours",
                             time_labels = "%H",
                             color_palette = "Set 3",
                             outlier_color = "grey50",
                             title_prefix = "Animal",
                             text_size = 12,
                             title_size = NULL,
                             id_col = id_col2(),
                             start_col = start_col2(),
                             tz = tz2()) {
  
  # Set default title_size if not provided
  if (is.null(title_size)) {
    title_size <- text_size + 2
  }
  
  # Validate inputs
  if (is.null(data)) {
    stop("data must be provided")
  }
  
  # Handle different input structures
  if (is.data.frame(data)) {
    combined_data <- data
  } else if (is.list(data)) {
    if (length(data) == 0) {
      stop("data list is empty")
    }
    
    if (!all(sapply(data, is.data.frame))) {
      stop("All items in data list must be dataframes")
    }
    
    # Combine list of dataframes
    combined_data <- do.call(rbind, data)
    rownames(combined_data) <- NULL
  } else {
    stop("data must be a dataframe or list of dataframes")
  }
  
  # Validate required columns
  required_cols <- c(id_col, start_col, "meal_id", "date")
  missing_cols <- setdiff(required_cols, names(combined_data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Handle empty data
  if (nrow(combined_data) == 0) {
    warning("No data to plot")
    return(ggplot2::ggplot() + 
           ggplot2::labs(title = "No data to plot") + 
           ggplot2::theme_minimal(base_size = text_size))
  }
  
  # Ensure proper data types
  combined_data[[start_col]] <- lubridate::as_datetime(combined_data[[start_col]], tz = tz)
  combined_data$date <- lubridate::date(combined_data[[start_col]])
  combined_data$meal_id <- as.integer(combined_data$meal_id)
  
  # Count unique animal-day combinations
  animal_day_combinations <- combined_data |>
    dplyr::distinct(.data[[id_col]], date) |>
    nrow()
  
  # Create time of day column for plotting
  combined_data$time_of_day <- combined_data[[start_col]]
  
  # Decide return structure based on number of combinations
  if (animal_day_combinations <= 5) {
    # Create single faceted plot
    return(create_faceted_plot(combined_data, point_size, point_alpha, 
                              ncol_facet, date_format, time_breaks, 
                              time_labels, color_palette, outlier_color, 
                              title_prefix, text_size, id_col, tz))
  } else {
    # Create nested list structure
    plot_list <- create_nested_plot_list(combined_data, point_size, point_alpha,
                                         date_format, time_breaks, time_labels,
                                         color_palette, outlier_color, title_prefix, 
                                         text_size, title_size, id_col, tz)
    
    return(plot_list)
  }
}

# -----------------------------------------------------------------------------#
# ------------------------- Internal helper functions ------------------------#
# -----------------------------------------------------------------------------#

#' Create a single plot for one animal on one day
#'
#' @param animal_day_data Data for single animal-day combination
#' @param point_size Size of points
#' @param point_alpha Transparency of points
#' @param date_format Format for date labels
#' @param time_breaks Time axis breaks
#' @param time_labels Time axis label format
#' @param color_palette Color palette name for meal colors
#' @param outlier_color Color for outliers
#' @param title_prefix Prefix for plot titles
#' @param text_size Base text size
#' @param title_size Size of plot title
#' @param id_col Name of animal ID column
#' @param single_plot Logical, if TRUE creates plot suitable for single display
#' @param tz Timezone for x-axis limits
#'
#' @return ggplot object
#' @noRd
#' @keywords internal
create_single_animal_day_plot <- function(animal_day_data,
                                         point_size = 2,
                                         point_alpha = 0.7,
                                         date_format = "%Y-%m-%d",
                                         time_breaks = "4 hours",
                                         time_labels = "%H",
                                         color_palette = "Set 3",
                                         outlier_color = "grey50",
                                         title_prefix = "Animal",
                                         text_size = 12,
                                         title_size = 14,
                                         id_col = id_col2(),
                                         single_plot = FALSE,
                                         tz = tz2()) {
  
  if (nrow(animal_day_data) == 0) {
    return(ggplot2::ggplot() + 
           ggplot2::labs(title = "No data") + 
           ggplot2::theme_minimal(base_size = text_size))
  }
  
  # Get animal ID for title
  animal_id <- animal_day_data[[id_col]][1]
  
  # Create meal_id factor for consistent coloring
  meal_ids <- unique(animal_day_data$meal_id)
  meal_ids <- meal_ids[order(meal_ids)]
  
  # Create color palette using hcl.colors for categorical data
  n_meals <- length(meal_ids[meal_ids > 0])
  if (n_meals > 0) {
    meal_colors <- grDevices::hcl.colors(n_meals, palette = color_palette)
    names(meal_colors) <- as.character(meal_ids[meal_ids > 0])
  } else {
    meal_colors <- character(0)
  }
  
  # Add outlier color
  if (0 %in% meal_ids) {
    meal_colors <- c("0" = outlier_color, meal_colors)
  }
  
  animal_day_data$meal_id_factor <- factor(animal_day_data$meal_id)
  
  # Create plot title
  plot_title <- if (is.null(title_prefix) || title_prefix == "") {
    NULL
  } else {
    paste(title_prefix, animal_id)
  }
  
  # Set up 24-hour x-axis limits
  date_for_limits <- animal_day_data$date[1]
  midnight_start <- lubridate::as_datetime(paste(date_for_limits, "00:00:00"), tz = tz)
  midnight_end <- lubridate::as_datetime(paste(date_for_limits, "23:59:59"), tz = tz)
  
  # Create base plot
  p <- ggplot2::ggplot(animal_day_data, 
                       ggplot2::aes(x = time_of_day, y = date,
                                   color = meal_id_factor)) +
    ggplot2::geom_point(size = point_size, alpha = point_alpha) +
    ggplot2::scale_color_manual(values = meal_colors,
                               name = "Meal ID",
                               labels = function(x) ifelse(x == "0", "Outlier", paste("Meal", x))) +
    ggplot2::scale_x_datetime(date_breaks = time_breaks,
                             date_labels = time_labels,
                             limits = c(midnight_start, midnight_end)) +
    ggplot2::labs(title = plot_title,
                  x = "Time of Day",
                  y = ifelse(single_plot, "Date", "")) +
    ggplot2::theme_minimal(base_size = text_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = title_size, hjust = 0.5),
      axis.text.x = ggplot2::element_text(angle = 0),
      axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5)
    )
  
  # Adjust y-axis for single plot vs stacked plots
  if (single_plot) {
    p <- p + ggplot2::scale_y_date(date_labels = date_format)
  } else {
    p <- p + 
      ggplot2::scale_y_date(date_labels = date_format) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5))
  }
  
  return(p)
}

#' Create faceted plot for multiple animal-day combinations
#'
#' @param combined_data Combined data for all animal-day combinations
#' @param point_size Size of points
#' @param point_alpha Transparency of points
#' @param ncol_facet Number of columns for faceting
#' @param date_format Format for date labels
#' @param time_breaks Time axis breaks
#' @param time_labels Time axis label format
#' @param color_palette Color palette name for meal colors
#' @param outlier_color Color for outliers
#' @param title_prefix Prefix for plot titles
#' @param text_size Base text size
#' @param id_col Name of animal ID column
#' @param tz Timezone for x-axis limits
#'
#' @return ggplot object with facets
#' @noRd
#' @keywords internal
create_faceted_plot <- function(combined_data,
                               point_size = 2,
                               point_alpha = 0.7,
                               ncol_facet = 1,
                               date_format = "%Y-%m-%d",
                               time_breaks = "4 hours",
                               time_labels = "%H",
                               color_palette = "Set 3",
                               outlier_color = "grey50",
                               title_prefix = "Animal",
                               text_size = 12,
                               id_col = id_col2(),
                               tz = tz2()) {
  
  # Create facet label with only animal ID (no date)
  if (is.null(title_prefix) || title_prefix == "") {
    combined_data$facet_label <- as.character(combined_data[[id_col]])
  } else {
    combined_data$facet_label <- paste(title_prefix, combined_data[[id_col]])
  }
  
  # Create meal_id factor for consistent coloring
  meal_ids <- unique(combined_data$meal_id)
  meal_ids <- meal_ids[order(meal_ids)]
  
  # Create color palette using hcl.colors for categorical data
  n_meals <- length(meal_ids[meal_ids > 0])
  if (n_meals > 0) {
    meal_colors <- grDevices::hcl.colors(n_meals, palette = color_palette)
    names(meal_colors) <- as.character(meal_ids[meal_ids > 0])
  } else {
    meal_colors <- character(0)
  }
  
  # Add outlier color 
  if (0 %in% meal_ids) {
    meal_colors <- c("0" = outlier_color, meal_colors)
  }
  
  combined_data$meal_id_factor <- factor(combined_data$meal_id)
  
  # Create plot title
  plot_title <- if (is.null(title_prefix) || title_prefix == "") {
    "Meal Clustering Results"
  } else {
    "Meal Clustering Results"
  }
  
  # Set up 24-hour x-axis limits using the first date in the data
  first_date <- combined_data$date[1]
  midnight_start <- lubridate::as_datetime(paste(first_date, "00:00:00"), tz = tz)
  midnight_end <- lubridate::as_datetime(paste(first_date, "23:59:59"), tz = tz)
  
  # Create faceted plot
  p <- ggplot2::ggplot(combined_data, 
                       ggplot2::aes(x = time_of_day, y = date,
                                   color = meal_id_factor)) +
    ggplot2::geom_point(size = point_size, alpha = point_alpha) +
    ggplot2::scale_color_manual(values = meal_colors,
                               name = "Meal ID",
                               labels = function(x) ifelse(x == "0", "Outlier", paste("Meal", x))) +
    ggplot2::scale_x_datetime(date_breaks = time_breaks,
                             date_labels = time_labels,
                             limits = c(midnight_start, midnight_end)) +
    ggplot2::scale_y_date(date_labels = date_format) +
    ggplot2::facet_wrap(~ facet_label, ncol = ncol_facet, scales = "free_y") +
    ggplot2::labs(title = plot_title,
                  x = "Time of Day",
                  y = "Date") +
    ggplot2::theme_minimal(base_size = text_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = text_size + 2, hjust = 0.5),
      strip.text = ggplot2::element_text(size = text_size - 1),
      axis.text.x = ggplot2::element_text(angle = 0),
      axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5)
    )
  
  return(p)
}

#' Create nested list of plots organized by animal then date
#'
#' @param combined_data Combined data for all animal-day combinations
#' @param point_size Size of points
#' @param point_alpha Transparency of points
#' @param date_format Format for date labels
#' @param time_breaks Time axis breaks
#' @param time_labels Time axis label format
#' @param color_palette Color palette name for meal colors
#' @param outlier_color Color for outliers
#' @param title_prefix Prefix for plot titles
#' @param text_size Base text size
#' @param title_size Size of plot titles
#' @param id_col Name of animal ID column
#' @param tz Timezone for x-axis limits
#'
#' @return Nested list of plots
#' @noRd
#' @keywords internal
create_nested_plot_list <- function(combined_data,
                                   point_size = 2,
                                   point_alpha = 0.7,
                                   date_format = "%Y-%m-%d",
                                   time_breaks = "4 hours",
                                   time_labels = "%H",
                                   color_palette = "Set 3",
                                   outlier_color = "grey50",
                                   title_prefix = "Animal",
                                   text_size = 12,
                                   title_size = 14,
                                   id_col = id_col2(),
                                   tz = tz2()) {
  
  # Get unique animals
  unique_animals <- unique(combined_data[[id_col]])
  
  # Create nested list structure
  plot_list <- list()
  
  for (animal in unique_animals) {
    animal_data <- combined_data[combined_data[[id_col]] == animal, ]
    unique_dates <- unique(animal_data$date)
    
    animal_plots <- list()
    
    for (date_index in seq_along(unique_dates)) {
      animal_day_data <- animal_data[animal_data$date == unique_dates[date_index], ]
      
      # Create plot for this animal-day combination
      plot <- create_single_animal_day_plot(
        animal_day_data, point_size, point_alpha, date_format,
        time_breaks, time_labels, color_palette, outlier_color, 
        title_prefix, text_size, title_size, id_col,
        single_plot = TRUE, tz = tz
      )
      
      # Use formatted date string as key instead of date object
      date_key <- as.character(unique_dates[date_index])
      animal_plots[[date_key]] <- plot
    }
    
    plot_list[[as.character(animal)]] <- animal_plots
  }
  
  return(plot_list)
} 