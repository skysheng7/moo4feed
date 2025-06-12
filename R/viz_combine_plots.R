# -----------------------------------------------------------------------------#
# -------------------- External user-facing functions -------------------------#
# -----------------------------------------------------------------------------#

#' Combine multiple days for one animal
#'
#' @description
#' Takes plots for multiple days of one animal and combines them into a single 
#' plot using either vertical stacking or grid arrangement.
#'
#' @param plot_list Nested list of plots from [viz_meal_clusters()] 
#' @param animal_id Character or numeric. Animal ID to combine plots for
#' @param method Character. Method for combining plots: "vertical" or "grid" (default: "vertical")
#' @param title Character. Optional custom title for the combined plot
#'
#' @return Combined ggplot object or patchwork object
#'
#' @examples
#' \dontrun{
#' # Assuming you have results from viz_meal_clusters with >10 combinations
#' result <- viz_meal_clusters(large_dataset)
#' combined_plot <- combine_animal_plots(result$plots, animal_id = "101")
#' }
#'
#' @export
combine_animal_plots <- function(plot_list, 
                                animal_id, 
                                method = c("vertical", "grid"),
                                title = NULL) {
  
  method <- match.arg(method)
  
  # Validate inputs
  if (!is.list(plot_list)) {
    stop("plot_list must be a list")
  }
  
  animal_id <- as.character(animal_id)
  
  if (!animal_id %in% names(plot_list)) {
    stop("Animal ID '", animal_id, "' not found in plot_list. Available animals: ", 
         paste(names(plot_list), collapse = ", "))
  }
  
  animal_plots <- plot_list[[animal_id]]
  
  if (length(animal_plots) == 0) {
    stop("No plots found for animal ", animal_id)
  }
  
  if (length(animal_plots) == 1) {
    # Only one plot, return as is
    return(animal_plots[[1]])
  }
  
  # Default title
  if (is.null(title)) {
    title <- paste("Animal", animal_id, "- All Days")
  }
  
  # Combine plots based on method
  if (method == "vertical") {
    combined_plot <- combine_plots_vertical(animal_plots, title)
  } else {
    combined_plot <- combine_plots_grid(animal_plots, title)
  }
  
  return(combined_plot)
}

#' Combine multiple animals for one date
#'
#' @description
#' Takes plots for multiple animals on one date and combines them into a single 
#' plot using either vertical stacking or grid arrangement.
#'
#' @param plot_list Nested list of plots from [viz_meal_clusters()]
#' @param date Character or Date. Date to combine plots for (format: "YYYY-MM-DD")
#' @param method Character. Method for combining plots: "vertical" or "grid" (default: "grid")
#' @param title Character. Optional custom title for the combined plot
#'
#' @return Combined ggplot object or patchwork object
#'
#' @examples
#' \dontrun{
#' # Assuming you have results from viz_meal_clusters with >10 combinations
#' result <- viz_meal_clusters(large_dataset)
#' combined_plot <- combine_date_plots(result$plots, date = "2023-01-01")
#' }
#'
#' @export
combine_date_plots <- function(plot_list, 
                              date, 
                              method = c("grid", "vertical"),
                              title = NULL) {
  
  method <- match.arg(method)
  
  # Validate inputs
  if (!is.list(plot_list)) {
    stop("plot_list must be a list")
  }
  
  date <- as.character(as.Date(date))
  
  # Extract plots for the specified date from all animals
  date_plots <- list()
  
  for (animal_id in names(plot_list)) {
    animal_plots <- plot_list[[animal_id]]
    
    if (date %in% names(animal_plots)) {
      date_plots[[animal_id]] <- animal_plots[[date]]
    }
  }
  
  if (length(date_plots) == 0) {
    stop("No plots found for date ", date)
  }
  
  if (length(date_plots) == 1) {
    # Only one plot, return as is
    return(date_plots[[1]])
  }
  
  # Default title
  if (is.null(title)) {
    title <- paste("All Animals -", date)
  }
  
  # Combine plots based on method
  if (method == "vertical") {
    combined_plot <- combine_plots_vertical(date_plots, title)
  } else {
    combined_plot <- combine_plots_grid(date_plots, title)
  }
  
  return(combined_plot)
}

#' Create small multiples overview plot
#'
#' @description
#' Creates an overview plot showing all animal-day combinations as small multiples
#' in a grid layout.
#'
#' @param plot_list Nested list of plots from [viz_meal_clusters()]
#' @param ncol Numeric. Number of columns for the grid layout (default: 3)
#' @param title_size Numeric. Size of individual plot titles (default: 8)
#' @param main_title Character. Main title for the overview plot
#'
#' @return Combined ggplot object using patchwork
#'
#' @examples
#' \dontrun{
#' # Assuming you have results from viz_meal_clusters with >10 combinations
#' result <- viz_meal_clusters(large_dataset)
#' overview_plot <- create_overview_plot(result$plots, ncol = 4)
#' }
#'
#' @export
create_overview_plot <- function(plot_list, 
                                ncol = 3, 
                                title_size = 8,
                                main_title = "Meal Clustering Overview") {
  
  # Validate inputs
  if (!is.list(plot_list)) {
    stop("plot_list must be a list")
  }
  
  # Flatten the nested list to get all individual plots
  all_plots <- list()
  
  for (animal_id in names(plot_list)) {
    animal_plots <- plot_list[[animal_id]]
    
    for (date in names(animal_plots)) {
      plot_name <- paste(animal_id, date, sep = "_")
      
      # Modify plot to have smaller title and remove legend for overview
      plot <- animal_plots[[date]] +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = title_size),
          legend.position = "none",
          axis.text = ggplot2::element_text(size = 6),
          axis.title = ggplot2::element_text(size = 7)
        )
      
      all_plots[[plot_name]] <- plot
    }
  }
  
  if (length(all_plots) == 0) {
    stop("No plots found in plot_list")
  }
  
  # Use patchwork to combine all plots
  combined_plot <- patchwork::wrap_plots(all_plots, ncol = ncol) +
    patchwork::plot_annotation(title = main_title,
                              theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 14, hjust = 0.5)))
  
  return(combined_plot)
}

#' Extract subset of plots by animal and/or date
#'
#' @description
#' Extracts a subset of plots from the nested list structure based on specified
#' animals and/or dates.
#'
#' @param plot_list Nested list of plots from [viz_meal_clusters()]
#' @param animals Character vector. Animal IDs to extract (default: NULL for all animals)
#' @param dates Character vector or Date vector. Dates to extract in "YYYY-MM-DD" format 
#'   (default: NULL for all dates)
#'
#' @return Nested list with same structure as input but containing only specified subsets
#'
#' @examples
#' \dontrun{
#' # Assuming you have results from viz_meal_clusters with >10 combinations
#' result <- viz_meal_clusters(large_dataset)
#' 
#' # Extract plots for specific animals
#' subset1 <- extract_plots(result$plots, animals = c("101", "102"))
#' 
#' # Extract plots for specific dates
#' subset2 <- extract_plots(result$plots, dates = c("2023-01-01", "2023-01-02"))
#' 
#' # Extract plots for specific animals and dates
#' subset3 <- extract_plots(result$plots, 
#'                         animals = c("101", "102"), 
#'                         dates = "2023-01-01")
#' }
#'
#' @export
extract_plots <- function(plot_list, 
                         animals = NULL, 
                         dates = NULL) {
  
  # Validate inputs
  if (!is.list(plot_list)) {
    stop("plot_list must be a list")
  }
  
  # Convert dates to character if provided
  if (!is.null(dates)) {
    dates <- as.character(as.Date(dates))
  }
  
  # Convert animals to character if provided
  if (!is.null(animals)) {
    animals <- as.character(animals)
  }
  
  # Filter by animals if specified
  if (!is.null(animals)) {
    missing_animals <- setdiff(animals, names(plot_list))
    if (length(missing_animals) > 0) {
      warning("Animals not found in plot_list: ", paste(missing_animals, collapse = ", "))
    }
    
    plot_list <- plot_list[names(plot_list) %in% animals]
  }
  
  # Filter by dates if specified
  if (!is.null(dates)) {
    for (animal_id in names(plot_list)) {
      animal_plots <- plot_list[[animal_id]]
      
      missing_dates <- setdiff(dates, names(animal_plots))
      if (length(missing_dates) > 0) {
        warning("Dates not found for animal ", animal_id, ": ", 
               paste(missing_dates, collapse = ", "))
      }
      
      plot_list[[animal_id]] <- animal_plots[names(animal_plots) %in% dates]
    }
    
    # Remove animals with no plots after date filtering
    plot_list <- plot_list[sapply(plot_list, length) > 0]
  }
  
  return(plot_list)
}

# -----------------------------------------------------------------------------#
# ------------------------- Internal helper functions ------------------------#
# -----------------------------------------------------------------------------#

#' Combine plots vertically using patchwork
#'
#' @param plots List of ggplot objects
#' @param title Main title for combined plot
#'
#' @return patchwork object
#' @noRd
#' @keywords internal
combine_plots_vertical <- function(plots, title) {
  
  # Remove individual plot titles to avoid clutter
  plots_no_titles <- lapply(plots, function(p) {
    p + ggplot2::theme(plot.title = ggplot2::element_blank())
  })
  
  # Combine vertically
  combined_plot <- patchwork::wrap_plots(plots_no_titles, ncol = 1) +
    patchwork::plot_annotation(title = title,
                              theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 14, hjust = 0.5)))
  
  return(combined_plot)
}

#' Combine plots in grid layout using patchwork
#'
#' @param plots List of ggplot objects
#' @param title Main title for combined plot
#'
#' @return patchwork object
#' @noRd  
#' @keywords internal
combine_plots_grid <- function(plots, title) {
  
  # Calculate optimal grid dimensions
  n_plots <- length(plots)
  ncol <- ceiling(sqrt(n_plots))
  
  # Remove individual plot titles to avoid clutter
  plots_no_titles <- lapply(plots, function(p) {
    p + ggplot2::theme(plot.title = ggplot2::element_blank())
  })
  
  # Combine in grid
  combined_plot <- patchwork::wrap_plots(plots_no_titles, ncol = ncol) +
    patchwork::plot_annotation(title = title,
                              theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 14, hjust = 0.5)))
  
  return(combined_plot)
} 