# -----------------------------------------------------------------------------#
# -------------------- External user-facing functions -------------------------#
# -----------------------------------------------------------------------------#

#' Combine multiple days for one animal with pagination
#'
#' @description
#' Takes plots for multiple days of one animal and creates paginated plots 
#' with a user-defined number of plots per page to avoid overcrowding.
#'
#' @param plot_list Nested list of plots from [viz_meal_clusters()] 
#' @param animal_id Character or numeric. Animal ID to combine plots for
#' @param plots_per_page Integer. Number of plots to include per page (default: 5)
#' @param method Character. Method for combining plots: "vertical" or "grid" (default: "vertical")
#' @param title_prefix Character. Prefix for page titles (default: NULL)
#'
#' @return Named list of combined ggplot/patchwork objects, with names "1", "2", "3", etc.
#'
#' @examples
#' \dontrun{
#' # Assuming you have results from viz_meal_clusters with >10 combinations
#' result <- viz_meal_clusters(large_dataset)
#' paginated_plots <- combine_animal_plots(result$plots, animal_id = "101", plots_per_page = 6)
#' # Access individual pages
#' paginated_plots[["1"]]  # First page
#' paginated_plots[["2"]]  # Second page
#' }
#'
#' @export
combine_animal_plots <- function(plot_list, 
                                animal_id, 
                                plots_per_page = 5,
                                method = c("vertical", "grid"),
                                title_prefix = NULL) {
  
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
  
  if (!is.numeric(plots_per_page) || plots_per_page < 1) {
    stop("plots_per_page must be a positive integer")
  }
  
  plots_per_page <- as.integer(plots_per_page)
  
  # Create paginated plots
  paginated_plots <- create_paginated_plots(animal_plots, plots_per_page, method, title_prefix)
  
  return(paginated_plots)
}

#' Combine multiple animals for one date with pagination
#'
#' @description
#' Takes plots for multiple animals on one date and creates paginated plots 
#' with a user-defined number of plots per page to avoid overcrowding.
#'
#' @param plot_list Nested list of plots from [viz_meal_clusters()]
#' @param date Character or Date. Date to combine plots for (format: "YYYY-MM-DD")
#' @param plots_per_page Integer. Number of plots to include per page (default: 5)
#' @param method Character. Method for combining plots: "vertical" or "grid" (default: "vertical")
#' @param title_prefix Character. Prefix for page titles (default: NULL)
#'
#' @return Named list of combined ggplot/patchwork objects, with names "1", "2", "3", etc.
#'
#' @examples
#' \dontrun{
#' # Assuming you have results from viz_meal_clusters with >10 combinations
#' result <- viz_meal_clusters(large_dataset)
#' paginated_plots <- combine_date_plots(result$plots, date = "2023-01-01", plots_per_page = 6)
#' # Access individual pages
#' paginated_plots[["1"]]  # First page
#' paginated_plots[["2"]]  # Second page
#' }
#'
#' @export
combine_date_plots <- function(plot_list, 
                              date, 
                              plots_per_page = 5,
                              method = c("vertical", "grid"),
                              title_prefix = NULL) {
  
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
  
  if (!is.numeric(plots_per_page) || plots_per_page < 1) {
    stop("plots_per_page must be a positive integer")
  }
  
  plots_per_page <- as.integer(plots_per_page)
  
  # Create paginated plots
  paginated_plots <- create_paginated_plots(date_plots, plots_per_page, method, title_prefix)
  
  return(paginated_plots)
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

#' Create paginated plots from a list of plots
#'
#' @param plots List of ggplot objects
#' @param plots_per_page Integer. Number of plots per page
#' @param method Character. Combination method ("vertical" or "grid")
#' @param title_prefix Character. Prefix for page titles
#'
#' @return Named list of combined plots with names "1", "2", "3", etc.
#' @noRd
#' @keywords internal
create_paginated_plots <- function(plots, plots_per_page, method, title_prefix) {
  
  n_plots <- length(plots)
  
  # If only one plot or plots fit in one page, return single page
  if (n_plots <= plots_per_page) {
    if (n_plots == 1) {
      # Single plot, return as is but in a list
      return(list("1" = plots[[1]]))
    } else {
      # Multiple plots but fit in one page
      if (method == "vertical") {
        combined_plot <- combine_plots_vertical(plots, title_prefix)
      } else {
        combined_plot <- combine_plots_grid(plots, title_prefix)
      }
      return(list("1" = combined_plot))
    }
  }
  
  # Calculate number of pages needed
  n_pages <- ceiling(n_plots / plots_per_page)
  
  # Create paginated plots
  paginated_plots <- list()
  
  for (page in 1:n_pages) {
    start_idx <- (page - 1) * plots_per_page + 1
    end_idx <- min(page * plots_per_page, n_plots)
    
    page_plots <- plots[start_idx:end_idx]
    page_title <- title_prefix
    
    if (method == "vertical") {
      combined_plot <- combine_plots_vertical(page_plots, page_title)
    } else {
      combined_plot <- combine_plots_grid(page_plots, page_title)
    }
    
    paginated_plots[[as.character(page)]] <- combined_plot
  }
  
  return(paginated_plots)
}

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