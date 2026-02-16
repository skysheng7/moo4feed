# Test file for viz_combine_plots.R
# Testing combine_animal_plots, combine_date_plots, extract_plots and internal helpers

# -----------------------------------------------------------------------------#
#                           Helper Functions for Testing                       #
# -----------------------------------------------------------------------------#

# Create mock ggplot objects for testing
create_mock_plot <- function(title = "Test Plot") {
  ggplot2::ggplot(data.frame(x = 1:5, y = 1:5), ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point() +
    ggplot2::labs(title = title)
}

# Create nested plot list structure (animal -> date -> plot)
create_test_plot_list <- function(n_animals = 2, n_days = 3) {
  plot_list <- list()
  
  for (i in 1:n_animals) {
    animal_id <- as.character(1000 + i)
    animal_plots <- list()
    
    for (j in 1:n_days) {
      date <- as.character(as.Date("2024-01-01") + j - 1)
      plot_title <- paste("Animal", animal_id, "Date", date)
      animal_plots[[date]] <- create_mock_plot(plot_title)
    }
    
    plot_list[[animal_id]] <- animal_plots
  }
  
  return(plot_list)
}

# Create large plot list for testing pagination
create_large_plot_list <- function(n_animals = 3, n_days = 5) {
  create_test_plot_list(n_animals, n_days)
}

# Create single animal plot list
create_single_animal_plots <- function(n_days = 6) {
  animal_plots <- list()
  
  for (j in 1:n_days) {
    date <- as.character(as.Date("2024-01-01") + j - 1)
    plot_title <- paste("Single Animal Date", date)
    animal_plots[[date]] <- create_mock_plot(plot_title)
  }
  
  return(list("5114" = animal_plots))
}

# -----------------------------------------------------------------------------#
#                      Test combine_animal_plots Function                      #
# -----------------------------------------------------------------------------#

test_that("combine_animal_plots works with default parameters", {
  plot_list <- create_test_plot_list(n_animals = 2, n_days = 3)
  
  # Test with existing animal
  result <- combine_animal_plots(plot_list, animal_id = "1001")
  
  # Should return a named list
  expect_type(result, "list")
  expect_true(all(names(result) %in% c("1", "2")))  # Page numbers
  
  # Each element should be a patchwork/ggplot object
  for (page in result) {
    expect_true("ggplot" %in% class(page) || "patchwork" %in% class(page))
  }
})

test_that("combine_animal_plots works with custom plots_per_page", {
  plot_list <- create_single_animal_plots(n_days = 6)
  
  # Test with 2 plots per page (should create 3 pages)
  result <- combine_animal_plots(plot_list, animal_id = "5114", plots_per_page = 2)
  
  expect_equal(length(result), 3)  # 6 plots / 2 per page = 3 pages
  expect_equal(names(result), c("1", "2", "3"))
})

test_that("combine_animal_plots works with single plot", {
  # Create single plot
  single_plot_list <- list("1001" = list("2024-01-01" = create_mock_plot()))
  
  result <- combine_animal_plots(single_plot_list, animal_id = "1001")
  
  expect_equal(length(result), 1)
  expect_equal(names(result), "1")
  expect_s3_class(result[["1"]], "ggplot")
})

test_that("combine_animal_plots works with vertical method", {
  plot_list <- create_single_animal_plots(n_days = 3)
  
  result <- combine_animal_plots(plot_list, animal_id = "5114", method = "vertical")
  
  expect_type(result, "list")
  expect_true("patchwork" %in% class(result[["1"]]) || "ggplot" %in% class(result[["1"]]))
})

test_that("combine_animal_plots works with grid method", {
  plot_list <- create_single_animal_plots(n_days = 4)
  
  result <- combine_animal_plots(plot_list, animal_id = "5114", method = "grid")
  
  expect_type(result, "list")
  expect_true("patchwork" %in% class(result[["1"]]) || "ggplot" %in% class(result[["1"]]))
})

test_that("combine_animal_plots works with custom title_prefix", {
  plot_list <- create_single_animal_plots(n_days = 2)
  
  result <- combine_animal_plots(plot_list, animal_id = "5114", 
                                title_prefix = "Custom Title")
  
  expect_type(result, "list")
  expect_equal(length(result), 1)
})

test_that("combine_animal_plots handles numeric animal_id", {
  plot_list <- create_test_plot_list(n_animals = 1, n_days = 2)
  
  # Test with numeric animal_id (should be converted to character)
  result <- combine_animal_plots(plot_list, animal_id = 1001)
  
  expect_type(result, "list")
  expect_equal(length(result), 1)
})

test_that("combine_animal_plots handles plots_per_page edge cases", {
  plot_list <- create_single_animal_plots(n_days = 5)
  
  # Test with plots_per_page = 1 (each plot on separate page)
  result1 <- combine_animal_plots(plot_list, animal_id = "5114", plots_per_page = 1)
  expect_equal(length(result1), 5)
  
  # Test with plots_per_page larger than number of plots
  result2 <- combine_animal_plots(plot_list, animal_id = "5114", plots_per_page = 10)
  expect_equal(length(result2), 1)
})

# -----------------------------------------------------------------------------#
#                       Test combine_date_plots Function                       #
# -----------------------------------------------------------------------------#

test_that("combine_date_plots works with default parameters", {
  plot_list <- create_test_plot_list(n_animals = 3, n_days = 2)
  
  # Test with existing date
  result <- combine_date_plots(plot_list, date = "2024-01-01")
  
  # Should return a named list
  expect_type(result, "list")
  expect_true(all(names(result) %in% c("1", "2")))  # Page numbers
  
  # Each element should be a patchwork/ggplot object
  for (page in result) {
    expect_true("ggplot" %in% class(page) || "patchwork" %in% class(page))
  }
})

test_that("combine_date_plots works with Date object input", {
  plot_list <- create_test_plot_list(n_animals = 2, n_days = 2)
  
  # Test with Date object
  result <- combine_date_plots(plot_list, date = as.Date("2024-01-01"))
  
  expect_type(result, "list")
  expect_equal(length(result), 1)  # 2 animals, default 4 plots per page
})

test_that("combine_date_plots works with custom plots_per_page", {
  plot_list <- create_test_plot_list(n_animals = 6, n_days = 1)
  
  # Test with 2 plots per page (should create 3 pages for 6 animals)
  result <- combine_date_plots(plot_list, date = "2024-01-01", plots_per_page = 2)
  
  expect_equal(length(result), 3)  # 6 animals / 2 per page = 3 pages
  expect_equal(names(result), c("1", "2", "3"))
})

test_that("combine_date_plots works with different methods", {
  plot_list <- create_test_plot_list(n_animals = 3, n_days = 1)
  
  # Test vertical method
  result_v <- combine_date_plots(plot_list, date = "2024-01-01", method = "vertical")
  expect_type(result_v, "list")
  
  # Test grid method
  result_g <- combine_date_plots(plot_list, date = "2024-01-01", method = "grid")
  expect_type(result_g, "list")
})

test_that("combine_date_plots handles single animal for date", {
  plot_list <- create_test_plot_list(n_animals = 1, n_days = 3)
  
  result <- combine_date_plots(plot_list, date = "2024-01-01")
  
  expect_equal(length(result), 1)
  expect_equal(names(result), "1")
})

test_that("combine_date_plots handles missing date for some animals", {
  plot_list <- create_test_plot_list(n_animals = 3, n_days = 2)
  
  # Remove date from one animal
  plot_list[["1001"]][["2024-01-02"]] <- NULL
  
  # Should still work with remaining animals that have the date
  result <- combine_date_plots(plot_list, date = "2024-01-02")
  
  expect_type(result, "list")
  expect_equal(length(result), 1)  # 2 remaining animals
})

# -----------------------------------------------------------------------------#
#                        Test extract_plots Function                           #
# -----------------------------------------------------------------------------#

test_that("extract_plots works with animal filtering", {
  plot_list <- create_test_plot_list(n_animals = 3, n_days = 2)
  
  # Extract plots for specific animals
  result <- extract_plots(plot_list, animals = c("1001", "1002"))
  
  expect_type(result, "list")
  expect_equal(length(result), 2)
  expect_equal(names(result), c("1001", "1002"))
  
  # Each animal should have 2 days of plots
  for (animal_plots in result) {
    expect_equal(length(animal_plots), 2)
  }
})

test_that("extract_plots works with date filtering", {
  plot_list <- create_test_plot_list(n_animals = 2, n_days = 3)
  
  # Extract plots for specific dates
  result <- extract_plots(plot_list, dates = c("2024-01-01", "2024-01-03"))
  
  expect_type(result, "list")
  expect_equal(length(result), 2)  # 2 animals
  
  # Each animal should have 2 days of plots (filtered dates)
  for (animal_plots in result) {
    expect_equal(length(animal_plots), 2)
    expect_true(all(names(animal_plots) %in% c("2024-01-01", "2024-01-03")))
  }
})

test_that("extract_plots works with both animal and date filtering", {
  plot_list <- create_test_plot_list(n_animals = 3, n_days = 3)
  
  # Extract plots for specific animals and dates
  result <- extract_plots(plot_list, 
                         animals = c("1001", "1003"), 
                         dates = "2024-01-02")
  
  expect_type(result, "list")
  expect_equal(length(result), 2)  # 2 animals
  expect_equal(names(result), c("1001", "1003"))
  
  # Each animal should have 1 day of plots
  for (animal_plots in result) {
    expect_equal(length(animal_plots), 1)
    expect_equal(names(animal_plots), "2024-01-02")
  }
})

test_that("extract_plots works with Date objects", {
  plot_list <- create_test_plot_list(n_animals = 2, n_days = 2)
  
  # Test with Date objects
  result <- extract_plots(plot_list, dates = as.Date(c("2024-01-01", "2024-01-02")))
  
  expect_type(result, "list")
  expect_equal(length(result), 2)
})

test_that("extract_plots works with numeric animal IDs", {
  plot_list <- create_test_plot_list(n_animals = 2, n_days = 2)
  
  # Test with numeric animal IDs (should be converted to character)
  result <- extract_plots(plot_list, animals = c(1001, 1002))
  
  expect_type(result, "list")
  expect_equal(length(result), 2)
})

test_that("extract_plots returns full list when no filters applied", {
  plot_list <- create_test_plot_list(n_animals = 2, n_days = 2)
  
  # No filtering
  result <- extract_plots(plot_list)
  
  expect_identical(result, plot_list)
})

test_that("extract_plots removes animals with no plots after date filtering", {
  plot_list <- create_test_plot_list(n_animals = 3, n_days = 2)
  
  # Remove specific date from one animal
  plot_list[["1002"]] <- plot_list[["1002"]][names(plot_list[["1002"]]) != "2024-01-01"]
  
  # Filter for the removed date
  result <- extract_plots(plot_list, dates = "2024-01-01")
  
  # Should only have 2 animals (1002 should be removed)
  expect_equal(length(result), 2)
  expect_false("1002" %in% names(result))
})

# -----------------------------------------------------------------------------#
#                         Test Error Handling                                  #
# -----------------------------------------------------------------------------#

test_that("combine_animal_plots errors with invalid inputs", {
  plot_list <- create_test_plot_list(n_animals = 2, n_days = 2)
  
  # Test non-list input
  expect_error(
    combine_animal_plots("not a list", animal_id = "1001"),
    "plot_list must be a list"
  )
  
  # Test non-existent animal ID
  expect_error(
    combine_animal_plots(plot_list, animal_id = "9999"),
    "Animal ID '9999' not found in plot_list"
  )
  
  # Test invalid plots_per_page
  expect_error(
    combine_animal_plots(plot_list, animal_id = "1001", plots_per_page = 0),
    "plots_per_page must be a positive integer"
  )
  
  expect_error(
    combine_animal_plots(plot_list, animal_id = "1001", plots_per_page = -1),
    "plots_per_page must be a positive integer"
  )
  
  expect_error(
    combine_animal_plots(plot_list, animal_id = "1001", plots_per_page = "invalid"),
    "plots_per_page must be a positive integer"
  )
})

test_that("combine_animal_plots errors with empty animal plots", {
  # Create plot list with empty animal
  plot_list <- list("1001" = list())
  
  expect_error(
    combine_animal_plots(plot_list, animal_id = "1001"),
    "No plots found for animal 1001"
  )
})

test_that("combine_date_plots errors with invalid inputs", {
  plot_list <- create_test_plot_list(n_animals = 2, n_days = 2)
  
  # Test non-list input
  expect_error(
    combine_date_plots("not a list", date = "2024-01-01"),
    "plot_list must be a list"
  )
  
  # Test non-existent date
  expect_error(
    combine_date_plots(plot_list, date = "2024-12-31"),
    "No plots found for date 2024-12-31"
  )
  
  # Test invalid plots_per_page
  expect_error(
    combine_date_plots(plot_list, date = "2024-01-01", plots_per_page = 0),
    "plots_per_page must be a positive integer"
  )
  
  expect_error(
    combine_date_plots(plot_list, date = "2024-01-01", plots_per_page = -5),
    "plots_per_page must be a positive integer"
  )
})

test_that("extract_plots errors with invalid inputs", {
  plot_list <- create_test_plot_list(n_animals = 2, n_days = 2)
  
  # Test non-list input
  expect_error(
    extract_plots("not a list"),
    "plot_list must be a list"
  )
})

test_that("extract_plots warns about missing animals and dates", {
  plot_list <- create_test_plot_list(n_animals = 2, n_days = 2)
  
  # Test warning for missing animals
  expect_warning(
    result <- extract_plots(plot_list, animals = c("1001", "9999")),
    "Animals not found in plot_list: 9999"
  )
  expect_equal(length(result), 1)  # Only 1001 should remain
  
  # Test warning for missing dates
  expect_warning(
    result <- extract_plots(plot_list, dates = c("2024-01-01", "2024-12-31")),
    "Dates not found for animal"
  )
})

# -----------------------------------------------------------------------------#
#                    Test Internal Helper Functions                            #
# -----------------------------------------------------------------------------#

test_that("create_paginated_plots handles single plot correctly", {
  plots <- list(create_mock_plot("Single Plot"))
  
  # Test internal function directly (accessing via :::)
  result <- moo4feed:::create_paginated_plots(plots, 4, "vertical", "Test Title")
  
  expect_equal(length(result), 1)
  expect_equal(names(result), "1")
  expect_s3_class(result[["1"]], "ggplot")
})

test_that("create_paginated_plots handles multiple plots fitting in one page", {
  plots <- list(
    create_mock_plot("Plot 1"),
    create_mock_plot("Plot 2"),
    create_mock_plot("Plot 3")
  )
  
  # 3 plots with 4 per page should create 1 page
  result <- moo4feed:::create_paginated_plots(plots, 4, "vertical", "Test Title")
  
  expect_equal(length(result), 1)
  expect_equal(names(result), "1")
  expect_true("patchwork" %in% class(result[["1"]]))
})

test_that("create_paginated_plots handles multiple pages correctly", {
  plots <- list(
    create_mock_plot("Plot 1"),
    create_mock_plot("Plot 2"),
    create_mock_plot("Plot 3"),
    create_mock_plot("Plot 4"),
    create_mock_plot("Plot 5")
  )
  
  # 5 plots with 2 per page should create 3 pages
  result <- moo4feed:::create_paginated_plots(plots, 2, "vertical", "Test Title")
  
  expect_equal(length(result), 3)
  expect_equal(names(result), c("1", "2", "3"))
  
  # Each page should be a patchwork object
  for (page in result) {
    expect_true("patchwork" %in% class(page))
  }
})

test_that("create_paginated_plots works with grid method", {
  plots <- list(
    create_mock_plot("Plot 1"),
    create_mock_plot("Plot 2"),
    create_mock_plot("Plot 3"),
    create_mock_plot("Plot 4")
  )
  
  result <- moo4feed:::create_paginated_plots(plots, 4, "grid", "Grid Title")
  
  expect_equal(length(result), 1)
  expect_true("patchwork" %in% class(result[["1"]]))
})

test_that("combine_plots_vertical removes individual titles", {
  plots <- list(
    create_mock_plot("Individual Title 1"),
    create_mock_plot("Individual Title 2")
  )
  
  result <- moo4feed:::combine_plots_vertical(plots, "Main Title")
  
  expect_true("patchwork" %in% class(result))
  
  # Check that main title is set
  expect_equal(result$patches$annotation$title, "Main Title")
})

test_that("combine_plots_grid calculates correct grid dimensions", {
  # Test with different numbers of plots
  plots_4 <- list(
    create_mock_plot("Plot 1"),
    create_mock_plot("Plot 2"),
    create_mock_plot("Plot 3"),
    create_mock_plot("Plot 4")
  )
  
  result_4 <- moo4feed:::combine_plots_grid(plots_4, "Grid Title")
  expect_true("patchwork" %in% class(result_4))
  
  # Test with 9 plots (should create 3x3 grid)
  plots_9 <- replicate(9, create_mock_plot("Plot"), simplify = FALSE)
  result_9 <- moo4feed:::combine_plots_grid(plots_9, "Grid Title")
  expect_true("patchwork" %in% class(result_9))
})

test_that("combine_plots_vertical and combine_plots_grid handle NULL title", {
  plots <- list(create_mock_plot("Plot 1"), create_mock_plot("Plot 2"))
  
  # Test with NULL title
  result_v <- moo4feed:::combine_plots_vertical(plots, NULL)
  expect_true("patchwork" %in% class(result_v))
  
  result_g <- moo4feed:::combine_plots_grid(plots, NULL)
  expect_true("patchwork" %in% class(result_g))
})

# -----------------------------------------------------------------------------#
#                         Test Edge Cases                                      #
# -----------------------------------------------------------------------------#

test_that("functions handle empty plot lists gracefully", {
  # Test with empty nested structure
  empty_plot_list <- list("1001" = list())
  
  expect_error(
    combine_animal_plots(empty_plot_list, animal_id = "1001"),
    "No plots found for animal 1001"
  )
})

test_that("functions handle very large plots_per_page values", {
  plot_list <- create_single_animal_plots(n_days = 3)
  
  # Test with plots_per_page much larger than available plots
  result <- combine_animal_plots(plot_list, animal_id = "5114", plots_per_page = 1000)
  
  expect_equal(length(result), 1)
  expect_equal(names(result), "1")
})

test_that("functions handle fractional plots_per_page (should be converted to integer)", {
  plot_list <- create_single_animal_plots(n_days = 4)
  
  # Test with fractional plots_per_page (should be converted to integer)
  result <- combine_animal_plots(plot_list, animal_id = "5114", plots_per_page = 2.7)
  
  expect_equal(length(result), 2)  # 4 plots / 2 per page = 2 pages
})

test_that("extract_plots handles empty results after filtering", {
  plot_list <- create_test_plot_list(n_animals = 2, n_days = 2)
  
  # Filter for non-existent animals
  expect_warning(
    result <- extract_plots(plot_list, animals = "9999"),
    "Animals not found in plot_list: 9999"
  )
  
  expect_equal(length(result), 0)
})

test_that("functions handle plot lists with mixed data types in names", {
  # Create plot list with mixed naming
  plot_list <- list()
  plot_list[["001"]] <- list("2024-01-01" = create_mock_plot())  # String with leading zeros
  plot_list[["1002"]] <- list("2024-01-01" = create_mock_plot()) # Regular string
  
  # Should work with string animal ID
  result <- combine_animal_plots(plot_list, animal_id = "001")
  expect_equal(length(result), 1)
  
  # Should work with extraction
  result2 <- extract_plots(plot_list, animals = c("001", "1002"))
  expect_equal(length(result2), 2)
})

test_that("functions handle dates at year boundaries", {
  # Create plots with dates spanning year boundary
  plot_list <- list(
    "1001" = list(
      "2023-12-31" = create_mock_plot("New Year's Eve"),
      "2024-01-01" = create_mock_plot("New Year's Day")
    )
  )
  
  result <- combine_date_plots(plot_list, date = "2023-12-31")
  expect_equal(length(result), 1)
  
  result2 <- extract_plots(plot_list, dates = c("2023-12-31", "2024-01-01"))
  expect_equal(length(result2[["1001"]]), 2)
})

# -----------------------------------------------------------------------------#
#                    Test Method Parameter Validation                          #
# -----------------------------------------------------------------------------#

test_that("functions validate method parameter correctly", {
  plot_list <- create_single_animal_plots(n_days = 2)
  
  # Test invalid method
  expect_error(
    combine_animal_plots(plot_list, animal_id = "5114", method = "invalid"),
    "'arg' should be one of"
  )
  
  # Test partial matching
  result1 <- combine_animal_plots(plot_list, animal_id = "5114", method = "vert")
  expect_type(result1, "list")
  
  result2 <- combine_animal_plots(plot_list, animal_id = "5114", method = "g")
  expect_type(result2, "list")
})

test_that("combine_date_plots validates method parameter correctly", {
  plot_list <- create_test_plot_list(n_animals = 2, n_days = 1)
  
  # Test invalid method
  expect_error(
    combine_date_plots(plot_list, date = "2024-01-01", method = "invalid"),
    "'arg' should be one of"
  )
})

# -----------------------------------------------------------------------------#
#                    Test Integration Scenarios                                #
# -----------------------------------------------------------------------------#

test_that("functions work together in realistic workflow", {
  # Create large plot list
  plot_list <- create_large_plot_list(n_animals = 4, n_days = 6)
  
  # Extract subset
  subset_plots <- extract_plots(plot_list, 
                               animals = c("1001", "1002"), 
                               dates = c("2024-01-01", "2024-01-02", "2024-01-03"))
  
  expect_equal(length(subset_plots), 2)
  expect_equal(length(subset_plots[["1001"]]), 3)
  
  # Combine animal plots from subset
  animal_combined <- combine_animal_plots(subset_plots, animal_id = "1001", plots_per_page = 2)
  expect_equal(length(animal_combined), 2)  # 3 plots / 2 per page = 2 pages
  
  # Combine date plots from subset
  date_combined <- combine_date_plots(subset_plots, date = "2024-01-01")
  expect_equal(length(date_combined), 1)  # 2 animals fit in 1 page
})

test_that("functions preserve plot quality and structure", {
  plot_list <- create_test_plot_list(n_animals = 1, n_days = 2)
  
  # Test that original plots are preserved in structure
  result <- combine_animal_plots(plot_list, animal_id = "1001")
  
  # Test extraction preserves structure
  extracted <- extract_plots(plot_list, animals = "1001")
  expect_identical(extracted, plot_list)
}) 