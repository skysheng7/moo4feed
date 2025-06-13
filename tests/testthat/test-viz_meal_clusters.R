# -----------------------------------------------------------------------------#
#                    Tests for viz_meal_clusters()                             #
# -----------------------------------------------------------------------------#

# Helper function to create toy data for testing viz_meal_clusters
create_meal_test_data <- function(n_animals = 2, n_days = 2, n_visits_per_day = 10) {
  set.seed(123)
  
  data_list <- list()
  
  for (day in 1:n_days) {
    date_val <- lubridate::ymd("2024-01-01") + lubridate::days(day - 1)
    
    # Create visits for each animal
    all_visits <- data.frame()
    
    for (animal in 1:n_animals) {
      animal_id <- 1000 + animal
      
      # Generate visit times throughout the day
      base_times <- lubridate::ymd_hms(paste(date_val, "03:00:00"), tz = tz2()) + 
        lubridate::hours(sort(sample(0:18, n_visits_per_day, replace = TRUE)))
      
      visits <- data.frame(
        transponder = 2000 + seq_len(n_visits_per_day),
        cow = animal_id,
        bin = sample(1:10, n_visits_per_day, replace = TRUE),
        start = base_times,
        end = base_times + lubridate::minutes(sample(5:30, n_visits_per_day, replace = TRUE)),
        duration = sample(300:1800, n_visits_per_day, replace = TRUE),
        intake = runif(n_visits_per_day, 0.5, 5),
        start_weight = runif(n_visits_per_day, 10, 20),
        end_weight = runif(n_visits_per_day, 5, 15),
        date = date_val
      )
      
      all_visits <- rbind(all_visits, visits)
    }
    
    # Rename columns to match global variable system
    names(all_visits)[names(all_visits) == "cow"] <- id_col2()
    names(all_visits)[names(all_visits) == "bin"] <- bin_col2()
    names(all_visits)[names(all_visits) == "duration"] <- duration_col2()
    names(all_visits)[names(all_visits) == "intake"] <- intake_col2()
    
    data_list[[as.character(date_val)]] <- all_visits
  }
  
  return(data_list)
}

# Helper function to create meal-labeled data
create_meal_labeled_data <- function(n_animals = 2, n_days = 2, n_visits_per_day = 10) {
  data_list <- create_meal_test_data(n_animals, n_days, n_visits_per_day)
  
  # Add meal clustering results to each day's data
  for (i in seq_along(data_list)) {
    df <- data_list[[i]]
    n_visits <- nrow(df)
    
    # Create realistic meal assignments
    # Most visits get assigned to meals (1-4), some are outliers (0)
    meal_ids <- c(
      rep(1, ceiling(n_visits * 0.3)),
      rep(2, ceiling(n_visits * 0.3)), 
      rep(3, ceiling(n_visits * 0.2)),
      rep(4, ceiling(n_visits * 0.1)),
      rep(0, ceiling(n_visits * 0.1))  # outliers
    )[1:n_visits]
    
    df$meal_id <- sample(meal_ids)
    
    # Add meal summary columns (required by merge_cluster_results output)
    df$meal_start <- df[[start_col2()]]
    df$meal_end <- df[[start_col2()]] + lubridate::hours(2)
    df$meal_duration <- 7200  # 2 hours in seconds
    df$total_intake <- df[[intake_col2()]] * 2
    df$visit_count <- sample(3:8, n_visits, replace = TRUE)
    
    data_list[[i]] <- df
  }
  
  return(data_list)
}

# Helper function to create single dataframe from list
create_single_meal_data <- function() {
  data_list <- create_meal_labeled_data(n_animals = 1, n_days = 1, n_visits_per_day = 15)
  return(data_list[[1]])
}

# Helper function to create data with many animal-day combinations (>5)
create_large_meal_data <- function() {
  return(create_meal_labeled_data(n_animals = 3, n_days = 3, n_visits_per_day = 8))
}

# -----------------------------------------------------------------------------#
#                      Test for Normal Usage                                   #
# -----------------------------------------------------------------------------#

test_that("viz_meal_clusters works with single dataframe (≤5 combinations)", {
  test_data <- create_single_meal_data()
  
  # Test the function
  p <- viz_meal_clusters(test_data)
  
  # Check that it returns a ggplot object
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$x, "Time of Day")
  expect_equal(p$labels$y, "Date")
  expect_equal(p$labels$title, "Meal Clustering Results")
  
  # Check that faceting is used
  expect_true("FacetWrap" %in% class(p$facet))
})

test_that("viz_meal_clusters works with list input (≤5 combinations)", {
  test_data <- create_meal_labeled_data(n_animals = 2, n_days = 1, n_visits_per_day = 10)
  
  # Test with list input
  p <- viz_meal_clusters(test_data)
  
  # Check that it returns a ggplot object
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Meal Clustering Results")
  
  # Check that faceting is used for multiple animals
  expect_true("FacetWrap" %in% class(p$facet))
})

test_that("viz_meal_clusters returns nested list for >5 combinations", {
  test_data <- create_large_meal_data()  # 3 animals × 3 days = 9 combinations
  
  # Test the function
  result <- viz_meal_clusters(test_data)
  
  # Check that it returns a nested list structure
  expect_type(result, "list")
  expect_equal(length(result), 3)  # 3 animals
  
  # Check that each animal has plots for each day
  for (animal_plots in result) {
    expect_type(animal_plots, "list")
    expect_equal(length(animal_plots), 3)  # 3 days
    
    # Check that each day has a ggplot
    for (day_plot in animal_plots) {
      expect_s3_class(day_plot, "ggplot")
    }
  }
})

test_that("viz_meal_clusters works with custom parameters", {
  test_data <- create_single_meal_data()
  
  # Test with custom parameters
  p <- viz_meal_clusters(
    test_data,
    point_size = 3,
    point_alpha = 0.5,
    ncol_facet = 2,
    date_format = "%m-%d",
    time_breaks = "2 hours",
    time_labels = "%H:%M",
    color_palette = "Dark 3",
    outlier_color = "red",
    title_prefix = "Cow",
    text_size = 14,
    title_size = 16
  )
  
  # Check that it returns a ggplot object
  expect_s3_class(p, "ggplot")
  
  # Check that custom parameters are applied
  expect_equal(p$labels$title, "Meal Clustering Results")
  
  # Check theme text size
  expect_equal(p$theme$text$size, 14)
  expect_equal(p$theme$plot.title$size, 16)
})

test_that("viz_meal_clusters works with custom column names", {
  test_data <- create_single_meal_data()
  
  # Rename columns
  names(test_data)[names(test_data) == id_col2()] <- "animal_id"
  names(test_data)[names(test_data) == start_col2()] <- "start_time"
  
  # Test with custom column names
  p <- viz_meal_clusters(
    test_data,
    id_col = "animal_id",
    start_col = "start_time"
  )
  
  # Check that it returns a ggplot object
  expect_s3_class(p, "ggplot")
})

test_that("viz_meal_clusters works with different timezones", {
  test_data <- create_single_meal_data()
  
  # Convert to different timezone
  test_data[[start_col2()]] <- lubridate::with_tz(test_data[[start_col2()]], "America/New_York")
  
  # Test with different timezone
  p <- viz_meal_clusters(test_data, tz = "America/New_York")
  
  # Check that it returns a ggplot object
  expect_s3_class(p, "ggplot")
})

test_that("viz_meal_clusters handles no title prefix", {
  test_data <- create_single_meal_data()
  
  # Test with NULL title prefix
  p1 <- viz_meal_clusters(test_data, title_prefix = NULL)
  expect_s3_class(p1, "ggplot")
  
  # Test with empty title prefix
  p2 <- viz_meal_clusters(test_data, title_prefix = "")
  expect_s3_class(p2, "ggplot")
})

test_that("viz_meal_clusters works with only outliers (meal_id = 0)", {
  test_data <- create_single_meal_data()
  
  # Set all meal_ids to 0 (outliers)
  test_data$meal_id <- 0
  
  # Test the function
  p <- viz_meal_clusters(test_data)
  
  # Check that it returns a ggplot object
  expect_s3_class(p, "ggplot")
  
  # Check that outlier color is used
  scale_colors <- p$scales$scales[[1]]$palette(1)
  expect_equal(names(scale_colors), "0")
})

test_that("viz_meal_clusters works with only regular meals (no outliers)", {
  test_data <- create_single_meal_data()
  
  # Remove all outliers
  test_data <- test_data[test_data$meal_id != 0, ]
  
  # Test the function
  p <- viz_meal_clusters(test_data)
  
  # Check that it returns a ggplot object
  expect_s3_class(p, "ggplot")
})

# -----------------------------------------------------------------------------#
#                       Test for Edge Cases                                    #
# -----------------------------------------------------------------------------#

test_that("viz_meal_clusters handles empty dataframe", {
  # Create empty dataframe with correct structure
  empty_data <- data.frame(
    cow = integer(0),
    start = lubridate::as_datetime(character(0), tz = tz2()),
    meal_id = integer(0),
    date = lubridate::as_date(character(0))
  )
  names(empty_data)[names(empty_data) == "cow"] <- id_col2()
  names(empty_data)[names(empty_data) == "start"] <- start_col2()
  
  # Test with empty data
  expect_warning(p <- viz_meal_clusters(empty_data), "No data to plot")
  
  # Check that it returns a ggplot object with appropriate message
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "No data to plot")
})

test_that("viz_meal_clusters handles empty list", {
  # Test with empty list
  expect_error(viz_meal_clusters(list()), "data list is empty")
})

test_that("viz_meal_clusters handles single visit", {
  # Create data with single visit
  single_visit <- data.frame(
    cow = 1001,
    start = lubridate::ymd_hms("2024-01-01 08:00:00", tz = tz2()),
    meal_id = 1,
    date = lubridate::ymd("2024-01-01"),
    meal_start = lubridate::ymd_hms("2024-01-01 08:00:00", tz = tz2()),
    meal_end = lubridate::ymd_hms("2024-01-01 10:00:00", tz = tz2()),
    meal_duration = 7200,
    total_intake = 5.0,
    visit_count = 1
  )
  names(single_visit)[names(single_visit) == "cow"] <- id_col2()
  names(single_visit)[names(single_visit) == "start"] <- start_col2()
  
  # Test the function
  p <- viz_meal_clusters(single_visit)
  
  # Check that it returns a ggplot object
  expect_s3_class(p, "ggplot")
})

test_that("viz_meal_clusters handles data with missing meal_ids", {
  test_data <- create_single_meal_data()
  
  # Add some NA meal_ids
  test_data$meal_id[1:3] <- NA
  
  # Convert NA to integer (should become 0 or be handled)
  test_data$meal_id <- as.integer(test_data$meal_id)
  test_data$meal_id[is.na(test_data$meal_id)] <- 0
  
  # Test the function
  p <- viz_meal_clusters(test_data)
  
  # Check that it returns a ggplot object
  expect_s3_class(p, "ggplot")
})

test_that("viz_meal_clusters handles data spanning multiple days", {
  # Create data spanning multiple days for single animal
  test_data <- create_meal_labeled_data(n_animals = 1, n_days = 3, n_visits_per_day = 5)
  combined_data <- do.call(rbind, test_data)
  rownames(combined_data) <- NULL
  
  # Test the function (should create faceted plot since ≤5 combinations)
  p <- viz_meal_clusters(combined_data)
  
  # Check that it returns a ggplot object with facets
  expect_s3_class(p, "ggplot")
  expect_true("FacetWrap" %in% class(p$facet))
})

test_that("viz_meal_clusters handles large meal_id numbers", {
  test_data <- create_single_meal_data()
  
  # Set large meal_id numbers
  test_data$meal_id <- test_data$meal_id + 100
  
  # Test the function
  p <- viz_meal_clusters(test_data)
  
  # Check that it returns a ggplot object
  expect_s3_class(p, "ggplot")
})

test_that("viz_meal_clusters handles data with extreme time ranges", {
  test_data <- create_single_meal_data()
  
  # Create visits spanning full 24 hours
  n_visits <- nrow(test_data)
  # Use integer minutes to avoid lubridate validation errors
  minute_seq <- round(seq(0, 1439, length.out = n_visits))
  test_data[[start_col2()]] <- lubridate::ymd_hms("2024-01-01 00:00:00", tz = tz2()) + 
    lubridate::minutes(minute_seq)  # 0 to 23:59
  
  # Update date column
  test_data$date <- lubridate::date(test_data[[start_col2()]])
  
  # Test the function
  p <- viz_meal_clusters(test_data)
  
  # Check that it returns a ggplot object
  expect_s3_class(p, "ggplot")
})

# -----------------------------------------------------------------------------#
#                       Test for Error Handling                                #
# -----------------------------------------------------------------------------#

test_that("viz_meal_clusters errors on NULL data", {
  expect_error(viz_meal_clusters(NULL), "data must be provided")
})

test_that("viz_meal_clusters errors on invalid data type", {
  expect_error(viz_meal_clusters("not_a_dataframe"), 
               "data must be a dataframe or list of dataframes")
  expect_error(viz_meal_clusters(123), 
               "data must be a dataframe or list of dataframes")
})

test_that("viz_meal_clusters errors on list with non-dataframes", {
  invalid_list <- list(
    data.frame(a = 1),
    "not_a_dataframe"
  )
  expect_error(viz_meal_clusters(invalid_list), 
               "All items in data list must be dataframes")
})

test_that("viz_meal_clusters errors on missing required columns", {
  # Create data missing meal_id column
  incomplete_data <- data.frame(
    cow = 1001,
    start = lubridate::ymd_hms("2024-01-01 08:00:00", tz = tz2())
  )
  names(incomplete_data)[names(incomplete_data) == "cow"] <- id_col2()
  names(incomplete_data)[names(incomplete_data) == "start"] <- start_col2()
  
  expect_error(viz_meal_clusters(incomplete_data), 
               "Missing required columns: meal_id, date")
})

test_that("viz_meal_clusters errors on missing id_col", {
  test_data <- create_single_meal_data()
  names(test_data)[names(test_data) == id_col2()] <- "wrong_name"
  
  expect_error(viz_meal_clusters(test_data), 
               paste("Missing required columns:", id_col2()))
})

test_that("viz_meal_clusters errors on missing start_col", {
  test_data <- create_single_meal_data()
  names(test_data)[names(test_data) == start_col2()] <- "wrong_name"
  
  expect_error(viz_meal_clusters(test_data), 
               paste("Missing required columns:", start_col2()))
})

# -----------------------------------------------------------------------------#
#                    Test for Internal Helper Functions                        #
# -----------------------------------------------------------------------------#

test_that("create_single_animal_day_plot handles empty data", {
  # Create empty dataframe
  empty_data <- data.frame(
    cow = integer(0),
    start = lubridate::as_datetime(character(0), tz = tz2()),
    meal_id = integer(0),
    date = lubridate::as_date(character(0)),
    time_of_day = lubridate::as_datetime(character(0), tz = tz2())
  )
  names(empty_data)[names(empty_data) == "cow"] <- id_col2()
  
  # Test internal function through main function with empty data
  expect_warning(p <- viz_meal_clusters(empty_data), "No data to plot")
  expect_s3_class(p, "ggplot")
})

test_that("create_faceted_plot works with custom facet columns", {
  test_data <- create_meal_labeled_data(n_animals = 2, n_days = 1, n_visits_per_day = 5)
  combined_data <- do.call(rbind, test_data)
  rownames(combined_data) <- NULL
  
  # Test with different ncol_facet
  p <- viz_meal_clusters(combined_data, ncol_facet = 2)
  
  # Check that it returns a ggplot object with facets
  expect_s3_class(p, "ggplot")
  expect_true("FacetWrap" %in% class(p$facet))
})

test_that("create_nested_plot_list creates correct structure", {
  test_data <- create_large_meal_data()  # >5 combinations
  
  # Test the function
  result <- viz_meal_clusters(test_data)
  
  # Check nested structure
  expect_type(result, "list")
  expect_true(all(sapply(result, is.list)))
  expect_true(all(sapply(result, function(x) all(sapply(x, function(y) "ggplot" %in% class(y))))))
  
  # Check that animal names are character strings
  expect_true(all(sapply(names(result), is.character)))
  
  # Check that date keys are character strings
  for (animal_plots in result) {
    expect_true(all(sapply(names(animal_plots), is.character)))
  }
})

# -----------------------------------------------------------------------------#
#                    Test for Color and Aesthetic Options                      #
# -----------------------------------------------------------------------------#

test_that("viz_meal_clusters works with different color palettes", {
  test_data <- create_single_meal_data()
  
  # Test different color palettes
  palettes <- c("Set 3", "Dark 3", "Pastel 1", "Viridis")
  
  for (palette in palettes) {
    p <- viz_meal_clusters(test_data, color_palette = palette)
    expect_s3_class(p, "ggplot")
  }
})

test_that("viz_meal_clusters works with custom outlier colors", {
  test_data <- create_single_meal_data()
  
  # Test different outlier colors
  colors <- c("red", "black", "grey30", "#FF5733")
  
  for (color in colors) {
    p <- viz_meal_clusters(test_data, outlier_color = color)
    expect_s3_class(p, "ggplot")
  }
})

test_that("viz_meal_clusters works with extreme point sizes and alpha", {
  test_data <- create_single_meal_data()
  
  # Test extreme values
  p1 <- viz_meal_clusters(test_data, point_size = 0.1, point_alpha = 0.1)
  expect_s3_class(p1, "ggplot")
  
  p2 <- viz_meal_clusters(test_data, point_size = 10, point_alpha = 1)
  expect_s3_class(p2, "ggplot")
})

test_that("viz_meal_clusters works with different time formatting", {
  test_data <- create_single_meal_data()
  
  # Test different time formats
  p1 <- viz_meal_clusters(test_data, 
                         date_format = "%Y/%m/%d",
                         time_breaks = "1 hour",
                         time_labels = "%H:%M")
  expect_s3_class(p1, "ggplot")
  
  p2 <- viz_meal_clusters(test_data,
                         date_format = "%b %d",
                         time_breaks = "6 hours", 
                         time_labels = "%I %p")
  expect_s3_class(p2, "ggplot")
})

# -----------------------------------------------------------------------------#
#                    Test for Data Type Handling                               #
# -----------------------------------------------------------------------------#

test_that("viz_meal_clusters handles different datetime formats", {
  test_data <- create_single_meal_data()
  
  # Convert to character and back (simulating different input formats)
  test_data[[start_col2()]] <- as.character(test_data[[start_col2()]])
  test_data[[start_col2()]] <- lubridate::as_datetime(test_data[[start_col2()]], tz = tz2())
  
  # Test the function
  p <- viz_meal_clusters(test_data)
  expect_s3_class(p, "ggplot")
})

test_that("viz_meal_clusters handles meal_id as character", {
  test_data <- create_single_meal_data()
  
  # Convert meal_id to character
  test_data$meal_id <- as.character(test_data$meal_id)
  
  # Test the function
  p <- viz_meal_clusters(test_data)
  expect_s3_class(p, "ggplot")
})

test_that("viz_meal_clusters handles date as character", {
  test_data <- create_single_meal_data()
  
  # Convert date to character
  test_data$date <- as.character(test_data$date)
  
  # Test the function (should still work as date gets recalculated from start_col)
  p <- viz_meal_clusters(test_data)
  expect_s3_class(p, "ggplot")
})

# -----------------------------------------------------------------------------#
#                    Test for Boundary Conditions                              #
# -----------------------------------------------------------------------------#

test_that("viz_meal_clusters handles exactly 5 animal-day combinations", {
  # Create exactly 5 combinations (should use faceted plot)
  test_data <- create_meal_labeled_data(n_animals = 5, n_days = 1, n_visits_per_day = 5)
  combined_data <- do.call(rbind, test_data)
  rownames(combined_data) <- NULL
  
  # Test the function
  p <- viz_meal_clusters(combined_data)
  
  # Should return faceted ggplot (not nested list)
  expect_s3_class(p, "ggplot")
  expect_true("FacetWrap" %in% class(p$facet))
})

test_that("viz_meal_clusters handles exactly 6 animal-day combinations", {
  # Create exactly 6 combinations (should use nested list)
  test_data <- create_meal_labeled_data(n_animals = 6, n_days = 1, n_visits_per_day = 5)
  
  # Test the function
  result <- viz_meal_clusters(test_data)
  
  # Should return nested list
  expect_type(result, "list")
  expect_equal(length(result), 6)
})

test_that("viz_meal_clusters handles single meal ID", {
  test_data <- create_single_meal_data()
  
  # Set all visits to same meal
  test_data$meal_id <- 1
  
  # Test the function
  p <- viz_meal_clusters(test_data)
  expect_s3_class(p, "ggplot")
})

test_that("viz_meal_clusters handles many meal IDs", {
  test_data <- create_single_meal_data()
  
  # Create many different meal IDs
  n_visits <- nrow(test_data)
  test_data$meal_id <- seq_len(n_visits)  # Each visit is its own meal
  
  # Test the function
  p <- viz_meal_clusters(test_data)
  expect_s3_class(p, "ggplot")
})

# -----------------------------------------------------------------------------#
#                    Test for Parameter Validation                             #
# -----------------------------------------------------------------------------#

test_that("viz_meal_clusters handles default title_size when NULL", {
  test_data <- create_single_meal_data()
  
  # Test with NULL title_size (should use text_size + 2)
  p <- viz_meal_clusters(test_data, text_size = 10, title_size = NULL)
  expect_s3_class(p, "ggplot")
  
  # Check that title size is set correctly in theme
  expect_equal(p$theme$plot.title$size, 12)  # text_size + 2
})

test_that("viz_meal_clusters handles extreme text sizes", {
  test_data <- create_single_meal_data()
  
  # Test with very small text
  p1 <- viz_meal_clusters(test_data, text_size = 6)
  expect_s3_class(p1, "ggplot")
  
  # Test with very large text
  p2 <- viz_meal_clusters(test_data, text_size = 24)
  expect_s3_class(p2, "ggplot")
})

# -----------------------------------------------------------------------------#
#                    Test for Integration with Real Data Structure             #
# -----------------------------------------------------------------------------#

test_that("viz_meal_clusters works with merge_cluster_results output structure", {
  # Create data that mimics merge_cluster_results output
  base_data <- create_meal_test_data(n_animals = 1, n_days = 1, n_visits_per_day = 10)[[1]]
  
  # Add all columns that merge_cluster_results would add
  merged_data <- base_data
  merged_data$meal_id <- sample(0:3, nrow(base_data), replace = TRUE)
  merged_data$meal_start <- merged_data[[start_col2()]]
  merged_data$meal_end <- merged_data[[start_col2()]] + lubridate::hours(1)
  merged_data$meal_duration <- 3600
  merged_data$total_intake <- merged_data[[intake_col2()]] * 1.5
  merged_data$visit_count <- sample(1:5, nrow(base_data), replace = TRUE)
  
  # Test the function
  p <- viz_meal_clusters(merged_data)
  expect_s3_class(p, "ggplot")
})

test_that("viz_meal_clusters handles data with no meals (all outliers)", {
  test_data <- create_single_meal_data()
  
  # Set all meal_ids to 0 (outliers only)
  test_data$meal_id <- 0
  
  # Test the function - should handle case where n_meals = 0
  p <- viz_meal_clusters(test_data)
  expect_s3_class(p, "ggplot")
  
  # Check that only outlier color is in the scale
  scale_colors <- p$scales$scales[[1]]$palette(1)
  expect_equal(names(scale_colors), "0")
})

test_that("viz_meal_clusters handles mixed positive and negative meal_ids", {
  test_data <- create_single_meal_data()
  
  # Set some meal_ids to negative values (edge case)
  test_data$meal_id[1:5] <- -1
  test_data$meal_id[6:10] <- 0  # outliers
  # Rest remain positive
  
  # Test the function
  p <- viz_meal_clusters(test_data)
  expect_s3_class(p, "ggplot")
})

test_that("viz_meal_clusters handles data with duplicate dates across different animals", {
  # Create data where multiple animals have visits on the same date
  test_data1 <- create_meal_labeled_data(n_animals = 1, n_days = 1, n_visits_per_day = 5)[[1]]
  test_data2 <- create_meal_labeled_data(n_animals = 1, n_days = 1, n_visits_per_day = 5)[[1]]
  
  # Change animal ID for second dataset but keep same date
  test_data2[[id_col2()]] <- 2000
  
  # Combine data
  combined_data <- rbind(test_data1, test_data2)
  rownames(combined_data) <- NULL
  
  # Test the function
  p <- viz_meal_clusters(combined_data)
  expect_s3_class(p, "ggplot")
  expect_true("FacetWrap" %in% class(p$facet))
})

test_that("viz_meal_clusters handles data with very early and very late times", {
  test_data <- create_single_meal_data()
  
  # Set some visits to very early (midnight) and very late (23:59)
  n_visits <- nrow(test_data)
  early_times <- lubridate::ymd_hms("2024-01-01 00:00:00", tz = tz2()) + lubridate::minutes(0:4)
  late_times <- lubridate::ymd_hms("2024-01-01 23:55:00", tz = tz2()) + lubridate::minutes(0:4)
  
  if (n_visits >= 10) {
    test_data[[start_col2()]][1:5] <- early_times
    test_data[[start_col2()]][6:10] <- late_times
  }
  
  # Update date column
  test_data$date <- lubridate::date(test_data[[start_col2()]])
  
  # Test the function
  p <- viz_meal_clusters(test_data)
  expect_s3_class(p, "ggplot")
})

test_that("viz_meal_clusters handles single animal with multiple days in nested structure", {
  # Create data that will trigger nested structure (>5 combinations)
  # 1 animal × 7 days = 7 combinations (>5)
  test_data <- create_meal_labeled_data(n_animals = 1, n_days = 7, n_visits_per_day = 3)
  
  # Test the function
  result <- viz_meal_clusters(test_data)
  
  # Should return nested list
  expect_type(result, "list")
  expect_equal(length(result), 1)  # 1 animal
  
  # Check that the single animal has 7 days of plots
  animal_plots <- result[[1]]
  expect_type(animal_plots, "list")
  expect_equal(length(animal_plots), 7)  # 7 days
  
  # Check that each day has a ggplot
  for (day_plot in animal_plots) {
    expect_s3_class(day_plot, "ggplot")
  }
})

test_that("viz_meal_clusters handles data with unsorted meal_ids", {
  test_data <- create_single_meal_data()
  
  # Create unsorted meal_ids (should be handled by order() in the function)
  test_data$meal_id <- sample(c(0, 5, 2, 8, 1, 3), nrow(test_data), replace = TRUE)
  
  # Test the function
  p <- viz_meal_clusters(test_data)
  expect_s3_class(p, "ggplot")
})

test_that("viz_meal_clusters handles data where first date is not the earliest", {
  test_data <- create_meal_labeled_data(n_animals = 1, n_days = 3, n_visits_per_day = 5)
  combined_data <- do.call(rbind, test_data)
  
  # Shuffle the data so first date is not necessarily the earliest
  combined_data <- combined_data[sample(nrow(combined_data)), ]
  rownames(combined_data) <- NULL
  
  # Test the function
  p <- viz_meal_clusters(combined_data)
  expect_s3_class(p, "ggplot")
})

test_that("viz_meal_clusters handles data with time_of_day column already present", {
  test_data <- create_single_meal_data()
  
  # Add a time_of_day column (should be overwritten by the function)
  test_data$time_of_day <- lubridate::ymd_hms("2024-01-01 12:00:00", tz = tz2())
  
  # Test the function
  p <- viz_meal_clusters(test_data)
  expect_s3_class(p, "ggplot")
})

test_that("viz_meal_clusters handles facet_label creation edge cases", {
  test_data <- create_meal_labeled_data(n_animals = 2, n_days = 1, n_visits_per_day = 5)
  combined_data <- do.call(rbind, test_data)
  rownames(combined_data) <- NULL
  
  # Test with NULL title_prefix (should use animal ID only)
  p1 <- viz_meal_clusters(combined_data, title_prefix = NULL)
  expect_s3_class(p1, "ggplot")
  
  # Test with empty title_prefix (should use animal ID only)
  p2 <- viz_meal_clusters(combined_data, title_prefix = "")
  expect_s3_class(p2, "ggplot")
  
  # Test with custom title_prefix
  p3 <- viz_meal_clusters(combined_data, title_prefix = "Animal")
  expect_s3_class(p3, "ggplot")
})

test_that("viz_meal_clusters handles meal_id_factor creation with edge cases", {
  test_data <- create_single_meal_data()
  
  # Test with meal_id already as factor
  test_data$meal_id <- factor(test_data$meal_id)
  
  # Test the function
  p <- viz_meal_clusters(test_data)
  expect_s3_class(p, "ggplot")
})

test_that("viz_meal_clusters handles x-axis limits edge cases", {
  test_data <- create_single_meal_data()
  
  # Create data with visits exactly at midnight boundaries
  test_data[[start_col2()]][1] <- lubridate::ymd_hms("2024-01-01 00:00:00", tz = tz2())
  test_data[[start_col2()]][2] <- lubridate::ymd_hms("2024-01-01 23:59:59", tz = tz2())
  
  # Update date column
  test_data$date <- lubridate::date(test_data[[start_col2()]])
  
  # Test the function
  p <- viz_meal_clusters(test_data)
  expect_s3_class(p, "ggplot")
}) 