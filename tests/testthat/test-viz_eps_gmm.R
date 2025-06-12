# -----------------------------------------------------------------------------#
#                    Tests for viz_eps_gmm()                                  #
# -----------------------------------------------------------------------------#

# Helper function to create toy data for testing viz_eps_gmm
create_gmm_test_data <- function(n_animals = 2, n_visits_per_animal = 20, seed = 123) {
  set.seed(seed)
  
  # Create data with bimodal gap patterns for GMM to work well
  data_list <- list()
  
  for (animal_id in 1:n_animals) {
    # Create visits with bimodal gap patterns (within-meal and between-meal)
    start_times <- lubridate::ymd_hms("2023-01-01 08:00:00", tz = tz2())
    
    # Create short gaps (within-meal) and long gaps (between-meal)
    within_meal_gaps <- sample(c(5, 8, 12, 15), size = n_visits_per_animal %/% 2, replace = TRUE)
    between_meal_gaps <- sample(c(60, 90, 120, 150), size = n_visits_per_animal %/% 2, replace = TRUE)
    
    # Interleave gaps to create realistic meal patterns
    gaps <- numeric(n_visits_per_animal - 1)
    meal_starts <- seq(1, n_visits_per_animal - 1, by = 4)  # Start new meals every 4 visits
    
    for (i in 1:(n_visits_per_animal - 1)) {
      if (i %in% meal_starts && i > 1) {
        gaps[i] <- sample(between_meal_gaps, 1)
      } else {
        gaps[i] <- sample(within_meal_gaps, 1)
      }
    }
    
    # Calculate cumulative start times
    start_times <- start_times + lubridate::minutes(c(0, cumsum(gaps)))
    end_times <- start_times + lubridate::minutes(sample(5:15, n_visits_per_animal, replace = TRUE))
    
    animal_data <- data.frame(
      cow = rep(paste0("animal_", animal_id), n_visits_per_animal),
      start = start_times[1:n_visits_per_animal],
      end = end_times[1:n_visits_per_animal],
      date = rep(as.Date("2023-01-01"), n_visits_per_animal),
      stringsAsFactors = FALSE
    )
    
    data_list[[animal_id]] <- animal_data
  }
  
  # Combine all animals into single dataframe
  combined_data <- do.call(rbind, data_list)
  rownames(combined_data) <- NULL
  
  return(combined_data)
}

# Helper function to create minimal test data for GMM
create_minimal_gmm_data <- function() {
  data.frame(
    cow = rep("A", 15),
    start = lubridate::ymd_hms("2023-01-01 08:00:00", tz = tz2()) + 
            lubridate::minutes(c(0, 10, 20, 30, 90, 100, 110, 120, 180, 190, 200, 210, 270, 280, 290)),
    end = lubridate::ymd_hms("2023-01-01 08:00:00", tz = tz2()) + 
          lubridate::minutes(c(5, 15, 25, 35, 95, 105, 115, 125, 185, 195, 205, 215, 275, 285, 295)),
    date = rep(as.Date("2023-01-01"), 15),
    stringsAsFactors = FALSE
  )
}

# Helper function to create insufficient data for GMM (< 10 points)
create_insufficient_gmm_data <- function() {
  data.frame(
    cow = rep("A", 5),
    start = lubridate::ymd_hms(c("2023-01-01 08:00:00", "2023-01-01 08:15:00", 
                                "2023-01-01 08:30:00", "2023-01-01 09:00:00", 
                                "2023-01-01 09:15:00"), tz = tz2()),
    end = lubridate::ymd_hms(c("2023-01-01 08:10:00", "2023-01-01 08:25:00", 
                              "2023-01-01 08:40:00", "2023-01-01 09:10:00", 
                              "2023-01-01 09:25:00"), tz = tz2()),
    date = rep(as.Date("2023-01-01"), 5),
    stringsAsFactors = FALSE
  )
}

# Helper function to create list of dataframes for GMM
create_gmm_list_data <- function() {
  day1 <- create_minimal_gmm_data()
  day2 <- create_minimal_gmm_data()
  day2$start <- day2$start + lubridate::days(1)
  day2$end <- day2$end + lubridate::days(1)
  day2$date <- as.Date("2023-01-02")
  
  list("2023-01-01" = day1, "2023-01-02" = day2)
}

# -----------------------------------------------------------------------------#
#                      Test for Normal Usage                                   #
# -----------------------------------------------------------------------------#

test_that("viz_eps_gmm renders a plot with default parameters", {
  test_data <- create_gmm_test_data()
  
  # Test the function
  p <- viz_eps_gmm(test_data, id_col = "cow", start_col = "start", end_col = "end", tz = tz2())
  
  # Check that it returns a ggplot object
  expect_s3_class(p, "ggplot")
  expect_true("ggplot" %in% class(p))
  
  # Check plot components
  expect_true(!is.null(p$data))
  expect_true(!is.null(p$layers))
  expect_true(length(p$layers) >= 2)  # histogram + vertical line + possibly components
  
  # Check labels
  expect_true(grepl("Distribution of time gap", p$labels$title))
  expect_equal(p$labels$y, "Density")
})

test_that("viz_eps_gmm works with log transformation disabled", {
  test_data <- create_gmm_test_data()
  
  # Test without log transformation
  p <- viz_eps_gmm(test_data, use_log_transform = FALSE, 
                   id_col = "cow", start_col = "start", end_col = "end", tz = tz2())
  
  expect_s3_class(p, "ggplot")
  expect_false(grepl("log-transformed", p$labels$title))
  expect_equal(p$labels$x, "Visit Gap Duration (minutes)")
})

test_that("viz_eps_gmm works with log transformation enabled", {
  test_data <- create_gmm_test_data()
  
  # Test with log transformation (default)
  p <- viz_eps_gmm(test_data, use_log_transform = TRUE, 
                   id_col = "cow", start_col = "start", end_col = "end", tz = tz2())
  
  expect_s3_class(p, "ggplot")
  expect_true(grepl("log-transformed", p$labels$title))
  expect_true(grepl("log\\(", p$labels$x))
})

test_that("viz_eps_gmm works with custom log transformation parameters", {
  test_data <- create_gmm_test_data()
  
  # Test with custom log parameters
  p <- viz_eps_gmm(test_data, use_log_transform = TRUE, 
                   log_multiplier = 10, log_offset = 2,
                   id_col = "cow", start_col = "start", end_col = "end", tz = tz2())
  
  expect_s3_class(p, "ggplot")
  expect_true(grepl("log\\(.*10.*\\+.*2", p$labels$x))
})

test_that("viz_eps_gmm works with custom bounds", {
  test_data <- create_gmm_test_data()
  
  # Test with bounds
  p <- viz_eps_gmm(test_data, lower_bound = 10, upper_bound = 50, 
                   id_col = "cow", start_col = "start", end_col = "end", tz = tz2())
  
  expect_s3_class(p, "ggplot")
  
  # Test with only lower bound
  p_lower <- viz_eps_gmm(test_data, lower_bound = 10, upper_bound = NULL,
                         id_col = "cow", start_col = "start", end_col = "end", tz = tz2())
  expect_s3_class(p_lower, "ggplot")
  
  # Test with only upper bound
  p_upper <- viz_eps_gmm(test_data, lower_bound = NULL, upper_bound = 50,
                         id_col = "cow", start_col = "start", end_col = "end", tz = tz2())
  expect_s3_class(p_upper, "ggplot")
})

test_that("viz_eps_gmm works with custom aesthetic parameters", {
  test_data <- create_gmm_test_data()
  
  # Test with custom parameters
  p <- viz_eps_gmm(
    test_data,
    bins = 50,
    colors = c("blue", "red", "green", "orange"),
    title_prefix = "Custom GMM Title",
    xlim = 30,
    id_col = "cow", 
    start_col = "start", 
    end_col = "end", 
    tz = tz2()
  )
  
  expect_s3_class(p, "ggplot")
  expect_true(grepl("Custom GMM Title", p$labels$title))
})

test_that("viz_eps_gmm works with show_components disabled", {
  test_data <- create_gmm_test_data()
  
  # Test without showing components
  p <- viz_eps_gmm(test_data, show_components = FALSE,
                   id_col = "cow", start_col = "start", end_col = "end", tz = tz2())
  
  expect_s3_class(p, "ggplot")
  # Should have fewer layers when components are not shown
  expect_true(length(p$layers) >= 2)  # histogram + vertical line
})

test_that("viz_eps_gmm works with show_components enabled", {
  test_data <- create_gmm_test_data()
  
  # Test with showing components (default)
  p <- viz_eps_gmm(test_data, show_components = TRUE,
                   id_col = "cow", start_col = "start", end_col = "end", tz = tz2())
  
  expect_s3_class(p, "ggplot")
  # Should have more layers when components are shown
  expect_true(length(p$layers) >= 2)
})

test_that("viz_eps_gmm works with custom column names", {
  test_data <- create_gmm_test_data()
  names(test_data)[names(test_data) == "cow"] <- "animal_id"
  names(test_data)[names(test_data) == "start"] <- "start_time"
  names(test_data)[names(test_data) == "end"] <- "end_time"
  
  p <- viz_eps_gmm(test_data, 
                   id_col = "animal_id", 
                   start_col = "start_time", 
                   end_col = "end_time", 
                   tz = tz2())
  
  expect_s3_class(p, "ggplot")
})

# -----------------------------------------------------------------------------#
#                       Test for List Input                                    #
# -----------------------------------------------------------------------------#

test_that("viz_eps_gmm works with a list of data frames", {
  test_data_list <- create_gmm_list_data()
  
  p <- viz_eps_gmm(test_data_list, id_col = "cow", start_col = "start", end_col = "end", tz = tz2())
  
  expect_s3_class(p, "ggplot")
  expect_true(!is.null(p$data))
})

test_that("viz_eps_gmm works with single-element list", {
  test_data <- create_minimal_gmm_data()
  test_data_list <- list("2023-01-01" = test_data)
  
  p <- viz_eps_gmm(test_data_list, id_col = "cow", start_col = "start", end_col = "end", tz = tz2())
  
  expect_s3_class(p, "ggplot")
})

# -----------------------------------------------------------------------------#
#                       Test for Edge Cases                                    #
# -----------------------------------------------------------------------------#

test_that("viz_eps_gmm handles empty data gracefully", {
  empty_data <- data.frame(
    cow = character(0),
    start = lubridate::as_datetime(character(0), tz = tz2()),
    end = lubridate::as_datetime(character(0), tz = tz2()),
    date = as.Date(character(0)),
    stringsAsFactors = FALSE
  )
  
  expect_warning(
    p <- viz_eps_gmm(empty_data, id_col = "cow", start_col = "start", end_col = "end", tz = tz2()),
    "No data provided"
  )
  
  expect_s3_class(p, "ggplot")
  expect_true(grepl("No data available", p$labels$title))
})

test_that("viz_eps_gmm handles insufficient data for GMM", {
  insufficient_data <- create_insufficient_gmm_data()
  
  # Should fall back to percentile method with warning
  expect_warning(
    p <- viz_eps_gmm(insufficient_data, id_col = "cow", start_col = "start", end_col = "end", tz = tz2()),
    "GMM fitting failed"
  )
  
  expect_s3_class(p, "ggplot")
  expect_true(grepl("fallback to percentile", p$labels$subtitle))
})

test_that("viz_eps_gmm handles single visit per animal", {
  single_visit_data <- data.frame(
    cow = c("A", "B"),
    start = lubridate::ymd_hms(c("2023-01-01 08:00:00", "2023-01-01 09:00:00"), tz = tz2()),
    end = lubridate::ymd_hms(c("2023-01-01 08:10:00", "2023-01-01 09:10:00"), tz = tz2()),
    date = rep(as.Date("2023-01-01"), 2),
    stringsAsFactors = FALSE
  )
  
  expect_warning(
    p <- viz_eps_gmm(single_visit_data, id_col = "cow", start_col = "start", end_col = "end", tz = tz2()),
    "No gaps between visits found"
  )
  
  expect_s3_class(p, "ggplot")
  expect_true(grepl("No gaps found", p$labels$title))
})

test_that("viz_eps_gmm handles single animal with multiple visits", {
  single_animal_data <- create_minimal_gmm_data()
  
  p <- viz_eps_gmm(single_animal_data, id_col = "cow", start_col = "start", end_col = "end", tz = tz2())
  
  expect_s3_class(p, "ggplot")
  expect_true(!is.null(p$data))
})

test_that("viz_eps_gmm handles data with overlapping visits", {
  # Create data where visits overlap (negative gaps)
  overlapping_data <- data.frame(
    cow = rep("A", 12),
    start = lubridate::ymd_hms("2023-01-01 08:00:00", tz = tz2()) + 
            lubridate::minutes(c(0, 5, 8, 15, 18, 25, 60, 65, 68, 75, 78, 85)),
    end = lubridate::ymd_hms("2023-01-01 08:00:00", tz = tz2()) + 
          lubridate::minutes(c(10, 15, 18, 25, 28, 35, 70, 75, 78, 85, 88, 95)),
    date = rep(as.Date("2023-01-01"), 12),
    stringsAsFactors = FALSE
  )
  
  # This should still work as calculate_gaps_by_animal removes negative gaps
  p <- viz_eps_gmm(overlapping_data, id_col = "cow", start_col = "start", end_col = "end", tz = tz2())
  
  expect_s3_class(p, "ggplot")
})

test_that("viz_eps_gmm handles GMM fitting failure gracefully", {
  # Create data that might cause GMM to fail (all identical gaps)
  identical_gaps_data <- data.frame(
    cow = rep("A", 15),
    start = lubridate::ymd_hms("2023-01-01 08:00:00", tz = tz2()) + 
            lubridate::minutes(seq(0, 140, by = 10)),
    end = lubridate::ymd_hms("2023-01-01 08:00:00", tz = tz2()) + 
          lubridate::minutes(seq(5, 145, by = 10)),
    date = rep(as.Date("2023-01-01"), 15),
    stringsAsFactors = FALSE
  )
  
  # Should handle GMM failure and fall back to percentile
  p <- viz_eps_gmm(identical_gaps_data, id_col = "cow", start_col = "start", end_col = "end", tz = tz2())
  
  expect_s3_class(p, "ggplot")
})

# -----------------------------------------------------------------------------#
#                       Test for Error Handling                               #
# -----------------------------------------------------------------------------#

test_that("viz_eps_gmm errors with NULL data", {
  expect_error(
    viz_eps_gmm(NULL),
    "data cannot be NULL"
  )
})

test_that("viz_eps_gmm errors with invalid bounds", {
  test_data <- create_minimal_gmm_data()
  
  # Test negative lower_bound
  expect_error(
    viz_eps_gmm(test_data, lower_bound = -5, id_col = "cow", start_col = "start", end_col = "end"),
    "lower_bound must be a single non-negative numeric value or NULL"
  )
  
  # Test negative upper_bound
  expect_error(
    viz_eps_gmm(test_data, upper_bound = -10, id_col = "cow", start_col = "start", end_col = "end"),
    "upper_bound must be a single non-negative numeric value or NULL"
  )
  
  # Test lower_bound > upper_bound
  expect_error(
    viz_eps_gmm(test_data, lower_bound = 50, upper_bound = 30, id_col = "cow", start_col = "start", end_col = "end"),
    "lower_bound must be less than or equal to upper_bound"
  )
  
  # Test non-numeric bounds
  expect_error(
    viz_eps_gmm(test_data, lower_bound = "10", id_col = "cow", start_col = "start", end_col = "end"),
    "lower_bound must be a single non-negative numeric value or NULL"
  )
  
  # Test multiple values for bounds
  expect_error(
    viz_eps_gmm(test_data, lower_bound = c(5, 10), id_col = "cow", start_col = "start", end_col = "end"),
    "lower_bound must be a single non-negative numeric value or NULL"
  )
})

test_that("viz_eps_gmm errors with invalid bins parameter", {
  test_data <- create_minimal_gmm_data()
  
  # Test negative bins
  expect_error(
    viz_eps_gmm(test_data, bins = -10, id_col = "cow", start_col = "start", end_col = "end"),
    "bins must be a single positive integer"
  )
  
  # Test zero bins
  expect_error(
    viz_eps_gmm(test_data, bins = 0, id_col = "cow", start_col = "start", end_col = "end"),
    "bins must be a single positive integer"
  )
  
  # Test non-integer bins
  expect_error(
    viz_eps_gmm(test_data, bins = 10.5, id_col = "cow", start_col = "start", end_col = "end"),
    "bins must be a single positive integer"
  )
  
  # Test non-numeric bins
  expect_error(
    viz_eps_gmm(test_data, bins = "10", id_col = "cow", start_col = "start", end_col = "end"),
    "bins must be a single positive integer"
  )
  
  # Test multiple bins
  expect_error(
    viz_eps_gmm(test_data, bins = c(10, 20), id_col = "cow", start_col = "start", end_col = "end"),
    "bins must be a single positive integer"
  )
})

test_that("viz_eps_gmm errors with invalid colors parameter", {
  test_data <- create_minimal_gmm_data()
  
  # Test insufficient colors (need at least 3)
  expect_error(
    viz_eps_gmm(test_data, colors = c("red", "blue"), id_col = "cow", start_col = "start", end_col = "end"),
    "colors must be a character vector with at least 3 colors"
  )
  
  # Test non-character colors
  expect_error(
    viz_eps_gmm(test_data, colors = c(1, 2, 3, 4), id_col = "cow", start_col = "start", end_col = "end"),
    "colors must be a character vector with at least 3 colors"
  )
})

test_that("viz_eps_gmm errors with invalid title_prefix", {
  test_data <- create_minimal_gmm_data()
  
  # Test non-character title_prefix
  expect_error(
    viz_eps_gmm(test_data, title_prefix = 123, id_col = "cow", start_col = "start", end_col = "end"),
    "title_prefix must be a single character string"
  )
  
  # Test multiple title_prefix
  expect_error(
    viz_eps_gmm(test_data, title_prefix = c("Title1", "Title2"), id_col = "cow", start_col = "start", end_col = "end"),
    "title_prefix must be a single character string"
  )
})

test_that("viz_eps_gmm errors with invalid show_components parameter", {
  test_data <- create_minimal_gmm_data()
  
  # Test non-logical show_components
  expect_error(
    viz_eps_gmm(test_data, show_components = "TRUE", id_col = "cow", start_col = "start", end_col = "end"),
    "show_components must be a single logical value"
  )
  
  # Test multiple show_components
  expect_error(
    viz_eps_gmm(test_data, show_components = c(TRUE, FALSE), id_col = "cow", start_col = "start", end_col = "end"),
    "show_components must be a single logical value"
  )
})

test_that("viz_eps_gmm errors with invalid use_log_transform parameter", {
  test_data <- create_minimal_gmm_data()
  
  # Test non-logical use_log_transform
  expect_error(
    viz_eps_gmm(test_data, use_log_transform = "TRUE", id_col = "cow", start_col = "start", end_col = "end"),
    "use_log_transform must be a single logical value"
  )
  
  # Test multiple use_log_transform
  expect_error(
    viz_eps_gmm(test_data, use_log_transform = c(TRUE, FALSE), id_col = "cow", start_col = "start", end_col = "end"),
    "use_log_transform must be a single logical value"
  )
})

test_that("viz_eps_gmm errors with invalid data types", {
  # Test with non-dataframe, non-list input
  expect_error(
    viz_eps_gmm("not a dataframe"),
    "data must be a dataframe or list of dataframes"
  )
  
  # Test with list containing non-dataframes
  invalid_list <- list("day1" = "not a dataframe", "day2" = data.frame(x = 1))
  expect_error(
    viz_eps_gmm(invalid_list),
    "All items in the list must be dataframes"
  )
  
  # Test with empty list
  expect_error(
    viz_eps_gmm(list()),
    "data list cannot be empty"
  )
})

test_that("viz_eps_gmm errors with missing required columns", {
  test_data <- create_minimal_gmm_data()
  
  # Test missing id column
  test_data_no_id <- test_data[, !names(test_data) %in% "cow"]
  expect_error(
    viz_eps_gmm(test_data_no_id, id_col = "cow", start_col = "start", end_col = "end"),
    "Missing required columns: cow"
  )
  
  # Test missing start column
  test_data_no_start <- test_data[, !names(test_data) %in% "start"]
  expect_error(
    viz_eps_gmm(test_data_no_start, id_col = "cow", start_col = "start", end_col = "end"),
    "Missing required columns: start"
  )
  
  # Test missing end column
  test_data_no_end <- test_data[, !names(test_data) %in% "end"]
  expect_error(
    viz_eps_gmm(test_data_no_end, id_col = "cow", start_col = "start", end_col = "end"),
    "Missing required columns: end"
  )
  
  # Test missing multiple columns
  test_data_minimal <- test_data[, "date", drop = FALSE]
  expect_error(
    viz_eps_gmm(test_data_minimal, id_col = "cow", start_col = "start", end_col = "end"),
    "Missing required columns: cow, start, end"
  )
})

# -----------------------------------------------------------------------------#
#                    Test Integration with meal_interval                       #
# -----------------------------------------------------------------------------#

test_that("viz_eps_gmm integrates correctly with meal_interval function", {
  test_data <- create_gmm_test_data(n_animals = 3, n_visits_per_animal = 25)
  
  # Get eps from meal_interval directly
  direct_eps <- meal_interval(test_data, method = "gmm", use_log_transform = FALSE,
                             id_col = "cow", start_col = "start", end_col = "end", tz = tz2())
  
  # Create plot and check that it uses the same eps
  p <- viz_eps_gmm(test_data, use_log_transform = FALSE,
                   id_col = "cow", start_col = "start", end_col = "end", tz = tz2())
  
  expect_s3_class(p, "ggplot")
  
  # Check that the subtitle contains the eps value (allowing for some numerical precision differences)
  expect_true(grepl(round(direct_eps, 1), p$labels$subtitle, fixed = TRUE) ||
              grepl(round(direct_eps, 2), p$labels$subtitle, fixed = TRUE))
})

# -----------------------------------------------------------------------------#
#                    Test Different Timezone Handling                         #
# -----------------------------------------------------------------------------#

test_that("viz_eps_gmm works with different timezones", {
  test_data <- create_minimal_gmm_data()
  
  # Test with UTC timezone
  p_utc <- viz_eps_gmm(test_data, id_col = "cow", start_col = "start", end_col = "end", tz = "UTC")
  expect_s3_class(p_utc, "ggplot")
  
  # Test with different timezone
  p_est <- viz_eps_gmm(test_data, id_col = "cow", start_col = "start", end_col = "end", tz = "America/New_York")
  expect_s3_class(p_est, "ggplot")
})

# -----------------------------------------------------------------------------#
#                    Test Plot Components and Structure                       #
# -----------------------------------------------------------------------------#

test_that("viz_eps_gmm creates correct plot structure", {
  test_data <- create_gmm_test_data()
  
  p <- viz_eps_gmm(test_data, id_col = "cow", start_col = "start", end_col = "end", tz = tz2())
  
  # Check that plot has correct minimum number of layers
  expect_true(length(p$layers) >= 2)  # histogram + vertical line + possibly components
  
  # Check that data is properly formatted
  expect_true("gap_minutes" %in% names(p$data))
  expect_true(is.numeric(p$data$gap_minutes))
  expect_true(all(p$data$gap_minutes >= 0))  # No negative gaps
})

test_that("viz_eps_gmm handles xlim parameter correctly", {
  test_data <- create_gmm_test_data()
  
  # Test with custom xlim
  p <- viz_eps_gmm(test_data, xlim = 50, id_col = "cow", start_col = "start", end_col = "end", tz = tz2())
  
  expect_s3_class(p, "ggplot")
  
  # Check that x-axis limits are set correctly
  x_limits <- ggplot2::layer_scales(p)$x$limits
  expect_equal(x_limits, c(0, 50))
})

test_that("viz_eps_gmm handles density histogram correctly", {
  test_data <- create_gmm_test_data()
  
  p <- viz_eps_gmm(test_data, id_col = "cow", start_col = "start", end_col = "end", tz = tz2())
  
  # Check that the first layer is a density histogram
  expect_true("GeomBar" %in% class(p$layers[[1]]$geom))
  
  # Check that y aesthetic is density
  expect_true("ggplot2::after_stat(density)" %in% as.character(p$layers[[1]]$mapping$y))
})

# -----------------------------------------------------------------------------#
#                    Test Log Transformation Edge Cases                       #
# -----------------------------------------------------------------------------#

test_that("viz_eps_gmm handles log transformation with custom parameters", {
  test_data <- create_gmm_test_data()
  
  # Test with different log multiplier and offset
  p1 <- viz_eps_gmm(test_data, use_log_transform = TRUE, log_multiplier = 5, log_offset = 0.5,
                    id_col = "cow", start_col = "start", end_col = "end", tz = tz2())
  expect_s3_class(p1, "ggplot")
  
  # Test with log_multiplier = 1 and log_offset = 0
  p2 <- viz_eps_gmm(test_data, use_log_transform = TRUE, log_multiplier = 1, log_offset = 0,
                    id_col = "cow", start_col = "start", end_col = "end", tz = tz2())
  expect_s3_class(p2, "ggplot")
})

test_that("viz_eps_gmm subtitle reflects method used", {
  test_data <- create_gmm_test_data()
  
  # Test successful GMM
  p_gmm <- viz_eps_gmm(test_data, use_log_transform = FALSE,
                       id_col = "cow", start_col = "start", end_col = "end", tz = tz2())
  expect_s3_class(p_gmm, "ggplot")
  expect_false(grepl("fallback", p_gmm$labels$subtitle))
  
  # Test fallback case with insufficient data
  insufficient_data <- create_insufficient_gmm_data()
  expect_warning(
    p_fallback <- viz_eps_gmm(insufficient_data, id_col = "cow", start_col = "start", end_col = "end", tz = tz2()),
    "GMM fitting failed"
  )
  expect_s3_class(p_fallback, "ggplot")
  expect_true(grepl("fallback", p_fallback$labels$subtitle))
}) 