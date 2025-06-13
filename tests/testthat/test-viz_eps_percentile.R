# -----------------------------------------------------------------------------#
#                    Tests for viz_eps_percentile()                           #
# -----------------------------------------------------------------------------#

# Helper function to create toy data for testing viz_eps_percentile
create_viz_test_data <- function(n_animals = 2, n_visits_per_animal = 10, seed = 123) {
  set.seed(seed)
  
  # Create data with known gap patterns for predictable testing
  data_list <- list()
  
  for (animal_id in 1:n_animals) {
    # Create visits with specific gap patterns
    start_times <- lubridate::ymd_hms("2023-01-01 08:00:00", tz = tz2())
    
    # Create short gaps (within-meal) and long gaps (between-meal)
    gaps <- c(rep(c(5, 10, 15), length.out = n_visits_per_animal - 1))  # Short gaps
    if (n_visits_per_animal > 5) {
      gaps[5] <- 90  # Add one long gap
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

# Helper function to create minimal test data
create_minimal_test_data <- function() {
  data.frame(
    cow = c("A", "A", "A"),
    start = lubridate::ymd_hms(c("2023-01-01 08:00:00", "2023-01-01 08:15:00", "2023-01-01 09:00:00"), tz = tz2()),
    end = lubridate::ymd_hms(c("2023-01-01 08:10:00", "2023-01-01 08:25:00", "2023-01-01 09:10:00"), tz = tz2()),
    date = rep(as.Date("2023-01-01"), 3),
    stringsAsFactors = FALSE
  )
}

# Helper function to create list of dataframes
create_list_test_data <- function() {
  day1 <- create_minimal_test_data()
  day2 <- create_minimal_test_data()
  day2$start <- day2$start + lubridate::days(1)
  day2$end <- day2$end + lubridate::days(1)
  day2$date <- as.Date("2023-01-02")
  
  list("2023-01-01" = day1, "2023-01-02" = day2)
}

# -----------------------------------------------------------------------------#
#                      Test for Normal Usage                                   #
# -----------------------------------------------------------------------------#

test_that("viz_eps_percentile renders a plot with default parameters", {
  test_data <- create_viz_test_data()
  
  # Test the function
  p <- viz_eps_percentile(test_data, id_col = "cow", start_col = "start", end_col = "end", tz = tz2())
  
  # Check that it returns a ggplot object
  expect_s3_class(p, "ggplot")
  expect_true("ggplot" %in% class(p))
  
  # Check plot components
  expect_true(!is.null(p$data))
  expect_true(!is.null(p$layers))
  expect_true(length(p$layers) >= 2)  # histogram + vertical line
  
  # Check labels
  expect_true(grepl("Distribution of time gap", p$labels$title))
  expect_equal(p$labels$x, "Visit Gap Duration (minutes)")
  expect_equal(p$labels$y, "Frequency")
})

test_that("viz_eps_percentile works with custom percentile", {
  test_data <- create_viz_test_data()
  
  # Test with different percentiles
  p80 <- viz_eps_percentile(test_data, percentile = 0.8, id_col = "cow", start_col = "start", end_col = "end", tz = tz2())
  p95 <- viz_eps_percentile(test_data, percentile = 0.95, id_col = "cow", start_col = "start", end_col = "end", tz = tz2())
  
  expect_s3_class(p80, "ggplot")
  expect_s3_class(p95, "ggplot")
  
  # Check that titles reflect the percentile
  expect_true(grepl("80th Percentile", p80$labels$title))
  expect_true(grepl("95th Percentile", p95$labels$title))
})

test_that("viz_eps_percentile works with custom bounds", {
  test_data <- create_viz_test_data()
  
  # Test with bounds
  p <- viz_eps_percentile(test_data, lower_bound = 10, upper_bound = 50, 
                         id_col = "cow", start_col = "start", end_col = "end", tz = tz2())
  
  expect_s3_class(p, "ggplot")
  
  # Test with only lower bound
  p_lower <- viz_eps_percentile(test_data, lower_bound = 10, upper_bound = NULL,
                               id_col = "cow", start_col = "start", end_col = "end", tz = tz2())
  expect_s3_class(p_lower, "ggplot")
  
  # Test with only upper bound
  p_upper <- viz_eps_percentile(test_data, lower_bound = NULL, upper_bound = 50,
                               id_col = "cow", start_col = "start", end_col = "end", tz = tz2())
  expect_s3_class(p_upper, "ggplot")
})

test_that("viz_eps_percentile works with custom aesthetic parameters", {
  test_data <- create_viz_test_data()
  
  # Test with custom parameters
  p <- viz_eps_percentile(
    test_data,
    bins = 50,
    colors = c("blue", "red"),
    title_prefix = "Custom Title",
    xlim = 30,
    id_col = "cow", 
    start_col = "start", 
    end_col = "end", 
    tz = tz2()
  )
  
  expect_s3_class(p, "ggplot")
  expect_true(grepl("Custom Title", p$labels$title))
})

test_that("viz_eps_percentile works with custom column names", {
  test_data <- create_viz_test_data()
  names(test_data)[names(test_data) == "cow"] <- "animal_id"
  names(test_data)[names(test_data) == "start"] <- "start_time"
  names(test_data)[names(test_data) == "end"] <- "end_time"
  
  p <- viz_eps_percentile(test_data, 
                         id_col = "animal_id", 
                         start_col = "start_time", 
                         end_col = "end_time", 
                         tz = tz2())
  
  expect_s3_class(p, "ggplot")
})

# -----------------------------------------------------------------------------#
#                       Test for List Input                                    #
# -----------------------------------------------------------------------------#

test_that("viz_eps_percentile works with a list of data frames", {
  test_data_list <- create_list_test_data()
  
  p <- viz_eps_percentile(test_data_list, id_col = "cow", start_col = "start", end_col = "end", tz = tz2())
  
  expect_s3_class(p, "ggplot")
  expect_true(!is.null(p$data))
})

test_that("viz_eps_percentile works with single-element list", {
  test_data <- create_minimal_test_data()
  test_data_list <- list("2023-01-01" = test_data)
  
  p <- viz_eps_percentile(test_data_list, id_col = "cow", start_col = "start", end_col = "end", tz = tz2())
  
  expect_s3_class(p, "ggplot")
})

# -----------------------------------------------------------------------------#
#                       Test for Edge Cases                                    #
# -----------------------------------------------------------------------------#

test_that("viz_eps_percentile handles empty data gracefully", {
  empty_data <- data.frame(
    cow = character(0),
    start = lubridate::as_datetime(character(0), tz = tz2()),
    end = lubridate::as_datetime(character(0), tz = tz2()),
    date = as.Date(character(0)),
    stringsAsFactors = FALSE
  )
  
  expect_warning(
    p <- viz_eps_percentile(empty_data, id_col = "cow", start_col = "start", end_col = "end", tz = tz2()),
    "No data provided"
  )
  
  expect_s3_class(p, "ggplot")
  expect_true(grepl("No data available", p$labels$title))
})

test_that("viz_eps_percentile handles single visit per animal", {
  single_visit_data <- data.frame(
    cow = c("A", "B"),
    start = lubridate::ymd_hms(c("2023-01-01 08:00:00", "2023-01-01 09:00:00"), tz = tz2()),
    end = lubridate::ymd_hms(c("2023-01-01 08:10:00", "2023-01-01 09:10:00"), tz = tz2()),
    date = rep(as.Date("2023-01-01"), 2),
    stringsAsFactors = FALSE
  )
  
  expect_warning(
    p <- viz_eps_percentile(single_visit_data, id_col = "cow", start_col = "start", end_col = "end", tz = tz2()),
    "No gaps between visits found"
  )
  
  expect_s3_class(p, "ggplot")
  expect_true(grepl("No gaps found", p$labels$title))
})

test_that("viz_eps_percentile handles single animal with multiple visits", {
  single_animal_data <- create_minimal_test_data()
  
  p <- viz_eps_percentile(single_animal_data, id_col = "cow", start_col = "start", end_col = "end", tz = tz2())
  
  expect_s3_class(p, "ggplot")
  expect_true(!is.null(p$data))
})

test_that("viz_eps_percentile handles data with no gaps (overlapping visits)", {
  # Create data where visits overlap (negative gaps)
  overlapping_data <- data.frame(
    cow = c("A", "A", "A"),
    start = lubridate::ymd_hms(c("2023-01-01 08:00:00", "2023-01-01 08:05:00", "2023-01-01 08:08:00"), tz = tz2()),
    end = lubridate::ymd_hms(c("2023-01-01 08:10:00", "2023-01-01 08:15:00", "2023-01-01 08:18:00"), tz = tz2()),
    date = rep(as.Date("2023-01-01"), 3),
    stringsAsFactors = FALSE
  )
  
  # This should still work as calculate_gaps_by_animal removes negative gaps
  p <- viz_eps_percentile(overlapping_data, id_col = "cow", start_col = "start", end_col = "end", tz = tz2())
  
  expect_s3_class(p, "ggplot")
})

test_that("viz_eps_percentile handles extreme percentile values", {
  test_data <- create_viz_test_data()
  
  # Test very low percentile
  p_low <- viz_eps_percentile(test_data, percentile = 0.01, id_col = "cow", start_col = "start", end_col = "end", tz = tz2())
  expect_s3_class(p_low, "ggplot")
  
  # Test very high percentile
  p_high <- viz_eps_percentile(test_data, percentile = 0.99, id_col = "cow", start_col = "start", end_col = "end", tz = tz2())
  expect_s3_class(p_high, "ggplot")
})

# -----------------------------------------------------------------------------#
#                       Test for Error Handling                               #
# -----------------------------------------------------------------------------#

test_that("viz_eps_percentile errors with NULL data", {
  expect_error(
    viz_eps_percentile(NULL),
    "data cannot be NULL"
  )
})

test_that("viz_eps_percentile errors with invalid percentile", {
  test_data <- create_minimal_test_data()
  
  # Test percentile = 0
  expect_error(
    viz_eps_percentile(test_data, percentile = 0, id_col = "cow", start_col = "start", end_col = "end"),
    "percentile must be a single numeric value between 0 and 1"
  )
  
  # Test percentile = 1
  expect_error(
    viz_eps_percentile(test_data, percentile = 1, id_col = "cow", start_col = "start", end_col = "end"),
    "percentile must be a single numeric value between 0 and 1"
  )
  
  # Test percentile > 1
  expect_error(
    viz_eps_percentile(test_data, percentile = 1.5, id_col = "cow", start_col = "start", end_col = "end"),
    "percentile must be a single numeric value between 0 and 1"
  )
  
  # Test negative percentile
  expect_error(
    viz_eps_percentile(test_data, percentile = -0.1, id_col = "cow", start_col = "start", end_col = "end"),
    "percentile must be a single numeric value between 0 and 1"
  )
  
  # Test non-numeric percentile
  expect_error(
    viz_eps_percentile(test_data, percentile = "0.5", id_col = "cow", start_col = "start", end_col = "end"),
    "percentile must be a single numeric value between 0 and 1"
  )
  
  # Test multiple percentiles
  expect_error(
    viz_eps_percentile(test_data, percentile = c(0.5, 0.8), id_col = "cow", start_col = "start", end_col = "end"),
    "percentile must be a single numeric value between 0 and 1"
  )
})

test_that("viz_eps_percentile errors with invalid bounds", {
  test_data <- create_minimal_test_data()
  
  # Test negative lower_bound
  expect_error(
    viz_eps_percentile(test_data, lower_bound = -5, id_col = "cow", start_col = "start", end_col = "end"),
    "lower_bound must be a single non-negative numeric value or NULL"
  )
  
  # Test negative upper_bound
  expect_error(
    viz_eps_percentile(test_data, upper_bound = -10, id_col = "cow", start_col = "start", end_col = "end"),
    "upper_bound must be a single non-negative numeric value or NULL"
  )
  
  # Test lower_bound > upper_bound
  expect_error(
    viz_eps_percentile(test_data, lower_bound = 50, upper_bound = 30, id_col = "cow", start_col = "start", end_col = "end"),
    "lower_bound must be less than or equal to upper_bound"
  )
  
  # Test non-numeric bounds
  expect_error(
    viz_eps_percentile(test_data, lower_bound = "10", id_col = "cow", start_col = "start", end_col = "end"),
    "lower_bound must be a single non-negative numeric value or NULL"
  )
  
  # Test multiple values for bounds
  expect_error(
    viz_eps_percentile(test_data, lower_bound = c(5, 10), id_col = "cow", start_col = "start", end_col = "end"),
    "lower_bound must be a single non-negative numeric value or NULL"
  )
})

test_that("viz_eps_percentile errors with invalid bins parameter", {
  test_data <- create_minimal_test_data()
  
  # Test negative bins
  expect_error(
    viz_eps_percentile(test_data, bins = -10, id_col = "cow", start_col = "start", end_col = "end"),
    "bins must be a single positive integer"
  )
  
  # Test zero bins
  expect_error(
    viz_eps_percentile(test_data, bins = 0, id_col = "cow", start_col = "start", end_col = "end"),
    "bins must be a single positive integer"
  )
  
  # Test non-integer bins
  expect_error(
    viz_eps_percentile(test_data, bins = 10.5, id_col = "cow", start_col = "start", end_col = "end"),
    "bins must be a single positive integer"
  )
  
  # Test non-numeric bins
  expect_error(
    viz_eps_percentile(test_data, bins = "10", id_col = "cow", start_col = "start", end_col = "end"),
    "bins must be a single positive integer"
  )
  
  # Test multiple bins
  expect_error(
    viz_eps_percentile(test_data, bins = c(10, 20), id_col = "cow", start_col = "start", end_col = "end"),
    "bins must be a single positive integer"
  )
})

test_that("viz_eps_percentile errors with invalid colors parameter", {
  test_data <- create_minimal_test_data()
  
  # Test empty colors
  expect_error(
    viz_eps_percentile(test_data, colors = character(0), id_col = "cow", start_col = "start", end_col = "end"),
    "colors must be a non-empty character vector"
  )
  
  # Test non-character colors
  expect_error(
    viz_eps_percentile(test_data, colors = c(1, 2), id_col = "cow", start_col = "start", end_col = "end"),
    "colors must be a non-empty character vector"
  )
})

test_that("viz_eps_percentile errors with invalid title_prefix", {
  test_data <- create_minimal_test_data()
  
  # Test non-character title_prefix
  expect_error(
    viz_eps_percentile(test_data, title_prefix = 123, id_col = "cow", start_col = "start", end_col = "end"),
    "title_prefix must be a single character string"
  )
  
  # Test multiple title_prefix
  expect_error(
    viz_eps_percentile(test_data, title_prefix = c("Title1", "Title2"), id_col = "cow", start_col = "start", end_col = "end"),
    "title_prefix must be a single character string"
  )
})

test_that("viz_eps_percentile errors with invalid data types", {
  # Test with non-dataframe, non-list input
  expect_error(
    viz_eps_percentile("not a dataframe"),
    "data must be a dataframe or list of dataframes"
  )
  
  # Test with list containing non-dataframes
  invalid_list <- list("day1" = "not a dataframe", "day2" = data.frame(x = 1))
  expect_error(
    viz_eps_percentile(invalid_list),
    "All items in the list must be dataframes"
  )
  
  # Test with empty list
  expect_error(
    viz_eps_percentile(list()),
    "data list cannot be empty"
  )
})

test_that("viz_eps_percentile errors with missing required columns", {
  test_data <- create_minimal_test_data()
  
  # Test missing id column
  test_data_no_id <- test_data[, !names(test_data) %in% "cow"]
  expect_error(
    viz_eps_percentile(test_data_no_id, id_col = "cow", start_col = "start", end_col = "end"),
    "Missing required columns: cow"
  )
  
  # Test missing start column
  test_data_no_start <- test_data[, !names(test_data) %in% "start"]
  expect_error(
    viz_eps_percentile(test_data_no_start, id_col = "cow", start_col = "start", end_col = "end"),
    "Missing required columns: start"
  )
  
  # Test missing end column
  test_data_no_end <- test_data[, !names(test_data) %in% "end"]
  expect_error(
    viz_eps_percentile(test_data_no_end, id_col = "cow", start_col = "start", end_col = "end"),
    "Missing required columns: end"
  )
  
  # Test missing multiple columns
  test_data_minimal <- test_data[, "date", drop = FALSE]
  expect_error(
    viz_eps_percentile(test_data_minimal, id_col = "cow", start_col = "start", end_col = "end"),
    "Missing required columns: cow, start, end"
  )
})

# -----------------------------------------------------------------------------#
#                    Test Integration with meal_interval                       #
# -----------------------------------------------------------------------------#

test_that("viz_eps_percentile integrates correctly with meal_interval function", {
  test_data <- create_viz_test_data(n_animals = 3, n_visits_per_animal = 15)
  
  # Get eps from meal_interval directly
  direct_eps <- meal_interval(test_data, method = "percentile", percentile = 0.93, lower_bound = NULL, 
                             upper_bound = NULL, id_col = "cow", start_col = "start", end_col = "end", 
                             tz = tz2())
  
  # Create plot and check that it uses the same eps
  p <- viz_eps_percentile(test_data, percentile = 0.93, lower_bound = NULL, upper_bound = NULL,
                         id_col = "cow", start_col = "start", end_col = "end", tz = tz2())
  
  expect_s3_class(p, "ggplot")
  
  # Check that the subtitle contains the eps value
  expect_true(grepl(round(direct_eps, 2), p$labels$subtitle, fixed = TRUE))
})

# -----------------------------------------------------------------------------#
#                    Test Different Timezone Handling                         #
# -----------------------------------------------------------------------------#

test_that("viz_eps_percentile works with different timezones", {
  test_data <- create_minimal_test_data()
  
  # Test with UTC timezone
  p_utc <- viz_eps_percentile(test_data, id_col = "cow", start_col = "start", end_col = "end", tz = "UTC")
  expect_s3_class(p_utc, "ggplot")
  
  # Test with different timezone
  p_est <- viz_eps_percentile(test_data, id_col = "cow", start_col = "start", end_col = "end", tz = "America/New_York")
  expect_s3_class(p_est, "ggplot")
})

# -----------------------------------------------------------------------------#
#                    Test Plot Components and Structure                       #
# -----------------------------------------------------------------------------#

test_that("viz_eps_percentile creates correct plot structure", {
  test_data <- create_viz_test_data()
  
  p <- viz_eps_percentile(test_data, id_col = "cow", start_col = "start", end_col = "end", tz = tz2())
  
  # Check that plot has correct number of layers
  expect_equal(length(p$layers), 2)  # histogram + vertical line
  
  # Check layer types
  expect_true("GeomBar" %in% class(p$layers[[1]]$geom))  # histogram
  expect_true("GeomVline" %in% class(p$layers[[2]]$geom))  # vertical line
  
  # Check that data is properly formatted
  expect_true("gap_minutes" %in% names(p$data))
  expect_true(is.numeric(p$data$gap_minutes))
  expect_true(all(p$data$gap_minutes >= 0))  # No negative gaps
})

test_that("viz_eps_percentile handles xlim parameter correctly", {
  test_data <- create_viz_test_data()
  
  # Test with custom xlim
  p <- viz_eps_percentile(test_data, xlim = 50, id_col = "cow", start_col = "start", end_col = "end", tz = tz2())
  
  expect_s3_class(p, "ggplot")
  
  # Check that x-axis limits are set correctly
  x_limits <- ggplot2::layer_scales(p)$x$limits
  expect_equal(x_limits, c(0, 50))
}) 