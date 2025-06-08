# -----------------------------------------------------------------------------#
#                    Tests for viz_outliers()                                     #
# -----------------------------------------------------------------------------#

# Helper function to create toy data for testing viz_outliers
create_test_data <- function() {
  set.seed(123)
  n <- 100
  
  # Generate regular data points
  df <- data.frame(
    transponder = 1000 + seq_len(n),
    cow = as.character(1:n),
    bin = sample(1:10, n, replace = TRUE),
    start = lubridate::ymd_hms("2025-01-01 08:00:00", tz = tz2()) + lubridate::minutes(1:n),
    end = lubridate::ymd_hms("2025-01-01 08:00:00", tz = tz2()) + lubridate::minutes(1:n) + lubridate::seconds(sample(30:300, n, replace = TRUE)),
    duration = sample(30:300, n, replace = TRUE),
    intake = runif(n, 0.5, 5),
    startweight = runif(n, 10, 20),
    endweight = runif(n, 5, 10),
    date = lubridate::ymd("2025-01-01"),
    outlier = "N"
  )
  
  # Add outliers (all marked as outlier = "Y")
  outliers <- data.frame(
    transponder = 2000 + seq_len(5),
    cow = as.character(101:105),
    bin = sample(1:10, 5, replace = TRUE),
    start = lubridate::ymd_hms("2025-01-01 12:00:00", tz = tz2()) + lubridate::minutes(1:5),
    end = lubridate::ymd_hms("2025-01-01 12:00:00", tz = tz2()) + lubridate::minutes(1:5) + lubridate::seconds(c(600, 700, 800, 750, 650)),
    duration = c(600, 700, 800, 750, 650),
    intake = c(15, 20, 18, 16, 19),
    startweight = runif(5, 20, 30),
    endweight = runif(5, 1, 5),
    date = lubridate::ymd("2025-01-01"),
    outlier = "Y"
  )
  
  df <- rbind(df, outliers)

  # Calculate rate
  df$rate <- df$intake / df$duration
  
  # Rename to match global variable system
  names(df)[names(df) == "cow"] <- id_col2()
  names(df)[names(df) == "bin"] <- bin_col2()
  names(df)[names(df) == "duration"] <- duration_col2()
  names(df)[names(df) == "intake"] <- intake_col2()
  
  return(df)
}

# Helper function to create day2 test data
create_day2_test_data <- function() {
  set.seed(456) # Different seed for day2
  n <- 120 # More observations
  
  # Generate regular data points with different distributions
  df <- data.frame(
    transponder = 3000 + seq_len(n),
    cow = as.character(201:(200+n)),
    bin = sample(1:12, n, replace = TRUE), # Different bin range
    start = lubridate::ymd_hms("2025-01-02 08:00:00", tz = tz2()) + lubridate::minutes(1:n),
    end = lubridate::ymd_hms("2025-01-02 08:00:00", tz = tz2()) + lubridate::minutes(1:n) + lubridate::seconds(sample(40:350, n, replace = TRUE)),
    duration = sample(40:350, n, replace = TRUE),
    intake = runif(n, 0.7, 6), # Different intake range
    startweight = runif(n, 12, 25),
    endweight = runif(n, 6, 12),
    date = lubridate::ymd("2025-01-02"),
    outlier = "N"
  )
  
  # Add outliers (all marked as outlier = "Y")
  outliers <- data.frame(
    transponder = 4000 + seq_len(7), # More outliers
    cow = as.character(401:407),
    bin = sample(1:12, 7, replace = TRUE),
    start = lubridate::ymd_hms("2025-01-02 14:00:00", tz = tz2()) + lubridate::minutes(1:7),
    end = lubridate::ymd_hms("2025-01-02 14:00:00", tz = tz2()) + lubridate::minutes(1:7) + lubridate::seconds(c(620, 710, 830, 780, 670, 700, 750)),
    duration = c(620, 710, 830, 780, 670, 700, 750),
    intake = c(16, 21, 19, 17, 20, 18, 22),
    startweight = runif(7, 22, 32),
    endweight = runif(7, 2, 6),
    date = lubridate::ymd("2025-01-02"),
    outlier = "Y"
  )
  
  df <- rbind(df, outliers)

  # Calculate rate
  df$rate <- df$intake / df$duration
  
  # Rename to match global variable system
  names(df)[names(df) == "cow"] <- id_col2()
  names(df)[names(df) == "bin"] <- bin_col2()
  names(df)[names(df) == "duration"] <- duration_col2()
  names(df)[names(df) == "intake"] <- intake_col2()
  
  return(df)
}

# Create a list of data frames for multi-day testing
create_multi_day_data <- function() {
  day1 <- create_test_data()
  
  day2 <- create_day2_test_data()

  df_list <- list(day1 = day1, day2 = day2)
  names(df_list) <- as.character(c(day1$date[1], day2$date[1]))
  
  return(df_list)
}

# -----------------------------------------------------------------------------#
#                      Test for Normal Usage                                   #
# -----------------------------------------------------------------------------#

test_that("viz_outliers renders a plot with default parameters", {
  test_data <- create_test_data()
  
  # Test the function
  p <- viz_outliers(test_data)
  
  # Check that it returns a ggplot object
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$x, duration_col2())
  expect_equal(p$labels$y, intake_col2())
  expect_equal(p$labels$title, "Outlier Detection Results")
})

test_that("viz_outliers works with custom x and y variables", {
  test_data <- create_test_data()
  
  # Test with custom x and y variables
  p <- viz_outliers(test_data, x_var = intake_col2(), y_var = "rate")
  
  # Check that it returns a ggplot object with correct axis labels
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$x, intake_col2())
  expect_equal(p$labels$y, "rate")
})

test_that("viz_outliers works with custom aesthetics", {
  test_data <- create_test_data()
  
  # Test with custom aesthetic parameters
  p <- viz_outliers(
    test_data,
    jitter_amount = 0.1,
    alpha = 0.5,
    title = "Custom Title",
    regular_color = "blue",
    outlier_color = "red"
  )
  
  # Check that it returns a ggplot object with correct title
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Custom Title")
  
  # Check colors in the scale
  scale_colors <- p$scales$scales[[1]]$palette(2)
  names(scale_colors) <- NULL
  expect_equal(scale_colors, c("blue", "red"))
})

test_that("viz_outliers works with custom axis labels", {
  test_data <- create_test_data()
  
  # Test with custom x_lab and y_lab
  p <- viz_outliers(
    test_data,
    x_var = duration_col2(),
    y_var = intake_col2(),
    x_lab = "Feeding Duration (seconds)",
    y_lab = "Feed Intake (kg)"
  )
  
  # Check that it returns a ggplot object with custom axis labels
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$x, "Feeding Duration (seconds)")
  expect_equal(p$labels$y, "Feed Intake (kg)")
  
  # Test default behavior (NULL should use the variable names)
  p2 <- viz_outliers(
    test_data,
    x_var = duration_col2(),
    y_var = intake_col2()
  )
  
  expect_equal(p2$labels$x, duration_col2())
  expect_equal(p2$labels$y, intake_col2())
})

# -----------------------------------------------------------------------------#
#                       Test for List Input                                    #
# -----------------------------------------------------------------------------#

test_that("viz_outliers works with a list of data frames", {
  # Create a list of data frames
  data_list <- create_multi_day_data()
  
  # Test the function with a list input
  p <- viz_outliers(data_list)
  
  # Check that it returns a ggplot object
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$x, duration_col2())
  expect_equal(p$labels$y, intake_col2())
})

# -----------------------------------------------------------------------------#
#                         Test for Edge Cases                                  #
# -----------------------------------------------------------------------------#

test_that("viz_outliers handles datasets with no outliers", {
  test_data <- create_test_data()
  # Remove outliers
  test_data$outlier <- "N"
  
  # Test the function
  p <- viz_outliers(test_data)
  
  # Check that it returns a ggplot object
  expect_s3_class(p, "ggplot")
})

test_that("viz_outliers handles datasets with all outliers", {
  test_data <- create_test_data()
  # Make all points outliers
  test_data$outlier <- "Y"
  
  # Test the function
  p <- viz_outliers(test_data)
  
  # Check that it returns a ggplot object
  expect_s3_class(p, "ggplot")
})

test_that("viz_outliers handles NA and infinite values in rate calculation", {
  test_data <- create_test_data()
  
  # Add problematic data points
  test_data$duration[1] <- 0  # Will create infinite rate
  test_data$intake[2] <- NA   # Will create NA rate
  
  # Recalculate rate to introduce Inf and NA values
  test_data$rate <- test_data$intake / test_data$duration
  
  # Test that the function handles these cases with a warning
  expect_warning(
    p <- viz_outliers(test_data, x_var = "duration", y_var = "rate"),
    "There are NA or infinite values in the selected variables"
  )
  
  # Check that it still returns a ggplot object
  expect_s3_class(p, "ggplot")
})

# -----------------------------------------------------------------------------#
#                         Test for Error Handling                              #
# -----------------------------------------------------------------------------#

test_that("viz_outliers throws error for non-dataframe or non-list input", {
  expect_error(
    viz_outliers("not a dataframe"),
    "'data' must be either a data frame or a list of data frames"
  )
})

test_that("viz_outliers throws error when outlier column is missing", {
  test_data <- create_test_data()
  test_data$outlier <- NULL
  
  expect_error(
    viz_outliers(test_data),
    "The data must contain an 'outlier' column with values 'Y' or 'N'"
  )
})

test_that("viz_outliers throws error when requested columns don't exist", {
  test_data <- create_test_data()
  
  expect_error(
    viz_outliers(test_data, x_var = "nonexistent_column"),
    "Column nonexistent_column not found in the data"
  )
  
  expect_error(
    viz_outliers(test_data, y_var = "nonexistent_column"),
    "Column nonexistent_column not found in the data"
  )
})

test_that("viz_outliers throws error when x_var or y_var are not character", {
  test_data <- create_test_data()
  
  expect_error(
    viz_outliers(test_data, x_var = 123),
    "'x_var' and 'y_var' must be character strings"
  )
  
  expect_error(
    viz_outliers(test_data, y_var = 123),
    "'x_var' and 'y_var' must be character strings"
  )
})

test_that("viz_outliers throws error when list contains non-dataframe elements", {
  # Create an invalid list with a non-dataframe element
  invalid_list <- list(create_test_data(), "not a dataframe")
  
  expect_error(
    viz_outliers(invalid_list),
    "All elements in 'data' must be data frames"
  )
}) 