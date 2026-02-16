# -----------------------------------------------------------------------------#
# ------------------- Tests for knn_outlier_detection ---------------------------#
# -----------------------------------------------------------------------------#

test_that("knn_outlier_detection works with normal data", {
  # Create sample test data
  set.seed(123)
  n <- 100
  
  # Generate regular data points
  df <- data.frame(
    duration = runif(n, 50, 300),
    intake = runif(n, 5, 15)
  )
  
  # Add a few outliers
  outliers <- data.frame(
    duration = c(500, 600, 700),
    intake = c(30, 35, 40)
  )
  
  df <- rbind(df, outliers)
  
  # Test default behavior
  result <- knn_outlier_detection(df, k = 5, threshold_percentile = 95)
  
  expect_s3_class(result, "data.frame")
  expect_true("outlier" %in% names(result))
  expect_equal(nrow(result), nrow(df))
  expect_true(sum(result$outlier == "Y") > 0)
  expect_true(sum(result$outlier == "N") > 0)
})

test_that("knn_outlier_detection handles custom column names", {
  # Create sample data with custom column names
  set.seed(123)
  n <- 50
  
  df <- data.frame(
    visit_duration = runif(n, 50, 300),
    feed_amount = runif(n, 5, 15)
  )
  
  # Add outliers
  outliers <- data.frame(
    visit_duration = c(500, 600),
    feed_amount = c(30, 35)
  )
  
  df <- rbind(df, outliers)
  
  # Test with custom column names
  result <- knn_outlier_detection(
    df, 
    k = 5, 
    threshold_percentile = 95,
    intake_col = "feed_amount",
    duration_col = "visit_duration"
  )
  
  expect_s3_class(result, "data.frame")
  expect_true("outlier" %in% names(result))
  expect_equal(nrow(result), nrow(df))
  expect_true(sum(result$outlier == "Y") > 0)
})

test_that("knn_outlier_detection works with custom_scaling", {
  set.seed(123)
  n <- 50
  
  df <- data.frame(
    duration = runif(n, 50, 300),
    intake = runif(n, 5, 15)
  )
  
  # Add outliers
  outliers <- data.frame(
    duration = c(500, 600),
    intake = c(30, 35)
  )
  
  df <- rbind(df, outliers)
  
  # Test with custom scaling
  result <- knn_outlier_detection(
    df, 
    k = 5, 
    threshold_percentile = 95,
    custom_scaling = list(rate = 10, intake = 5, duration = 0.1)
  )
  
  expect_s3_class(result, "data.frame")
  expect_true("outlier" %in% names(result))
})

test_that("knn_outlier_detection works with partial custom_scaling", {
  set.seed(123)
  n <- 50
  
  df <- data.frame(
    duration = runif(n, 50, 300),
    intake = runif(n, 5, 15)
  )
  
  # Test with partial custom scaling (missing some factors)
  expect_warning(
    result <- knn_outlier_detection(
      df, 
      k = 5, 
      threshold_percentile = 95,
      custom_scaling = list(rate = 10, intake = 5)  # Missing duration
    )
  )
  
  expect_s3_class(result, "data.frame")
})

test_that("knn_outlier_detection can remove outliers", {
  set.seed(123)
  n <- 50
  
  df <- data.frame(
    duration = runif(n, 50, 300),
    intake = runif(n, 5, 15)
  )
  
  # Add outliers
  outliers <- data.frame(
    duration = c(500, 600),
    intake = c(30, 35)
  )
  
  df <- rbind(df, outliers)
  
  # Test with remove_outliers = TRUE
  result <- knn_outlier_detection(
    df, 
    k = 5, 
    threshold_percentile = 95,
    remove_outliers = TRUE
  )
  
  expect_s3_class(result, "data.frame")
  expect_false("outlier" %in% names(result))
  expect_true(nrow(result) < nrow(df))
})

test_that("knn_outlier_detection handles NA and Inf values correctly", {
  # Create sample data with NA and Inf values
  df <- data.frame(
    duration = c(100, 200, 300, 400, 0, 500),
    intake = c(10, 20, 30, 40, 10, 50)
  )
  
  # Force some problematic values
  df$rate <- df$intake / df$duration  # This will create an Inf value for the row with duration=0
  df$rate[2] <- NA  # Set one rate to NA
  
  # Test that warnings are raised
  expect_warning(
    result <- knn_outlier_detection(df, k = 2, threshold_percentile = 90)
  )
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(df))
  expect_true(result$outlier[2] == "Y")  # Row with NA should be marked as outlier
  expect_true(result$outlier[5] == "Y")  # Row with Inf should be marked as outlier
})

test_that("knn_outlier_detection handles dataset with all problematic rates", {
  # Create sample data where all rows have problematic rates
  df <- data.frame(
    duration = c(0, 0, 0),
    intake = c(10, 20, 30)
  )
  
  # Calculate rates (all will be Inf)
  df$rate <- df$intake / df$duration
  
  # Test with all problematic rates
  expect_warning(
    result <- knn_outlier_detection(df, k = 2, threshold_percentile = 90)
  )
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(df))
  expect_true(all(result$outlier == "Y"))
})

# Edge cases
test_that("knn_outlier_detection handles small datasets correctly", {
  # Test with 1 row
  df_single <- data.frame(duration = 100, intake = 10)
  result_single <- knn_outlier_detection(df_single, k = 5)
  expect_equal(nrow(result_single), 1)
  expect_equal(result_single$outlier, "N")
  
  # Test with 2 rows
  df_two <- data.frame(duration = c(100, 200), intake = c(10, 20))
  result_two <- knn_outlier_detection(df_two, k = 5)
  expect_equal(nrow(result_two), 2)
  expect_true(all(result_two$outlier == "N"))
})

test_that("knn_outlier_detection handles empty dataframe", {
  df_empty <- data.frame(duration = numeric(0), intake = numeric(0))
  result_empty <- knn_outlier_detection(df_empty, k = 5)
  expect_equal(nrow(result_empty), 0)
  expect_true("outlier" %in% names(result_empty))
})

test_that("knn_outlier_detection adjusts k when it's too large", {
  # Create small dataset
  df <- data.frame(
    duration = c(100, 200, 300, 400),
    intake = c(10, 20, 30, 40)
  )
  
  # Test with k larger than n-1
  result <- knn_outlier_detection(df, k = 10)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 4)
})

# Error handling
test_that("knn_outlier_detection validates inputs correctly", {
  # Test with non-dataframe input
  expect_error(
    knn_outlier_detection("not a dataframe"),
    "df must be a data frame"
  )
  
  # Test with invalid threshold_percentile
  df <- data.frame(duration = c(100, 200), intake = c(10, 20))
  expect_error(
    knn_outlier_detection(df, threshold_percentile = 101),
    "threshold_percentile must be a number between 0 and 100"
  )
  expect_error(
    knn_outlier_detection(df, threshold_percentile = -1),
    "threshold_percentile must be a number between 0 and 100"
  )
  
  # Test with invalid k
  expect_error(
    knn_outlier_detection(df, k = 0),
    "k must be a non-negative integer with a minimum value of 1"
  )
  expect_error(
    knn_outlier_detection(df, k = -1),
    "k must be a non-negative integer with a minimum value of 1"
  )
  
  # Test with missing required columns
  df_missing <- data.frame(x = 1:5, y = 1:5)
  expect_error(
    knn_outlier_detection(df_missing),
    "Required columns not found in data frame"
  )
})

# -----------------------------------------------------------------------------#
# ------------------- Tests for knn_clean_feed --------------------------------#
# -----------------------------------------------------------------------------#

test_that("knn_clean_feed works with a data frame input", {
  # Create sample data
  set.seed(123)
  n <- 50
  
  df <- data.frame(
    duration = runif(n, 50, 300),
    intake = runif(n, 5, 15),
    date = rep(as.Date("2023-01-01"), n)
  )
  
  # Add some outliers
  outliers <- data.frame(
    duration = c(500, 600),
    intake = c(30, 35),
    date = rep(as.Date("2023-01-01"), 2)
  )
  
  df <- rbind(df, outliers)
  
  # Test with data frame input
  result <- knn_clean_feed(
    df, 
    k = 5, 
    threshold_percentile = 95
  )
  
  expect_s3_class(result, "data.frame")
  expect_true("outlier" %in% names(result))
})

test_that("knn_clean_feed works with a list of data frames", {
  # Create sample data for multiple days
  set.seed(123)
  
  # Day 1
  df1 <- data.frame(
    duration = runif(20, 50, 300),
    intake = runif(20, 5, 15),
    date = rep(as.Date("2023-01-01"), 20)
  )
  
  # Day 2
  df2 <- data.frame(
    duration = runif(20, 50, 300),
    intake = runif(20, 5, 15),
    date = rep(as.Date("2023-01-02"), 20)
  )
  
  # Add outliers to day 1
  outlier1 <- data.frame(
    duration = 500,
    intake = 30,
    date = as.Date("2023-01-01")
  )
  
  df1 <- rbind(df1, outlier1)
  
  # Create list
  feed_list <- list(
    "2023-01-01" = df1,
    "2023-01-02" = df2
  )
  
  # Test with list input
  result <- knn_clean_feed(
    feed_list, 
    k = 5, 
    threshold_percentile = 95
  )
  
  expect_type(result, "list")
  expect_length(result, 2)
  expect_named(result, c("2023-01-01", "2023-01-02"))
  expect_true("outlier" %in% names(result[[1]]))
})

test_that("knn_clean_feed errors with invalid inputs", {
  # Test with invalid input type
  expect_error(
    knn_clean_feed("not a data frame or list"),
    "feed_data must be either a list of data frames or a single data frame"
  )
  
  # Test with list containing non-data frames
  invalid_list <- list(
    "2023-01-01" = data.frame(x = 1:5),
    "2023-01-02" = "not a data frame"
  )
  
  expect_error(
    knn_clean_feed(invalid_list),
    "All elements in feed_data must be data frames"
  )
  
  # Test with missing date column in merged data
  df_no_date <- data.frame(
    duration = 1:10,
    intake = 1:10
  )
  
  feed_list_no_date <- list(
    "2023-01-01" = df_no_date,
    "2023-01-02" = df_no_date
  )
  
  expect_error(
    knn_clean_feed(feed_list_no_date),
    "Date column date not found in the merged data frame"
  )
})

test_that("knn_clean_feed can remove outliers", {
  # Create sample data
  set.seed(123)
  n <- 50
  
  df <- data.frame(
    duration = runif(n, 50, 300),
    intake = runif(n, 5, 15),
    date = rep(as.Date("2023-01-01"), n)
  )
  
  # Add outliers
  outliers <- data.frame(
    duration = c(500, 600),
    intake = c(30, 35),
    date = rep(as.Date("2023-01-01"), 2)
  )
  
  df <- rbind(df, outliers)
  
  # Test with remove_outliers = TRUE
  result <- knn_clean_feed(
    df, 
    k = 5, 
    threshold_percentile = 95,
    remove_outliers = TRUE
  )
  
  expect_s3_class(result, "data.frame")
  expect_false("outlier" %in% names(result))
  expect_true(nrow(result) < nrow(df))
})

# -----------------------------------------------------------------------------#
# ------------------- Tests for knn_clean_water -------------------------------#
# -----------------------------------------------------------------------------#

test_that("knn_clean_water works with a data frame input", {
  # Create sample data
  set.seed(123)
  n <- 50
  
  df <- data.frame(
    duration = runif(n, 10, 50),
    intake = runif(n, 1, 5),
    date = rep(as.Date("2023-01-01"), n)
  )
  
  # Add outliers
  outliers <- data.frame(
    duration = c(100, 120),
    intake = c(10, 12),
    date = rep(as.Date("2023-01-01"), 2)
  )
  
  df <- rbind(df, outliers)
  
  # Test with data frame input
  result <- knn_clean_water(
    df, 
    k = 5, 
    threshold_percentile = 95
  )
  
  expect_s3_class(result, "data.frame")
  expect_true("outlier" %in% names(result))
})

test_that("knn_clean_water works with a list of data frames", {
  # Create sample data for multiple days
  set.seed(123)
  
  # Day 1
  df1 <- data.frame(
    duration = runif(20, 10, 50),
    intake = runif(20, 1, 5),
    date = rep(as.Date("2023-01-01"), 20)
  )
  
  # Day 2
  df2 <- data.frame(
    duration = runif(20, 10, 50),
    intake = runif(20, 1, 5),
    date = rep(as.Date("2023-01-02"), 20)
  )
  
  # Add outlier to day 1
  outlier1 <- data.frame(
    duration = 100,
    intake = 10,
    date = as.Date("2023-01-01")
  )
  
  df1 <- rbind(df1, outlier1)
  
  # Create list
  water_list <- list(
    "2023-01-01" = df1,
    "2023-01-02" = df2
  )
  
  # Test with list input
  result <- knn_clean_water(
    water_list, 
    k = 5, 
    threshold_percentile = 95
  )
  
  expect_type(result, "list")
  expect_length(result, 2)
  expect_named(result, c("2023-01-01", "2023-01-02"))
  expect_true("outlier" %in% names(result[[1]]))
})

test_that("knn_clean_water handles custom column names and scaling", {
  # Create sample data with custom column names
  set.seed(123)
  n <- 50
  
  df <- data.frame(
    water_duration = runif(n, 10, 50),
    water_intake = runif(n, 1, 5),
    date = rep(as.Date("2023-01-01"), n)
  )
  
  # Test with custom column names and scaling
  result <- knn_clean_water(
    df, 
    k = 5, 
    threshold_percentile = 95,
    custom_scaling = list(rate = 30, intake = 2, duration = 0.02),
    intake_col = "water_intake",
    duration_col = "water_duration"
  )
  
  expect_s3_class(result, "data.frame")
  expect_true("outlier" %in% names(result))
})

# -----------------------------------------------------------------------------#
# ------------------- Tests for helper functions ------------------------------#
# -----------------------------------------------------------------------------#

test_that("create_scaled_matrix works correctly", {
  # Create test data
  df <- data.frame(
    duration = c(100, 200, 300),
    intake = c(10, 20, 30),
    rate = c(0.1, 0.1, 0.1),
    extra_col = c("a", "b", "c")  # Column that should be ignored
  )
  
  # Test with default scaling (1,1,1)
  scaling <- list(rate = 1, intake = 1, duration = 1)
  result <- create_scaled_matrix(df, scaling, "intake", "duration")
  
  expect_true(is.matrix(result))
  expect_equal(dim(result), c(3, 3))
  expect_equal(colnames(result), c("duration", "intake", "rate"))
  expect_equal(result[, "duration"], df$duration)
  expect_equal(result[, "intake"], df$intake)
  expect_equal(result[, "rate"], df$rate)
  
  # Test with custom scaling
  scaling <- list(rate = 10, intake = 2, duration = 0.5)
  result <- create_scaled_matrix(df, scaling, "intake", "duration")
  
  expect_true(is.matrix(result))
  expect_equal(result[, "duration"], df$duration * 0.5)
  expect_equal(result[, "intake"], df$intake * 2)
  expect_equal(result[, "rate"], df$rate * 10)
})

test_that("get_scaling_factors works correctly", {
  # Test with NULL input (defaults to 1,1,1)
  result <- get_scaling_factors(NULL)
  expect_equal(result, list(rate = 1, intake = 1, duration = 1))
  
  # Test with complete custom scaling
  custom <- list(rate = 10, intake = 2, duration = 0.5)
  result <- get_scaling_factors(custom)
  expect_equal(result, custom)
  
  # Test with incomplete custom scaling (should fill missing with 1)
  partial <- list(rate = 10, intake = 2)
  expect_warning(
    result <- get_scaling_factors(partial)
  )
  expect_equal(result$rate, 10)
  expect_equal(result$intake, 2)
  expect_equal(result$duration, 1)  # Filled with default
}) 