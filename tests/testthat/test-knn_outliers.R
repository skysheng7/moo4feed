# Tests for KNN outlier detection functions

test_that("knn_outlier_detection correctly identifies outliers", {
  # Create test data
  test_data <- data.frame(
    duration = c(100, 120, 110, 500, 95, 105),
    intake = c(2, 2.5, 2.2, 10, 1.8, 2.1),
    rate = c(0.02, 0.021, 0.02, 0.02, 0.019, 0.02)
  )
  
  # Test with feed data threshold and k=3 (suitable for small test dataset)
  result_feed <- knn_outlier_detection(
    test_data, 
    k = 3,
    threshold_percentile = 99.936
  )
  
  # The 4th row should be flagged as an outlier
  expect_equal(result_feed$outlier[4], "Y")
  expect_equal(sum(result_feed$outlier == "Y"), 1)
  
  # Test with water data threshold
  result_water <- knn_outlier_detection(
    test_data, 
    k = 3,
    threshold_percentile = 99.9
  )
  
  # The 4th row should still be flagged as an outlier
  expect_equal(result_water$outlier[4], "Y")
  expect_equal(sum(result_water$outlier == "Y"), 1)
  
  # Test with custom column names
  test_data_custom <- data.frame(
    visit_time = c(100, 120, 110, 500, 95, 105),
    consumption = c(2, 2.5, 2.2, 10, 1.8, 2.1)
  )
  
  result_custom <- knn_outlier_detection(
    test_data_custom, 
    k = 3,
    threshold_percentile = 99.9,
    intake_col = "consumption",
    duration_col = "visit_time"
  )
  
  expect_true("rate" %in% names(result_custom))
  expect_equal(result_custom$outlier[4], "Y")
})

test_that("knn_outlier_detection handles empty data frames", {
  empty_df <- data.frame(
    duration = numeric(0),
    intake = numeric(0)
  )
  
  # Should still error on empty data frame due to missing columns
  expect_error(
    knn_outlier_detection(empty_df, threshold_percentile = 99.9),
    "Required columns not found in data frame. Need duration and intake columns."
  )
  
  # Test with too small dataset (should return all as non-outliers)
  tiny_df <- data.frame(
    duration = c(100),
    intake = c(2)
  )
  
  result <- knn_outlier_detection(tiny_df, k = 3, threshold_percentile = 99.9)
  expect_equal(nrow(result), 1)
  expect_equal(result$outlier, "N")
})

test_that("knn_clean_feed processes multiple days correctly", {
  # Create test data for 2 days
  day1 <- data.frame(
    duration = c(100, 120, 110, 500, 95, 105),
    intake = c(2, 2.5, 2.2, 10, 1.8, 2.1)
  )
  
  day2 <- data.frame(
    duration = c(105, 115, 125, 600, 90, 110),
    intake = c(2.1, 2.3, 2.4, 12, 1.9, 2.2)
  )
  
  feed_list <- list(day1 = day1, day2 = day2)
  
  # Process the feed list with k=3 (suitable for small test dataset)
  result <- knn_clean_feed(feed_list, k = 3)
  
  # Check that we have outliers in both days
  expect_equal(length(result), 2)
  expect_true("outlier" %in% names(result$day1))
  expect_true("outlier" %in% names(result$day2))
  expect_equal(result$day1$outlier[4], "Y")
  expect_equal(result$day2$outlier[4], "Y")
})

test_that("knn_clean_water processes multiple days correctly", {
  # Create test data for 2 days
  day1 <- data.frame(
    duration = c(100, 120, 110, 500, 95, 105),
    intake = c(2, 2.5, 2.2, 10, 1.8, 2.1)
  )
  
  day2 <- data.frame(
    duration = c(105, 115, 125, 600, 90, 110),
    intake = c(2.1, 2.3, 2.4, 12, 1.9, 2.2)
  )
  
  water_list <- list(day1 = day1, day2 = day2)
  
  # Process the water list with k=3 (suitable for small test dataset)
  result <- knn_clean_water(water_list, k = 3)
  
  # Check that we have outliers in both days
  expect_equal(length(result), 2)
  expect_true("outlier" %in% names(result$day1))
  expect_true("outlier" %in% names(result$day2))
  expect_equal(result$day1$outlier[4], "Y")
  expect_equal(result$day2$outlier[4], "Y")
})

test_that("knn_outlier_detection handles custom scaling", {
  test_data <- data.frame(
    duration = c(100, 120, 110, 500, 95, 105),
    intake = c(2, 2.5, 2.2, 10, 1.8, 2.1)
  )
  
  custom_scaling <- list(
    rate = 5000,
    intake = 3,
    duration = 0.02
  )
  
  result <- knn_outlier_detection(
    test_data, 
    k = 3,
    threshold_percentile = 99.9,
    custom_scaling = custom_scaling
  )
  
  # The 4th row should still be flagged as an outlier
  expect_equal(result$outlier[4], "Y")
}) 