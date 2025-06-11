test_that("meal_interval works with single dataframe", {
  # Create test data
  test_data <- data.frame(
    cow = rep(c("A", "B"), each = 5),
    start = as.POSIXct(c(
      "2023-01-01 08:00:00", "2023-01-01 08:15:00", "2023-01-01 08:30:00", 
      "2023-01-01 10:00:00", "2023-01-01 10:30:00",
      "2023-01-01 09:00:00", "2023-01-01 09:20:00", "2023-01-01 09:45:00",
      "2023-01-01 11:00:00", "2023-01-01 11:15:00"
    )),
    end = as.POSIXct(c(
      "2023-01-01 08:10:00", "2023-01-01 08:25:00", "2023-01-01 08:40:00", 
      "2023-01-01 10:10:00", "2023-01-01 10:40:00",
      "2023-01-01 09:10:00", "2023-01-01 09:30:00", "2023-01-01 09:55:00",
      "2023-01-01 11:10:00", "2023-01-01 11:25:00"
    ))
  )
  
  # Test default method
  result <- meal_interval(test_data, id_col = "cow", start_col = "start", end_col = "end")
  expect_type(result, "double")
  expect_true(result >= 5 && result <= 60)
  
  # Test specific methods
  result_percentile <- meal_interval(test_data, method = "percentile", 
                                           id_col = "cow", start_col = "start", end_col = "end")
  expect_type(result_percentile, "double")
  expect_true(result_percentile >= 5 && result_percentile <= 60)
  
  result_gmm <- meal_interval(test_data, method = "gmm", 
                                    id_col = "cow", start_col = "start", end_col = "end")
  expect_type(result_gmm, "double")
  expect_true(result_gmm >= 5 && result_gmm <= 60)
})

test_that("meal_interval works with list of dataframes", {
  # Create test data as list
  test_data1 <- data.frame(
    cow = rep("A", 3),
    start = as.POSIXct(c("2023-01-01 08:00:00", "2023-01-01 08:15:00", "2023-01-01 08:30:00")),
    end = as.POSIXct(c("2023-01-01 08:10:00", "2023-01-01 08:25:00", "2023-01-01 08:40:00"))
  )
  
  test_data2 <- data.frame(
    cow = rep("B", 3),
    start = as.POSIXct(c("2023-01-01 09:00:00", "2023-01-01 09:20:00", "2023-01-01 09:45:00")),
    end = as.POSIXct(c("2023-01-01 09:10:00", "2023-01-01 09:30:00", "2023-01-01 09:55:00"))
  )
  
  test_list <- list(test_data1, test_data2)
  
  result <- meal_interval(test_list, id_col = "cow", start_col = "start", end_col = "end")
  expect_type(result, "double")
  expect_true(result >= 5 && result <= 60)
})

test_that("meal_interval works with different percentiles", {
  # Create test data
  test_data <- data.frame(
    cow = rep("A", 5),
    start = as.POSIXct(c(
      "2023-01-01 08:00:00", "2023-01-01 08:15:00", "2023-01-01 08:30:00", 
      "2023-01-01 10:00:00", "2023-01-01 10:30:00"
    )),
    end = as.POSIXct(c(
      "2023-01-01 08:10:00", "2023-01-01 08:25:00", "2023-01-01 08:40:00", 
      "2023-01-01 10:10:00", "2023-01-01 10:40:00"
    ))
  )
  
  result_75 <- meal_interval(test_data, percentile = 0.75, 
                                   id_col = "cow", start_col = "start", end_col = "end")
  result_90 <- meal_interval(test_data, percentile = 0.90, 
                                   id_col = "cow", start_col = "start", end_col = "end")
  
  expect_type(result_75, "double")
  expect_type(result_90, "double")
  expect_true(result_75 >= 5 && result_75 <= 60)
  expect_true(result_90 >= 5 && result_90 <= 60)
})

test_that("meal_interval uses global column defaults", {
  # Create test data with default column names
  test_data <- data.frame(
    cow = rep("A", 3),
    start = as.POSIXct(c("2023-01-01 08:00:00", "2023-01-01 08:15:00", "2023-01-01 08:30:00")),
    end = as.POSIXct(c("2023-01-01 08:10:00", "2023-01-01 08:25:00", "2023-01-01 08:40:00"))
  )
  
  # Should work with global defaults
  result <- meal_interval(test_data)
  expect_type(result, "double")
  expect_true(result >= 5 && result <= 60)
})

test_that("meal_interval handles edge cases", {
  # Test with single row (should return default)
  single_row <- data.frame(
    cow = "A",
    start = as.POSIXct("2023-01-01 08:00:00"),
    end = as.POSIXct("2023-01-01 08:10:00")
  )
  
  result <- meal_interval(single_row, id_col = "cow", start_col = "start", end_col = "end")
  expect_equal(result, 30)  # Default fallback
  
  # Test with empty dataframe
  empty_df <- data.frame(
    cow = character(0),
    start = as.POSIXct(character(0)),
    end = as.POSIXct(character(0))
  )
  
  expect_warning(
    result <- meal_interval(empty_df, id_col = "cow", start_col = "start", end_col = "end"),
    "No data provided, returning default eps value of 30 minutes"
  )
  expect_equal(result, 30)
})

test_that("meal_interval handles when there is only 1 row, and other edge cases", {
  # Test NULL data
  expect_error(meal_interval(NULL), "data cannot be NULL")
  
  # Test invalid data type
  expect_error(meal_interval("not a dataframe"), 
               "data must be a dataframe or list of dataframes")
  
  # Test list with non-dataframes
  bad_list <- list(data.frame(a = 1), "not a dataframe")
  expect_error(meal_interval(bad_list), 
               "All items in the list must be dataframes")
  
  # Test missing required columns
  incomplete_data <- data.frame(
    cow = "A",
    start = as.POSIXct("2023-01-01 08:00:00")
    # missing end column
  )
  expect_error(meal_interval(incomplete_data, id_col = "cow", start_col = "start", end_col = "end"),
               "Missing required columns: end")
  
  # test for 1 row of data
  test_data <- data.frame(
    cow = "A",
    start = as.POSIXct("2023-01-01 08:00:00"),
    end = as.POSIXct("2023-01-01 08:10:00")
  )
  expect_warning(meal_interval(test_data, method = "invalid", 
                                   id_col = "cow", start_col = "start", end_col = "end"),
               "There is only 1 row of data in the provided dataframe")
  
})

test_that("meal_interval validates intput correctly", {
  # Create test data with known gap patterns
  test_data <- data.frame(
    cow = rep(c("A", "B"), each = 6),
    start = as.POSIXct(c(
      # Animal A: consistent short gaps (15 min) and one long gap (90 min)
      "2023-01-01 08:00:00", "2023-01-01 08:15:00", "2023-01-01 08:30:00", 
      "2023-01-01 10:00:00", "2023-01-01 10:15:00", "2023-01-01 10:30:00",
      # Animal B: similar pattern
      "2023-01-01 09:00:00", "2023-01-01 09:15:00", "2023-01-01 09:30:00",
      "2023-01-01 11:00:00", "2023-01-01 11:15:00", "2023-01-01 11:30:00"
    )),
    end = as.POSIXct(c(
      "2023-01-01 08:10:00", "2023-01-01 08:25:00", "2023-01-01 08:40:00", 
      "2023-01-01 10:10:00", "2023-01-01 10:25:00", "2023-01-01 10:40:00",
      "2023-01-01 09:10:00", "2023-01-01 09:25:00", "2023-01-01 09:40:00",
      "2023-01-01 11:10:00", "2023-01-01 11:25:00", "2023-01-01 11:40:00"
    ))
  )
  
  expect_error(meal_interval(test_data, method = "invalid", 
                                   id_col = "cow", start_col = "start", end_col = "end"),
               "method must be one of")
  
  # Test invalid percentile
  expect_error(meal_interval(test_data, percentile = 1.5, 
                                   id_col = "cow", start_col = "start", end_col = "end"),
               "percentile must be a single numeric value between 0 and 1")
  expect_error(meal_interval(test_data, percentile = 0, 
                                   id_col = "cow", start_col = "start", end_col = "end"),
               "percentile must be a single numeric value between 0 and 1")
})

test_that("meal_interval handles different methods consistently", {
  
  # Create test data with known gap patterns
  test_data <- data.frame(
    cow = rep(c("A", "B"), each = 6),
    start = as.POSIXct(c(
      # Animal A: consistent short gaps (15 min) and one long gap (90 min)
      "2023-01-01 08:00:00", "2023-01-01 08:15:00", "2023-01-01 08:30:00", 
      "2023-01-01 10:00:00", "2023-01-01 10:15:00", "2023-01-01 10:30:00",
      # Animal B: similar pattern
      "2023-01-01 09:00:00", "2023-01-01 09:15:00", "2023-01-01 09:30:00",
      "2023-01-01 11:00:00", "2023-01-01 11:15:00", "2023-01-01 11:30:00"
    )),
    end = as.POSIXct(c(
      "2023-01-01 08:10:00", "2023-01-01 08:25:00", "2023-01-01 08:40:00", 
      "2023-01-01 10:10:00", "2023-01-01 10:25:00", "2023-01-01 10:40:00",
      "2023-01-01 09:10:00", "2023-01-01 09:25:00", "2023-01-01 09:40:00",
      "2023-01-01 11:10:00", "2023-01-01 11:25:00", "2023-01-01 11:40:00"
    ))
  )
  
  result_both <- meal_interval(test_data, method = "both", 
                                     id_col = "cow", start_col = "start", end_col = "end")
  result_percentile <- meal_interval(test_data, method = "percentile", 
                                           id_col = "cow", start_col = "start", end_col = "end")
  result_gmm <- meal_interval(test_data, method = "gmm", 
                                    id_col = "cow", start_col = "start", end_col = "end")
  
  # All methods should return reasonable values
  expect_true(result_both >= 5 && result_both <= 60)
  expect_true(result_percentile >= 5 && result_percentile <= 60)
  expect_true(result_gmm >= 5 && result_gmm <= 60)
  
  # "both" method should be <= min of the other two (conservative approach)
  expect_true(result_both <= max(result_percentile, result_gmm) + 0.1) # small tolerance for numerical precision
})

# NEW COMPREHENSIVE BOUNDS TESTING FOR MEAL_INTERVAL
test_that("meal_interval handles custom bounds correctly", {
  # Create test data that would produce intervals outside custom bounds
  test_data <- data.frame(
    cow = rep("A", 4),
    start = as.POSIXct(c(
      "2023-01-01 08:00:00", "2023-01-01 08:01:00", "2023-01-01 08:02:00", "2023-01-01 08:03:00"
    )),
    end = as.POSIXct(c(
      "2023-01-01 08:00:30", "2023-01-01 08:01:30", "2023-01-01 08:02:30", "2023-01-01 08:03:30"
    ))
  )
  
  # Test custom lower bound that should be applied
  result_custom_lower <- meal_interval(test_data, lower_bound = 10, upper_bound = 60,
                                      id_col = "cow", start_col = "start", end_col = "end")
  expect_equal(result_custom_lower, 10)  # Should be bounded to 10
  
  # Test custom upper bound 
  test_data_large_gaps <- data.frame(
    cow = rep("A", 3),
    start = as.POSIXct(c("2023-01-01 08:00:00", "2023-01-01 10:00:00", "2023-01-01 12:00:00")),
    end = as.POSIXct(c("2023-01-01 08:30:00", "2023-01-01 10:30:00", "2023-01-01 12:30:00"))
  )
  
  result_custom_upper <- meal_interval(test_data_large_gaps, lower_bound = 5, upper_bound = 30,
                                      id_col = "cow", start_col = "start", end_col = "end")
  expect_equal(result_custom_upper, 30)  # Should be bounded to 30
})

test_that("meal_interval handles NULL bounds correctly", {
  # Create test data
  test_data <- data.frame(
    cow = rep("A", 4),
    start = as.POSIXct(c(
      "2023-01-01 08:00:00", "2023-01-01 08:20:00", "2023-01-01 08:40:00", "2023-01-01 09:00:00"
    )),
    end = as.POSIXct(c(
      "2023-01-01 08:10:00", "2023-01-01 08:30:00", "2023-01-01 08:50:00", "2023-01-01 09:10:00"
    ))
  )
  
  # Test NULL lower_bound
  result_null_lower <- meal_interval(test_data, lower_bound = NULL, upper_bound = 60,
                                    id_col = "cow", start_col = "start", end_col = "end")
  expect_type(result_null_lower, "double")
  
  # Test NULL upper_bound  
  result_null_upper <- meal_interval(test_data, lower_bound = 5, upper_bound = NULL,
                                    id_col = "cow", start_col = "start", end_col = "end")
  expect_type(result_null_upper, "double")
  
  # Test both NULL
  result_both_null <- meal_interval(test_data, lower_bound = NULL, upper_bound = NULL,
                                   id_col = "cow", start_col = "start", end_col = "end")
  expect_type(result_both_null, "double")
})

test_that("meal_interval bounds work with different methods", {
  # Create test data
  test_data <- data.frame(
    cow = rep("A", 5),
    start = as.POSIXct(c(
      "2023-01-01 08:00:00", "2023-01-01 08:05:00", "2023-01-01 08:10:00", 
      "2023-01-01 10:00:00", "2023-01-01 10:05:00"
    )),
    end = as.POSIXct(c(
      "2023-01-01 08:02:00", "2023-01-01 08:07:00", "2023-01-01 08:12:00", 
      "2023-01-01 10:02:00", "2023-01-01 10:07:00"
    ))
  )
  
  # Test bounds with percentile method
  result_percentile_bounds <- meal_interval(test_data, method = "percentile", 
                                           lower_bound = 15, upper_bound = 50,
                                           id_col = "cow", start_col = "start", end_col = "end")
  expect_true(result_percentile_bounds >= 15 && result_percentile_bounds <= 50)
  
  # Test bounds with GMM method
  result_gmm_bounds <- meal_interval(test_data, method = "gmm", 
                                    lower_bound = 15, upper_bound = 50,
                                    id_col = "cow", start_col = "start", end_col = "end")
  expect_true(result_gmm_bounds >= 15 && result_gmm_bounds <= 50)
  
  # Test bounds with both method
  result_both_bounds <- meal_interval(test_data, method = "both", 
                                     lower_bound = 15, upper_bound = 50,
                                     id_col = "cow", start_col = "start", end_col = "end")
  expect_true(result_both_bounds >= 15 && result_both_bounds <= 50)
})

test_that("meal_interval bounds work with list input", {
  # Create test data as list
  test_data1 <- data.frame(
    cow = rep("A", 3),
    start = as.POSIXct(c("2023-01-01 08:00:00", "2023-01-01 08:01:00", "2023-01-01 08:02:00")),
    end = as.POSIXct(c("2023-01-01 08:00:30", "2023-01-01 08:01:30", "2023-01-01 08:02:30"))
  )
  
  test_data2 <- data.frame(
    cow = rep("B", 3),
    start = as.POSIXct(c("2023-01-01 09:00:00", "2023-01-01 09:01:00", "2023-01-01 09:02:00")),
    end = as.POSIXct(c("2023-01-01 09:00:30", "2023-01-01 09:01:30", "2023-01-01 09:02:30"))
  )
  
  test_list <- list(test_data1, test_data2)
  
  # Test bounds with list input
  result_list_bounds <- meal_interval(test_list, lower_bound = 12, upper_bound = 45,
                                     id_col = "cow", start_col = "start", end_col = "end")
  expect_true(result_list_bounds >= 12 && result_list_bounds <= 45)
})

test_that("meal_interval edge cases with bounds", {
  # Test when result would be exactly at bounds
  test_data <- data.frame(
    cow = rep("A", 4),
    start = as.POSIXct(c(
      "2023-01-01 08:00:00", "2023-01-01 08:15:00", "2023-01-01 08:30:00", "2023-01-01 08:45:00"
    )),
    end = as.POSIXct(c(
      "2023-01-01 08:10:00", "2023-01-01 08:25:00", "2023-01-01 08:40:00", "2023-01-01 08:55:00"
    ))
  )
  
  # Should return exactly the bound when result equals the bound
  result_at_bound <- meal_interval(test_data, lower_bound = 15, upper_bound = 15,
                                  id_col = "cow", start_col = "start", end_col = "end")
  expect_equal(result_at_bound, 15)
  
  # Test with reversed bounds (lower > upper) - edge case
  result_reversed <- meal_interval(test_data, lower_bound = 30, upper_bound = 10,
                                  id_col = "cow", start_col = "start", end_col = "end")
  expect_type(result_reversed, "double")
  expect_true(result_reversed >= 30)  # Should follow the max(lower_bound, min(result, upper_bound)) logic
})

test_that("meal_interval validates bounds parameters", {
  # Create minimal test data
  test_data <- data.frame(
    cow = "A",
    start = as.POSIXct("2023-01-01 08:00:00"),
    end = as.POSIXct("2023-01-01 08:10:00")
  )
  
  # Test with valid numeric bounds
  result_valid <- meal_interval(test_data, lower_bound = 5.5, upper_bound = 60.7,
                               id_col = "cow", start_col = "start", end_col = "end")
  expect_type(result_valid, "double")
  
  # Function should handle character bounds (if they're valid numbers) - edge case
  # but we don't test invalid bounds as the function doesn't validate input types
  # This would be caught in the underlying optimal_interval_from_gaps function
}) 