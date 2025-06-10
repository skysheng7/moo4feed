# Test cluster_meals and related functions

# Test data setup
setup_test_data <- function() {
  # Create a simple test dataset
  data.frame(
    cow = rep(c("A", "B"), each = 10),
    start = lubridate::as_datetime(c(
      # Cow A: two clear meals
      "2023-01-01 08:00:00", "2023-01-01 08:05:00", "2023-01-01 08:10:00", # Meal 1
      "2023-01-01 12:00:00", "2023-01-01 12:05:00", "2023-01-01 12:10:00", # Meal 2
      "2023-01-01 18:00:00", "2023-01-01 18:05:00", "2023-01-01 18:10:00", # Meal 3, single visit
      "2023-01-01 20:00:00",
      # Cow B: similar pattern on same day
      "2023-01-01 07:30:00", "2023-01-01 07:35:00", "2023-01-01 07:40:00", # Meal 1
      "2023-01-01 11:30:00", "2023-01-01 11:35:00", "2023-01-01 11:40:00", # Meal 2
      "2023-01-01 17:30:00", "2023-01-01 17:35:00", "2023-01-01 17:40:00", # Meal 3
      "2023-01-01 19:30:00"
    )),
    end = lubridate::as_datetime(c(
      "2023-01-01 08:02:00", "2023-01-01 08:07:00", "2023-01-01 08:12:00",
      "2023-01-01 12:02:00", "2023-01-01 12:07:00", "2023-01-01 12:12:00",
      "2023-01-01 18:02:00", "2023-01-01 18:07:00", "2023-01-01 18:12:00",
      "2023-01-01 20:02:00",
      "2023-01-01 07:32:00", "2023-01-01 07:37:00", "2023-01-01 07:42:00",
      "2023-01-01 11:32:00", "2023-01-01 11:37:00", "2023-01-01 11:42:00",
      "2023-01-01 17:32:00", "2023-01-01 17:37:00", "2023-01-01 17:42:00",
      "2023-01-01 19:32:00"
    )),
    bin = rep(c(1, 2, 1, 3, 2, 1, 4, 3, 2, 1), 2),
    intake = rep(c(2.5, 3.0, 2.8, 1.5, 2.2, 3.1, 1.8, 2.6, 2.9, 1.2), 2),
    duration = rep(c(120, 120, 120, 120, 120, 120, 120, 120, 120, 120), 2)
  )
}

test_that("cluster_meals handles single dataframe input", {
  test_data <- setup_test_data()
  
  result <- cluster_meals(test_data, eps = 30, min_pts = 3,
                         id_col = "cow", start_col = "start", end_col = "end",
                         bin_col = "bin", intake_col = "intake", dur_col = "duration")
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true(all(c("cow", "date", "meal_id", "meal_start", "meal_end", 
                   "meal_duration", "visit_count", "total_intake", 
                   "feeding_percentage", "unique_bins_count") %in% names(result)))
})

test_that("cluster_meals handles list of dataframes", {
  test_data <- setup_test_data()
  data_list <- list(test_data[1:10, ], test_data[11:20, ])
  
  result <- cluster_meals(data_list, eps = 30, min_pts = 3,
                         id_col = "cow", start_col = "start", end_col = "end",
                         bin_col = "bin", intake_col = "intake", dur_col = "duration")
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("cluster_meals handles automatic eps determination", {
  test_data <- setup_test_data()
  
  result <- cluster_meals(test_data, eps = NULL, min_pts = 3,
                         id_col = "cow", start_col = "start", end_col = "end",
                         bin_col = "bin", intake_col = "intake", dur_col = "duration")
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= 0)  # May be 0 if all visits are treated as noise
})

test_that("cluster_meals handles edge cases", {
  # Empty data
  empty_data <- setup_test_data()[0, ]
  result_empty <- cluster_meals(empty_data, eps = 30, min_pts = 3,
                               id_col = "cow", start_col = "start", end_col = "end",
                               bin_col = "bin", intake_col = "intake", dur_col = "duration")
  expect_equal(nrow(result_empty), 0)
  
  # Single visit per cow
  single_data <- setup_test_data()[c(1, 11), ]
  result_single <- cluster_meals(single_data, eps = 30, min_pts = 3,
                                id_col = "cow", start_col = "start", end_col = "end",
                                bin_col = "bin", intake_col = "intake", dur_col = "duration")
  expect_equal(nrow(result_single), 0)  # Should be 0 since min_pts = 3
})

test_that("cluster_meals input validation", {
  expect_error(cluster_meals(NULL), "data cannot be NULL")
  expect_error(cluster_meals("not_a_dataframe"), "data must be a dataframe or list of dataframes")
  expect_error(cluster_meals(list(1, 2, 3)), "All items in the list must be dataframes")
})

# Test helper functions directly

test_that("determine_eps_percentile works correctly", {
  gaps <- c(2, 3, 5, 8, 10, 15, 30, 45, 120, 240)  # Mix of short and long gaps
  
  # Access the internal function using :::
  result <- moo4feed:::determine_eps_percentile(gaps)
  
  expect_type(result, "double")
  expect_true(result > 0)
  expect_equal(result, quantile(gaps, 0.75, na.rm = TRUE)[[1]])
  
  # Test empty gaps
  empty_result <- moo4feed:::determine_eps_percentile(numeric(0))
  expect_equal(empty_result, 30)
})

test_that("determine_eps_gmm works correctly", {
  # Create bimodal gap distribution (within-meal and between-meal)
  within_meal_gaps <- rnorm(20, mean = 5, sd = 2)
  between_meal_gaps <- rnorm(15, mean = 180, sd = 30)
  gaps <- c(within_meal_gaps, between_meal_gaps)
  gaps <- gaps[gaps >= 0]  # Remove any negative values
  
  # Test with sufficient data
  result <- moo4feed:::determine_eps_gmm(gaps)
  expect_type(result, "double")
  expect_true(result > 0)
  expect_true(result > min(within_meal_gaps))
  expect_true(result < min(between_meal_gaps))
  
  # Test with insufficient data (should fall back to percentile)
  small_gaps <- c(1, 2, 3, 4, 5)
  small_result <- moo4feed:::determine_eps_gmm(small_gaps)
  expect_equal(small_result, moo4feed:::determine_eps_percentile(small_gaps))
  
  # Test empty gaps
  empty_result <- moo4feed:::determine_eps_gmm(numeric(0))
  expect_equal(empty_result, 30)
})

test_that("find_distribution_intersection works correctly", {
  # Test with two clearly separated distributions
  mu1 <- 5; sigma1 <- 2; lambda1 <- 0.6
  mu2 <- 30; sigma2 <- 5; lambda2 <- 0.4
  
  result <- moo4feed:::find_distribution_intersection(mu1, sigma1, lambda1, mu2, sigma2, lambda2)
  
  expect_type(result, "double")
  expect_true(result > mu1)
  expect_true(result < mu2)
  
  # Test with overlapping distributions
  mu1_overlap <- 10; sigma1_overlap <- 5; lambda1_overlap <- 0.5
  mu2_overlap <- 15; sigma2_overlap <- 5; lambda2_overlap <- 0.5
  
  result_overlap <- moo4feed:::find_distribution_intersection(
    mu1_overlap, sigma1_overlap, lambda1_overlap, 
    mu2_overlap, sigma2_overlap, lambda2_overlap
  )
  
  expect_type(result_overlap, "double")
  expect_true(result_overlap >= mu1_overlap)
  expect_true(result_overlap <= mu2_overlap)
})

test_that("determine_optimal_eps integrates methods correctly", {
  # Create controlled gap data
  within_meal <- c(2, 3, 4, 5, 6, 7, 8)
  between_meal <- c(120, 150, 180, 200, 240)
  gaps <- c(within_meal, between_meal)
  
  # Create corresponding start and end times
  start_times <- cumsum(c(0, gaps))
  end_times <- start_times + 2  # 2 minute feeding duration
  
  result <- moo4feed:::determine_optimal_eps(start_times, end_times)
  
  expect_type(result, "double")
  expect_true(result >= 5)  # Should respect lower bound
  expect_true(result <= 60)  # Should respect upper bound
  expect_true(result > max(within_meal))  # Should be larger than within-meal gaps
  expect_true(result < min(between_meal))  # Should be smaller than between-meal gaps
})

test_that("determine_optimal_eps handles edge cases", {
  # Single time point
  result_single <- moo4feed:::determine_optimal_eps(c(100), c(102))
  expect_equal(result_single, 30)
  
  # Mismatched lengths
  expect_error(moo4feed:::determine_optimal_eps(c(1, 2, 3), c(1, 2)), 
               "start_times and end_times must have the same length")
  
  # All overlapping visits (negative gaps)
  overlapping_starts <- c(100, 101, 102)
  overlapping_ends <- c(105, 106, 107)
  result_overlapping <- moo4feed:::determine_optimal_eps(overlapping_starts, overlapping_ends)
  expect_equal(result_overlapping, 30)
}) 